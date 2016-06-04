# system("ls ../input")
# system("echo \n\n")
# system("head ../input/*")
### Kaggle Scripts: Ponpare Coupon Purchase Prediction ###
### Original Author: Subhajit Mandal ###
### Some score improving changes by Fred H Seymour
start.time <- Sys.time()
setwd("E:/Documents/MLearn/Projects/Coupon/refer")
source("../refer/myutil.R")
dir <- '../input_uk/'
validate <- TRUE
validate.dir <- "../validate_2012-06-10_2012-06-16/"
if (validate) {
  dir <- validate.dir
}
cat("Reading data\n")
cpdtr <- read.csv(paste0(dir,"coupon_detail_train_en.csv"))
cpltr <- read.csv(paste0(dir,"coupon_list_train_en.csv"))
cplte <- read.csv(paste0(dir,"coupon_list_test_en.csv"))
ulist <- read.csv(paste0(dir,"user_list_en.csv"))
cpvtr <- read.csv(paste0(dir,"coupon_visit_train_en.csv"),nrows=-1) # big file
cpvtr <- cpvtr[cpvtr$PURCHASE_FLG!=1,c("VIEW_COUPON_ID_hash","USER_ID_hash")]
# fix cpltr$VALIDPERIOD error where valid should be VALIDEND minu VALIDFROM plus one!
# Feature engineering, put into factor of 0s for NAs and 1s actual values
cpltr$VALIDPERIOD[is.na(cpltr$VALIDPERIOD)] <- -1
cpltr$VALIDPERIOD <- cpltr$VALIDPERIOD+1
cpltr$VALIDPERIOD[cpltr$VALIDPERIOD>0] <- 1
cpltr$VALIDPERIOD <- as.factor(cpltr$VALIDPERIOD)
cplte$VALIDPERIOD[is.na(cplte$VALIDPERIOD)] <- -1
cplte$VALIDPERIOD <- cplte$VALIDPERIOD+1
cplte$VALIDPERIOD[cplte$VALIDPERIOD>0] <- 1
cplte$VALIDPERIOD <- as.factor(cplte$VALIDPERIOD)

# sets up sum of coupon USABLE_DATEs for training and test dataset
for (i in 12:20) {
  cpltr[is.na(cpltr[,i]),i] <- 0;    cpltr[cpltr[,i]>1,i] <- 1
  cplte[is.na(cplte[,i]),i] <- 0;    cplte[cplte[,i]>1,i] <- 1
}
cpltr$USABLE_DATE_sum <- rowSums(cpltr[,12:20])
cplte$USABLE_DATE_sum <- rowSums(cplte[,12:20])

# start train set by merging coupon_detail_train and coupon_list_train
# to get USER_ID_hash by coupon
train <- merge(cpdtr,cpltr)
train <- train[,c("COUPON_ID_hash","USER_ID_hash",
                  "GENRE_NAME","DISCOUNT_PRICE","DISPPERIOD",
                  "large_area_name","small_area_name","VALIDPERIOD","USABLE_DATE_sum")]
# append test set to the training set for model.matrix factor column conversion
cplte$USER_ID_hash <- "dummyuser"
cpchar <- cplte[,c("COUPON_ID_hash","USER_ID_hash",
                   "GENRE_NAME","DISCOUNT_PRICE","DISPPERIOD",
                   "large_area_name","small_area_name","VALIDPERIOD","USABLE_DATE_sum")]
train <- rbind(train,cpchar)

#NA imputation to values of 1
train[is.na(train)] <- 1
#feature engineering
train$DISCOUNT_PRICE <- 1/log10(train$DISCOUNT_PRICE)    
train$DISPPERIOD[train$DISPPERIOD>7] <- 7;train$DISPPERIOD <- train$DISPPERIOD/7
train$USABLE_DATE_sum <- train$USABLE_DATE_sum/9

#convert the factors to columns of 0's and 1's
train <- cbind(train[,c(1,2)],model.matrix(~ -1 + .,train[,-c(1,2)],
                                           contrasts.arg=lapply(train[,names(which(sapply(train[,-c(1,2)], is.factor)==TRUE))], 
                                                                contrasts, contrasts=FALSE)))
#separate the test from train following factor column conversion
test <- train[train$USER_ID_hash=="dummyuser",]
test <- test[,-2]
train <- train[train$USER_ID_hash!="dummyuser",]

#Numeric attributes cosine multiplication factors set to 1
train$DISCOUNT_PRICE <- 1
train$DISPPERIOD <- 1
train$USABLE_DATE_sum <- 1

# Create starting uchar for all users initialized to zero
uchar <- data.frame(USER_ID_hash=ulist[,"USER_ID_hash"])
uchar <- cbind(uchar,matrix(0, nrow=dim(uchar)[1], ncol=(dim(train)[2] -2)))
names(uchar) <- names(train)[2:dim(train)[2]]

# Incorporate the purchase training data from train, use sum function    
uchar <- aggregate(.~USER_ID_hash, data=rbind(uchar,train[,-1]),FUN=sum)

# Add visit training data in chunks due to large dataset
imax <- dim(cpvtr)[1]   
i2 <- 1
while (i2 < imax) {  # this loop takes a few minutes      
  i1 <- i2
  i2 <- i1 + 100000
  if (i2 > imax) i2 <- imax
  cat("Merging coupon visit data i1=",i1," i2=",i2,"\n")
  trainv <- merge(cpvtr[i1:i2,],cpltr, by.x="VIEW_COUPON_ID_hash", by.y="COUPON_ID_hash")
  trainv <- trainv[,c("VIEW_COUPON_ID_hash","USER_ID_hash",
                      "GENRE_NAME","DISCOUNT_PRICE","DISPPERIOD",                          
                      "large_area_name","small_area_name","VALIDPERIOD","USABLE_DATE_sum")]
  #same treatment as with coupon_detail train data
  trainv$DISCOUNT_PRICE <- 1;trainv$DISPPERIOD <- 1;trainv$USABLE_DATE_sum <- 1;trainv[is.na(trainv)] <- 1
  trainv <- cbind(trainv[,c(1,2)],model.matrix(~ -1 + .,trainv[,-c(1,2)],
                                               contrasts.arg=lapply(trainv[,names(which(sapply(trainv[,-c(1,2)], is.factor)==TRUE))], 
                                                                    contrasts, contrasts=FALSE)))
  # discount coupon visits relative to coupon purchases
  couponVisitFactor <- .005      
  trainv[,3:dim(trainv)[2]] <- trainv[,3:dim(trainv)[2]] * couponVisitFactor  
  uchar <- aggregate(.~USER_ID_hash, data=rbind(uchar,trainv[,-1]),FUN=sum)    
}

# list feature
cplte.ar <- read.csv(paste0(dir,"coupon_area_test_en.csv"))
#To make the feature same between the buy and the test list area, combine them to cpall
cplte.ar <- cplte.ar[c("COUPON_ID_hash", "SMALL_AREA_NAME")]
cplte.ar$USER_ID_hash <- "TEST"
cpdtr.ar <-
  cpdtr[c("USER_ID_hash", "COUPON_ID_hash", "SMALL_AREA_NAME")]
cpall.ar <- rbind(cplte.ar, cpdtr.ar)
cpall.ar.fea <-
  cbind(cpall.ar[c("USER_ID_hash", "COUPON_ID_hash")] ,model.matrix( ~ -1 + SMALL_AREA_NAME,
                                                                     cpall.ar["SMALL_AREA_NAME"]))
#get the u.cp feature
cpdtr.ar <-
  subset(cpall.ar.fea[cpall.ar.fea$USER_ID_hash != "TEST",], select = -COUPON_ID_hash)
uchar.ar <-
  aggregate(. ~ USER_ID_hash, data = cpdtr.ar, FUN = sum)

#get the test feature
test.ar <- aggregate(
  . ~ COUPON_ID_hash,
  data = subset(cpall.ar.fea[cpall.ar.fea$USER_ID_hash == "TEST",], select = -USER_ID_hash),
  FUN = sum
)

## combine feature
uchar <- merge(uchar, uchar.ar, all.x = T)
uchar[is.na(uchar)] <- 0
test <- merge(test, test.ar)


# Weight matrix with 7 factors, separate for male and female users 
#Weight Matrix: GENRE_NAME, DISCOUNT_PRICE, DISPPERIOD, large_area_name, small_area_name, VALIDPERIOD, USABLE_DATE_sum
require(Matrix)
SMALL <- c(0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0)
#SMALL <- c(1.6, 1.7, 1.8, 1.9, 2.0, 2.1, 2.2, 2.3, 2.4, 2.5, 2.6, 2.7, 2.8, 2.9)
if(!validate) {
  SMALL <- 2.4
}
param <- data.frame(SMALL)
param$score_ <- 0
if (validate) {
  real <- read.csv(paste0(dir, "uc.csv"))
}
for (i in c(1:dim(param[1])[1])) {
weightm <- c(2.00, 1.25, 1.25, 1.00, 4.50, 0.625, 0.35, param$SMALL[i]) # males weights
weightf <- c(1.75, 0.75, 1.50, 1.00, 4.50, 0.625, 0.25, param$SMALL[i]) # female weights
Wm <- as.matrix(Diagonal(x=c(rep(weightm[1],13), rep(weightm[2],1), rep(weightm[3],1), rep(weightm[4],9), 
                             rep(weightm[5],55),rep(weightm[6],2),rep(weightm[7],1), rep(weightm[8],55))))
Wf <- as.matrix(Diagonal(x=c(rep(weightf[1],13), rep(weightf[2],1), rep(weightf[3],1), rep(weightf[4],9), 
                             rep(weightf[5],55),rep(weightf[6],2),rep(weightf[7],1), rep(weightf[8],55))))

#calculation of cosine similarities of users and coupons
score = as.matrix(uchar[,2:ncol(uchar)]) %*% Wm %*% t(as.matrix(test[,2:ncol(test)]))
score[ulist$SEX_ID=='f',] = as.matrix(uchar[ulist$SEX_ID=='f',2:ncol(uchar)]) %*% Wf %*% t(as.matrix(test[,2:ncol(test)]))
#order the list of coupons according to similairties and take only first 10 coupons
uchar$PURCHASED_COUPONS <- do.call(rbind, lapply(1:nrow(uchar),FUN=function(i){
  purchased_cp <- paste(test$COUPON_ID_hash[order(score[i,], decreasing = TRUE)][1:10],collapse=" ")
  return(purchased_cp)
}))
#make submission
submission <- uchar[,c("USER_ID_hash","PURCHASED_COUPONS")]
# submission$PURCHASED_COUPONS[rowSums(score)==0] <- ""
if (validate) {
  all <- merge(real, submission)
  param$score_[i] <-
    my.mapk(10, as.character(all$COUPON_ID_hash), as.character(all$PURCHASED_COUPONS))
  uchar <- subset(uchar, select = -PURCHASED_COUPONS)
}
}
if (validate) {
  dir.res <- dir
} else {
  dir.res <- "result"
}
write.csv(
  submission, file = paste(
    "../",dir.res, "/", format(Sys.time(), format = "%m-%d-%H-%M_"), "submission.csv", sep = ""
  ), quote = FALSE,row.names = FALSE
)
end.time <- Sys.time()
proce.time <- end.time - start.time