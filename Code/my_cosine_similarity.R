start.time <- Sys.time()
setwd("E:/Documents/MLearn/Projects/Coupon/refer")
source("../refer/myutil.R")
validate <- TRUE
quick <- TRUE
#read in all the input data
cpdtr <- read.csv("../input_uk/coupon_detail_train_en.csv")
cpltr <- read.csv("../input_uk/coupon_list_train_en.csv")
cplte <- read.csv("../input_uk/coupon_list_test_en.csv")
ulist <- read.csv("../input_uk/user_list_en.csv")
if (!quick) {
  cpvtr <-
    read.csv("../input_uk/coupon_visit_train_en.csv",nrows = -1) # big file
}


cpltr.ar <- read.csv("../input_uk/coupon_area_train_en.csv")
cplte.ar <- read.csv("../input_uk/coupon_area_test_en.csv")

if (validate) {
  start.date <- as.Date("2012-6-17")
  end.date <- as.Date("2012-6-23")
  vali.result <- cpdtr[as.Date(cpdtr$I_DATE) >= start.date, c(5,6)]
  #factor is also a bad thing ,it may show value of levels rather than the real value
    vali.result$COUPON_ID_hash <- as.character(vali.result$COUPON_ID_hash)
    vali.result <- aggregate(.~USER_ID_hash, data = vali.result, FUN = function(seri) {
      paste(as.character(seri), collapse = " ")
    })
    vali.result <- merge(ulist["USER_ID_hash"], vali.result, all.x = T)
    write.csv(vali.result, file = paste("../validate/uc_",
                                        format(start.date, format = "_%Y-%m-%d"),
                                        format(end.date, format = "_%Y-%m-%d"),
                                        ".csv",
                                        sep = ""),
              quote = F, row.names = F)
  cpdtr <- cpdtr[as.Date(cpdtr$I_DATE) < start.date,]
  #   write.csv(cpdtr, file = paste("../validate/detail_train_en",
  #                                 format(start.date, format = "_%Y-%m-%d"),
  #                                 ".csv", sep = ""),
  #             quote = FALSE,row.names = FALSE)
  cplte <- cpltr[as.Date(cpltr$DISPFROM) >= start.date,]
  cpltr <- cpltr[as.Date(cpltr$DISPFROM) < start.date,]
  if (!quick) {
    cpvtr <-
      cpvtr[as.Date(cpvtr$I_DATE) < start.date, c("VIEW_COUPON_ID_hash","USER_ID_hash")]
  }
  cplte.ar <- merge(cpltr.ar, cplte["COUPON_ID_hash"])
}
if (!quick) {
  cpvtr <-
    cpvtr[cpvtr$PURCHASE_FLG != 1,c("VIEW_COUPON_ID_hash","USER_ID_hash")]
}

#get the prefer matrix of user to coupon feature
##get feature of coupon

FeatureFormat <- function(df, features) {
  df <- df[, features]
  df$DISCOUNT_PRICE <- 1 / log10(df$DISCOUNT_PRICE)
  df$DISPPERIOD[df$DISPPERIOD > 7] <-
    7;df$DISPPERIOD <- df$DISPPERIOD / 7
  df$USABLE_DATE_sum <- df$USABLE_DATE_sum / 9
  df[is.na(df)] <- 1  ##replace NA
  df <- cbind(df[, c(1,2)], model.matrix( ~ -1 + ., df[,-c(1,2)],
                                          contrasts.arg = lapply(df[,names(which(sapply(df[,-c(1,2)], is.factor) == TRUE))],
                                                                 contrasts, contrasts = FALSE)))
  df
}

# fix cpltr$VALIDPERIOD error where valid should be VALIDEND minu VALIDFROM plus one!
# Feature engineering, put into factor of 0s for NAs and 1s actual values
cpltr$VALIDPERIOD[is.na(cpltr$VALIDPERIOD)] <- -1
cpltr$VALIDPERIOD <- cpltr$VALIDPERIOD + 1
cpltr$VALIDPERIOD[cpltr$VALIDPERIOD > 0] <- 1
cpltr$VALIDPERIOD <- as.factor(cpltr$VALIDPERIOD)
cplte$VALIDPERIOD[is.na(cplte$VALIDPERIOD)] <- -1
cplte$VALIDPERIOD <- cplte$VALIDPERIOD + 1
cplte$VALIDPERIOD[cplte$VALIDPERIOD > 0] <- 1
cplte$VALIDPERIOD <- as.factor(cplte$VALIDPERIOD)

# sets up sum of coupon USABLE_DATEs for training and test dataset
for (i in 12:20) {
  cpltr[is.na(cpltr[,i]),i] <- 0;    cpltr[cpltr[,i] > 1,i] <- 1
  cplte[is.na(cplte[,i]),i] <- 0;    cplte[cplte[,i] > 1,i] <- 1
}
cpltr$USABLE_DATE_sum <- rowSums(cpltr[,12:20])
cplte$USABLE_DATE_sum <- rowSums(cplte[,12:20])

cpltr$KIND <- "train"
cplte$KIND <- "test"
cplall <- rbind(cpltr, cplte)
features <- c(
  "COUPON_ID_hash","KIND",
  "GENRE_NAME","DISCOUNT_PRICE","DISPPERIOD",
  "large_area_name","small_area_name","VALIDPERIOD","USABLE_DATE_sum"
)
#
#find that: the missing DISPFROM value as ' ' not NA
#atrribute has NA: "VALIDPERIOD", "USABLE_DATE_MON","USABLE_DATE_TUE","USABLE_DATE_WED","USABLE_DATE_THU",
#"USABLE_DATE_FRI","USABLE_DATE_SAT","USABLE_DATE_SUN","USABLE_DATE_HOLIDAY",
#"USABLE_DATE_BEFORE_HOLIDAY"

cplall.fea <- FeatureFormat(cplall, features)
cpltr.fea <-
  subset(cplall.fea[cplall.fea$KIND == "train",], select = -KIND)
cplte.fea <-
  subset(cplall.fea[cplall.fea$KIND == "test",], select = -KIND)
## user to coupon feature
u.cp <- merge(cpdtr[c("USER_ID_hash", "COUPON_ID_hash")], cpltr.fea)
u.cp$DISCOUNT_PRICE <- 1
u.cp$DISPPERIOD <- 1
u.cp$USABLE_DATE_sum <- 1
u.cp.fea <-
  aggregate(. ~ USER_ID_hash, data = subset(u.cp, select = -COUPON_ID_hash), FUN = sum)

if (!quick) {
  # Add visit training data in chunks due to large dataset
  imax <- dim(cpvtr)[1]
  i2 <- 1
  while (i2 < imax) {
    # this loop takes a few minutes
    i1 <- i2
    i2 <- i1 + 100000
    if (i2 > imax)
      i2 <- imax
    cat("Merging coupon visit data i1=",i1," i2=",i2,"\n")
    trainv <-
      merge(cpvtr[i1:i2,],cpltr, by.x = "VIEW_COUPON_ID_hash", by.y = "COUPON_ID_hash")
    trainv <- trainv[,c(
      "VIEW_COUPON_ID_hash","USER_ID_hash",
      "GENRE_NAME","DISCOUNT_PRICE","DISPPERIOD",
      "large_area_name","small_area_name","VALIDPERIOD","USABLE_DATE_sum"
    )]
    #same treatment as with coupon_detail train data
    trainv$DISCOUNT_PRICE <-
      1;trainV$DISPPERIOD <-
      1;trainV$USABLE_DATE_sum <- 1;trainv[is.na(trainv)] <- 1
    trainv <-
      cbind(trainv[,c(1,2)],model.matrix(
        ~ -1 + .,trainv[,-c(1,2)],
        contrasts.arg = lapply(trainv[,names(which(sapply(trainv[,-c(1,2)], is.factor) ==
                                                     TRUE))],
                               contrasts, contrasts =
                                 FALSE)
      ))
    # discount coupon visits relative to coupon purchases
    couponVisitFactor <- .005
    trainv[,3:dim(trainv)[2]] <-
      trainv[,3:dim(trainv)[2]] * couponVisitFactor
    u.cp.fea <-
      aggregate(. ~ USER_ID_hash, data = rbind(u.cp.fea,trainv[,-1]),FUN = sum)
  }
}

## list area feture between the buy and test list area
cpltr.ar <- cpltr.ar[c("COUPON_ID_hash", "SMALL_AREA_NAME")]
cpltr.ar$KIND <- "train"
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
cpdtr.ar.fea <-
  subset(cpall.ar.fea[cpall.ar.fea$USER_ID_hash != "TEST",], select = -COUPON_ID_hash)
u.cp.ar.fea <-
  aggregate(. ~ USER_ID_hash, data = cpdtr.ar.fea, FUN = mean)

#get the test feature
cplte.ar.fea <- aggregate(
  . ~ COUPON_ID_hash,
  data = subset(cpall.ar.fea[cpall.ar.fea$USER_ID_hash == "TEST",], select = -USER_ID_hash),
  FUN = sum
)

## combine feature
u.cp.fea <- merge(u.cp.fea, u.cp.ar.fea)
cplte.fea <- merge(cplte.fea, cplte.ar.fea)

## faciliate to use different matrix for male and female
u.cp.fea <- merge(ulist[c("USER_ID_hash", "SEX_ID")], u.cp.fea)

## get result
require(Matrix)
# cnt.GENRE_NAME = length(levels(cplall$GENRE_NAME))
# cnt.large_area_name = length(levels(cplall$large_area_name))
# cnt.ken_name = length(levels(cplall$ken_name))
# cnt.small_area_name = length(levels(cplall$small_area_name))
cnt.SMALL_AREA_NAME = length(levels(cpall.ar$SMALL_AREA_NAME))
# set Weight Matrix
#Weight Matrix: GENRE_NAME DISCOUNT_PRICE USABLE_DATE_ large_area_name ken_name small_area_name SMALL_AREA_NAME

real <- read.csv(file = "../validate/uc__2012-06-17_2012-06-23.csv")
w.SMALL <- c(4.0, 4.1, 4.2, 4.3, 4.4, 4.5, 4.6, 4.7, 4.8, 4.9, 5.0)
v.score <- rep(0, length(w.SMALL))
require(Matrix)
for (i in 1:length(w.SMALL)) {
  # Weight matrix with 7 factors, separate for male and female users
  #Weight Matrix: GENRE_NAME, DISCOUNT_PRICE, DISPPERIOD, large_area_name, small_area_name, VALIDPERIOD, USABLE_DATE_sum, SMALL_AREA_NAME
  weightm <-
    c(2.00, 1.25, 1.25, 1.00, 4.50, 0.625, 0.35, w.SMALL[i]) # males weights
  weightf <-
    c(1.75, 0.75, 1.50, 1.00, 4.50, 0.625, 0.25, w.SMALL[i]) # female weights
  Wm <-
    as.matrix(Diagonal(x = c(
      rep(weightm[1],13), rep(weightm[2],1), rep(weightm[3],1), rep(weightm[4],9),
      rep(weightm[5],55),rep(weightm[6],2),rep(weightm[7],1),rep(weightm[8],cnt.SMALL_AREA_NAME)
    )))
  Wf <-
    as.matrix(Diagonal(x = c(
      rep(weightf[1],13), rep(weightf[2],1), rep(weightf[3],1), rep(weightf[4],9),
      rep(weightf[5],55),rep(weightf[6],2),rep(weightf[7],1),rep(weightf[8], cnt.SMALL_AREA_NAME)
    )))
  all <- data.frame()
  score <-
    as.matrix(subset(u.cp.fea, select = c(-USER_ID_hash,-SEX_ID))) %*% Wm %*% t(as.matrix(cplte.fea[,-1]))
  score[u.cp.fea$SEX_ID == 'f',] <-
    as.matrix(subset(u.cp.fea[u.cp.fea$SEX_ID == 'f',], select = c(-USER_ID_hash,-SEX_ID))) %*% Wf %*% t(as.matrix(cplte.fea[,-1]))
  u.cp.fea$PURCHASED_COUPONS <-
    sapply(
      1:nrow(u.cp.fea), FUN = function(j) {
        pur.cp <-
          paste(cplte.fea$COUPON_ID_hash[order(score[j,], decreasing = TRUE)[1:10]],
                collapse = " ")###use of paste  about collapse
        return(pur.cp)
      }
    )
  result <-
    merge(ulist, u.cp.fea, all.x = T)[,c("USER_ID_hash", "PURCHASED_COUPONS")]
  if (validate) {
    all <- merge(real, result)
    v.score[i] <-
      my.mapk(10, as.character(all$COUPON_ID_hash), as.character(all$PURCHASED_COUPONS))
    u.cp.fea <- subset(u.cp.fea, select = -PURCHASED_COUPONS)
  }
}

if (validate) {
  test.result <- data.frame(w.SMALL, v.score)
}

if (validate) {
  dir.res <- "validate"
} else {
  dir.res <- "result"
}
write.csv(
  result, file = paste(
    "../", dir.res, "/", format(Sys.time(), format = "%m-%d-%H-%M_"), "submission.csv", sep = ""
  ), quote = FALSE,row.names = FALSE
)
end.time <- Sys.time()
proce.time <- end.time - start.time