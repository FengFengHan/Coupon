setwd("E:/Documents/MLearn/Projects/Coupon/refer")
source("./myutil.R")
#read in all the input data
cpdtr <- read.csv("../input_uk/coupon_detail_train_en.csv")
cpltr <- read.csv("../input_uk/coupon_list_train_en.csv")
cplte <- read.csv("../input_uk/coupon_list_test_en.csv")
ulist <- read.csv("../input_uk/user_list_en.csv")
cpltr.ar <- read.csv("../input_uk/coupon_area_train_en.csv")
cplte.ar <- read.csv("../input_uk/coupon_area_test_en.csv")
validate <- FALSE
if(validate) {
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
  cplte.ar <- cpltr.ar
}
#get the prefer matrix of user to coupon feature
##get feature of purchase and test
hasNa <- function(seri) {
  any(is.na(seri))
}

fill.na <- function(seri) {
  if (hasNa(seri)) {
    seri <- as.numeric(as.character(seri))
    seri[is.na(seri)] <- mean(seri, na.rm = TRUE);
  }
  seri
}

FeatureFormat <- function(df, features) {
  df <- df[, features]
  df$DISCOUNT_PRICE <- 1/log10(df$DISCOUNT_PRICE)
  df$PRICE_RATE <- (df$PRICE_RATE*df$PRICE_RATE)/(100*100)
  df[is.na(df)] <- 1  ##replace NA
  df <- cbind(df[, c(1,2)], model.matrix(~ -1 + ., df[, -c(1,2)],
                                    contrasts.arg = lapply(
                                      df[,names(which(sapply(df[,-c(1,2)], is.factor) == TRUE))], 
                                      contrasts, contrasts = FALSE)))
  df
}
cpltr$KIND <- "train"
cplte$KIND <- "test"
cplall <- rbind(cpltr, cplte)
features <- c("COUPON_ID_hash","KIND",
              "GENRE_NAME","DISCOUNT_PRICE","PRICE_RATE",
             "USABLE_DATE_MON","USABLE_DATE_TUE","USABLE_DATE_WED","USABLE_DATE_THU",
             "USABLE_DATE_FRI","USABLE_DATE_SAT","USABLE_DATE_SUN","USABLE_DATE_HOLIDAY",
             "USABLE_DATE_BEFORE_HOLIDAY","large_area_name","ken_name")
# 
#find that: the missing DISPFROM value as ' ' not NA
#atrribute has NA: "VALIDPERIOD", "USABLE_DATE_MON","USABLE_DATE_TUE","USABLE_DATE_WED","USABLE_DATE_THU",
#"USABLE_DATE_FRI","USABLE_DATE_SAT","USABLE_DATE_SUN","USABLE_DATE_HOLIDAY",
#"USABLE_DATE_BEFORE_HOLIDAY"
# index <- which(sapply(cplall, FUN = hasNa))
# index.hasNa <- names(cplall)[index]
cplall.fea <- FeatureFormat(cplall, features)
cpltr.fea <- subset(cplall.fea[cplall.fea$KIND == "train",], select = -KIND)
cplte.fea <- subset(cplall.fea[cplall.fea$KIND == "test",], select = -KIND)




#list area Feature
cpltr.ar <- cpltr.ar[c("COUPON_ID_hash", "SMALL_AREA_NAME")]
cpltr.ar$KIND <- "train"
cplte.ar <- cplte.ar[c("COUPON_ID_hash", "SMALL_AREA_NAME")]
cplte.ar$KIND <- "test"
cpdtr.ar <- cpdtr[c("COUPON_ID_hash", "SMALL_AREA_NAME")]
cpdtr.ar$KIND <- "purchase"
cpall.ar <- rbind(cpltr.ar, cplte.ar, cpdtr.ar)
cpall.ar.fea <- cbind(cpall.ar[c("KIND", "COUPON_ID_hash")] ,model.matrix(~-1 + SMALL_AREA_NAME,
                                              cpall.ar["SMALL_AREA_NAME"]))
# cpltr.ar.fea <- aggregate(.~COUPON_ID_hash, 
#                           data = subset(cpall.ar.fea[cplall.ar.fea$KIND == "train",], select = -KIND),
#                           FUN = function(x) {
#                             return(0)
#                           })
cplte.ar.fea <- aggregate(.~COUPON_ID_hash, 
                          data = subset(cpall.ar.fea[cpall.ar.fea$KIND == "test",], select = -KIND),
                          FUN = sum)
cpdtr.ar.fea <- subset(cpall.ar.fea[cpall.ar.fea$KIND == "purchase",], select = -KIND)
#combine features
cpdtr.fea <- cbind(cpdtr["USER_ID_hash"], cpdtr.ar.fea)
u.cp <- cbind(merge(cpdtr.fea[c("USER_ID_hash", "COUPON_ID_hash")], cpltr.fea),
              subset(cpdtr.fea, select = c(-USER_ID_hash, -COUPON_ID_hash)))




## user to coupon feature
u.cp$DISCOUNT_PRICE <- 1
u.cp$PRICE_RATE <- 1

u.cpfea <- aggregate(.~USER_ID_hash, data =subset(u.cp, select = -COUPON_ID_hash), FUN = mean)

# set Weight Matrix
#Weight Matrix: GENRE_NAME DISCOUNT_PRICE PRICE_RATE USABLE_DATE_ large_area_name ken_name small_area_name
require(Matrix)
cnt.GENRE_NAME = length(levels(cplall$GENRE_NAME))
cnt.large_area_name = length(levels(cplall$large_area_name))
cnt.ken_name = length(levels(cplall$ken_name))
cnt.small_area_name = length(levels(cplall$small_area_name))
W <- as.matrix(Diagonal(x=c(rep(2.05, cnt.GENRE_NAME), rep(2,1), rep(-0.13,1), rep(0,9), rep(0.5,cnt.large_area_name), rep(1.01,cnt.ken_name), rep(2,cnt.small_area_name))))

# get result
score <- as.matrix(u.cpfea[,-1]) %*% W %*% t(as.matrix(cplte.fea[,-1]))
u.cpfea$PURCHASED_COUPONS <- sapply(1:nrow(u.cpfea), FUN = function(i){
  pur.cp <- paste( cplte$COUPON_ID_hash[order(score[i,], decreasing = TRUE)[1:10]], 
                   collapse = " ")###use of paste  about collapse
  return(pur.cp)
})
result <- merge(ulist, u.cpfea, all.x = T)
result <- result[,c("USER_ID_hash", "PURCHASED_COUPONS")]
if (validate) {
  dir.res <- "validate"
} else {
  dir.res <- "result"
}
write.csv(result, file = paste("../", dir.res, format(Sys.time(), format = "%m-%d-%H-%M_") + "/cosine_similarity_ar.csv", sep = ""), quote = FALSE,row.names = FALSE)


