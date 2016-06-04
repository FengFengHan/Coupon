setwd("E:/Documents/MLearn/Projects/Coupon/validate")
source("../refer/myutil.R")
# real <- read.csv(file = "uc__2012-06-17_2012-06-23.csv")
# # pred <- read.csv(file = "cosine_similarity_ar.csv") # score: 0.000396
# # pred <- read.csv(file = "cosine_similarity.csv") # score: 0.009872
# pred <- read.csv(file = "09-29-14-08_cosine_similarity.csv")
# all <- merge(real, pred)
# score <- my.mapk(10, as.character(all$COUPON_ID_hash), as.character(all$PURCHASED_COUPONS))

#divide the test file
start.date <- as.Date("2012-6-10")
end.date <- as.Date("2012-6-16")
cpltr <- read.csv("../input_uk/coupon_list_train_en.csv")
cpltr.ar <- read.csv("../input_uk/coupon_area_train_en.csv")
cplte <- read.csv("../input_uk/coupon_list_test_en.csv")
cplte.ar <- read.csv("../input_uk/coupon_area_test_en.csv")
cpdtr <- read.csv("../input_uk/coupon_detail_train_en.csv")
cpvtr <- read.csv("../input_uk/coupon_visit_train_en.csv",nrows = -1) # big file
ulist <- read.csv("../input_uk/user_list_en.csv")
dir <- paste0("../validate_",
              format(start.date, format("%Y-%m-%d")),
              "_",
              format(end.date, format("%Y-%m-%d")),
              "/")
cplte <- cpltr[(as.Date(cpltr$DISPFROM) >= start.date) & (as.Date(cpltr$DISPFROM) <= end.date),]
cplte.ar <- merge(cpltr.ar, cplte["COUPON_ID_hash"])
cpltr <- cpltr[as.Date(cpltr$DISPFROM) < start.date, ]
cpltr.ar <- merge(cpltr.ar, cpltr["COUPON_ID_hash"])
cpdtr$I_DATE <- as.Date(cpdtr$I_DATE)
vali.result <- cpdtr[(cpdtr$I_DATE >= start.date) & (cpdtr$I_DATE <= end.date),c(5,6)]
cpdtr <- cpdtr[cpdtr$I_DATE < start.date,]
cpvtr <- cpvtr[as.Date(cpvtr$I_DATE) < start.date,]
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

write.csv(cplte, file = paste0(dir, "coupon_list_test_en.csv"), row.names = FALSE)
write.csv(cplte.ar, file = paste0(dir, "coupon_area_test_en.csv"),row.names = FALSE)
write.csv(cpltr, file = paste0(dir, "coupon_list_train_en.csv"), row.names = FALSE)
write.csv(cpltr.ar, file =paste0(dir, "coupon_area_train_en.csv"), row.names = FALSE)
write.csv(vali.result, file = paste0(dir,"purchase.csv"), row.names = FALSE)
write.csv(cpdtr, file =paste0(dir, "coupon_detail_train_en.csv"), row.names = FALSE)
write.csv(cpvtr, file = paste0(dir, "coupon_visit_train_en.csv"), row.names = FALSE)
write.csv(cpvtr[,c("PURCHASE_FLG","VIEW_COUPON_ID_hash","USER_ID_hash")],
          file = paste0(dir, "coupon_visit_train_en_simp.csv"), row.names = FALSE)

