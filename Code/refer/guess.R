setwd("E:/Documents/MLearn/Projects/Coupon/refer")
source("myutil.R")
#read in all the input data
# cpdtr <- read.csv("../input_uk/coupon_detail_train_en.csv")
# cpltr <- read.csv("../input_uk/coupon_list_train_en.csv")
# cplte <- read.csv("../input_uk/coupon_list_test_en.csv")
# ulist <- read.csv("../input_uk/user_list_en.csv")
# #the same rate between shop area and user buy
# #result : 0.53
# cpd.l.tr <- merge(cpdtr, cpltr)
# same_rate = length(which(cpd.l.tr$SMALL_AREA_NAME == cpd.l.tr$small_area_name))/(dim(cpd.l.tr)[1])

file1 <- "../result/09-30-13-56_submission.csv"
file2 <- "../result/09-30-15-07_submission.csv"
d1 <- read.csv(file1)
d2 <- read.csv(file2)
res <- consame.df(d1,d2)