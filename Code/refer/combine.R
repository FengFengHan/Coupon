setwd("E:/Documents/MLearn/Projects/Coupon/refer")
visit <- read.csv("../result/submission_visit.csv")
cosine <- read.csv("../result/09-30-23-09_submission.csv")
final <- visit
final$PURCHASED_COUPONS <- paste(visit$PURCHASED_COUPONS, cosine$PURCHASED_COUPONS)
write.csv(final, '../result/09-30-23-09_submission_combine.csv', quote = FALSE,row.names = FALSE)
