## ---- global

miss_values_vector <- c(991, 992, 993, 994, 995, 996, 997, 998, 999)
ds_label <- "Walking speed"
ds_label_walking_speed_c <- "Walking speed. Speed at metres per second (fastest record).  Continuous variable."
ds_label_walking_speed <- "Walking speed. Speed at metres per second (fastest record).  Variable dichotomised by first quartil with value 0 and higher with value 1."
cat_label <- c("<=25%"=0, ">25%"=1, "Missing" = 999, "Do not know" = 998, "Refuse" = 997, "Not applicable" = 996, "Does not answer" = 995, "Not attempt not done" = 994, "Disable to measure" = 993, "Impute" = 992, "CAPI interviewer error" = 991)
cont_label <- cat_label[3:11]
datafolder <- "../RData/"
hd_vbl <- "walking_speed"
hd_vbl_c <- "walking_speed_c"


