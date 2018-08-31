## ---- global

miss_values_vector <- c(991, 992, 993, 994, 995, 996, 997, 998, 999)
ds_label <- "Delayed recall"
ds_label_cog_del_c <- "Test that assesses delayed recall using the common nouns from the list previously employed for measuring Immediate recall. The total number of words correctly recalled after a delay"
ds_label_cog_del <- "Test that assesses delayed recall using the common nouns from the list previously employed for measuring Immediate recall. Variable dichotomised by first quartil with value 0 and higher with value 1"
cat_label <- c("<=25%"=0, ">25%"=1, "Missing" = 999, "Do not know" = 998, "Refuse" = 997, "Not applicable" = 996, "Does not answer" = 995, "Not attempt not done" = 994, "Disable to measure" = 993, "Impute" = 992, "CAPI interviewer error" = 991)
cont_label <- cat_label[3:11]
datafolder <- "../RData/"
hd_vbl <- "cog_del"
hd_vbl_c <- "cog_del_c"


