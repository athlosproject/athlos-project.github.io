## ---- global

miss_values_vector <- c(991, 992, 993, 994, 995, 996, 997, 998, 999)
ds_label <- "Immediate recall"
ds_label_cog_imm_c <- "Immediate recall of common nouns from a list. The total number of words correctly immediately recalled during a test of verbal memory"
ds_label_cog_imm <- "Immediate recall of common nouns from a list. Variable dichotomised by first quartil with value 0 and higher with value 1"
cat_label <- c("<=25%"=0, ">25%"=1, "Missing" = 999, "Do not know" = 998, "Refuse" = 997, "Not applicable" = 996, "Does not answer" = 995, "Not attempt not done" = 994, "Disable to measure" = 993, "Impute" = 992, "CAPI interviewer error" = 991)
cont_label <- cat_label[3:11]
datafolder <- "../RData/"
hd_vbl <- "cog_imm"
hd_vbl_c <- "cog_imm_c"


