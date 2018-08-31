## ---- global

miss_values_vector <- c(991, 992, 993, 994, 995, 996, 997, 998, 999)
ds_label <- "Verbal fluency"
ds_label_cog_verb_c <- "Cognition - verbal fluency. Test that assesses verbal (semantic) fluency: name as many animals as they could think of in 1 minute"
ds_label_cog_verb <- "Cognition - verbal fluency. Test that assesses verbal (semantic) fluency: name as many animals as they could think of in 1 minute. Discrete variable: value 0 = <=25%; value 1 = >25%"
cat_label <- c("<=25%"=0, ">25%"=1, "Missing" = 999, "Do not know" = 998, "Refuse" = 997, "Not applicable" = 996, "Does not answer" = 995, "Not attempt not done" = 994, "Disable to measure" = 993, "Impute" = 992, "CAPI interviewer error" = 991)
cont_label <- cat_label[3:11]
datafolder <- "../RData/"
hd_vbl <- "cog_verb"
hd_vbl_c <- "cog_verb_c"


