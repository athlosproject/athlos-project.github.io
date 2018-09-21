## ---- global

miss_values_vector <- c(991, 992, 993, 994, 995, 996, 997, 998, 999)
ds_label <- "Myocardial infarction or heart attack"
ds_label_age <- "Age of each episode of myocardial infarction or heart attack"
cat_label <- c("No" = 0, "Yes" = 1, "Missing" = 999, "Do not know" = 998, "Refuse" = 997, "Not applicable" = 996, "Does not answer" = 995, "Not attempt not done" = 994, "Disable to measure" = 993, "Impute" = 992, "CAPI interviewer error" = 991)
cont_label <- cat_label[3:11]
datafolder <- "../RData/"
hd_vbl <- "mi_ha"
hd_age_vbl <- "mi_ha_age"

