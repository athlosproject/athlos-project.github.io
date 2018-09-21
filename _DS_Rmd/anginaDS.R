## ---- global

miss_values_vector <- c(991, 992, 993, 994, 995, 996, 997, 998, 999)
ds_label <- "History of angina"
ds_label_age <- "Age first diagnosed angina"
cat_label <- c("No" = 0, "Yes" = 1, "Missing" = 999, "Do not know" = 998, "Refuse" = 997, "Not applicable" = 996, "Does not answer" = 995, "Not attempt not done" = 994, "Disable to measure" = 993, "Impute" = 992, "CAPI interviewer error" = 991)
cont_label <- cat_label[3:11]
datafolder <- "../RData/"
hd_vbl <- "h_angina"
hd_age_vbl <- "h_angina_age"

