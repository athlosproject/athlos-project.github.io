## ---- global

miss_values_vector <- c(991, 992, 993, 994, 995, 996, 997, 998, 999)
ds_label <- "Participant is currently with unpaid work"
cat_label <- c("housework" = 1, "family business work" = 2, "other" = 3, "Missing" = 999, "Do not know" = 998, "Refuse" = 997, "Not applicable" = 996, "Does not answer" = 995, "Not attempt not done" = 994, "Disable to measure" = 993, "Impute" = 992, "CAPI interviewer error" = 991)
datafolder <- "../RData/"
hd_vbl <- "nonpaid_work"