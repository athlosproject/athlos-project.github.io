## ---- global

miss_values_vector <- c(991, 992, 993, 994, 995, 996, 997, 998, 999)
ds_label <- "Activity level"
cat_label <- c("a lot/high" = 1, "fairly/moderate" = 2, "not much/low" = 3, "not at all/inactive" = 4, "Missing" = 999, "Do not know" = 998, "Refuse" = 997, "Not applicable" = 996, "Does not answer" = 995, "Not attempt not done" = 994, "Disable to measure" = 993, "Impute" = 992, "CAPI interviewer error" = 991)
datafolder <- "../RData/"
hd_vbl <- "level_pa"