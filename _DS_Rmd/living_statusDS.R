## ---- global

miss_values_vector <- c(991, 992, 993, 994, 995, 996, 997, 998, 999)
ds_label <- "Living status"
ds_label_all <- "The individual is dead or alive?"
cat_label <- c("Alive" = 0, "Dead" = 1, "Dropout"=2, "Missing" = 999, "Do not know" = 998, "Refuse" = 997, "Not applicable" = 996, "Does not answer" = 995, "Not attempt not done" = 994, "Disable to measure" = 993, "Impute" = 992, "CAPI interviewer error" = 991)
datafolder <- "../RData/"
hd_vbl <- "living_status"
