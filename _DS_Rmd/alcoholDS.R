## ---- global

miss_values_vector <- c(991, 992, 993, 994, 995, 996, 997, 998, 999)
ds_label_curr <- "Current alcohol drinker"
ds_label_freq <- "Frequency of alcohol drinking"
ds_label_past <- "Past drinking"
cat_label <- c("No" = 0, "Yes" = 1, "Missing" = 999, "Do not know" = 998, "Refuse" = 997, "Not applicable" = 996, "Does not answer" = 995, "Not attempt not done" = 994, "Disable to measure" = 993, "Impute" = 992, "CAPI interviewer error" = 991)
freq_label <- c("never" = 0, "rare" = 1, "often" = 2, "Missing" = 999, "Do not know" = 998, "Refuse" = 997, "Not applicable" = 996, "Does not answer" = 995, "Not attempt not done" = 994, "Disable to measure" = 993, "Impute" = 992, "CAPI interviewer error" = 991)
datafolder <- "../RData/"
hd_curr_vbl <- "current_oh"
hd_freq_vbl <- "freq_oh"
hd_past_vbl <- "past_oh"