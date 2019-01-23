## ---- global

miss_values_vector <- c(991, 992, 993, 994, 995, 996, 997, 998, 999)
ds_label <- "Frequency of face to face and phone contacts with friends"
cat_label <- c("Daily/Almost daily"=1, "Once a week"=2, "Once or few times a month"=3, "Less than once a month"=4, "Never/No friends"=5, "Missing" = 999, "Do not know" = 998, "Refuse" = 997, "Not applicable" = 996, "Does not answer" = 995, "Not attempt not done" = 994, "Disable to measure" = 993, "Impute" = 992, "CAPI interviewer error" = 991)
datafolder <- "../RData/"
hd_vbl <- "f_cont_fr"