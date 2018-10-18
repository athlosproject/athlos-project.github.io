## ---- global

miss_values_vector <- c(991, 992, 993, 994, 995, 996, 997, 998, 999)
ds_label <- "Obesity"
ds_label_all <- "Presence of obesity based in Body Mass Index classification according to the WHO BMI classification (http://apps.who.int/bmi/index.jsp?introPage=intro_3.html): BMI value equal to or above 30Kg/m^2"
cat_label <- c("No" = 0, "Yes" = 1, "Missing" = 999, "Do not know" = 998, "Refuse" = 997, "Not applicable" = 996, "Does not answer" = 995, "Not attempt not done" = 994, "Disable to measure" = 993, "Impute" = 992, "CAPI interviewer error" = 991)
datafolder <- "../RData/"
hd_vbl <- "obesity"