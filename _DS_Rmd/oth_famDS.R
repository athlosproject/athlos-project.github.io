## ---- global

miss_values_vector <- c(991, 992, 993, 994, 995, 996, 997, 998, 999)
ds_label <- "Has other family members?"
cat_label <- c("No"=0, "Yes"=1, "Missing"=999, "Do not know"=998, "Refuse"=997, "Not applicable"=996, "Don't answer"=995,  "Not attempt/not done/not yet recorded"=994, "Disable to measure"=993, "Impute"=992, "CAPI/interviewer error"=991)
datafolder <- "../RData/"
hd_vbl <- "oth_fam"