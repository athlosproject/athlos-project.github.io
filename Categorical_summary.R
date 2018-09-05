Categorical_summary <- function(var, missing_values = NA){
  OUT <- NULL
  # Number of id's/observations
  n <- length(var)
  #n_na <- 0
  # First, assume missing_values contains NA; later, it doesn't.
  if(sum(is.na(missing_values))>0){
    # Get cardinal of NA values
    n_na <- sum(is.na(var))
    n_missings <- n_na # missings = NA
    # Get frequencies (values_table)
    Frequency <- table(var)
    Percentage <- round(table(var)/n,4)*100
    OUT$values_table <- rbind(Frequency,Percentage)
  } else{
    # Keep in var_no_missing all values which are not in missing_values
    var_no_missing <- var
    for(i in 1:length(missing_values)){
      var_no_missing <- var_no_missing[var_no_missing!=missing_values[i]]
    }
    # Get cardinal of NA values
    n_na <- sum(is.na(var))
    # Get cardinal of all missing values
    n_missings <- (n - length(var_no_missing)) + n_na # missings = NA, 991, 992, ..., 999
    # Get frequencies (values_table)
    Frequency <- table(var_no_missing)
    Percentage <- round(table(var_no_missing)/n,4)*100
    OUT$values_table <- rbind(Frequency,Percentage)
    # Get frequencies of all missing values but NA and joint with NA frequency (missing_values_table)
    count_mis <- NULL
    for(i in 1:length(missing_values)){
      count_mis[i] <- sum(na.omit(var)==missing_values[i])
    }
    miss_table <- rbind(c(count_mis, n_na),round(c(count_mis, n_na)/n,4)*100)
    colnames(miss_table) <- c(missing_values, 'NA')
    rownames(miss_table) <- c('Frequency', 'Percentage')
    OUT$missing_values_table <- miss_table
  }
  # Get total number of missings against sample size
  n_table <- cbind(n, n_missings)
  colnames(n_table) <- c("Sample size", "Missing values")
  OUT$n_table <- n_table
  OUT
}


## If missing_values does not contain NA, Categorical_summary returns 3 tables: 
##    1 -> values_table
##    2 -> missing_values_table
##    3 -> n_table

## Otherwise, it returns 2 tables:
##    1 -> values_table
##    2 -> n_table
