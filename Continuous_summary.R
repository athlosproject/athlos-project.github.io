Continuous_summary <- function(var, missing_values = NA){
  OUT <- NULL
  # Number of id's/observations
  n <- length(var)
  # First, assume missing_values contains NA; later, it doesn't.
  if(sum(is.na(missing_values))>0){
    # Get summary of values different from NA (summary)
    var_no_missing <- na.omit(var)
    OUT$summary <- summary(var)
    # Get number of NA-values against number of non NA values (values_table)
    n_no_mis <- length(na.omit(var))
    n_na <- sum(is.na(var))
    n_missings <- n_na # missings = NA
    miss_table <- rbind(c(n_no_mis, n_na), round(c(n_no_mis, n_na)/n,4)*100)
    colnames(miss_table) <- c('No-missings', 'NA')
    rownames(miss_table) <- c('Frequency', 'Percentage')
    OUT$values_table <- miss_table
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
    # Get summary of non-missing values (summary)
    var_no_missing <- na.omit(var_no_missing)
    OUT$summary <- summary(var_no_missing)
    # Get frequencies of all missing values but NA, joint with NA frequency and against cardinal of non-missing values (values_table)
    count_mis <- NULL
    for(i in 1:length(missing_values)){
      count_mis[i] <- sum(na.omit(var)==missing_values[i])
    }
    n_no_mis <- length(na.omit(var_no_missing))
    miss_table <- rbind(c(n_no_mis, count_mis, n_na),
                        round(c(n_no_mis,count_mis, n_na)/n,4)*100)
    colnames(miss_table) <- c('No-missings',missing_values, 'NA')
    rownames(miss_table) <- c('Frequency', 'Percentage')
    OUT$values_table <- miss_table
  }
  # Get total number of missings against sample size
  n_table <- cbind(n, n_missings)
  colnames(n_table) <- c("Sample size", "Missing values")
  OUT$n_table <- n_table
  OUT
}


## It returns 3 tables: 
##    1 -> summary
##    2 -> values_table
##    3 -> n_table
