# Setup file


# Cleaning local memory
rm(list = ls())
gc(reset=T)

# For the first time, uncomment and run the following two lines to install needed packages.
#install.packages(c('RCurl', 'rjson'), repos=c('http://cran.rstudio.com/', 'http://www.stats.ox.ac.uk/pub/RWin/'))
#install.packages('opal', repos='http://cran.obiba.org', type='source')
knitr::opts_chunk$set(echo = TRUE, cache = FALSE, warning = FALSE, message = FALSE, tidy = TRUE, results = 'asis')

# Before running the next lines, review if the following packages are already installed. Otherwise, install them and load all.
list.of.packages <- c("knitr", "haven", "sjmisc", "bitops", "RCurl", "rjson", "mime", "opal", "tibble", "car", "labelled", "Hmisc", "scales", "dplyr", "lubridate", "stringi", "gridExtra", "pander", "tidyr", "stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

source("absolute_path_to_(project_path) Continuous_summary.R")   # R function to summarize continuous variables
source("absolute_path_to_(project_path) Categorical_summary.R")  # R function to summarize categorical variables

# authenticate within opal server 
o <- opal.login(username = 'your username', password = 'your password', url = 'http://opal.pssjd.org')
