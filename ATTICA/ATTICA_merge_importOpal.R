# Clean present environment
rm(list=ls())

# Set working directory where RData folder with Rdata files are saved for current study
setwd("M:/WPs/WP1/Maelstrom/data_process/athlos-project.github.io/ATTICA")
# Load some useful packages and Opal session
source("../setup.r")

# Load data base with 'athlos_id' and 'athlos_id2' for id's in current study
load("M:/WPs/WP1/Maelstrom/data_process/ATHLOS_cohort/ids2/ids_athlos_ds_ATTICA.Rdata")

# For ATTICA there is no health measure data

# lf is a list of Rdata folder addresses. 
lf   <- list("./RData/w1", "./RData/w2", "./RData/w3")

# lhd is a list of harmonised table names.
lhd <- list("attica_w1_hd", "attica_w2_hd", "attica_w3_hd")

# Vector with the names of those variables that should be integer format
integ.var <- c("adl_bathing","adl_bed","adl_dressing","adl_eating","adl_moving","adl_toilet",
               "ah","anxiety_symp","athlos_id2","bereav","cancer","child","childsep","close_spouse",
               "cog_del","cog_imm","cog_num","cog_proc","cog_stat","cog_verb",
               "confidant","conseq_falls","cont_fr","cont_rel","country","current_oh","current_smoking",
               "cvd","cvd_all","cvd_hard","dementia","depression","divorce","dizziness",
               "education","emot_sup","employed","energy","evaluative_wb","ever_smoked","eye_far","eye_gen","eye_near",
               "f_cont_fr","f_mod_pa","f_other_fr","f_vig_pa","fin_prob","freq_oh","grchild",
               "h_angina","h_asthma","h_cpd","h_diabetes","h_hypertension","h_joint_disorders","h_respiratory",
               "hearing_conv","hearing_gen",
               "iadl_housewk","iadl_map","iadl_meals","iadl_medication","iadl_money","iadl_outhome","iadl_phone","iadl_shopping",
               "incontinence","inst_sup","level_pa","lifethreat_sit","living_alone","living_status","loneliness",
               "marital_status","memory","mi_ha","migration",
               "mob_arm","mob_climb","mob_coin","mob_lift","mob_pull","mob_sit","mob_stoop","mob_up","mob_walk",
               "multimorbid","nat_dis","nonpaid_work","obesity","oral_problems","orientation","oth_fam",
               "pain","parentedu","part_clubs","past_oh","pol_act","pol_vot","proxy",
               "recent_falls","relig","resid_place","residence","respondent","retired",
               "sen_club","sex","sickdis","sleep","smoking","sport","spouse","srh","stroke","study",
               "suicidal_ideation_12m","suicidal_ideation_lm",
               "t_walks","trust","vig_pa","violence","volun","walking_speed","wave","wealth","weight_loss")

# Vector with the names of those variables that should be text format
text.var <- c("id","household_id","cohort","athlos_id")

# For each Rdata folder-harmonised table, load the data in the folder to the table in Opal.
for (i in 1:length(lhd)) {
  # List RData files and load R objects (tibbles)
  file_list <- list.files(lf[[i]])
  for (file in file_list) load(paste(lf[[i]],file,sep="/"))
  rm(file_list, file)
  
  # Get and merge all the tibbles loaded from the Rdata folders* into one called "ds".
  # *These are all the data frames up to the ones with athlos id's and health measure data loaded at the beginning.
  DF_obj <- lapply(Filter(function(x) is.data.frame(get(x)), setdiff(setdiff(ls(),ls(pattern = "^ids_athlos_ds_")),ls(pattern = "^hs$"))), get)
  f.m    <- function(x, y) dplyr::full_join(x, y, by= "id")
  ds     <- Reduce(f.m, DF_obj )
  
  # Avoid local interest variable cd_all to upload to OPAL
  if("cd_all" %in% names(ds)){
    ds <- select(ds, -cd_all)
  }
  
  # Remove labels
  ds <- update_labelled(ds)
  val_labels(ds) <- NULL
  
  # Add athlos_id and athlos_id2
  ds <- left_join(ds, ids_athlos_ds_ATTICA, by = 'id')

  
  #variable country
  ds$country <- 60  # Greece

  #variable cohort
  ds$cohort <- "Athens"  
  
  # variable wave
  ds$wave <- i

  # variable study
  ds$study <- 3  # ATTICA

  # All NA values are recodified as 996
  ds <- ds %>% mutate_all(~replace(.,list = which(is.na(.)), values = 996))

  # "iv" variables must have integer format.
  iv <- integ.var[integ.var %in% names(ds)]
  for (j in 1:length(iv)) {
    ds[[iv[j]]] <- as.integer(ds[[iv[j]]])
  }
  # "niv" variables (neither text nor integer) must have double/numeric format.
  niv <- names(ds)[!names(ds) %in% union(integ.var,text.var)]
  for (j in 1:length(niv)) {
    ds[[niv[j]]] <- as.numeric(ds[[niv[j]]])
  }
  

  # "ds" tibble is imported to Opal
  opal.assign.data(  o, lhd[[i]], ds)
  opal.symbol_import(o, lhd[[i]], project='ATTICA_HD')
  
  # Remove all objects but the listed in the vector.
  rm(list=setdiff(ls(), c("o","lf", "lhd", "integ.var", "text.var", "i","ids_athlos_ds_ATTICA","hs") )) 
}

# Close Opal session
opal.logout(o)