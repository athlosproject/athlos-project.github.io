# Clean present environment
rm(list=ls())

# Set working directory where RData folder with Rdata files are saved for current study
setwd("M:/WPs/WP1/Maelstrom/data_process/athlos-project.github.io/1066")
# Load some useful packages and Opal session
source("../setup.r")

# Load data base with 'athlos_id' and 'athlos_id2' for id's in current study
load("M:/WPs/WP1/Maelstrom/data_process/ATHLOS_cohort/ids2/ids_athlos_ds_1066.Rdata")

# Load 'healthstatus' measure data for current study
load("M:/WPs/WP1/Maelstrom/data_process/hsm_process/athlos_v1.4_hsm_rescaled_by_study_country_wave.rdata")
hs <- filter(ath, study==1)
rm(ath)
hs <- hs %>% select(-healthstatus)
names(hs)[match("hsm",names(hs))] <- "healthstatus"


# lf is a list of Rdata folder addresses. 
lf   <- list("./RData/cuba_w1", "./RData/cuba_w2",
             "./RData/DR_w1", "./RData/DR_w2",
             "./RData/india_w1", 
             "./RData/PR_w1", "./RData/PR_w2",
             "./RData/RCh_w1", "./RData/RCh_w2",
             "./RData/RM_w1", "./RData/RM_w2",
             "./RData/RP_w1", "./RData/RP_w2",
             "./RData/UCh_w1", "./RData/UCh_w2",
             "./RData/UM_w1", "./RData/UM_w2",
             "./RData/UP_w1", "./RData/UP_w2",
             "./RData/Ven_w1", "./RData/Ven_w2")

# lhd is a list of harmonised table names.
lhd <- list("t1066_cuba_w1_hd" , "t1066_cuba_w2_hd",
            "t1066_DR_w1_hd"   , "t1066_DR_w2_hd",
            "t1066_india_w1_hd", 
            "t1066_PR_w1_hd"   , "t1066_PR_w2_hd",
            "t1066_RCh_w1_hd"  , "t1066_RCh_w2_hd",
            "t1066_RM_w1_hd"   , "t1066_RM_w2_hd",
            "t1066_RP_w1_hd"   , "t1066_RP_w2_hd",
            "t1066_UCh_w1_hd"  , "t1066_UCh_w2_hd",
            "t1066_UM_w1_hd"   , "t1066_UM_w2_hd",
            "t1066_UP_w1_hd"   , "t1066_UP_w2_hd",
            "t1066_Ven_w1_hd"  , "t1066_Ven_w2_hd")

# Vector with the names of those variables that should be integer format
integ.var <- c("adl_bathing","adl_bed","adl_dressing","adl_eating","adl_moving","adl_toilet",
               "ah","anxiety_symp","athlos_id2","bereav","cancer","child","close_spouse",
               "cog_del","cog_imm","cog_num","cog_proc","cog_stat","cog_verb",
               "confidant","conseq_falls","cont_fr","cont_rel","country","current_oh","current_smoking",
               "cvd","cvd_all","cvd_hard","depression","divorce","dizziness",
               "education","emot_sup","employed","energy","ever_smoked","eye_far","eye_gen","eye_near",
               "f_cont_fr","f_mod_pa","f_other_fr","f_vig_pa","fin_prob","freq_oh","grchild",
               "h_angina","h_asthma","h_cpd","h_diabetes","h_hypertension","h_joint_disorders","h_respiratory",
               "hearing_conv","hearing_gen",
               "iadl_housewk","iadl_map","iadl_meals","iadl_medication","iadl_money","iadl_outhome","iadl_phone","iadl_shopping",
               "incontinence","inst_sup","level_pa","lifethreat_sit","living_alone","living_status","loneliness",
               "marital_status","memory","mi_ha","migration",
               "mob_arm","mob_climb","mob_coin","mob_lift","mob_pull","mob_sit","mob_stoop","mob_up","mob_walk",
               "nat_dis","nonpaid_work","obesity","oral_problems","orientation","oth_fam",
               "pain","part_clubs","past_oh","pol_act","pol_vot","proxy",
               "recent_falls","relig","resid_place","residence","respondent","retired",
               "sen_club","sex","sickdis","sleep","smoking","sport","spouse","srh","stroke","study",
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
  ds <- left_join(ds, ids_athlos_ds_1066, by = 'id')

  
  # Variable 'country' and 'cohort' are created as follows:
  if (i %in% c(1,2))   {
    ds$country <- 21   # Cuba
    ds$cohort <- 'Cuba'  
  }
  if (i %in% c(3,4))  {
    ds$country <- 22   # Dominican Republic
    ds$cohort <- 'Dominican Republic'
  } 
  if (i %in% c(5))  {
    ds$country <- 32   # India
    ds$cohort <- 'India'
  }   
  if (i %in% c(6,7)) {
    ds$country <- 25   # Puerto Rico
    ds$cohort <- 'Puerto Rico'
  } 
  if (i %in% c(8,9)){
    ds$country <- 31   # China
    ds$cohort <- 'Rural China'
  }   
  if (i %in% c(10,11)) {
    ds$country <- 23   # Mexico
    ds$cohort <- 'Rural Mexico'
  }
  if (i %in% c(12,13)) {
    ds$country <- 24   # Peru
    ds$cohort <- 'Rural Peru'
  }
  if (i %in% c(14,15)) {
    ds$country <- 31   # China
    ds$cohort <- 'Urban China'
  }
  if (i %in% c(16,17)) {
    ds$country <- 23   # Mexico
    ds$cohort <- 'Urban Mexico'
  }
  if (i %in% c(18,19)) {
    ds$country <- 24   # Peru
    ds$cohort <- 'Urban Peru'
  }
  if (i %in% c(20,21)) {
    ds$country <- 27   # Venezuela
    ds$cohort <- 'Venezuela'
  }
  
  # Create variable wave and merge health status data
  if (i %in% c(1,3,5,6,8,10,12,14,16,18,20)) {
    ds$wave <- 1L
    hsf <- filter(hs, wave==1) 
    hsf <- select(hsf, athlos_id2, trait, healthstatus)
    ds <- left_join(ds, hsf, by = 'athlos_id2')
    }
  if (i %in% c(2,4,7,9,11,13,15,17,19,21))  {
    ds$wave <- 2L
    hsf <- filter(hs, wave==2) 
    hsf <- select(hsf, athlos_id2, trait, healthstatus)
    ds <- left_join(ds, hsf, by = 'athlos_id2')
  }

  # Variable 'study' is created as follows:
  ds$study <- 1  #10/66


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
  opal.symbol_import(o, lhd[[i]], project='10-66_HD')
  
  # Remove all objects but the listed in the vector.
  rm(list=setdiff(ls(), c("o","lf", "lhd", "integ.var","text.var", "i","ids_athlos_ds_1066","hs") )) 
}

# Close Opal session
opal.logout(o)