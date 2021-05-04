# Clean present environment
rm(list=ls())

# Set working directory where RData folder with Rdata files are saved for current study
setwd("M:/WPs/WP1/Maelstrom/data_process/athlos-project.github.io/COURAGE")
# Load some useful packages and Opal session
source("../setup.r")

# Load data base with 'athlos_id' and 'athlos_id2' for id's in current study
load("M:/WPs/WP1/Maelstrom/data_process/ATHLOS_cohort/ids2/ids_athlos_ds_COURAGE.Rdata")

# Load 'healthstatus' measure data for current study
load("M:/WPs/WP1/Maelstrom/data_process/hsm_process/mirt/hs.rdata")
names(hs)[match("trait_eq",names(hs))] <- "trait"
names(hs)[match("hs_eq",names(hs))] <- "healthstatus"

# lf is a list of Rdata folder addresses. 
lf   <- list("./RData/pol_w1", "./RData/pol_w2",
             "./RData/spain_w1", "./RData/spain_w2",
             "./RData/fin_w1")

# lhd is a list of harmonised table names.
lhd <- list("courage_pol_w1_hd"  , "courage_pol_w2_hd",
            "courage_spain_w1_hd", "courage_spain_w2_hd",
            "courage_fin_w1_hd")

# Vector with the names of those variables that should be integer format
integ.var <- c("adl_bathing","adl_bed","adl_dressing","adl_eating","adl_moving","adl_toilet",
               "ah","anxiety_symp","athlos_id2","bereav","cancer","child","childsep","close_spouse",
               "cog_del","cog_imm","cog_num","cog_proc","cog_stat","cog_verb",
               "confidant","conseq_falls","cont_fr","cont_rel","country","current_oh","current_smoking",
               "cvd","cvd_all","cvd_hard","dementia","depression","divorce","dizziness",
               "education","emot_sup","employed","energy","eudaimonic_wb","evaluative_wb","ever_smoked","eye_far","eye_gen","eye_near",
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
  DF_obj <- lapply(Filter(function(x) is.data.frame(get(x)),  setdiff(setdiff(ls(),ls(pattern = "^ids_athlos_ds_")),ls(pattern = "^hs$"))), get)
  f.m    <- function(x, y) dplyr::full_join(x, y, by= "id")
  ds     <- Reduce(f.m, DF_obj )
  
  # Avoid local interest variable cd_all to upload to OPAL
  if("cd_all" %in% names(ds)){
    ds <- select(ds, -cd_all)
  }
  
  # Remove labels
  ds <- update_labelled(ds)
  val_labels(ds) <- NULL
  
  # Add athlos_id
  ds <- left_join(ds, ids_athlos_ds_COURAGE, by = 'id')

    
  #variable country and cohort
  if (i %in% c(1,2)) {
    ds$country <- 67 # Poland
    ds$cohort <- "Poland"  
  }
  if (i %in% c(3,4)) {
    ds$country <- 70 # Spain
    ds$cohort <- "Spain"  
  }
  if (i %in% c(5)) {
    ds$country <- 57   # Finland
    ds$cohort <- "Finland"  
  }

  # Create variable wave and merge health status data
  if (i %in% c(1,3,5)){
    ds$wave <- 1L
    hsf <- filter(hs, wave==1) 
    hsf <- select(hsf, athlos_id2, trait, healthstatus)
    ds <- left_join(ds, hsf, by = 'athlos_id2')
  }
  
  if (i %in% c(2,4)){
    ds$wave <- 2L
    hsf <- filter(hs, wave==2) 
    hsf <- select(hsf, athlos_id2, trait, healthstatus)
    ds <- left_join(ds, hsf, by = 'athlos_id2')
  }
  
  # variable study
  ds$study <- 5

  
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
  opal.symbol_import(o, lhd[[i]], project='COURAGE_HD')
  
  # Remove all objects but the listed in the vector.
  rm(list=setdiff(ls(), c("o","lf", "lhd", "integ.var", "text.var", "i","ids_athlos_ds_COURAGE","hs") )) 
}

# Close Opal session
opal.logout(o)