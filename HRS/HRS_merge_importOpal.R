# Clean present environment
rm(list=ls())

# Set working directory where RData folder with Rdata files are saved for current study
setwd("M:/WPs/WP1/Maelstrom/data_process/athlos-project.github.io/HRS")
# Load some useful packages and Opal session
source("../setup.r")

# Load data base with 'athlos_id' and 'athlos_id2' for id's in current study
load("M:/WPs/WP1/Maelstrom/data_process/ATHLOS_cohort/ids2/ids_athlos_ds_HRS.Rdata")

# Load 'healthstatus' measure data for current study
load("M:/WPs/WP1/Maelstrom/data_process/hsm_process/mirt/hs.rdata")
names(hs)[match("trait_eq",names(hs))] <- "trait"
names(hs)[match("hs_eq",names(hs))] <- "healthstatus"

# lf is a list of Rdata folder addresses. 
lf   <- list("./RData/hrs_w1" , "./RData/hrs_w2" , "./RData/hrs_w3" , "./RData/hrs_w4" , "./RData/hrs_w5" , "./RData/hrs_w6" , "./RData/hrs_w7" , "./RData/hrs_w8" , "./RData/hrs_w9" , "./RData/hrs_w10" , "./RData/hrs_w11" ,
             "./RData/ahead_w2" , "./RData/ahead_w3" , "./RData/ahead_w4" , "./RData/ahead_w5" , "./RData/ahead_w6" , "./RData/ahead_w7" , "./RData/ahead_w8" , "./RData/ahead_w9" , "./RData/ahead_w10" , "./RData/ahead_w11" ,
             "./RData/coda_w4" , "./RData/coda_w5" , "./RData/coda_w6" , "./RData/coda_w7" , "./RData/coda_w8" , "./RData/coda_w9" , "./RData/coda_w10" , "./RData/coda_w11" ,
             "./RData/wb_w4" , "./RData/wb_w5" , "./RData/wb_w6" , "./RData/wb_w7" , "./RData/wb_w8" , "./RData/wb_w9" , "./RData/wb_w10" , "./RData/wb_w11" , 
             "./RData/ebb_w7" , "./RData/ebb_w8" , "./RData/ebb_w9" , "./RData/ebb_w10" , "./RData/ebb_w11" , 
             "./RData/mbb_w10" , "./RData/mbb_w11")


# lhd is a list of harmonised table names.
lhd <- list("hrs_hrs_w1" , "hrs_hrs_w2" , "hrs_hrs_w3" , "hrs_hrs_w4" , "hrs_hrs_w5" , "hrs_hrs_w6" , "hrs_hrs_w7" , "hrs_hrs_w8" , "hrs_hrs_w9" , "hrs_hrs_w10" , "hrs_hrs_w11" ,
            "hrs_ahead_w2" , "hrs_ahead_w3" , "hrs_ahead_w4" , "hrs_ahead_w5" , "hrs_ahead_w6" , "hrs_ahead_w7" , "hrs_ahead_w8" , "hrs_ahead_w9" , "hrs_ahead_w10" , "hrs_ahead_w11" ,
            "hrs_coda_w4" , "hrs_coda_w5" , "hrs_coda_w6" , "hrs_coda_w7" , "hrs_coda_w8" , "hrs_coda_w9" , "hrs_coda_w10" , "hrs_coda_w11" ,
            "hrs_wb_w4" , "hrs_wb_w5" , "hrs_wb_w6" , "hrs_wb_w7" , "hrs_wb_w8" , "hrs_wb_w9" , "hrs_wb_w10" , "hrs_wb_w11" , 
            "hrs_ebb_w7" , "hrs_ebb_w8" , "hrs_ebb_w9" , "hrs_ebb_w10" , "hrs_ebb_w11" , 
            "hrs_mbb_w10" , "hrs_mbb_w11")

# Vector with the names of those variables that should be integer format
integ.var <- c("adl_bathing","adl_bed","adl_dressing","adl_eating","adl_moving","adl_toilet",
               "ah","anxiety_symp","athlos_id2","bereav","cancer","child","close_spouse",
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
               "nat_dis","nonpaid_work","obesity","oral_problems","orientation","oth_fam",
               "pain","part_clubs","past_oh","pol_act","pol_vot","proxy",
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
  ds <- left_join(ds, ids_athlos_ds_HRS, by = 'id')
 
    
  #variable country
  ds$country <- 26 #US

  #variable cohort
  if (i %in% c(1:11)) ds$cohort <- "hrs"
  if (i %in% c(12:21)) ds$cohort <- "ahead"
  if (i %in% c(22:29)) ds$cohort <- "coda"
  if (i %in% c(30:37)) ds$cohort <- "wb"
  if (i %in% c(38:42)) ds$cohort <- "ebb"
  if (i %in% c(43:44)) ds$cohort <- "mbb"
  
  # Create variable wave and merge health status data
  if (i %in% c(1)){
    ds$wave <- 1L
    hsf <- filter(hs, wave==1) 
    hsf <- select(hsf, athlos_id2, trait, healthstatus)
    ds <- left_join(ds, hsf, by = 'athlos_id2')
  }  
  if (i %in% c(2,12)){
    ds$wave <- 2L
    hsf <- filter(hs, wave==2) 
    hsf <- select(hsf, athlos_id2, trait, healthstatus)
    ds <- left_join(ds, hsf, by = 'athlos_id2')
  }
  if (i %in% c(3,13)){
    ds$wave <- 3L
    hsf <- filter(hs, wave==3) 
    hsf <- select(hsf, athlos_id2, trait, healthstatus)
    ds <- left_join(ds, hsf, by = 'athlos_id2')
  }   
  if (i %in% c(4,14,22,30)){
    ds$wave <- 4L
    hsf <- filter(hs, wave==4) 
    hsf <- select(hsf, athlos_id2, trait, healthstatus)
    ds <- left_join(ds, hsf, by = 'athlos_id2')
  }     
  if (i %in% c(5,15,23,31)){
    ds$wave <- 5L
    hsf <- filter(hs, wave==5) 
    hsf <- select(hsf, athlos_id2, trait, healthstatus)
    ds <- left_join(ds, hsf, by = 'athlos_id2')
  }     
  if (i %in% c(6,16,24,32)){
    ds$wave <- 6L
    hsf <- filter(hs, wave==6) 
    hsf <- select(hsf, athlos_id2, trait, healthstatus)
    ds <- left_join(ds, hsf, by = 'athlos_id2')
  }        
  if (i %in% c(7,17,25,33,38)){
    ds$wave <- 7L
    hsf <- filter(hs, wave==7) 
    hsf <- select(hsf, athlos_id2, trait, healthstatus)
    ds <- left_join(ds, hsf, by = 'athlos_id2')
  }        
  if (i %in% c(8,18,26,34,39)){
    ds$wave <- 8L
    hsf <- filter(hs, wave==8) 
    hsf <- select(hsf, athlos_id2, trait, healthstatus)
    ds <- left_join(ds, hsf, by = 'athlos_id2')
  }        
  if (i %in% c(9,19,27,35,40)){
    ds$wave <- 9L
    hsf <- filter(hs, wave==9) 
    hsf <- select(hsf, athlos_id2, trait, healthstatus)
    ds <- left_join(ds, hsf, by = 'athlos_id2')
  }              
  if (i %in% c(10,20,28,36,41,43)){
    ds$wave <- 10L
    hsf <- filter(hs, wave==10) 
    hsf <- select(hsf, athlos_id2, trait, healthstatus)
    ds <- left_join(ds, hsf, by = 'athlos_id2')
  }             
  if (i %in% c(11,21,29,37,42,44)){
    ds$wave <- 11L
    hsf <- filter(hs, wave==11) 
    hsf <- select(hsf, athlos_id2, trait, healthstatus)
    ds <- left_join(ds, hsf, by = 'athlos_id2')
  }                

  # variable study
  ds$study <- 9
  
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
  
  # All NA values (individuals which were harmonised only for some variables) are recodified as 996
  ds[is.na(ds)] <- 996L
  
  if (i %in% c(12:21)){
    ds <- ds %>% filter(athlos_id2!="90204982")
  }
  
  # "ds" tibble is imported to Opal
  opal.assign.data(  o, lhd[[i]], ds)
  opal.symbol_import(o, lhd[[i]], project='HRS_HD')
  
  # Remove all objects but the listed in the vector.
  rm(list=setdiff(ls(), c("o","lf", "lhd", "integ.var", "text.var", "i","ids_athlos_ds_HRS","hs") )) 
}

# Close Opal session
opal.logout(o)