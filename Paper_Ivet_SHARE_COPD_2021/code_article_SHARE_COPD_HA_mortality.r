# Cleaning local memory
rm(list = ls())
gc(reset=T)
require(haven)
require(survival)
library(dplyr)
library(tidyr)
library(sjmisc)
library(labelled)
library(reshape2)
require(timereg)
require(ggplot2)
require(compareGroups)
source(file="plot.cum.r")

load("athlos_v2.0.rdata")

#######################################################
## Selection: Individuals from SHARE-wave 1 across all waves. 
## New individuals in later waves are NOT included.

ath <- athlos %>% filter(study==16)
missings <- c(991:999) 
for (m in missings) { ath <- na_if(ath, m)}
ath <- ath %>% filter(age>=50)
ath     <- arrange(ath, athlos_id2, wave)
ath$idw <- paste(ath$athlos_id2, ath$wave, sep = "w")
ids_w1 <- ath[ath$wave==1,]$athlos_id2 
aths <- ath %>% filter(athlos_id2 %in% ids_w1)

# YBC: year of birth in three categories:
aths$ybc<-NA
aths$ybc[!is.na(aths$ybirth) & aths$ybirth>1945] <- 0
aths$ybc[!is.na(aths$ybirth) & aths$ybirth>1935 & aths$ybirth<=1945] <- 1
aths$ybc[!is.na(aths$ybirth) & aths$ybirth<=1935] <- 2

#######################################################
## Healthstatus LCGA

#require(lcmm)
#set.seed(2020)

# <= 1935
#aths_35 <- aths %>% filter(ybc==2) %>% as.data.frame()

#lcga1 <- hlme(healthstatus ~ wave, subject = 'athlos_id2', ng=1, data=aths_35)
#lcga2 <- gridsearch(rep= 100, maxiter=10, minit=lcga1, hlme(healthstatus ~ wave, subject = 'athlos_id2', ng=2, data=aths_35, mixture= ~ wave, random = ~-1))
#lcga3 <- gridsearch(rep= 100, maxiter=10, minit=lcga1, hlme(healthstatus ~ wave, subject = 'athlos_id2', ng=3, data=aths_35, mixture= ~ wave, random = ~-1))
#lcga4 <- gridsearch(rep= 100, maxiter=10, minit=lcga1, hlme(healthstatus ~ wave, subject = 'athlos_id2', ng=4, data=aths_35, mixture= ~ wave, random = ~-1))
#lcga5 <- gridsearch(rep= 100, maxiter=10, minit=lcga1, hlme(healthstatus ~ wave, subject = 'athlos_id2', ng=5, data=aths_35, mixture= ~ wave, random = ~-1))

#summary(lcga5)
#plot(lcga5, which = "fit", var.time = "wave", ylim=c(0,100))
#(st <- summarytable(lcga1, lcga2, lcga3, lcga4, lcga5))
# (p.val <- pchisq(-2*(-), df = 3, lower.tail = FALSE))

#save(lcga1, file="lcga1_35.rdata")
#save(lcga2, file="lcga2_35.rdata")
#save(lcga3, file="lcga3_35.rdata")
#save(lcga4, file="lcga4_35.rdata")
#save(lcga5, file="lcga5_35.rdata")

#plot(lcga4, which = "fit", var.time = "wave", ylim=c(0,100), shades = T)
load(file="lcga3_35.rdata")
aths_lcga_35 <- lcga3$pprob[,c("athlos_id2", "class")]

# > 1935 <= 1945
#aths_3545 <- aths %>% filter(ybc==1) %>% as.data.frame()

#lcga1 <- hlme(healthstatus ~ wave, subject = 'athlos_id2', ng=1, data=aths_3545)
#lcga2 <- gridsearch(rep= 100, maxiter=10, minit=lcga1, hlme(healthstatus ~ wave, subject = 'athlos_id2', ng=2, data=aths_3545, mixture= ~ wave, random = ~-1))
#lcga3 <- gridsearch(rep= 100, maxiter=10, minit=lcga1, hlme(healthstatus ~ wave, subject = 'athlos_id2', ng=3, data=aths_3545, mixture= ~ wave, random = ~-1))
#lcga4 <- gridsearch(rep= 100, maxiter=10, minit=lcga1, hlme(healthstatus ~ wave, subject = 'athlos_id2', ng=4, data=aths_3545, mixture= ~ wave, random = ~-1))
#lcga5 <- gridsearch(rep= 100, maxiter=10, minit=lcga1, hlme(healthstatus ~ wave, subject = 'athlos_id2', ng=5, data=aths_3545, mixture= ~ wave, random = ~-1))

#summary(lcga4)
#plot(lcga4, which = "fit", var.time = "wave", ylim=c(0,100))
#(st <- summarytable(lcga1, lcga2, lcga3, lcga4, lcga5))
# (p.val <- pchisq(-2*(-), df = 3, lower.tail = FALSE))

#save(lcga1, file="lcga1_3545.rdata")
#save(lcga2, file="lcga2_3545.rdata")
#save(lcga3, file="lcga3_3545.rdata")
#save(lcga4, file="lcga4_3545.rdata")
#save(lcga5, file="lcga5_3545.rdata")

load("lcga3_3545.rdata")
aths_lcga_3545 <- lcga3$pprob[,c("athlos_id2", "class")]

# > 1945
#aths_45 <- aths %>% filter(ybc==0) %>% as.data.frame()

#lcga1 <- hlme(healthstatus ~ wave, subject = 'athlos_id2', ng=1, data=aths_45)
#lcga2 <- gridsearch(rep= 100, maxiter=10, minit=lcga1, hlme(healthstatus ~ wave, subject = 'athlos_id2', ng=2, data=aths_45, mixture= ~ wave, random = ~-1))
#lcga3 <- gridsearch(rep= 100, maxiter=10, minit=lcga1, hlme(healthstatus ~ wave, subject = 'athlos_id2', ng=3, data=aths_45, mixture= ~ wave, random = ~-1))
#lcga4 <- gridsearch(rep= 100, maxiter=10, minit=lcga1, hlme(healthstatus ~ wave, subject = 'athlos_id2', ng=4, data=aths_45, mixture= ~ wave, random = ~-1))
#lcga5 <- gridsearch(rep= 100, maxiter=10, minit=lcga1, hlme(healthstatus ~ wave, subject = 'athlos_id2', ng=5, data=aths_45, mixture= ~ wave, random = ~-1))

#summary(lcga4)
#plot(lcga4, which = "fit", var.time = "wave", ylim=c(0,100))
#(st <- summarytable(lcga1, lcga2, lcga3, lcga4, lcga5))
# (p.val <- pchisq(-2*(-), df = 3, lower.tail = FALSE))

#save(lcga1, file="lcga1_45.rdata")
#save(lcga2, file="lcga2_45.rdata")
#save(lcga3, file="lcga3_45.rdata")
#save(lcga4, file="lcga4_45.rdata")
#save(lcga5, file="lcga5_45.rdata")

load("lcga3_45.rdata")
aths_lcga_45 <- lcga3$pprob[,c("athlos_id2", "class")]

#######################################################
## Selection of variables to work with a smaller dataset.

g1 <- aths %>% select(athlos_id2, wave, country, sex, age, education, wealth, marital_status,  
                      ybirth, ybc, yintw, ydeath, living_status, healthstatus, 
                      ever_smoked, vig_pa, depression, h_cpd, h_cpd_age, 
                      h_diabetes, h_asthma, h_hypertension, h_joint_disorders, mi_ha, stroke,
                      h_diabetes_age, h_asthma_age, h_hypertension_age, h_joint_disorders_age, mi_ha_age, stroke_age)

#######################################################
## Dataset change: each variable will converted in five variables, each one from one wave.

g1s <- g1 %>% gather(variable, value, -(athlos_id2:wave)) %>%
  unite(temp, variable, wave) %>%
  spread(temp, value)

#######################################################
## Merge healthstatus latent class into g1s:
dim(g1s)
dim(aths_lcga_35)
g1s <- left_join(g1s, aths_lcga_35, by="athlos_id2")
dim(g1s)
g1s %>% filter(ybc_1==2) %>% group_by(class) %>% dplyr::summarise(mean=mean(healthstatus_4, na.rm=T))
g1s$hs <- NA
g1s$hs[g1s$class==2] <- 1
g1s$hs[g1s$class==3] <- 2
g1s$hs[g1s$class==1] <- 3
g1s %>% filter(ybc_1==2) %>% group_by(hs) %>% dplyr::summarise(mean=mean(healthstatus_4, na.rm=T))
g1s <- g1s %>% select(-class)

dim(aths_lcga_3545)
g1s <- left_join(g1s, aths_lcga_3545, by="athlos_id2")
dim(g1s)
g1s %>% filter(ybc_1==1) %>% group_by(class) %>% dplyr::summarise(mean=mean(healthstatus_4, na.rm=T))
g1s$hs[g1s$class==1] <- 1
g1s$hs[g1s$class==2] <- 3
g1s$hs[g1s$class==3] <- 2
g1s %>% filter(ybc_1==1) %>% group_by(hs) %>% dplyr::summarise(mean=mean(healthstatus_4, na.rm=T))
g1s <- g1s %>% select(-class)

dim(aths_lcga_45)
g1s <- left_join(g1s, aths_lcga_45, by="athlos_id2")
dim(g1s)
g1s %>% filter(ybc_1==0) %>% group_by(class) %>% dplyr::summarise(mean=mean(healthstatus_4, na.rm=T))
g1s$hs[g1s$class==1] <- 3
g1s$hs[g1s$class==2] <- 2
g1s$hs[g1s$class==3] <- 1
g1s %>% filter(ybc_1==0) %>% group_by(hs) %>% dplyr::summarise(mean=mean(healthstatus_4, na.rm=T))
g1s <- g1s %>% select(-class)


#######################################################
## Data management for the following variables:
## ybirth, region, sex, education, wealth, ever_smoked, vig_pa, 
## ydeath, death_age, yintw, ycens, cens_age.

g1s$ybirth <- g1s$ybirth_1 # first wave

g1s$region[g1s$country_1 %in% c(55, 71)] <- 0  #Denmark, Sweden
g1s$region[g1s$country_1 %in% c(51, 52, 58, 59, 66, 72)] <- 1
g1s$region[g1s$country_1 %in% c(33, 60, 63, 70)] <- 2 #Israel, Italy, Greece, Spain
 
g1s$sex <- g1s$sex_1 # first wave

g1s$education <- apply(g1s[,c("education_1", "education_2","education_3","education_4", "education_5")],1,max, na.rm=T) # maximum education
g1s$education[g1s$education=="-Inf"] <- NA

g1s$wealth <- apply(g1s[,c("wealth_1", "wealth_2","wealth_3","wealth_4", "wealth_5")],1,max, na.rm=T) # maximum wealth
g1s$wealth[g1s$wealth=="-Inf"] <- NA

g1s$ever_smoked <- apply(g1s[,c("ever_smoked_1", "ever_smoked_2","ever_smoked_3","ever_smoked_4", "ever_smoked_5")],1,max, na.rm=T)
g1s$ever_smoked[g1s$ever_smoked=="-Inf"] <- NA

g1s$vig_pa <- apply(g1s[,c("vig_pa_1", "vig_pa_2","vig_pa_3","vig_pa_4", "vig_pa_5")],1,max, na.rm=T)
g1s$vig_pa[g1s$vig_pa=="-Inf"] <- NA

g1s$ydeath <- apply(g1s[,c("ydeath_1", "ydeath_2","ydeath_3","ydeath_4", "ydeath_5")],1,max, na.rm=T)
g1s$ydeath[g1s$ydeath=="-Inf"] <- NA
g1s$death_age <- g1s$ydeath - g1s$ybirth

g1s$yintw <- apply(g1s[,c("yintw_1", "yintw_2","yintw_3","yintw_4", "yintw_5")],1,max, na.rm=T)
g1s$yintw[g1s$yintw=="-Inf"] <- NA

g1s$ycens <- g1s$yintw + 1 # Sum 1 to avoid that Tstart is < Tstop in Surv function
g1s$ycens[!is.na(g1s$ydeath)] <- g1s$ydeath[!is.na(g1s$ydeath)]
g1s$cens_age <- g1s$ycens - g1s$ybirth

#######################################################
## Data management for the following variables:
## marital_status, depression, h_cpd, 
## h_diabetes, h_asthma, h_hypertension, h_joint_disorders, mi_ha, stroke.

g1s$marital_status_1[is.na(g1s$marital_status_1)] <- g1s$marital_status_2[is.na(g1s$marital_status_1)]
g1s$marital_status_2[is.na(g1s$marital_status_2)] <- g1s$marital_status_1[is.na(g1s$marital_status_2)]
g1s$marital_status_3 <- g1s$marital_status_2

# single:
g1s$ms_single_age_1 <- g1s$ms_single_age_2 <- g1s$ms_single_age_3 <- g1s$ms_single_age_4 <- g1s$ms_single_age_5 <- NA
g1s$ms_single_age_1[!is.na(g1s$marital_status_1) & g1s$marital_status_1==1] <- g1s$age_1[!is.na(g1s$marital_status_1) & g1s$marital_status_1==1]
g1s$ms_single_age_2[!is.na(g1s$marital_status_2) & g1s$marital_status_2==1] <- g1s$age_2[!is.na(g1s$marital_status_2) & g1s$marital_status_2==1]
g1s$ms_single_age_3[!is.na(g1s$marital_status_3) & g1s$marital_status_3==1] <- g1s$age_3[!is.na(g1s$marital_status_3) & g1s$marital_status_3==1]
g1s$ms_single_age_4[!is.na(g1s$marital_status_4) & g1s$marital_status_4==1] <- g1s$age_4[!is.na(g1s$marital_status_4) & g1s$marital_status_4==1]
g1s$ms_single_age_5[!is.na(g1s$marital_status_5) & g1s$marital_status_5==1] <- g1s$age_5[!is.na(g1s$marital_status_5) & g1s$marital_status_5==1]

g1s$ms_single_age <- apply(g1s[,c("ms_single_age_1", "ms_single_age_2","ms_single_age_3","ms_single_age_4","ms_single_age_5")],1,min, na.rm=T)
g1s$ms_single_age[g1s$ms_single_age=="Inf"] <- NA

# married:
g1s$ms_marr_age_1 <- g1s$ms_marr_age_2 <- g1s$ms_marr_age_3 <- g1s$ms_marr_age_4 <- g1s$ms_marr_age_5 <- NA
g1s$ms_marr_age_1[!is.na(g1s$marital_status_1) & g1s$marital_status_1==2] <- g1s$age_1[!is.na(g1s$marital_status_1) & g1s$marital_status_1==2]
g1s$ms_marr_age_2[!is.na(g1s$marital_status_2) & g1s$marital_status_2==2] <- g1s$age_2[!is.na(g1s$marital_status_2) & g1s$marital_status_2==2]
g1s$ms_marr_age_3[!is.na(g1s$marital_status_3) & g1s$marital_status_3==2] <- g1s$age_3[!is.na(g1s$marital_status_3) & g1s$marital_status_3==2]
g1s$ms_marr_age_4[!is.na(g1s$marital_status_4) & g1s$marital_status_4==2] <- g1s$age_4[!is.na(g1s$marital_status_4) & g1s$marital_status_4==2]
g1s$ms_marr_age_5[!is.na(g1s$marital_status_5) & g1s$marital_status_5==2] <- g1s$age_5[!is.na(g1s$marital_status_5) & g1s$marital_status_5==2]

g1s$ms_marr_age <- apply(g1s[,c("ms_marr_age_1", "ms_marr_age_2","ms_marr_age_3","ms_marr_age_4","ms_marr_age_5")],1,min, na.rm=T)
g1s$ms_marr_age[g1s$ms_marr_age=="Inf"] <- NA

# Divorced:
g1s$ms_div_age_1 <- g1s$ms_div_age_2 <- g1s$ms_div_age_3 <- g1s$ms_div_age_4 <- g1s$ms_div_age_5 <- NA
g1s$ms_div_age_1[!is.na(g1s$marital_status_1) & g1s$marital_status_1==3] <- g1s$age_1[!is.na(g1s$marital_status_1) & g1s$marital_status_1==3]
g1s$ms_div_age_2[!is.na(g1s$marital_status_2) & g1s$marital_status_2==3] <- g1s$age_2[!is.na(g1s$marital_status_2) & g1s$marital_status_2==3]
g1s$ms_div_age_3[!is.na(g1s$marital_status_3) & g1s$marital_status_3==3] <- g1s$age_3[!is.na(g1s$marital_status_3) & g1s$marital_status_3==3]
g1s$ms_div_age_4[!is.na(g1s$marital_status_4) & g1s$marital_status_4==3] <- g1s$age_4[!is.na(g1s$marital_status_4) & g1s$marital_status_4==3]
g1s$ms_div_age_5[!is.na(g1s$marital_status_5) & g1s$marital_status_5==3] <- g1s$age_5[!is.na(g1s$marital_status_5) & g1s$marital_status_5==3]

g1s$ms_div_age <- apply(g1s[,c("ms_div_age_1", "ms_div_age_2","ms_div_age_3","ms_div_age_4","ms_div_age_5")],1,min, na.rm=T)
g1s$ms_div_age[g1s$ms_div_age=="Inf"] <- NA

# Widowed:
g1s$ms_wid_age_1 <- g1s$ms_wid_age_2 <- g1s$ms_wid_age_3 <- g1s$ms_wid_age_4 <- g1s$ms_wid_age_5 <- NA
g1s$ms_wid_age_1[!is.na(g1s$marital_status_1) & g1s$marital_status_1==4] <- g1s$age_1[!is.na(g1s$marital_status_1) & g1s$marital_status_1==4]
g1s$ms_wid_age_2[!is.na(g1s$marital_status_2) & g1s$marital_status_2==4] <- g1s$age_2[!is.na(g1s$marital_status_2) & g1s$marital_status_2==4]
g1s$ms_wid_age_3[!is.na(g1s$marital_status_3) & g1s$marital_status_3==4] <- g1s$age_3[!is.na(g1s$marital_status_3) & g1s$marital_status_3==4]
g1s$ms_wid_age_4[!is.na(g1s$marital_status_4) & g1s$marital_status_4==4] <- g1s$age_4[!is.na(g1s$marital_status_4) & g1s$marital_status_4==4]
g1s$ms_wid_age_5[!is.na(g1s$marital_status_5) & g1s$marital_status_5==4] <- g1s$age_5[!is.na(g1s$marital_status_5) & g1s$marital_status_5==4]

g1s$ms_wid_age <- apply(g1s[,c("ms_wid_age_1", "ms_wid_age_2","ms_wid_age_3","ms_wid_age_4","ms_wid_age_5")],1,min, na.rm=T)
g1s$ms_wid_age[g1s$ms_wid_age=="Inf"] <- NA

# depression:
g1s$depression_age_1 <- g1s$depression_age_2 <- g1s$depression_age_4 <- g1s$depression_age_5 <- NA
g1s$depression_age_1[!is.na(g1s$depression_1) & g1s$depression_1==1] <- g1s$age_1[!is.na(g1s$depression_1) & g1s$depression_1==1]
g1s$depression_age_2[!is.na(g1s$depression_2) & g1s$depression_2==1] <- g1s$age_2[!is.na(g1s$depression_2) & g1s$depression_2==1]
g1s$depression_age_4[!is.na(g1s$depression_4) & g1s$depression_4==1] <- g1s$age_4[!is.na(g1s$depression_4) & g1s$depression_4==1]
g1s$depression_age_5[!is.na(g1s$depression_5) & g1s$depression_5==1] <- g1s$age_5[!is.na(g1s$depression_5) & g1s$depression_5==1]

g1s$depression_age <- apply(g1s[,c("depression_age_1", "depression_age_2","depression_age_4","depression_age_5")], 1, min, na.rm=T)
g1s$depression_age[g1s$depression_age=="Inf"] <- NA

# h_cpd
g1s$acpd_1 <- g1s$acpd_2 <- g1s$acpd_4 <- g1s$acpd_5 <- NA
g1s$acpd_1[!is.na(g1s$h_cpd_1) & g1s$h_cpd_1==1] <- g1s$age_1[!is.na(g1s$h_cpd_1) & g1s$h_cpd_1==1]
g1s$acpd_2[!is.na(g1s$h_cpd_2) & g1s$h_cpd_2==1] <- g1s$age_2[!is.na(g1s$h_cpd_2) & g1s$h_cpd_2==1]
g1s$acpd_4[!is.na(g1s$h_cpd_4) & g1s$h_cpd_4==1] <- g1s$age_4[!is.na(g1s$h_cpd_4) & g1s$h_cpd_4==1]
g1s$acpd_5[!is.na(g1s$h_cpd_5) & g1s$h_cpd_5==1] <- g1s$age_5[!is.na(g1s$h_cpd_5) & g1s$h_cpd_5==1]

g1s$cpd_age <- apply(g1s[,c("acpd_1", "acpd_2","acpd_4","acpd_5")],1,min, na.rm=T)
g1s$cpd_age[g1s$cpd_age=="Inf"] <- NA

# h_diabetes
g1s$adiab_1 <- g1s$adiab_2 <- g1s$adiab_4 <- g1s$adiab_5 <- NA
g1s$adiab_1[!is.na(g1s$h_diabetes_1) & g1s$h_diabetes_1==1] <- g1s$age_1[!is.na(g1s$h_diabetes_1) & g1s$h_diabetes_1==1]
g1s$adiab_2[!is.na(g1s$h_diabetes_2) & g1s$h_diabetes_2==1] <- g1s$age_2[!is.na(g1s$h_diabetes_2) & g1s$h_diabetes_2==1]
g1s$adiab_4[!is.na(g1s$h_diabetes_4) & g1s$h_diabetes_4==1] <- g1s$age_4[!is.na(g1s$h_diabetes_4) & g1s$h_diabetes_4==1]
g1s$adiab_5[!is.na(g1s$h_diabetes_5) & g1s$h_diabetes_5==1] <- g1s$age_5[!is.na(g1s$h_diabetes_5) & g1s$h_diabetes_5==1]

g1s$diab_age <- apply(g1s[,c("adiab_1", "adiab_2","adiab_4","adiab_5")],1,min, na.rm=T)
g1s$diab_age[g1s$diab_age=="Inf"] <- NA

# h_asthma
g1s$aasthma_1 <- g1s$aasthma_2 <- g1s$aasthma_4 <- g1s$aasthma_5 <- NA
g1s$aasthma_1[!is.na(g1s$h_asthma_1) & g1s$h_asthma_1==1] <- g1s$age_1[!is.na(g1s$h_asthma_1) & g1s$h_asthma_1==1]
g1s$aasthma_2[!is.na(g1s$h_asthma_2) & g1s$h_asthma_2==1] <- g1s$age_2[!is.na(g1s$h_asthma_2) & g1s$h_asthma_2==1]
g1s$aasthma_4[!is.na(g1s$h_asthma_4) & g1s$h_asthma_4==1] <- g1s$age_4[!is.na(g1s$h_asthma_4) & g1s$h_asthma_4==1]
g1s$aasthma_5[!is.na(g1s$h_asthma_5) & g1s$h_asthma_5==1] <- g1s$age_5[!is.na(g1s$h_asthma_5) & g1s$h_asthma_5==1]

g1s$asthma_age <- apply(g1s[,c("aasthma_1", "aasthma_2","aasthma_4","aasthma_5")],1,min, na.rm=T)
g1s$asthma_age[g1s$asthma_age=="Inf"] <- NA

# h_hypertension
g1s$ahypertension_1 <- g1s$ahypertension_2 <- g1s$ahypertension_4 <- g1s$ahypertension_5 <- NA
g1s$ahypertension_1[!is.na(g1s$h_hypertension_1) & g1s$h_hypertension_1==1] <- g1s$age_1[!is.na(g1s$h_hypertension_1) & g1s$h_hypertension_1==1]
g1s$ahypertension_2[!is.na(g1s$h_hypertension_2) & g1s$h_hypertension_2==1] <- g1s$age_2[!is.na(g1s$h_hypertension_2) & g1s$h_hypertension_2==1]
g1s$ahypertension_4[!is.na(g1s$h_hypertension_4) & g1s$h_hypertension_4==1] <- g1s$age_4[!is.na(g1s$h_hypertension_4) & g1s$h_hypertension_4==1]
g1s$ahypertension_5[!is.na(g1s$h_hypertension_5) & g1s$h_hypertension_5==1] <- g1s$age_5[!is.na(g1s$h_hypertension_5) & g1s$h_hypertension_5==1]

g1s$hypertension_age <- apply(g1s[,c("ahypertension_1", "ahypertension_2","ahypertension_4","ahypertension_5")],1,min, na.rm=T)
g1s$hypertension_age[g1s$hypertension_age=="Inf"] <- NA

# h_joint_disorders
g1s$ajoint_disorders_1 <- g1s$ajoint_disorders_2 <- g1s$ajoint_disorders_4 <- g1s$ajoint_disorders_5 <- NA
g1s$ajoint_disorders_1[!is.na(g1s$h_joint_disorders_1) & g1s$h_joint_disorders_1==1] <- g1s$age_1[!is.na(g1s$h_joint_disorders_1) & g1s$h_joint_disorders_1==1]
g1s$ajoint_disorders_2[!is.na(g1s$h_joint_disorders_2) & g1s$h_joint_disorders_2==1] <- g1s$age_2[!is.na(g1s$h_joint_disorders_2) & g1s$h_joint_disorders_2==1]
g1s$ajoint_disorders_4[!is.na(g1s$h_joint_disorders_4) & g1s$h_joint_disorders_4==1] <- g1s$age_4[!is.na(g1s$h_joint_disorders_4) & g1s$h_joint_disorders_4==1]
g1s$ajoint_disorders_5[!is.na(g1s$h_joint_disorders_5) & g1s$h_joint_disorders_5==1] <- g1s$age_5[!is.na(g1s$h_joint_disorders_5) & g1s$h_joint_disorders_5==1]

g1s$joint_disorders_age <- apply(g1s[,c("ajoint_disorders_1", "ajoint_disorders_2","ajoint_disorders_4","ajoint_disorders_5")],1,min, na.rm=T)
g1s$joint_disorders_age[g1s$joint_disorders_age=="Inf"] <- NA

# mi_ha
g1s$ami_ha_1 <- g1s$ami_ha_2 <- g1s$ami_ha_4 <- g1s$ami_ha_5 <- NA
g1s$ami_ha_1[!is.na(g1s$mi_ha_1) & g1s$mi_ha_1==1] <- g1s$age_1[!is.na(g1s$mi_ha_1) & g1s$mi_ha_1==1]
g1s$ami_ha_2[!is.na(g1s$mi_ha_2) & g1s$mi_ha_2==1] <- g1s$age_2[!is.na(g1s$mi_ha_2) & g1s$mi_ha_2==1]
g1s$ami_ha_4[!is.na(g1s$mi_ha_4) & g1s$mi_ha_4==1] <- g1s$age_4[!is.na(g1s$mi_ha_4) & g1s$mi_ha_4==1]
g1s$ami_ha_5[!is.na(g1s$mi_ha_5) & g1s$mi_ha_5==1] <- g1s$age_5[!is.na(g1s$mi_ha_5) & g1s$mi_ha_5==1]

g1s$mi_ha_age <- apply(g1s[,c("ami_ha_1", "ami_ha_2","ami_ha_4","ami_ha_5")],1,min, na.rm=T)
g1s$mi_ha_age[g1s$mi_ha_age=="Inf"] <- NA

# stroke
g1s$astroke_1 <- g1s$astroke_2 <- g1s$astroke_4 <- g1s$astroke_5 <- NA
g1s$astroke_1[!is.na(g1s$stroke_1) & g1s$stroke_1==1] <- g1s$age_1[!is.na(g1s$stroke_1) & g1s$stroke_1==1]
g1s$astroke_2[!is.na(g1s$stroke_2) & g1s$stroke_2==1] <- g1s$age_2[!is.na(g1s$stroke_2) & g1s$stroke_2==1]
g1s$astroke_4[!is.na(g1s$stroke_4) & g1s$stroke_4==1] <- g1s$age_4[!is.na(g1s$stroke_4) & g1s$stroke_4==1]
g1s$astroke_5[!is.na(g1s$stroke_5) & g1s$stroke_5==1] <- g1s$age_5[!is.na(g1s$stroke_5) & g1s$stroke_5==1]

g1s$stroke_age <- apply(g1s[,c("astroke_1", "astroke_2","astroke_4","astroke_5")],1,min, na.rm=T)
g1s$stroke_age[g1s$stroke_age=="Inf"] <- NA

#save(g1s, file="g1s.rdata")
#######################################################
## Selection of final variables and complete cases.

g1n <- g1s %>% select(athlos_id2, age_1, region, sex, ybirth, education, wealth, 
                      ms_single_age, ms_marr_age, ms_div_age,ms_wid_age, 
                      vig_pa, ever_smoked, depression_age, 
                      healthstatus_1, healthstatus_2, healthstatus_4, healthstatus_5, hs, 
                      cpd_age, diab_age, asthma_age, hypertension_age, 
                      joint_disorders_age, mi_ha_age, stroke_age,
                      cens_age, death_age)

# Individuals with any missing in the following variables are removed:
g1n_cc <- g1n[complete.cases(g1n[,c("education", "wealth", "hs", "ever_smoked", "vig_pa")]), ]

#save(g1n_cc, file="g1n_cc.rdata")


####################################################################################################################################
####################################################################################################################################
####################################################################################################################################

load("g1n_cc.rdata")

g1n_cc <- g1n_cc %>% filter(is.na(g1n_cc$cpd_age) | (g1n_cc$age_1 - g1n_cc$cpd_age <= 0))

g1n_cc$age_1 <- g1n_cc$age_1 - 1
g1n_cc$ybc<-NA
g1n_cc$ybc[!is.na(g1n_cc$ybirth) & g1n_cc$ybirth>1945] <- 0
g1n_cc$ybc[!is.na(g1n_cc$ybirth) & g1n_cc$ybirth>1935 & g1n_cc$ybirth<=1945] <- 1
g1n_cc$ybc[!is.na(g1n_cc$ybirth) & g1n_cc$ybirth<=1935] <- 2

## Latent growth trajectories figure
g1n_lgm <- g1n_cc %>% select(athlos_id2, healthstatus_1, healthstatus_2, healthstatus_4, healthstatus_5, hs, ybc)
g1n_lgm <- gather(g1n_lgm, hss, healthstatus, healthstatus_1:healthstatus_5)
g1n_lgm <- arrange(g1n_lgm, athlos_id2, hss)
g1n_lgm$hss[g1n_lgm$hss == "healthstatus_1"] <- "Wave 1"
g1n_lgm$hss[g1n_lgm$hss == "healthstatus_2"] <- "Wave 2"
g1n_lgm$hss[g1n_lgm$hss == "healthstatus_4"] <- "Wave 4"
g1n_lgm$hss[g1n_lgm$hss == "healthstatus_5"] <- "Wave 5"
g1n_lgm <- g1n_lgm %>% filter(!is.na(healthstatus))

lower_ci <- function(mean, se, n, conf_level = 0.95){
  lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}
upper_ci <- function(mean, se, n, conf_level = 0.95){
  upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}

gd <- g1n_lgm %>% group_by(ybc, hs, hss) %>% 
  summarise(n=n(), mean_scores = mean(healthstatus), ssd = sd(healthstatus)) %>% 
  mutate(se = ssd / sqrt(n),
         lower_ci = lower_ci(mean_scores, se, n),
         upper_ci = upper_ci(mean_scores, se, n))  
  
gd$ybc[gd$ybc==0] <- "A) Birth cohort (>1945)"
gd$ybc[gd$ybc==1] <- "B) Birth cohort (1936-1945)"
gd$ybc[gd$ybc==2] <- "C) Birth cohort (<=1935)"

gd$hs[gd$hs==1] <- "A) High scores"
gd$hs[gd$hs==2] <- "B) Average scores"
gd$hs[gd$hs==3] <- "C) Low scores"

ggplot(gd, aes(x = hss, y = mean_scores, group = hs, shape=hs)) +
  geom_line(alpha = 0.4, size = 1) +  
#  geom_ribbon(data=gd, aes(ymin=lower_ci,ymax=upper_ci, group=hs, fill=hs)) +
  geom_point( size = 3) + 
  facet_grid(. ~ ybc)
##

g1m <- tmerge(g1n_cc, g1n_cc, id=athlos_id2, tstart=age_1, tstop=cens_age, 
                   death = event(death_age),
                   ms_single   = tdc(ms_single_age),
#                   ms_marr   = tdc(ms_marr_age),
                   ms_div   = tdc(ms_div_age),
                   ms_wid   = tdc(ms_wid_age),
                   depression   = tdc(depression_age),
                   diabetes   = tdc(diab_age),
                   asthma   = tdc(asthma_age),
                   hyper   = tdc(hypertension_age),
                   jdis   = tdc(joint_disorders_age),
                   mi_ha   = tdc(mi_ha_age),
                   stroke   = tdc(stroke_age),
                   cpd   = tdc(cpd_age)
)


aa <- aalen(Surv(tstart, tstop, death==1) ~ 
                   const(factor(region)) + const(factor(ybc)) + const(sex) + 
                   const(factor(education)) + const(factor(wealth)) + 
                   ms_single + ms_div + ms_wid + const(ever_smoked) + const(vig_pa) + 
                   const(factor(hs)) + diabetes + asthma + hyper + jdis + mi_ha + stroke +
                   depression + cpd,
                 id=g1m$athlos_id2, data=g1m, start.time = 49, max.time=100)

coef.aalen(aa)[,c(1,2,5)]

ggp <- ggplot.aalen(aa)
multiplot(ggp[1:12], col=3)

# Arranging dataset on account of time-varying covariates by YEAR of BIRTH + sum of conditions
#g1n_cc$age_1 <- g1n_cc$age_1 - 1
g1n_1935 <- g1n_cc %>% filter(ybirth <= 1935)
g1n_1945 <- g1n_cc %>% filter(ybirth >  1935 & ybirth <= 1945)
g1n_1957 <- g1n_cc %>% filter(ybirth >  1945)

g1m_1935 <- tmerge(g1n_1935, g1n_1935, id=athlos_id2, tstart=age_1, tstop=cens_age, 
                   death = event(death_age),
                   ms_single   = tdc(ms_single_age),
                   ms_div   = tdc(ms_div_age),
                   ms_wid   = tdc(ms_wid_age),
                   depression   = tdc(depression_age),
                   diabetes   = tdc(diab_age),
                   asthma   = tdc(asthma_age),
                   hyper   = tdc(hypertension_age),
                   jdis   = tdc(joint_disorders_age),
                   mi_ha   = tdc(mi_ha_age),
                   stroke   = tdc(stroke_age),
                   cpd   = tdc(cpd_age)
)

g1m_1945 <- tmerge(g1n_1945, g1n_1945, id=athlos_id2, tstart=age_1, tstop=cens_age, 
                   death = event(death_age),
                   ms_single   = tdc(ms_single_age),
                   ms_div   = tdc(ms_div_age),
                   ms_wid   = tdc(ms_wid_age),
                   depression   = tdc(depression_age),
                   diabetes   = tdc(diab_age),
                   asthma   = tdc(asthma_age),
                   hyper   = tdc(hypertension_age),
                   jdis   = tdc(joint_disorders_age),
                   mi_ha   = tdc(mi_ha_age),
                   stroke   = tdc(stroke_age),
                   cpd   = tdc(cpd_age)
)

g1m_1957 <- tmerge(g1n_1957, g1n_1957, id=athlos_id2, tstart=age_1, tstop=cens_age, 
                   death = event(death_age),
                   ms_single   = tdc(ms_single_age),
                   ms_div   = tdc(ms_div_age),
                   ms_wid   = tdc(ms_wid_age),
                   depression   = tdc(depression_age),
                   diabetes   = tdc(diab_age),
                   asthma   = tdc(asthma_age),
                   hyper   = tdc(hypertension_age),
                   jdis   = tdc(joint_disorders_age),
                   mi_ha   = tdc(mi_ha_age),
                   stroke   = tdc(stroke_age),
                   cpd   = tdc(cpd_age)
)

##
aa_1935 <- aalen(Surv(tstart, tstop, death==1) ~ 
                   (factor(region)) + (sex) + (factor(education)) + (factor(wealth)) + 
                   (ever_smoked) + (vig_pa) +  
                   ms_single + ms_div + ms_wid + 
                   diabetes + asthma + hyper + mi_ha + stroke + jdis + 
                   depression + cpd*factor(hs) ,
                 id=g1m_1935$athlos_id2, data=g1m_1935, start.time = 67, max.time=100)

adj_ggp_1935 <- ggplot.aalen.mod(aa_1935, xlab="Age", ylim=c(-0.2,0.5), xlim=c(50,100))
multiplot(adj_ggp_1935[1:23], col=4)
multiplot(adj_ggp_1935[24:28], col=2)

ds <- adj_ggp_1935[[24]]$data
ggp <- ggplot(data=ds, aes(x=time, y=est)) + geom_step() + coord_cartesian(ylim = c(-0.2,0.6), xlim=c(50,100))
ggp <- ggp + xlab("") + ylab("") + geom_hline(yintercept=0, linetype="dashed", color = "red")
(adj_cpd_1935 <- ggp + geom_ribbon(data=ds, aes(ymin=ul,ymax=nl),alpha=0.3, fill="blue") + theme(legend.position="none"))

dsg <- rbind(cbind(adj_ggp_1935[[25]]$data, group = "middle"), cbind(adj_ggp_1935[[26]]$data, group = "poor"))
ggp <- ggplot(data=dsg, aes(x=time, y=est, group=group, fill=group)) + geom_step() + coord_cartesian(ylim = c(-0.2,0.6), xlim=c(50,100))
ggp <- ggp + xlab("") + ylab("") + geom_hline(yintercept=0, linetype="dashed", color = "red")
(adj_hsg_1935 <- ggp + geom_ribbon(data=dsg, aes(ymin=ul,ymax=nl, group=group, fill=group),alpha=0.3) + theme(legend.position="none"))

dsg <- rbind(cbind(adj_ggp_1935[[27]]$data, group = "middle"), cbind(adj_ggp_1935[[28]]$data, group = "poor"))
ggp <- ggplot(data=dsg, aes(x=time, y=est, group=group, fill=group)) + geom_step() + coord_cartesian(ylim = c(-0.2,0.6), xlim=c(50,100))
ggp <- ggp + xlab("") + ylab("") + geom_hline(yintercept=0, linetype="dashed", color = "red")
(adj_cpd_hsg_1935 <- ggp + geom_ribbon(data=dsg, aes(ymin=ul,ymax=nl, group=group, fill=group),alpha=0.3) + theme(legend.position="none"))

##

##
aa_1945 <- aalen(Surv(tstart, tstop, death==1) ~ 
                   (factor(region)) + (sex) + (factor(education)) + (factor(wealth)) + 
                   ms_single + ms_div + ms_wid + (ever_smoked) + (vig_pa) + 
                   diabetes + asthma + hyper + mi_ha + stroke + jdis + 
                   depression + cpd + factor(hs),
                 id=g1m_1945$athlos_id2, data=g1m_1945, start.time = 57, max.time=100)

adj_ggp_1945 <- ggplot.aalen.mod(aa_1945, xlab="Age", ylim=c(-0.1,0.1), xlim=c(50,100))
multiplot(adj_ggp_1945[1:23], col=4)
multiplot(adj_ggp_1945[24:26], col=2)

ds <- adj_ggp_1945[[24]]$data
ggp <- ggplot(data=ds, aes(x=time, y=est)) + geom_step() + coord_cartesian(ylim = c(-0.1,0.2), xlim=c(50,100))
ggp <- ggp + xlab("") + ylab("") + geom_hline(yintercept=0, linetype="dashed", color = "red")
(adj_cpd_1945 <- ggp + geom_ribbon(data=ds, aes(ymin=ul,ymax=nl),alpha=0.3, fill="blue"))

dsg <- rbind(cbind(adj_ggp_1945[[25]]$data, group = "middle"), cbind(adj_ggp_1945[[26]]$data, group = "poor"))
ggp <- ggplot(data=dsg, aes(x=time, y=est, group=group, fill=group)) + geom_step() + coord_cartesian(ylim = c(-0.1,0.2), xlim=c(50,100))
ggp <- ggp + xlab("") + ylab("") + geom_hline(yintercept=0, linetype="dashed", color = "red")
(adj_hsg_1945 <- ggp + geom_ribbon(data=dsg, aes(ymin=ul,ymax=nl, group=group, fill=group),alpha=0.3) + theme(legend.position="none"))

##
aa_1957 <- aalen(Surv(tstart, tstop, death==1) ~ 
                   (factor(region)) + (sex) + (factor(education)) + (factor(wealth)) + 
                   ms_single + ms_div + ms_wid + (ever_smoked) + (vig_pa) + 
                   diabetes + asthma + hyper + mi_ha + stroke + jdis + 
                   depression + cpd + factor(hs),
                 id=g1m_1957$athlos_id2, data=g1m_1957, start.time = 49, max.time=80)

adj_ggp_1957 <- ggplot.aalen.mod(aa_1957, xlab="Age", ylim=c(-0.05,0.1), xlim=c(50,100))
multiplot(adj_ggp_1957[1:23], col=4)
multiplot(adj_ggp_1957[24:26], col=2)

ds <- adj_ggp_1957[[24]]$data
ggp <- ggplot(data=ds, aes(x=time, y=est)) + geom_step() + coord_cartesian(ylim = c(-0.05,0.1), xlim=c(50,100))
ggp <- ggp + xlab("") + ylab("") + geom_hline(yintercept=0, linetype="dashed", color = "red")
(adj_cpd_1957 <- ggp + geom_ribbon(data=ds, aes(ymin=ul,ymax=nl),alpha=0.3, fill="blue"))

dsg <- rbind(cbind(adj_ggp_1957[[25]]$data, group = "middle"), cbind(adj_ggp_1957[[26]]$data, group = "poor"))
ggp <- ggplot(data=dsg, aes(x=time, y=est, group=group, fill=group)) + geom_step() + coord_cartesian(ylim = c(-0.05,0.1), xlim=c(50,100))
ggp <- ggp + xlab("") + ylab("") + geom_hline(yintercept=0, linetype="dashed", color = "red")
(adj_hsg_1957 <- ggp + geom_ribbon(data=dsg, aes(ymin=ul,ymax=nl, group=group, fill=group),alpha=0.3) + theme(legend.position="none"))

##

g1n_1935 <- g1n_cc %>% filter(ybirth <= 1935)
g1n_1945 <- g1n_cc %>% filter(ybirth >  1935 & ybirth <= 1945)
g1n_1957 <- g1n_cc %>% filter(ybirth >  1945)

g1m_1935 <- tmerge(g1n_1935, g1n_1935, id=athlos_id2, tstart=age_1, tstop=cens_age, 
                   death = event(death_age),
                   cpd   = tdc(cpd_age)
)

g1m_1945 <- tmerge(g1n_1945, g1n_1945, id=athlos_id2, tstart=age_1, tstop=cens_age, 
                   death = event(death_age),
                   cpd   = tdc(cpd_age)
)

g1m_1957 <- tmerge(g1n_1957, g1n_1957, id=athlos_id2, tstart=age_1, tstop=cens_age, 
                   death = event(death_age),
                   cpd   = tdc(cpd_age)
)

##
aa_1935 <- aalen(Surv(tstart, tstop, death==1) ~ cpd , id=g1m_1935$athlos_id2, data=g1m_1935, start.time = 67, max.time=100)
ggp_1935 <- ggplot.aalen(aa_1935)
ds <- ggp_1935[[2]]$data
ggp <- ggplot(data=ds, aes(x=time, y=est)) + geom_step() + coord_cartesian(ylim = c(-0.2,0.6), xlim=c(50,100))
ggp <- ggp + xlab("") + ylab("") + geom_hline(yintercept=0, linetype="dashed", color = "red")
(A_cpd_1935 <- ggp + geom_ribbon(data=ds, aes(ymin=ul,ymax=nl),alpha=0.3, fill="blue")+ theme(legend.position="none"))

#
aa_1935 <- aalen(Surv(tstart, tstop, death==1) ~ factor(hs) , id=g1m_1935$athlos_id2, data=g1m_1935, start.time = 67, max.time=100)
ggp_1935 <- ggplot.aalen(aa_1935)
dsg <- rbind(cbind(ggp_1935[[2]]$data, group = "middle"), cbind(ggp_1935[[3]]$data, group = "poor"))
ggp <- ggplot(data=dsg, aes(x=time, y=est, group=group, fill=group)) + geom_step() + coord_cartesian(ylim = c(-0.2,0.6), xlim=c(50,100))
ggp <- ggp + xlab("") + ylab("") + geom_hline(yintercept=0, linetype="dashed", color = "red")  #+ labs(title = "COPD <=1935")
(B_hsg_1935 <- ggp + geom_ribbon(data=dsg, aes(ymin=ul,ymax=nl, group=group, fill=group),alpha=0.3)  + theme(legend.position="none"))

#
aa_1935 <- aalen(Surv(tstart, tstop, death==1) ~ cpd*factor(hs) , id=g1m_1935$athlos_id2, data=g1m_1935, start.time = 67, max.time=100)
ggp_1935 <- ggplot.aalen(aa_1935)
ds <- ggp_1935[[2]]$data
ggp <- ggplot(data=ds, aes(x=time, y=est)) + geom_step() + coord_cartesian(ylim = c(-0.2,0.6), xlim=c(50,100))
ggp <- ggp + xlab("") + ylab("") + geom_hline(yintercept=0, linetype="dashed", color = "red")
(C_cpd_1935 <- ggp + geom_ribbon(data=ds, aes(ymin=ul,ymax=nl),alpha=0.3, fill="blue") + theme(legend.position="none"))

dsg <- rbind(cbind(ggp_1935[[3]]$data, group = "middle"), cbind(ggp_1935[[4]]$data, group = "poor"))
ggp <- ggplot(data=dsg, aes(x=time, y=est, group=group, fill=group)) + geom_step() + coord_cartesian(ylim = c(-0.2,0.6), xlim=c(50,100))
ggp <- ggp + xlab("") + ylab("") + geom_hline(yintercept=0, linetype="dashed", color = "red")
(C_hsg_1935 <- ggp + geom_ribbon(data=dsg, aes(ymin=ul,ymax=nl, group=group, fill=group),alpha=0.3) + theme(legend.position="none"))

dsg <- rbind(cbind(ggp_1935[[5]]$data, group = "middle"), cbind(ggp_1935[[6]]$data, group = "poor"))
ggp <- ggplot(data=dsg, aes(x=time, y=est, group=group, fill=group)) + geom_step() + coord_cartesian(ylim = c(-0.2,0.6), xlim=c(50,100))
ggp <- ggp + xlab("") + ylab("") + geom_hline(yintercept=0, linetype="dashed", color = "red")
(C_cpd_hsg_1935 <- ggp + geom_ribbon(data=dsg, aes(ymin=ul,ymax=nl, group=group, fill=group),alpha=0.3) + theme(legend.position="none"))

##

aa_1945 <- aalen(Surv(tstart, tstop, death==1) ~ cpd , id=g1m_1945$athlos_id2, data=g1m_1945, start.time = 57, max.time=100)
ggp_1945 <- ggplot.aalen(aa_1945)
ds <- ggp_1945[[2]]$data
ggp <- ggplot(data=ds, aes(x=time, y=est)) + geom_step() + coord_cartesian(ylim = c(-0.1,0.2), xlim=c(50,100))
ggp <- ggp + xlab("") + ylab("") + geom_hline(yintercept=0, linetype="dashed", color = "red")
(A_cpd_1945 <- ggp + geom_ribbon(data=ds, aes(ymin=ul,ymax=nl),alpha=0.3, fill="blue")+ theme(legend.position="none"))

#
aa_1945 <- aalen(Surv(tstart, tstop, death==1) ~ factor(hs) , id=g1m_1945$athlos_id2, data=g1m_1945, start.time = 57, max.time=100)
ggp_1945 <- ggplot.aalen(aa_1945)
dsg <- rbind(cbind(ggp_1945[[2]]$data, group = "middle"), cbind(ggp_1945[[3]]$data, group = "poor"))
ggp <- ggplot(data=dsg, aes(x=time, y=est, group=group, fill=group)) + geom_step() + coord_cartesian(ylim = c(-0.1,0.2), xlim=c(50,100))
ggp <- ggp + xlab("") + ylab("") + geom_hline(yintercept=0, linetype="dashed", color = "red")
(B_hsg_1945 <- ggp + geom_ribbon(data=dsg, aes(ymin=ul,ymax=nl, group=group, fill=group),alpha=0.3) + theme(legend.position="none"))

#
aa_1945 <- aalen(Surv(tstart, tstop, death==1) ~ cpd*factor(hs) , id=g1m_1945$athlos_id2, data=g1m_1945, start.time = 57, max.time=100)
ggp_1945 <- ggplot.aalen(aa_1945)

ds <- ggp_1945[[2]]$data
ggp <- ggplot(data=ds, aes(x=time, y=est)) + geom_step() + coord_cartesian(ylim = c(-0.1,0.2), xlim=c(50,100))
ggp <- ggp + xlab("") + ylab("") + geom_hline(yintercept=0, linetype="dashed", color = "red")
(C_cpd_1945 <- ggp + geom_ribbon(data=ds, aes(ymin=ul,ymax=nl),alpha=0.3, fill="blue"))

dsg <- rbind(cbind(ggp_1945[[3]]$data, group = "middle"), cbind(ggp_1945[[4]]$data, group = "poor"))
ggp <- ggplot(data=dsg, aes(x=time, y=est, group=group, fill=group)) + geom_step() + coord_cartesian(ylim = c(-0.1,0.2), xlim=c(50,100))
ggp <- ggp + xlab("") + ylab("") + geom_hline(yintercept=0, linetype="dashed", color = "red")
(C_hsg_1945 <- ggp + geom_ribbon(data=dsg, aes(ymin=ul,ymax=nl, group=group, fill=group),alpha=0.3) + theme(legend.position="none"))

dsg <- rbind(cbind(ggp_1945[[5]]$data, group = "middle"), cbind(ggp_1945[[6]]$data, group = "poor"))
ggp <- ggplot(data=dsg, aes(x=time, y=est, group=group, fill=group)) + geom_step() + coord_cartesian(ylim = c(-0.1,0.2), xlim=c(50,100))
ggp <- ggp + xlab("") + ylab("") + geom_hline(yintercept=0, linetype="dashed", color = "red")
(C_cpd_hsg_1945 <- ggp + geom_ribbon(data=dsg, aes(ymin=ul,ymax=nl, group=group, fill=group),alpha=0.3)+ theme(legend.position="none"))

##

aa_1957 <- aalen(Surv(tstart, tstop, death==1) ~ cpd , id=g1m_1957$athlos_id2, data=g1m_1957, start.time = 49, max.time=80)
ggp_1957 <- ggplot.aalen(aa_1957)
ds <- ggp_1957[[2]]$data
ggp <- ggplot(data=ds, aes(x=time, y=est)) + geom_step() + coord_cartesian(ylim = c(-0.05,0.1), xlim=c(50,100))
ggp <- ggp + xlab("") + ylab("") + geom_hline(yintercept=0, linetype="dashed", color = "red")
(A_cpd_1957 <- ggp + geom_ribbon(data=ds, aes(ymin=ul,ymax=nl),alpha=0.3, fill="blue")+ theme(legend.position="none"))

#
aa_1957 <- aalen(Surv(tstart, tstop, death==1) ~ factor(hs) , id=g1m_1957$athlos_id2, data=g1m_1957, start.time = 49, max.time=80)
ggp_1957 <- ggplot.aalen(aa_1957)
dsg <- rbind(cbind(ggp_1957[[2]]$data, group = "middle"), cbind(ggp_1957[[3]]$data, group = "poor"))
ggp <- ggplot(data=dsg, aes(x=time, y=est, group=group, fill=group)) + geom_step() + coord_cartesian(ylim = c(-0.05,0.1), xlim=c(50,100))
ggp <- ggp + xlab("") + ylab("") + geom_hline(yintercept=0, linetype="dashed", color = "red")
(B_hsg_1957 <- ggp + geom_ribbon(data=dsg, aes(ymin=ul,ymax=nl, group=group, fill=group),alpha=0.3) + theme(legend.position="none"))

#
aa_1957 <- aalen(Surv(tstart, tstop, death==1) ~ cpd*factor(hs) , id=g1m_1957$athlos_id2, data=g1m_1957, start.time = 49, max.time=80)
ggp_1957 <- ggplot.aalen(aa_1957)
ds <- ggp_1957[[2]]$data
ggp <- ggplot(data=ds, aes(x=time, y=est)) + geom_step() + coord_cartesian(ylim = c(-0.05,0.1), xlim=c(50,100))
ggp <- ggp + xlab("") + ylab("") + geom_hline(yintercept=0, linetype="dashed", color = "red")
(C_cpd_1957 <- ggp + geom_ribbon(data=ds, aes(ymin=ul,ymax=nl),alpha=0.3, fill="blue"))

dsg <- rbind(cbind(ggp_1957[[3]]$data, group = "middle"), cbind(ggp_1957[[4]]$data, group = "poor"))
ggp <- ggplot(data=dsg, aes(x=time, y=est, group=group, fill=group)) + geom_step() + coord_cartesian(ylim = c(-0.05,0.1), xlim=c(50,100))
ggp <- ggp + xlab("") + ylab("") + geom_hline(yintercept=0, linetype="dashed", color = "red")
(C_hsg_1957 <- ggp + geom_ribbon(data=dsg, aes(ymin=ul,ymax=nl, group=group, fill=group),alpha=0.3) + theme(legend.position="none"))

dsg <- rbind(cbind(ggp_1957[[5]]$data, group = "middle"), cbind(ggp_1957[[6]]$data, group = "poor"))
ggp <- ggplot(data=dsg, aes(x=time, y=est, group=group, fill=group)) + geom_step() + coord_cartesian(ylim = c(-0.05,0.1), xlim=c(50,100))
ggp <- ggp + xlab("") + ylab("") + geom_hline(yintercept=0, linetype="dashed", color = "red")
(C_cpd_hsg_1957 <- ggp + geom_ribbon(data=dsg, aes(ymin=ul,ymax=nl, group=group, fill=group),alpha=0.3)+ theme(legend.position="none"))


##
require(gridExtra)
grid.arrange(A_cpd_1935,B_hsg_1935,C_cpd_1935,C_hsg_1935,C_cpd_hsg_1935, adj_cpd_1935,adj_hsg_1935,adj_cpd_hsg_1935, 
             A_cpd_1945,B_hsg_1945,C_cpd_1945,C_hsg_1945,C_cpd_hsg_1945, adj_cpd_1945,adj_hsg_1945, adj_hsg_1945, #adj_cpd_hsg_1945, 
             A_cpd_1957,B_hsg_1957,C_cpd_1957,C_hsg_1957,C_cpd_hsg_1957, adj_cpd_1957,adj_hsg_1957, adj_hsg_1957, #adj_cpd_hsg_1957, 
             nrow=3, bottom="Age", left="Cumulative coefficients")

grid.arrange(A_cpd_1935,       A_cpd_1945,       A_cpd_1957,
             B_hsg_1935,       B_hsg_1945,       B_hsg_1957,
             C_cpd_1935,       C_cpd_1945,       C_cpd_1957,
             C_hsg_1935,       C_hsg_1945,       C_hsg_1957,
             C_cpd_hsg_1935,   C_cpd_hsg_1945,   C_cpd_hsg_1957, 
             adj_cpd_1935,     adj_cpd_1945,     adj_cpd_1957,
             adj_hsg_1935,     adj_hsg_1945,     adj_hsg_1957,
             adj_cpd_hsg_1935, #adj_cpd_hsg_1945, adj_cpd_hsg_1957, 
             nrow=8, bottom="Age", left="Cumulative coefficients")

grid.arrange(adj_cpd_1935,adj_hsg_1935,adj_cpd_hsg_1935, 
             adj_cpd_1945,adj_hsg_1945, adj_hsg_1945, #adj_cpd_hsg_1945, 
             adj_cpd_1957,adj_hsg_1957, adj_hsg_1957, #adj_cpd_hsg_1957, 
             nrow=3, bottom="Age", left="Cumulative coefficients")


##############################################################
##############################################################

g1Sel <- g1n_cc %>% select(ybc, sex, age_1, education, wealth, region, hs, vig_pa, ever_smoked)

g1Sel$age_1 <- g1Sel$age_1 + 1
g1Sel$sex <- as.factor(g1Sel$sex)
g1Sel$education <- as.factor(g1Sel$education)
g1Sel$wealth <- as.factor(g1Sel$wealth)
g1Sel$region <- as.factor(g1Sel$region)
g1Sel$hs <- as.factor(g1Sel$hs)
g1Sel$vig_pa <- as.factor(g1Sel$vig_pa)
g1Sel$ever_smoked <- as.factor(g1Sel$ever_smoked)

ext <- compareGroups(ybc ~ ., data=g1Sel) 
ext <- createTable(ext)
export2word(ext, file = "table_1.docx")

g1Sel_bis <- g1n_cc %>% select(ybc, ms_single_age, ms_marr_age, ms_div_age, ms_wid_age,
                           depression_age, cpd_age, diab_age, asthma_age, hypertension_age, 
                           joint_disorders_age, mi_ha_age, stroke_age, death_age)

g1Sel_bis$single <- 'No' 
g1Sel_bis$single[!is.na(g1Sel_bis$ms_single_age)] <- 'Yes'
frq(g1Sel_bis$single)

g1Sel_bis$married <-'No' 
g1Sel_bis$married[!is.na(g1Sel_bis$ms_marr_age)] <- 'Yes'
frq(g1Sel_bis$married)

g1Sel_bis$div <-'No' 
g1Sel_bis$div[!is.na(g1Sel_bis$ms_div_age)] <- 'Yes'
frq(g1Sel_bis$div)

g1Sel_bis$wid <-'No' 
g1Sel_bis$wid[!is.na(g1Sel_bis$ms_wid_age)] <- 'Yes'
frq(g1Sel_bis$wid)

g1Sel_bis$depression <-'No' 
g1Sel_bis$depression[!is.na(g1Sel_bis$depression_age)] <- 'Yes'
frq(g1Sel_bis$depression)

g1Sel_bis$cpd <-'No' 
g1Sel_bis$cpd[!is.na(g1Sel_bis$cpd_age)] <- 'Yes'
frq(g1Sel_bis$cpd)

g1Sel_bis$diab <-'No' 
g1Sel_bis$diab[!is.na(g1Sel_bis$diab_age)] <- 'Yes'
frq(g1Sel_bis$diab)

g1Sel_bis$asthma <-'No' 
g1Sel_bis$asthma[!is.na(g1Sel_bis$asthma_age)] <- 'Yes'
frq(g1Sel_bis$asthma)

g1Sel_bis$hyp <-'No' 
g1Sel_bis$hyp[!is.na(g1Sel_bis$hypertension_age)] <- 'Yes'
frq(g1Sel_bis$hyp)

g1Sel_bis$jdis <-'No' 
g1Sel_bis$jdis[!is.na(g1Sel_bis$joint_disorders_age)] <- 'Yes'
frq(g1Sel_bis$jdis)

g1Sel_bis$mi_ha <-'No' 
g1Sel_bis$mi_ha[!is.na(g1Sel_bis$mi_ha_age)] <- 'Yes'
frq(g1Sel_bis$mi_ha)

g1Sel_bis$stroke <-'No' 
g1Sel_bis$stroke[!is.na(g1Sel_bis$stroke_age)] <- 'Yes'
frq(g1Sel_bis$stroke)

g1Sel_bis$death <-'No' 
g1Sel_bis$death[!is.na(g1Sel_bis$death_age)] <- 'Yes'
frq(g1Sel_bis$death)

g1Sel_bis <- g1Sel_bis %>% select(ybc, single, married, div, wid,
                               depression, cpd, 
                               diab, asthma, 
                               hyp, 
                               jdis, 
                               mi_ha, stroke, death)

ext <- compareGroups(ybc ~ ., data=g1Sel_bis) 
ext <- createTable(ext)
export2word(ext, file = "table_1_bis.docx")


########################################################################
## MODEL 1:
  
load("g1n_cc.rdata")

g1n_cc$mm <- NA
g1n_cc$mm[!is.na(g1n_cc$cpd_age)] <- 0   
g1n_cc$mm[g1n_cc$age_1==g1n_cc$cpd_age] <- 1  # cpd va ser segurament abans de l'inici de l'estudi

g1n_cc <- g1n_cc %>% filter(is.na(g1n_cc$cpd_age) | (g1n_cc$age_1 - g1n_cc$cpd_age < 0))

g1n_cc$age_1 <- g1n_cc$age_1 - 1
g1n_1935 <- g1n_cc %>% filter(ybirth <= 1935)
g1n_1945 <- g1n_cc %>% filter(ybirth >  1935 & ybirth <= 1945)
g1n_1957 <- g1n_cc %>% filter(ybirth >  1945)

g1m_1935 <- tmerge(g1n_1935, g1n_1935, id=athlos_id2, tstart=age_1, tstop=cens_age, 
                   cpd = event(cpd_age),
                   ms_single   = tdc(ms_single_age),
                   #                   ms_marr   = tdc(ms_marr_age),
                   ms_div   = tdc(ms_div_age),
                   ms_wid   = tdc(ms_wid_age),
                   depression   = tdc(depression_age),
                   diabetes   = tdc(diab_age),
                   asthma   = tdc(asthma_age),
                   hyper   = tdc(hypertension_age),
                   jdis   = tdc(joint_disorders_age),
                   mi_ha   = tdc(mi_ha_age),
                   stroke   = tdc(stroke_age)
)

g1m_1945 <- tmerge(g1n_1945, g1n_1945, id=athlos_id2, tstart=age_1, tstop=cens_age, 
                   cpd = event(cpd_age),
                   ms_single   = tdc(ms_single_age),
                   #                   ms_marr   = tdc(ms_marr_age),
                   ms_div   = tdc(ms_div_age),
                   ms_wid   = tdc(ms_wid_age),
                   depression   = tdc(depression_age),
                   diabetes   = tdc(diab_age),
                   asthma   = tdc(asthma_age),
                   hyper   = tdc(hypertension_age),
                   jdis   = tdc(joint_disorders_age),
                   mi_ha   = tdc(mi_ha_age),
                   stroke   = tdc(stroke_age)
)

g1m_1957 <- tmerge(g1n_1957, g1n_1957, id=athlos_id2, tstart=age_1, tstop=cens_age, 
                   cpd = event(cpd_age),
                   ms_single   = tdc(ms_single_age),
                   #                   ms_marr   = tdc(ms_marr_age),
                   ms_div   = tdc(ms_div_age),
                   ms_wid   = tdc(ms_wid_age),
                   depression   = tdc(depression_age),
                   diabetes   = tdc(diab_age),
                   asthma   = tdc(asthma_age),
                   hyper   = tdc(hypertension_age),
                   jdis   = tdc(joint_disorders_age),
                   mi_ha   = tdc(mi_ha_age),
                   stroke   = tdc(stroke_age)
)

##
aa_1935 <- aalen(Surv(tstart, tstop, cpd==1) ~ 
                   const(factor(region)) + const(sex) + const(factor(education)) + const(factor(wealth)) + 
                   const(ever_smoked) + const(vig_pa) + factor(hs) + 
                   ms_single + ms_div + ms_wid + 
                   diabetes + asthma + hyper + mi_ha + stroke + jdis + 
                   depression,
                 id=g1m_1935$athlos_id2, data=g1m_1935, start.time = 67, max.time=100)

coef.aalen(aa_1935)[,c(1,2,5)]

ggp_1935 <- ggplot.aalen(aa_1935)
multiplot(ggp_1935[1:13], col=4)
##

##
aa_1945 <- aalen(Surv(tstart, tstop, cpd==1) ~ 
                   const(factor(region)) + const(sex) + const(factor(education)) + const(factor(wealth)) + 
                   ms_single + ms_div + ms_wid + const(ever_smoked) + const(vig_pa) + 
                   const(factor(hs)) + diabetes + asthma + hyper + mi_ha + stroke + jdis + 
                   depression,
                 id=g1m_1945$athlos_id2, data=g1m_1945, start.time = 57, max.time=100)

coef.aalen(aa_1945)[,c(1,2,5)]

ggp_1945 <- ggplot.aalen(aa_1945)
multiplot(ggp_1945[1:12], col=3)
##

##
aa_1957 <- aalen(Surv(tstart, tstop, cpd==1) ~ 
                   const(factor(region)) + const(sex) + const(factor(education)) + const(factor(wealth)) + 
                   ms_single + ms_div + ms_wid + const(ever_smoked) + const(vig_pa) + 
                   const(factor(hs)) + diabetes + asthma + hyper + mi_ha + stroke + jdis + 
                   depression,
                 id=g1m_1957$athlos_id2, data=g1m_1957, start.time = 49, max.time=80)

coef.aalen(aa_1957)[,c(1,2,5)]

ggp_1957 <- ggplot.aalen(aa_1957)
multiplot(ggp_1957[1:12], col=3)
##


########################################################################
## MODEL 2:

load("g1n_cc.rdata")

g1n_cc$mm <- NA
g1n_cc$mm[!is.na(g1n_cc$cpd_age)] <- 0   
g1n_cc$mm[g1n_cc$age_1==g1n_cc$cpd_age] <- 1  # cpd va ser segurament abans de l'inici de l'estudi

g1n_cc <- g1n_cc %>% filter(is.na(g1n_cc$cpd_age) | (g1n_cc$age_1 - g1n_cc$cpd_age < 0))

g1n_cc$age_1 <- g1n_cc$age_1 - 1
g1n_1935 <- g1n_cc %>% filter(ybirth <= 1935)
g1n_1945 <- g1n_cc %>% filter(ybirth >  1935 & ybirth <= 1945)
g1n_1957 <- g1n_cc %>% filter(ybirth >  1945)

g1m_1935 <- tmerge(g1n_1935, g1n_1935, id=athlos_id2, tstart=age_1, tstop=cens_age, 
                   death = event(death_age),
                   ms_single   = tdc(ms_single_age),
                   #                   ms_marr   = tdc(ms_marr_age),
                   ms_div   = tdc(ms_div_age),
                   ms_wid   = tdc(ms_wid_age),
                   depression   = tdc(depression_age),
                   diabetes   = tdc(diab_age),
                   asthma   = tdc(asthma_age),
                   hyper   = tdc(hypertension_age),
                   jdis   = tdc(joint_disorders_age),
                   mi_ha   = tdc(mi_ha_age),
                   stroke   = tdc(stroke_age),
                   cpd   = tdc(cpd_age)
)

g1m_1945 <- tmerge(g1n_1945, g1n_1945, id=athlos_id2, tstart=age_1, tstop=cens_age, 
                   death = event(death_age),
                   ms_single   = tdc(ms_single_age),
                   #                   ms_marr   = tdc(ms_marr_age),
                   ms_div   = tdc(ms_div_age),
                   ms_wid   = tdc(ms_wid_age),
                   depression   = tdc(depression_age),
                   diabetes   = tdc(diab_age),
                   asthma   = tdc(asthma_age),
                   hyper   = tdc(hypertension_age),
                   jdis   = tdc(joint_disorders_age),
                   mi_ha   = tdc(mi_ha_age),
                   stroke   = tdc(stroke_age),
                   cpd   = tdc(cpd_age)
)

g1m_1957 <- tmerge(g1n_1957, g1n_1957, id=athlos_id2, tstart=age_1, tstop=cens_age, 
                   death = event(death_age),
                   ms_single   = tdc(ms_single_age),
                   #                   ms_marr   = tdc(ms_marr_age),
                   ms_div   = tdc(ms_div_age),
                   ms_wid   = tdc(ms_wid_age),
                   depression   = tdc(depression_age),
                   diabetes   = tdc(diab_age),
                   asthma   = tdc(asthma_age),
                   hyper   = tdc(hypertension_age),
                   jdis   = tdc(joint_disorders_age),
                   mi_ha   = tdc(mi_ha_age),
                   stroke   = tdc(stroke_age),
                   cpd   = tdc(cpd_age)
)

##
aa_1935 <- aalen(Surv(tstart, tstop, death==1) ~ 
                   const(factor(region)) + const(sex) + const(factor(education)) + const(factor(wealth)) + 
                   const(ever_smoked) + const(vig_pa) + factor(hs) + 
                   ms_single + ms_div + ms_wid + 
                   diabetes + asthma + hyper + mi_ha + stroke + jdis + 
                   depression + cpd ,
                 id=g1m_1935$athlos_id2, data=g1m_1935, start.time = 67, max.time=100)

coef.aalen(aa_1935)[,c(1,2,5)]

ggp_1935 <- ggplot.aalen(aa_1935)
multiplot(ggp_1935[1:14], col=4)
##

##
aa_1945 <- aalen(Surv(tstart, tstop, death==1) ~ 
                   const(factor(region)) + const(sex) + const(factor(education)) + const(factor(wealth)) + 
                   ms_single + ms_div + ms_wid + const(ever_smoked) + const(vig_pa) + 
                   const(factor(hs)) + diabetes + asthma + hyper + mi_ha + stroke + jdis + 
                   depression + cpd ,
                 id=g1m_1945$athlos_id2, data=g1m_1945, start.time = 57, max.time=100)

coef.aalen(aa_1945)[,c(1,2,5)]

ggp_1945 <- ggplot.aalen(aa_1945)
multiplot(ggp_1945[1:12], col=3)
##

##
aa_1957 <- aalen(Surv(tstart, tstop, death==1) ~ 
                   const(factor(region)) + const(sex) + const(factor(education)) + const(factor(wealth)) + 
                   ms_single + ms_div + ms_wid + const(ever_smoked) + const(vig_pa) + 
                   const(factor(hs)) + diabetes + asthma + hyper + mi_ha + stroke + jdis + 
                   depression + cpd ,
                 id=g1m_1957$athlos_id2, data=g1m_1957, start.time = 49, max.time=80)

coef.aalen(aa_1957)[,c(1,2,5)]

ggp_1957 <- ggplot.aalen(aa_1957)
multiplot(ggp_1957[1:12], col=3)
##

########################################################################
## MODEL 3:

load("g1n_cc.rdata")

g1n_cc$mm <- NA
g1n_cc$mm[!is.na(g1n_cc$cpd_age)] <- 0   
g1n_cc$mm[g1n_cc$age_1==g1n_cc$cpd_age] <- 1  # cpd va ser segurament abans de l'inici de l'estudi

g1n_cc <- g1n_cc %>% filter(is.na(g1n_cc$cpd_age) | (g1n_cc$age_1 - g1n_cc$cpd_age < 0))

g1n_cc$age_1 <- g1n_cc$age_1 - 1
g1n_1935 <- g1n_cc %>% filter(ybirth <= 1935)
g1n_1945 <- g1n_cc %>% filter(ybirth >  1935 & ybirth <= 1945)
g1n_1957 <- g1n_cc %>% filter(ybirth >  1945)

g1m_1935 <- tmerge(g1n_1935, g1n_1935, id=athlos_id2, tstart=age_1, tstop=cens_age, 
                   death = event(death_age),
                   ms_single   = tdc(ms_single_age),
                   #                   ms_marr   = tdc(ms_marr_age),
                   ms_div   = tdc(ms_div_age),
                   ms_wid   = tdc(ms_wid_age),
                   depression   = tdc(depression_age),
                   diabetes   = tdc(diab_age),
                   asthma   = tdc(asthma_age),
                   hyper   = tdc(hypertension_age),
                   jdis   = tdc(joint_disorders_age),
                   mi_ha   = tdc(mi_ha_age),
                   stroke   = tdc(stroke_age)
)

g1m_1945 <- tmerge(g1n_1945, g1n_1945, id=athlos_id2, tstart=age_1, tstop=cens_age, 
                   death = event(death_age),
                   ms_single   = tdc(ms_single_age),
                   #                   ms_marr   = tdc(ms_marr_age),
                   ms_div   = tdc(ms_div_age),
                   ms_wid   = tdc(ms_wid_age),
                   depression   = tdc(depression_age),
                   diabetes   = tdc(diab_age),
                   asthma   = tdc(asthma_age),
                   hyper   = tdc(hypertension_age),
                   jdis   = tdc(joint_disorders_age),
                   mi_ha   = tdc(mi_ha_age),
                   stroke   = tdc(stroke_age),
                   cpd   = tdc(cpd_age)
)

g1m_1957 <- tmerge(g1n_1957, g1n_1957, id=athlos_id2, tstart=age_1, tstop=cens_age, 
                   death = event(death_age),
                   ms_single   = tdc(ms_single_age),
                   #                   ms_marr   = tdc(ms_marr_age),
                   ms_div   = tdc(ms_div_age),
                   ms_wid   = tdc(ms_wid_age),
                   depression   = tdc(depression_age),
                   diabetes   = tdc(diab_age),
                   asthma   = tdc(asthma_age),
                   hyper   = tdc(hypertension_age),
                   jdis   = tdc(joint_disorders_age),
                   mi_ha   = tdc(mi_ha_age),
                   stroke   = tdc(stroke_age),
                   cpd   = tdc(cpd_age)
)

##
aa_1935 <- aalen(Surv(tstart, tstop, death==1) ~ 
                   const(factor(region)) + const(sex) + const(factor(education)) + const(factor(wealth)) + 
                   ms_single + ms_div + ms_wid + const(ever_smoked) + const(vig_pa) + 
                   diabetes + asthma + hyper + mi_ha + stroke + jdis + 
                   depression + cpd*factor(hs),
                 id=g1m_1935$athlos_id2, data=g1m_1935, start.time = 67, max.time=100)

coef.aalen(aa_1935)[,c(1,2,5)]

ggp_1935 <- ggplot.aalen(aa_1935)
multiplot(ggp_1935[1:14], col=4)
##

##
aa_1945 <- aalen(Surv(tstart, tstop, death==1) ~ 
                   const(factor(region)) + const(sex) + const(factor(education)) + const(factor(wealth)) + 
                   ms_single + ms_div + ms_wid + const(ever_smoked) + const(vig_pa) + 
                   diabetes + asthma + hyper + mi_ha + stroke + jdis + 
                   depression + cpd*factor(hs) ,
                 id=g1m_1945$athlos_id2, data=g1m_1945, start.time = 57, max.time=100)

coef.aalen(aa_1945)[,c(1,2,5)]

ggp_1945 <- ggplot.aalen(aa_1945)
multiplot(ggp_1945[1:12], col=3)
##

##
aa_1957 <- aalen(Surv(tstart, tstop, death==1) ~ 
                   const(factor(region)) + const(sex) + const(factor(education)) + const(factor(wealth)) + 
                   ms_single + ms_div + ms_wid + const(ever_smoked) + const(vig_pa) + 
                   diabetes + asthma + hyper + mi_ha + stroke + jdis + 
                   depression + cpd*factor(hs) ,
                 id=g1m_1957$athlos_id2, data=g1m_1957, start.time = 49, max.time=80)

coef.aalen(aa_1957)[,c(1,2,5)]

ggp_1957 <- ggplot.aalen(aa_1957)
multiplot(ggp_1957[1:12], col=3)


####################################################################################################################################
####################################################################################################################################

