########################################################################
## Software specifications
########################################################################

Rpckgs <- c("dplyr","tidyr","mirt","lordif","psy","mirtCAT","knitr")
new.Rpckgs <- Rpckgs[!(Rpckgs %in% installed.packages()[,"Package"])]
if(length(new.Rpckgs)) install.packages(new.Rpckgs)
lapply(Rpckgs, require, character.only = TRUE)

########################################################################
## Data manipulation
########################################################################

load("athlos_v2.0.rdata")
ath <- athlos
ath <- ath %>%
       select(athlos_id2, study, wave, age, memory:cog_num) %>%
       select(-ends_with("_c", ignore.case = TRUE)) %>%
       mutate_all(~replace(.,list = which(. %in% 991:999), values = NA)) %>%
       filter_at(.vars=c(5:45), any_vars(complete.cases(.))) %>%
       filter(age >= 18) %>%
       arrange(athlos_id2, study, wave) %>%
       distinct(athlos_id2, .keep_all = TRUE)

########################################################################
## Item response theory (IRT) model
########################################################################

items.names <- ath %>% select(memory:cog_num) %>% names()
items <- ath %>% select(items.names)
m.irt <- mirt(items, 1, itemtype = "2PL", SE=TRUE, verbose=F)
(irt.params <- coef(m.irt, simplify=TRUE)$items[, c(1,2)])
(irt.params.true <- coef(m.irt, simplify=TRUE, IRTpars=TRUE)$items[, c(1,2)])

########################################################################
## Goodness of fit
########################################################################

for (i in 1:10) {
  fulldata <- imputeMissing(m.irt, fscores(m.irt))
  fullmod <- mirt(fulldata, 1, itemtype="2PL", SE=TRUE, verbose=FALSE)
  m2_m.irt <- M2(fullmod)
  print(m2_m.irt)
}

########################################################################
## Latent trait score estimation
########################################################################

ath$trait <- fscores(m.irt)

########################################################################
## Intraclass correlation coefficient
########################################################################

not_all_na <- function(x) {!all(is.na(x))}

# Random number resulted from "> round(runif(1, 0, 100),0)"
rseed <- 60

f.sa.ICC <- function(ds, i, coefIRT, allItems, rs) {
  i_study <- ds %>% filter(study==i) %>%
  filter_at(.vars=allItems, any_vars(complete.cases(.))) %>%
  select_if(not_all_na)
  nms <- names(i_study)[names(i_study) %in% allItems]
  mirt.obj <- generate.mirt_object(coefIRT[nms,], '2PL')
  items_s <- ds %>% select(athlos_id2, trait, nms) %>%
  filter_at(.vars=nms, any_vars(complete.cases(.)))
  hs_study <- fscores(mirt.obj, response.pattern = items_s[,-c(1,2)])
  hs_study <- as.data.frame(hs_study)
  hs_study$athlos_id2 <- items_s$athlos_id2
  hs_study$trait <- items_s$trait
  set.seed(rs)
  sid <- sample(hs_study$athlos_id2, size=1000)
  db.icc <- hs_study[hs_study$athlos_id2 %in% sid,]
  db.icc <- db.icc %>% select(trait, F1)
  dbicc <- icc(db.icc)
  return(dbicc$icc.agreement)
}

for(i in c(1,2,4:17)) {
  print(f.sa.ICC(ath, i, irt.params, items.names, rseed))
}

########################################################################
## Differential Item Functioning (IRT) and the Equating approach
########################################################################

f.DIF_equate <- function(ds, i, rs, coefIRT, allItems) {
 ####################################################
 # LORDIF approach:
 ####################################################
 data.Study <- ds %>% filter(study==i)
 if (dim(data.Study)[1] == 0) stop("No observations found")
 set.seed(rs)
 sid <- sample(ds[ds$study!=i,]$athlos_id2, size=round((dim(data.Study)[1])*2))
 data.AlltheRest <- ds[ds$athlos_id2 %in% sid,]
 data.AlltheRest$g <- 1 # Individuals from the rest of ATHLOS dataset
 data.Study$g <- 2 # Individuals from study "i"
 data.Study <- data.Study %>% select_if(function(x){!all(is.na(x))})
 nn <- names(data.Study)
 data.AlltheRest <- data.AlltheRest %>% select(nn)
 data <- rbind(data.AlltheRest, data.Study)
 names.use <- names(data)[names(data) %in% allItems]
 items <- data %>% select(names.use) %>% data.frame()
 gr <- data$g
 dif <- lordif(items, gr, criterion="Beta", control=list(NCYCLES=5000))
 items.study <- colnames(items)
 nameStudy <- ath[ath$study==i,]$study[1]
 print(cbind(items.study, dif$flag))
 print(table(dif$flag))
 
 ####################################################
 # EQUATING approach:
 ####################################################
 mg <- multipleGroup(items, 1, factor(gr), pars='values', technical=list(removeEmptyRows=TRUE))
 params <- coef(coefIRT, simplify=T)$items[c(items.study),]
 params <- as.vector(t(params))
 mg$value[1:length(params)] <- params
 mg$value[(length(params)+3):(length(params)*2+2)] <- params
 mg$est[1:length(params)] <- FALSE
 its <- items.study[!dif$flag]
 mg[mg$group=="2" & mg$item %in% its,]$est <- FALSE
 mge <- multipleGroup(items, 1, factor(gr), pars=mg, technical=list(removeEmptyRows=TRUE), verbose=F)
 theta.grid <- seq(-4, 4, 0.1)
 coef.AlltheRest<-as.data.frame(coef(mge,simplify=T)$`1`$items)[,c(1,2)]
 coef.study <- as.data.frame(coef(mge, simplify=T)$`2`$items)[,c(1,2)]
 coef.AlltheRest[,2] <- - coef.AlltheRest[,2]/coef.AlltheRest[,1]
 coef.study[,2] <- - coef.study[,2]/coef.study[,1]
 ceq.study <- equate(coef.AlltheRest, coef.study, theta.grid)
 print(kable(cbind(c("Multiplicative ctt", "Additive ctt"), ceq.study)))
 ds[ds$study==i,]$trait_eq <-
 ceq.study[1]*ds[ds$study==i,]$trait + ceq.study[2]
 return(ds)
}

ath$trait_eq <- NA

for(i in c(1,2,4:17)) {
  ath <- f.DIF_equate(ath, i, rseed, m.irt, items.names)
}

########################################################################
## T-score rescaling
########################################################################

ath$Ttrait <- ath$trait_eq * 10 / sd(ath$trait_eq) + 50



########################################################################
########################################################################

## Example of applying the ATHLOS scale to a new study dataset

########################################################################
## Calculating the trait scores of the new study
########################################################################

irt.params <- coef(m.irt, simplify=TRUE)$items[, c(1,2)]
newStudy$trait <- as.data.frame(fscores(m.irt, response.pattern = newStudy))$F1

########################################################################
## Detection of Differential Item Functioning
########################################################################

newStudy$trait_eq <- NA
newStudy_item.names <- names(newStudy)

athlos$g <- 1 # Individuals from ATHLOS dataset
newStudy$g <- 2 # Individuals from the new study

athlos <- apply(as.data.frame(athlos), 2, as.numeric)
newStudy <- apply(as.data.frame(newStudy), 2, as.numeric)
allData <- as.data.frame(rbind(data.athlos, study))

names.use <- names(allData)[names(allData) %in% newStudy_item.names]
items <- allData %>% select(names.use) %>% data.frame()
gr <- allData$g
dif <- lordif(items, gr, criterion="Beta", control = list(NCYCLES=5000))
items.study <- colnames(items)
print(cbind(items.study, dif$flag))
print(table(dif$flag))

########################################################################
## Equating approach
########################################################################

mg <- multipleGroup(items, 1, factor(gr), pars='values', technical=list(removeEmptyRows=TRUE))
params <- coef(m.irt, simplify=TRUE)$items[c(items.study),]
params <- as.vector(t(params))
mg$value[1:length(params)] <- params
mg$value[(length(params)+3):(length(params)*2+2)] <- params
mg$est[1:length(params)] <- FALSE
its <- items.study[!dif$flag]
mg[mg$group=="2" & mg$item %in% its,]$est <- FALSE
mge <- multipleGroup(items, 1, factor(gr), pars=mg, technical=list(removeEmptyRows=TRUE), verbose=FALSE)

theta.grid <- seq(-4, 4, 0.1)
coef.athlos <- as.data.frame(coef(mge, simplify=TRUE)$`1`$items)[,c(1,2)]
coef.newStudy <- as.data.frame(coef(mge, simplify=TRUE)$`2`$items)[,c(1,2)]
coef.athlos[,2] <- - coef.athlos[,2]/coef.athlos[,1]
coef.newStudy[,2] <- - coef.newStudy[,2]/coef.newStudy[,1]
ceq.newStudy <- equate(coef.athlos, coef.newStudy, theta.grid)
print(ceq.newStudy)

########################################################################
## Calculating the equated T-scores for the new study
########################################################################

newStudy <- as.data.frame(newStudy)
newStudy$trait_eq <- ceq.newStudy[1]*newStudy$trait + ceq.newStudy[2]
newStudy$Ttrait <- newStudy$trait_eq*10 / sd(newStudy$trait_eq) + 50