# ==========================================
# FILE:   5000 rdinal Logistic Regression models with missing data
# AUTHOR: FEDERICA BIANCHI 
# DATE:   JULY 2022
# Updated Srebrenka Letina October 2022
# Updated Mark McCann December 2022
# ==========================================

### CLEANING ENVIRONMENT AND CONSOLE ###
rm(list = ls(all = TRUE)) 
cat("\014")               

#install.packages("MCMCglmm")
library(MCMCglmm)
library(coda)
#install.packages("stargazer")
library(stargazer)
library(dplyr)


working_dir = "C:/Users/mmc78h/Documents/GitHub/ICAROS"
setwd(working_dir)

### LOADING FUNCTIONS ### 
source("00_FUNCTIONS_MCMCglmm.R")

##Read data 
DF <- read.csv("ICAROS dataset with missing.csv")
dim(DF)
str(DF$EGO_PHYSICAL_HEALTH)
table(DF$TIE_RATING_01)
table(DF$TIE_RATING_15)
table(DF$EGO_GENDER, useNA = "ifany")
table(DF$ALTER_ADDICTION_STATUS)
table(DF$ALTER_ID_1_DEGREE)
DF$EGO_AREA_ID <- as.character(DF$EGO_AREA_ID)
DF$EGO_AREA_ID <- factor(DF$EGO_AREA_ID, labels = c("Area 1","Area 2","Area 3","Area 4","Area 5"))


dim(DF)

## COMPLETE CASES IN DATA
DF = DF[complete.cases(DF), ]
dim(DF)

OVERLAPPING_ALTER_TIES = DF %>% 
  group_by(ALTER_ID_1) %>% 
  summarize(ALTER_ID_1, N = n()) %>% 
  filter(N > 1)

sum(OVERLAPPING_ALTER_TIES$N)
dim(DF)

# ******************************************
# CROSS-CLASSIFIED MULTIPLE-MEMBERSHIP MODEL
# ordinal
# ******************************************

### :::::::::::::::::::::::::::::::::: ###
### M0
### NETWORK OVERLAP: NO OVERLAP 
### RANDOM EFFECTS:  NO RANDOM EFFECTS
### COVARIATES:      NO
### :::::::::::::::::::::::::::::::::: ###

## BASELINE MODEL


start_time = Sys.time()
## MCMC PARAMETERS
nitt = 110000
thin = 100
burnin = 1000
set.seed(32022) # single chain

## PRIOR(S)
MO_priors = list(R = list(V = 1, fix = 1))

## MODEL ESTIMATION
M0 = MCMCglmm(TIE_RATING_15 ~ 1, 
              family = "ordinal", 
              data = DF,
              prior = MO_priors, slice = TRUE, 
              nitt = nitt, thin = thin, burnin = burnin,
              verbose = TRUE)

# M0 = glm(TIE_RATING_01 ~ 1, 
#          family = binomial(link = "logit"), 
#          data = DATA)

sink(file = "_MCMCglmm_ord_missing_/M0_output.txt")
summary(M0)
sink(file = NULL)

M0.sol = rescale.loc(M0$Sol, s2.units = 1, s2.e = 0)
N0.vcv = rescale.vc(M0$VCV, s2.units = 1, s2.e = 0)

## Get estimated fixed effect
mean(M0$Sol)
# From this get estimated probability that tie value = 1
exp(mean(M0$Sol))/(1 + exp(mean(M0$Sol)))

## Also get the #deviance Information Criterion (DIC) for the model. 
## Smaller the value the better the model fit.
M0$DIC
## This is the baseline value we hope to improve upon.

# ----------------------------
# ALTER-ALTER OVERLAPPING DATA
# ----------------------------

### :::::::::::::::::::::::::::::::::::: ###
### M1 
### NETWORK OVERLAP: ALTER-ALTER OVERLAP
### RANDOM EFFECTS:  EGOS
### COVARIATES:      NO
### :::::::::::::::::::::::::::::::::::: ###

## MCMC PARAMETERS
nitt = 110000  
thin = 100     
burnin = 1000 
set.seed(32022) # single chain

## PRIORS
M1_priors = list(R = list(V = 1, fix = 1),
                 G = list(G1 = list(V = 1, nu = 0.002)))

## MODEL ESTIMATION
M1 = MCMCglmm(TIE_RATING_15 ~ 1, 
              random = ~ EGO_ID, 
              family = "ordinal", 
              data = DF, 
              prior = M1_priors, slice = TRUE, 
              nitt = nitt, thin = thin, burnin = burnin,
              pr = TRUE,
              verbose = TRUE)

sink(file = "_MCMCglmm_ord_missing_/M1_output.txt")
summary(M1)
sink(file = NULL)

## MODEL DIAGNOSTICS 
#plot(mcmc.list(M1$Sol))
autocorr(M1$Sol)

#plot(mcmc.list(M1$VCV))
# autocorr(M1$VCV)

## EXPORT OUTPUT 
pdf(file = "_MCMCglmm_ord_missing_/M1_Sol.pdf", width = 5, height = 8)
plot(mcmc.list(M1$Sol))
dev.off()

pdf(file = "_MCMCglmm_ord_missing_/M1_VCV.pdf", width = 8, height = 6)
plot(mcmc.list(M1$VCV))
dev.off()

M1_stargazer = clean_MCMC(M1)  
M1_stargazer 

M1_stargazer = stargazer(M1_stargazer, type = "text", summary = FALSE)
write.table(M1_stargazer, file = "_MCMCglmm_ord_missing_/M1.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)

M1.sol = rescale.loc(M1$Sol, s2.units = 1, s2.e = 0)
M1.vcv = rescale.vc(M1$VCV, s2.units = 1, s2.e = 0)

## VARIANCE PARTITION COEFFICIENT (HADATAIELD FORMULA)
vpc(vcv = M1$VCV, level = "EGO_ID") %>% mean

# Note that result with LEMMA formula is the same when using rescaled variance components
 summary(M1.vcv)$statistics["EGO_ID","Mean"]/(summary(M1.vcv)$statistics["EGO_ID","Mean"]  + 3.29)

#write.table(M1, file = "_MCMCglmm_ord_missing_/M1.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)


rm(M1_priors)

### :::::::::::::::::::::::::::::::::::: ###
### M2 
### NETWORK OVERLAP: ALTER-ALTER OVERLAP
### RANDOM EFFECTS:  EGOS + ALTERS
### COVARIATES:      NO
### :::::::::::::::::::::::::::::::::::: ###

## PRIORS
M2_priors = list(R = list(V = 1, fix = 1),
                 G = list(G1 = list(V = 1, nu = 0.002), G2 = list(V = 1, nu = 0.002)))

## MCMC PARAMETERS
nitt = 110000  # 105000
thin = 100     # 100
burnin = 1000  # 5000
set.seed(32022) # single chain

## MODEL ESTIMATION
M2 = MCMCglmm(TIE_RATING_15 ~ 1, 
              random = ~ EGO_ID + ALTER_ID_1, 
              family = "ordinal", 
              data = DF,
              prior = M2_priors, slice = TRUE, 
              nitt = nitt, thin = thin, burnin = burnin,
              pr = TRUE,
              verbose = TRUE)

sink(file = "_MCMCglmm_ord_missing_/M2_output.txt")
summary(M2)
sink(file = NULL)

## MODEL DIAGNOSTICS 
##plot(mcmc.list(M2$Sol))
autocorr(M2$Sol)

##plot(mcmc.list(M2$VCV))
# autocorr(M2$VCV)

## EXPORT OUTPUT 
pdf(file = "_MCMCglmm_ord_missing_/M2_Sol.pdf", width = 5, height = 8)
plot(mcmc.list(M2$Sol))
dev.off()

pdf(file = "_MCMCglmm_ord_missing_/M2_VCV.pdf", width = 8, height = 6)
plot(mcmc.list(M2$VCV))
dev.off()

M2_stargazer = clean_MCMC(M2)
M2_stargazer 
M2_stargazer = stargazer(M2_stargazer, type = "text", summary = FALSE)
write.table(M2_stargazer, file = "_MCMCglmm_ord_missing_/M2.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)

## Rescale for assumption se=0
M2.sol = rescale.loc(M2$Sol, s2.units = 1, s2.e = 0)
M2.vcv = rescale.vc(M2$VCV, s2.units = 1, s2.e = 0)

## VARIANCE PARTITION COEFFICIENT (HADATAIELD FORMULA)
# vpc(vcv = M2$VCV, level = "EGO_ID") %>% mean
vpc(vcv = M2$VCV, level = "ALTER_ID_1") %>% mean

# Note that result with LEMMA formula is the same when using rescaled variance components
#summary(M2.vcv)$statistics["EGO_ID","Mean"]/(summary(M2.vcv)$statistics["EGO_ID","Mean"] + summary(M2.vcv)$statistics["ALTER_ID_1","Mean"] + 3.29)
summary(M2.vcv)$statistics["ALTER_ID_1","Mean"]/(summary(M2.vcv)$statistics["EGO_ID","Mean"] + summary(M2.vcv)$statistics["ALTER_ID_1","Mean"] + 3.29)

summary(M2.vcv)$statistics["EGO_ID","Mean"]/(summary(M2.vcv)$statistics["EGO_ID","Mean"] + summary(M2.vcv)$statistics["ALTER_ID_1","Mean"] + 3.29)


rm(M2_priors)

### :::::::::::::::::::::::::::::::::::: ###
### M3 
### NETWORK OVERLAP: ALTER-ALTER OVERLAP
### RANDOM EFFECTS:  EGOS + ALTERS
### COVARIATES:      EGOS
### :::::::::::::::::::::::::::::::::::: ###

## MCMC PARAMETERS
nitt = 110000  # 105000
thin = 100     # 100
burnin = 1000  # 5000
set.seed(32022) # single chain

## PRIORS
M3_priors = list(R = list(V = 1, fix = 1),
                 G = list(G1 = list(V = 1, nu = 0.002), G2 = list(V = 1, nu = 0.002)))



## MODEL ESTIMATION
M3 = MCMCglmm(TIE_RATING_15 ~ 1 + 
                EGO_AREA_ID, 
              random = ~ EGO_ID + ALTER_ID_1,  
              family = "ordinal", 
              data = DF, 
              prior = M3_priors, slice = TRUE, #singular.ok = TRUE,
              thin = thin, burnin = burnin, nitt = nitt,
              pr = TRUE,
              verbose = TRUE)

sink(file = "_MCMCglmm_ord_missing_/M3_output.txt")
summary(M3)
sink(file = NULL)

## MODEL DIAGNOSTICS 
#plot(mcmc.list(M3$Sol))
autocorr(M3$Sol)

#plot(mcmc.list(M3$VCV))

## EXPORT OUTPUT 
pdf(file = "_MCMCglmm_ord_missing_/M3_Sol.pdf", width = 5, height = 8)
plot(mcmc.list(M3$Sol))
dev.off()

pdf(file = "_MCMCglmm_ord_missing_/M3_VCV.pdf", width = 8, height = 6)
plot(mcmc.list(M3$VCV))
dev.off()


M3_stargazer = clean_MCMC(M3)  
M3_stargazer 

M3_stargazer = stargazer(M3_stargazer, type = "text", summary = FALSE)
write.table(M3_stargazer, file = "_MCMCglmm_ord_missing_/M3.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)



rm(M3_priors)

### :::::::::::::::::::::::::::::::::::: ###
### M4 
### NETWORK OVERLAP: ALTER-ALTER OVERLAP
### RANDOM EFFECTS:  EGOS + ALTERS
### COVARIATES:      EGOS + ALTERS
### :::::::::::::::::::::::::::::::::::: ###

## MCMC PARAMETERS
nitt = 110000   # 105000
thin = 100     # 100
burnin = 1000  # 5000
set.seed(32022) # single chain


## PRIORS
M4_priors = list(R = list(V = 1, fix = 1),
                 G = list(G1 = list(V = 1, nu = 0.002), G2 = list(V = 1, nu = 0.002)))

## MODEL ESTIMATION
M4 = MCMCglmm(TIE_RATING_15 ~ 1 + 
                EGO_AREA_ID + 
                ALTER_TYPE_X + ALTER_ADDICTION_STATUS,
              random = ~ EGO_ID + ALTER_ID_1,  
              family = "ordinal", 
              data = DF, 
              prior = M4_priors, slice = TRUE, #singular.ok = TRUE,
              thin = thin, burnin = burnin, nitt = nitt,
              pr = TRUE,
              verbose = TRUE)

sink(file = "_MCMCglmm_ord_missing_/M4_output.txt")
summary(M4)
sink(file = NULL)

## MODEL DIAGNOSTICS 
#plot(mcmc.list(M4$Sol))
autocorr(M4$Sol)

#plot(mcmc.list(M4$VCV))
# autocorr(M4$VCV)

## EXPORT OUTPUT 
pdf(file = "_MCMCglmm_ord_missing_/M4_Sol.pdf", width = 5, height = 8)
plot(mcmc.list(M4$Sol))
dev.off()

pdf(file = "_MCMCglmm_ord_missing_/M4_VCV.pdf", width = 8, height = 6)
plot(mcmc.list(M4$VCV))
dev.off()

## Rescale for assumption se=0
M4.sol = rescale.loc(M4$Sol, s2.units = 1, s2.e = 0)
M4.vcv = rescale.vc(M4$VCV, s2.units = 1, s2.e = 0)

## VARIANCE PARTITION COEFFICIENT (HADATAIELD FORMULA)
# vpc(vcv = M2$VCV, level = "EGO_ID") %>% mean
vpc(vcv = M4$VCV, level = "ALTER_ID_1") %>% mean



summary(M4.vcv)$statistics["ALTER_ID_1","Mean"]/(summary(M4.vcv)$statistics["EGO_ID","Mean"] + summary(M4.vcv)$statistics["ALTER_ID_1","Mean"] + 3.29)
summary(M4.vcv)$statistics["EGO_ID","Mean"]/(summary(M4.vcv)$statistics["EGO_ID","Mean"] + summary(M4.vcv)$statistics["ALTER_ID_1","Mean"] + 3.29)




rm(M4_priors)

### :::::::::::::::::::::::::::::::::::: ###
### M5 
### NETWORK OVERLAP: ALTER-ALTER OVERLAP
### RANDOM EFFECTS:  EGOS + ALTERS
### COVARIATES:      EGOS + ALTERS
### :::::::::::::::::::::::::::::::::::: ###

## MCMC PARAMETERS
nitt = 110000  # 105000
thin = 100     # 100
burnin = 1000  # 5000
set.seed(32022) # single chain

## PRIORS
M5_priors = list(R = list(V = 1, fix = 1),
                 G = list(G1 = list(V = 1, nu = 0.002), G2 = list(V = 1, nu = 0.002)))

## MODEL ESTIMATION
M5 = MCMCglmm(TIE_RATING_15 ~ 1 + 
                EGO_AREA_ID + EGO_GENDER + 
                ALTER_TYPE_X + ALTER_ADDICTION_STATUS, 
              random = ~ EGO_ID + ALTER_ID_1,  
              family = "ordinal", 
              data = DF, 
              prior = M5_priors, slice = TRUE, #singular.ok = TRUE,
              thin = thin, burnin = burnin, nitt = nitt,
              pr = TRUE,
              verbose = TRUE)

sink(file = "_MCMCglmm_ord_missing_/M5_output.txt")
summary(M5)
sink(file = NULL)

## MODEL DIAGNOSTICS 
#plot(mcmc.list(M5$Sol))
autocorr(M5$Sol)

#plot(mcmc.list(M5$VCV))
# autocorr(M5$VCV)

## EXPORT OUTPUT 
pdf(file = "_MCMCglmm_ord_missing_/M5_Sol.pdf", width = 5, height = 8)
plot(mcmc.list(M5$Sol))
dev.off()

pdf(file = "_MCMCglmm_ord_missing_/M5_VCV.pdf", width = 8, height = 6)
plot(mcmc.list(M5$VCV))
dev.off()

rm(M5_priors)

### :::::::::::::::::::::::::::::::::::: ###
### M6 
### NETWORK OVERLAP: ALTER-ALTER OVERLAP
### RANDOM EFFECTS:  ALTERS
### COVARIATES:      EGOS + ALTERS
### :::::::::::::::::::::::::::::::::::: ###

## MCMC PARAMETERS
nitt = 110000  # 105000
thin = 100     # 100
burnin = 1000  # 5000
set.seed(32022) # single chain

## PRIORS
M6_priors = list(R = list(V = 1, fix = 1),
                 G = list(G1 = list(V = 1, nu = 0.002), G2 = list(V = 1, nu = 0.002)))

## MODEL ESTIMATION
M6 = MCMCglmm(TIE_RATING_15 ~ 1 + 
                EGO_AREA_ID + EGO_GENDER + EGO_PHYSICAL_HEALTH +
                ALTER_TYPE_X + ALTER_ADDICTION_STATUS , 
              random = ~ EGO_ID + ALTER_ID_1,  
              family = "ordinal", 
              data = DF,
              prior = M6_priors, slice = TRUE, #singular.ok = TRUE,
              thin = thin, burnin = burnin, nitt = nitt,
              pr = TRUE,
              verbose = TRUE)

sink(file = "_MCMCglmm_ord_missing_/M6_output.txt")
summary(M6)
sink(file = NULL)

## MODEL DIAGNOSTICS 
#plot(mcmc.list(M6$Sol))
autocorr(M6$Sol)

#plot(mcmc.list(M6$VCV))
# autocorr(M6$VCV)

## EXPORT OUTPUT 
pdf(file = "_MCMCglmm_ord_missing_/M6_Sol.pdf", width = 5, height = 8)
plot(mcmc.list(M6$Sol))
dev.off()

pdf(file = "_MCMCglmm_ord_missing_/M6_VCV.pdf", width = 8, height = 6)
plot(mcmc.list(M6$VCV))
dev.off()

rm(M6_priors)

### :::::::::::::::::::::::::::::::::::: ###
### M7 
### NETWORK OVERLAP: ALTER-ALTER OVERLAP
### RANDOM EFFECTS:  ALTERS
### COVARIATES:      EGOS + ALTERS
### :::::::::::::::::::::::::::::::::::: ###

## MCMC PARAMETERS
nitt = 110000  
thin = 100     
burnin = 1000  
set.seed(32022)

## PRIORS
M7_priors = list(R = list(V = 1, fix = 1),
                 G = list(G1 = list(V = 1, nu = 0.002), G2 = list(V = 1, nu = 0.002)))

## MODEL ESTIMATION 
M7 = MCMCglmm(TIE_RATING_15 ~ 1 + 
                EGO_AREA_ID + EGO_GENDER + EGO_PHYSICAL_HEALTH +
                ALTER_TYPE_X + ALTER_ADDICTION_STATUS + ALTER_ID_1_DEGREE, 
              random = ~ EGO_ID + ALTER_ID_1,  
              family = "ordinal", 
              data = DF,
              prior = M7_priors, slice = TRUE, #singular.ok = TRUE,
              thin = thin, burnin = burnin, nitt = nitt,
              pr = TRUE,
              verbose = TRUE)

sink(file = "_MCMCglmm_ord_missing_/M7_output.txt")
summary(M7)
sink(file = NULL)

## MODEL DIAGNOSTICS 
#plot(mcmc.list(M7$Sol))
autocorr(M7$Sol)

#plot(mcmc.list(M7$VCV))
# autocorr(M7$VCV)
# #M0: Baseline Single Level Model:
M0$DIC

#M1: Cross classified ML model alters/egos:
M1$DIC

#M2: Cross classified ML model alters/egos with overlap covariate:
M2$DIC


## EXPORT OUTPUT 
pdf(file = "_MCMCglmm_ord_missing_/M7_Sol.pdf", width = 5, height = 8)
plot(mcmc.list(M7$Sol))
dev.off()

pdf(file = "_MCMCglmm_ord_missing_/M7_VCV.pdf", width = 8, height = 6)
plot(mcmc.list(M7$VCV))
dev.off()

rm(M7_priors)

end_time = Sys.time()

end_time - start_time
setwd("C:/Users/mmc78h/Documents/GitHub/ICAROS")
save.image(file = 'MCMCglmm_ord_RESULTS with missing.RData')