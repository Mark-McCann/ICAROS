# ==========================================
# FILE:   7000 Additional analysis in response to reviewers
# AUTHOR: Mark McCann
# DATE:   January 2024
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
DF <- read.csv("ICAROS dataset.csv")

DF$EGO_AREA_ID2 <- factor(DF$EGO_AREA_ID, labels = c("Area 1", "Area 2", "Area 3", "Area 4", "Area 5"))

# "A brief description could also be added to clarify whether 
# increases and reductions in ordinal levels assumed modeling 
# equidistance and whether equal effects of predictors across 
# levels were assumed. Ideally, Results should also clarify if
# these assumptions, if present, were likely to have been met in the data."

install.packages("brant")
library(brant)
DF$TIE_RATING_15B <- factor(DF$TIE_RATING_15)
table(DF$TIE_RATING_15B, DF$TIE_RATING_15)


model1 = MASS::polr(
  TIE_RATING_15B ~ 1 + EGO_AREA_ID + EGO_GENDER + EGO_PHYSICAL_HEALTH +
    ALTER_TYPE_X + + ALTER_ADDICTION_STATUS + ALTER_ID_1_DEGREE,
  data = DF,
  Hess = TRUE
)
summary(model1)
brant(model1) 

sink(file = "Outputs/M7_brant.txt")
brant(model1) 
sink(file = NULL)

#https://peopleanalytics-regression-book.org/gitbook/multinomial-logistic-regression-for-nominal-category-outcomes.html

##Check multinomial logistic outcomes for the variables suggested by brant test.

library(nnet)
multinom()

DF$TIE_RATING_15B <- relevel(DF$TIE_RATING_15B, ref = "3")

multi_model <- multinom(
  formula = TIE_RATING_15B ~ 1 + ALTER_TYPE_X + ALTER_ADDICTION_STATUS + ALTER_ID_1_DEGREE,
  data = DF,
  Hess = TRUE
  
)

summary(multi_model)

z_stats <- summary(multi_model)$coefficients/
  summary(multi_model)$standard.errors

# convert to p-values
p_values <- (1 - pnorm(abs(z_stats)))*2


# display p-values in transposed data frame
data.frame(t(p_values))
round(data.frame(t(p_values)),2)

odds_ratios <- exp(summary(multi_model)$coefficients)
data.frame(t(odds_ratios))
odds_table <- round(data.frame(t(odds_ratios)),2)

odds_table[data.frame(t(p_values)) > 0.05] <- 1.00
odds_table


table(DF$ALTER_TYPE_X,DF$TIE_RATING_15B)



brantmodel <- brant(model1)
brantmodel <- as.data.frame(brantmodel)
brantmodel <- round(brantmodel, 3)
write.xlsx(brantmodel, "brantmodel 7.xlsx", rowNames = T)
# H0: Parallel Regression Assumption holds
# Warning message:
#   In brant(model1) :
#   351 combinations in table(dv,ivs) do not occur. Because of that, 
#   the test results might be invalid.

model1 = MASS::polr(
  TIE_RATING_15B ~ 1 + EGO_AREA_ID ,
  data = DF,
  Hess = TRUE
)
summary(model1)
brant(model1) 


model1 = MASS::polr(
  TIE_RATING_15B ~ 1 + EGO_GENDER,
  data = DF,
  Hess = TRUE
)
summary(model1)
brant(model1) 

model1 = MASS::polr(
  TIE_RATING_15B ~ 1 + EGO_PHYSICAL_HEALTH ,
  data = DF,
  Hess = TRUE
)
summary(model1)
brant(model1) 


model1 = MASS::polr(
  TIE_RATING_15B ~ 1 + ALTER_TYPE_X  ,
  data = DF,
  Hess = TRUE
)
summary(model1)
brant(model1) 


model1 = MASS::polr(
  TIE_RATING_15B ~ 1 + ALTER_ADDICTION_STATUS,
  data = DF,
  Hess = TRUE
)
summary(model1)
brant(model1) 

model1 = MASS::polr(
  TIE_RATING_15B ~ 1 + ALTER_ID_1_DEGREE,
  data = DF,
  Hess = TRUE
)
summary(model1)

brant(model1) 
models

