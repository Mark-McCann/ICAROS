# ==========================================
# FILE:   6000 ICAROS_tables.R
# AUTHOR: FEDERICA BIANCHI/ Srebrenka Letina 
# DATE:   JULY 2022
# ==========================================

# ******************************************
# CREATING TABLES
# ******************************************

# AFTER RUNNING 04_COVID19_MODELS_MCMCglmm_ord_M1-M7.R

rm(list = ls())

#################################################
# making tables for models results
#################################################
library(openxlsx)
library(dplyr)
library(janitor)

setwd("C:/Users/mmc78h/Documents/GitHub/ICAROS")

load("MCMCglmm_ord_RESULTS with missing.Rdata")


models<- list(M1, M2, M3, M4, M5, M6, M7) # ordinal regression models
# M0 not included!


dataListNames <- list("M1",
                      "M2",
                      "M3",
                      "M4",
                      "M5",
                      "M6",
                      "M7")
readyList <- mapply(cbind, lapply(models, clean_MCMC), "modelName" = dataListNames, SIMPLIFY = F)

mcmcOutputs <- as.data.frame(do.call(rbind, readyList), stringsAsFactors = FALSE)
mcmcOutputs$modelName

#####Collapse table so Est (LCi, UCI) are in single cell

coll.table <- mcmcOutputs


head(coll.table)
coll.table$summ <- paste0(round(coll.table$post.mean,2), " (", round(coll.table$l.95..CI,2),", ",round(coll.table$u.95..CI,2),")" )

coll.table <- coll.table[,c("variable","summ","modelName")]

setwd("C:/Users/mmc78h/Documents/GitHub/ICAROS/Outputs/missing")


write.csv(coll.table, "Ordinal model summaries.csv")


# turn the table 
perm<-list()
for(i in 1:length(models)){
  df <- mcmcOutputs[mcmcOutputs$modelName == dataListNames[i],]
  # excluding some variables
  df$eff.samp <- NULL
  df$modelName <- NULL
  df$pMCMC <- NULL #- not sure if we should exclude this one, but I did
  # get rid of the last "units" row
  df <- df[0:(nrow(df)-1),]
  names(df)[names(df) == 'post.mean'] <- 'M'
  names(df)[names(df) == 'l.95..CI'] <- 'l_CI'
  names(df)[names(df) == 'u.95..CI'] <- 'u_CI'
  perm[[i]] <- df
  
}
perm[[3]]

for(i in 1:length(models)){
  # round to TWO digits cols 2-5
  COLs<-c(2,3,4)
  for(k in COLs ){
    perm[[i]][, k] <- round(perm[[i]][, k],2)
  }
  # change names of columns
  colnames(perm[[i]]) <-paste(colnames(perm[[i]]), dataListNames[i], sep="_")
  colnames(perm[[i]])[1]<- "variable"
}


# merge seven dataset in one
re1 <-Reduce(function(...) merge(..., by="variable", all=T), perm)
# sort by effect
re1 <- re1[order(re1$effect_M7),] 

# make better/shorter names of variables
newVarNames<- c("(Intercept)" , 
                "Alter degree",
                "Social activity",
                "Family Friends",
                "Food",
                "General Health",
                "IEP",
                "Mental Health",
                "Peer Support",
                "Pharmacy",
                "Social Work",
                "Supplier", 
                "Area 2", 
                "Area 3", 
                "Area 4", 
                "Area 5", 
                "Alter",
                "Ego","GenderM")

re1$variable <- newVarNames


write.xlsx(re1, "Model_results_table_ord_reg.xlsx")

# MANUALLY (in excel file):
# delete effect column except the last one (effect_M7)
# delete suffix for each column except for the first one
# insert a new first row for M1, to M7 (and merge)
# change "effect_M7" to "effect" and make it the first column
# insert "Alter type row"

# DELETE ANY OTHER VARIABLES?

#################################################
# descriptive tables of ego and alter variables
#################################################


# for numeric and int variables (3)

dfn<- subset(DF, select = c(EGO_ID,
                            EGO_AGE,
                            EGO_PHYSICAL_HEALTH,
                            EGO_MENTAL_HEALTH,
                            TIE_RATING_15))
colnames(dfn)
# aggregate by ego
dfn<- aggregate(cbind(EGO_AGE,
                      EGO_PHYSICAL_HEALTH,
                      EGO_MENTAL_HEALTH,
                      TIE_RATING_15)~ EGO_ID, FUN=mean, data = dfn)
nrow(dfn)# 57 unique egos
dfn$EGO_ID <- NULL
des1 <- as.data.frame(psych::describe(dfn,  quant=c(.25,.50,.75),
                                      skew = FALSE, ranges = FALSE, IQR = TRUE))
# to_delete <- c("vars" , "n", "sd"  ,  "se", 
#                "Q0.25" , "Q0.75")
# 
# for(i in to_delete){
#   des1[,i] <- NULL
# }
des1$mean <- round(des1$mean, 2)
des1$IQR <- round(des1$IQR, 2)
#des1 <- des1[ ,c(1,3,2)]
names(des1)[names(des1) == 'Q0.5'] <- 'median'
des1

# save
write.xlsx(des1, "num_descriptives_ego.xlsx", rowNames= TRUE)

# descriptive of non-numeric variables

ego_dat<- subset(DF, select = c(EGO_ID,
                                EGO_GENDER,
                                EGO_AREA_ID2))

# aggregate by ego by exclude duplicates
ego_dat<- ego_dat[!duplicated(ego_dat$EGO_ID), ]
ego_dat$EGO_ID <- NULL
ego_dat$EGO_AGE
# change gender labels
ego_dat$EGO_GENDER<- ifelse(ego_dat$EGO_GENDER == "F", "Women", "Men")

vars_to_des <- c("EGO_AREA_ID2", 
                 "EGO_GENDER")


desc_c<- list()
for(i in 1:length(vars_to_des)){
  tab1 <-tabyl(ego_dat[,vars_to_des[i]])
  tab1$percent <- round(tab1$percent, 2)
  colnames(tab1)[1]<- vars_to_des[i]
  tab1$percent<- tab1$percent*100
  desc_c[[i]]<- as.data.frame(tab1)
}
desc_c[[1]]
for(i in  1:length(vars_to_des)){
  write.xlsx(desc_c[[i]], paste("cat_descriptives_ego_",vars_to_des[i],".xlsx", sep=""), rowNames= FALSE)
}

# alter variables
nrow(DF) # 369 
ua <- unique(DF$ALTER_TYPE_X)
desc_a<- list()
for(i in 1:length(ua)){
  ndf <- DF[DF$ALTER_TYPE_X == ua[i],]
  
  ndfc<-as.data.frame(psych::describe(ndf$TIE_RATING_15,  quant=c(.25,.50,.75),
                                      skew = FALSE, ranges = FALSE))
  names(ndfc)[names(ndfc) == 'Q0.5'] <- 'median'
  ndfc$vars[1] <- ua[i]
  ndfc$mean <- round(ndfc$mean,2)
  ndfc$sd<- NULL
  ndfc$se<- NULL
  desc_a[[i]] <- ndfc
  
}

# add Total for TIE_RATING_15
DFc<-as.data.frame(psych::describe(DF$TIE_RATING_15,  quant=c(.25,.50,.75),
                                    skew = FALSE, ranges = FALSE))
names(DFc)[names(DFc) == 'Q0.5'] <- 'median'
DFc$vars[1] <- "all alters"
DFc$mean <- round(DFc$mean,2)
DFc$sd<- NULL
DFc$se<- NULL


desc_A <- as.data.frame(do.call(rbind, desc_a), stringsAsFactors = FALSE)
desc_A <- rbind(desc_A, DFc) # add descriptive for all alters
names(desc_A)[names(desc_A) == 'vars'] <- 'alters'

# save
write.xlsx(desc_A, "descriptives_alters.xlsx", rowNames= FALSE)

# MANUALLY - join all the outputs
# MANUALLY add where appropriate: add info about N of egos and N of alters (overlapping not considered!)
# N ego : 57
# N alters: 369 - is already in the table

#Q:
# Remove duplicate alters??? 
length(unique(DF$ALTER_ID_1)) # 324

dfa <- DF[DF$ALTER_TYPE_X =="Addiction", ]
dfa$TIE_RATING_15 # one ego can have more than one Addiction service

alter_dat<- DF[!duplicated(DF$ALTER_ID_1), ]
nrow(alter_dat) 
length(unique(alter_dat$ALTER_ID_1)) 

names_non_dup <- unique(alter_dat$ALTER_ID_1)
length(names_non_dup) 

# take the duplicates out

alter_dat_dup<- DF[duplicated(DF$ALTER_ID_1), ]
nrow(alter_dat_dup) # 45
length(unique(alter_dat_dup$ALTER_ID_1)) # 10 overlapping

names_dup <- unique(alter_dat_dup$ALTER_ID_1)
length(names_dup) # 10

# take the names of those who appear more than once from the first vector:
v1 <- intersect(names_dup,names_non_dup)
x2 <- names_non_dup[!names_non_dup %in% v1]
length(x2) #314
x2
# there are 4 with nn-L format
# "21-C", "01-F" , "01-G", "01-H" 
# which types of laters they are

x3 <- c("21-C", "01-F" , "01-G", "01-H" ) # they are not overlapping
for(i in x3){
  ndf <- DF[DF$ALTER_ID_1== i, ]
  res <- ndf$ALTER_TYPE_X
  print(res)
}
#[1] "Peer Support"
#[1] "Addiction"
#[1] "Addiction"
#[1] "Addiction"
table(DF$OVERLAPPING_ALTER_ID_1)
#NO YES 
#311  58 
# OR
#314  10

# make a dataset with overlapping alters only (based on above)
names_dup
x3 <- c("21-C", "01-F" , "01-G", "01-H" ) # they are not overlapping
rs<- list()
for(i in names_dup){
  ndf <- DF[DF$ALTER_ID_1== i, ]
  rs[[i]]<- ndf
}

rs2 <- do.call(rbind, rs)
nrow(rs2)# 55 ratings
rsc<-as.data.frame(psych::describe(rs2$TIE_RATING_15,  quant=c(.25,.50,.75),
                                   skew = FALSE, ranges = FALSE))
names(rsc)[names(rsc) == 'Q0.5'] <- 'median'
rsc$vars[1] <- "all alters"
rsc$mean <- round(rsc$mean,2)
rsc$sd<- NULL
rsc$se<- NULL


table(rs2$ALTER_TYPE_4, rs2$ALTER_ID_1)

ua <- unique(rs2$ALTER_TYPE_X)
desc_a<- list()
for(i in 1:length(ua)){
  ndf <- rs2[rs2$ALTER_TYPE_X == ua[i],]
  
  ndfc<-as.data.frame(psych::describe(ndf$TIE_RATING_15,  quant=c(.25,.50,.75),
                                      skew = FALSE, ranges = FALSE))
  names(ndfc)[names(ndfc) == 'Q0.5'] <- 'median'
  ndfc$vars[1] <- ua[i]
  ndfc$mean <- round(ndfc$mean,2)
  ndfc$sd<- NULL
  ndfc$se<- NULL
  desc_a[[i]] <- ndfc
  
}


desc_A <- as.data.frame(do.call(rbind, desc_a), stringsAsFactors = FALSE)
names(desc_A)[names(desc_A) == 'vars'] <- 'alters'

# save
write.xlsx(desc_A, "descriptives_overlapping_alters.xlsx", rowNames= FALSE)

# FIGURES

#
library(lme4)     # Estimating multilevel models 
library(lmerTest) # Printing p-values for the regression coefficients
library(lmtest)   # Likelihood ratio tests
library(optimx)  


library(sjPlot)
library(lattice)
library(car)
library(performance)
library(mctest)
library(MuMIn)
library(report)    # to report the model results
library(stevemisc)
library(merTools)

# lmer models for figures
dfl<- DF # new lables
names(dfl)[names(dfl) == 'EGO_ID'] <- 'Person_ID'

names(dfl)[names(dfl) == 'ALTER_ID_1'] <- 'Sevice_ID'

Mego.lmer <- lmer(TIE_RATING_15 ~ 1+ (1|Person_ID),
                    data   = dfl,
                    REML   = TRUE,
                    control = lmerControl(optimizer ="Nelder_Mead"))
plotREsim(REsim(Mego.lmer))

Malter.lmer <- lmer(TIE_RATING_15 ~ 1+  (1|Sevice_ID),
                      data   = dfl,
                      REML   = TRUE,
                      control = lmerControl(optimizer ="nloptwrap"))
plotREsim(REsim(Malter.lmer))

Mego_alt.lmer <- lmer(TIE_RATING_15 ~ 1+ (1|Person_ID)+  (1|Sevice_ID),
                       data   = dfl,
                       REML   = TRUE,
                       control = lmerControl(optimizer ="Nelder_Mead"))

#Caterpillar plots
png(file = "Caterpillar_Mego.png",   
    width = 500, # The width of the plot in inches
    height = 350, units = "px") 
par(cex.main=2, #change font size of title
        cex.sub=2, #change font size of subtitle
        cex.lab=2, #change font size of axis labels
        cex.axis=2)
plotREsim(REsim(Mego.lmer))
dev.off()

jpeg(file = "Caterpillar_Mego.jpeg",   
    width = 500, # The width of the plot in inches
    height = 350, units = "px") 
par(cex.main=2, #change font size of title
    cex.sub=2, #change font size of subtitle
    cex.lab=2, #change font size of axis labels
    cex.axis=2)
plotREsim(REsim(Mego.lmer))
dev.off()

bmp(file = "Caterpillar_Mego.bmp",   
     width = 500, # The width of the plot in inches
     height = 350, units = "px") 
par(cex.main=2, #change font size of title
    cex.sub=2, #change font size of subtitle
    cex.lab=2, #change font size of axis labels
    cex.axis=2)
plotREsim(REsim(Mego.lmer))
dev.off()

png(file = "Caterpillar_Malter.png",   
    width = 500, # The width of the plot in inches
    height = 350, units = "px") 
plotREsim(REsim(Malter.lmer))
dev.off()

png(file = "Caterpillar_Mego_alter.png",   
    width = 600, # The width of the plot in inches
    height = 330, units = "px") 
plotREsim(REsim(Mego_alt.lmer))
dev.off()


# change label
DFc <- DF
colnames(DFc)
names(DFc)[names(DFc) == 'TIE_RATING_15'] <- 'Ratings'


# without intercept
M.lmer = lmer(Ratings ~ 1 + 
                EGO_AREA_ID2 + 
                ALTER_TYPE_X -1 + 
                + (1|EGO_ID)+  (1|ALTER_ID_1),
              data   = DFc,
              REML   = TRUE,
              control = lmerControl(optimizer ="Nelder_Mead"))

# center ratings with - 3
DFc$Ratings <- DFc$Ratings -3
M.lmer2 = lmer(Ratings ~ 1 + 
                ALTER_TYPE_X -1 + 
                + (1|EGO_ID)+  (1|ALTER_ID_1),
              data   = DFc,
              REML   = TRUE,
              control = lmerControl(optimizer ="Nelder_Mead"))
plot_model(M.lmer2)

library(ggplot2)
# 2 is government funding
# change labels on the plot
# not ordered
png(file = "M_not ordered estimates.png",   
    width = 400, height = 500 , units = "px") # 1 cm ~ 37.7 px
plot_model(M.lmer2) + scale_x_discrete(labels=rev(c("Addiction",
                                                   "Leisure act.",
                                                   "Family/Friends",
                                                   "Food",
                                                   "GP Health",
                                                   "IEP",
                                                   "Mental health",
                                                   "Peer support", 
                                                   "Pharmacy",
                                                   "Social work",
                                                   "Supplier")))
dev.off()

png(file = "M_ordered estimates.png",   
    width = 400, height = 500 , units = "px") # 1 cm ~ 37.7 px
plot_model(M.lmer2, sort.est = T) + scale_x_discrete(labels=rev(c(
                                                    "Pharmacy",
                                                    "Peer support",
                                                    "IEP",
                                                    "Addiction",
                                                    "Supplier",
                                                    "Social work",
                                                    "Food",
                                                    "Family/Friends",
                                                    "Leisure act.",
                                                    "GP Health",
                                                    "Mental health"
                                                    )))
dev.off()

summary(M.lmer2)
summary(M4)
