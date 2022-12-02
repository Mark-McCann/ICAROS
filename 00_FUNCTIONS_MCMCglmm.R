## Rescaling MCMCglmm estimates under assumption of residual var = value 
## From Hadfield course notes, p. 51-52
## Arguments: 
# 1. s2.units: the sigma units variance in HaDATAield. This is typically set to 1
# via the R= argument in the priors
# 2. s2.e: the residual variance value to which we want to rescale
# ====
## Location effects
rescale.loc <- function(chain, s2.units=1, s2.e=0) {
  c2 <- ((16*sqrt(3))/(15*pi))^2
  chain * (sqrt(1+c2*s2.e)/sqrt(1+c2*s2.units))
}

## Variance components
rescale.vc <- function(chain, s2.units=1, s2.e=0) {
  c2 <- ((16*sqrt(3))/(15*pi))^2
  chain * ((1+c2*s2.e)/(1+c2*s2.units))
}

## Variance Partition Coefficient (HaDATAield, p. 51)
vpc <- function(vcv, level) {
  vcv[,level]/(rowSums(vcv) + pi^2/3) 
}

## Return Variance Partition Coefficient using HaDATAield formula, and LEMMA formula, and difference between the 2
return.vpc <- function(mod, lev) {
  # Rescale variances
  mod.vcv <- rescale.vc(mod$VCV, s2.units=1, s2.e=0)
  
  ## Variance partition coeff using HaDATAield formula
  vpc.hf <- vpc(vcv= mod$VCV, level= lev) %>% mean
  
  ## VPC with LEMMA formula is the same, using rescaled variance
  ## components
  mod.vcv.stats <- summary(mod.vcv)$statistics
  vpc.lemma <- mod.vcv.stats[lev,"Mean"]/(sum(mod.vcv.stats[rownames(mod.vcv.stats)!="units", "Mean"]) + 3.29)
  
  ## Result
  c(vpc.hf = vpc.hf,
    vpc.lemma = vpc.lemma,
    diff = vpc.hf - vpc.lemma) 
}

## Calculate linear predictor from fitted MCMCglmm model
lin.pred <- function(mod) {
  stopifnot((class(mod)=="MCMCglmm"), ("X" %in% names(mod)))
  lin.pred <- mod$X %*% summary(mod)$solutions[,"post.mean"]
  rownames(temp) <- NULL
  drop(temp)
}

## Calculate R2 measure from Snijders & Bosker (1999:225).
## Note that *rescaled* variances are used.
R2.sb <- function(mod) {
  
  ## Rescale variances
  vcv <- rescale.vc(mod$VCV, s2.units=1, s2.e=0)
  
  ## Get rescaled variance estimates, just for relevant variance components
  summ.vcv <- summary(vcv)$statistics
  variance.names <- rownames(summ.vcv)[rownames(summ.vcv)!="units"]
  var.mean <- summ.vcv[variance.names,"Mean"]
  
  ## Get estimated linear predictor for model
  lin <- lin.pred(mod)
  
  ## Get its variance
  var.lin <- var(lin)
  
  ## GEt R2 measure
  var.lin/(var.lin + sum(var.mean) + 3.29)
}

## Extract (rescaled) coefficients from MCMCglmm model
get.mod.coeff <- function(mod, s2.units=1, s2.e=0) {
  rescale.loc(mod$Sol, s2.units=s2.units, s2.e=s2.e) %>% 
    summary %>%
    .[["statistics"]] %>%
    .[,"Mean"]
}



clean_MCMC <- function(x) {
    sols <- summary(x)$solutions  ## pull out relevant info from model summary
    Gcovs <- summary(x)$Gcovariances
    Rcovs <- summary(x)$Rcovariances
    fixed <- data.frame(row.names(sols), sols, row.names = NULL)  ## convert to dataframes with the row.names as the first col
    random <- data.frame(row.names(Gcovs), Gcovs, row.names = NULL)
    residual <- data.frame(row.names(Rcovs), Rcovs, row.names = NULL)
    names(fixed)[names(fixed) == "row.names.sols."] <- "variable"  ## change the columns names to variable, so they all match
    names(random)[names(random) == "row.names.Gcovs."] <- "variable"
    names(residual)[names(residual) == "row.names.Rcovs."] <- "variable"
    fixed$effect <- "fixed"  ## add ID column for type of effect (fixed, random, residual)
    random$effect <- "random"
    residual$effect <- "residual"
    modelTerms <- as.data.frame(bind_rows(fixed, random, residual))  # merge it all together
}

getName_MCMC <- function(x) deparse(substitute(x))  # add the model name

