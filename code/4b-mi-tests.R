### THE DIRE DISREGARD OF MEASUREMENT INVARIANCE TESTING ###
### This is code to perform step 4 of the main study

# Install and load packages/data ------------------------------------------
# Clear workspace
rm(list=ls())

# `httr` to load data from OSF into R
#install.packages("httr")
library(httr)

# `haven` to load sav files into R
#install.packages("haven")
library(haven)

# `readxl` to load xlsx files into R
#install.packages("readxl")
library(readxl)

# `lavaan` to test for measurement invariance
#install.packages("lavaan")
library(lavaan)

# `semTools` to construct syntax for thresholds
#install.packages("semTools")
library(semTools)

# `psych` to run parallel analysis
#install.packages("psych")
library(psych)

# `data.table` to rbind results from step 4
#install.packages("data.table")
library(data.table)

# `rempsyc` to construct APA table from results from step 4
#install.packages("rempsyc")
library(rempsyc)

# `GPArotation` to run parallel analysis
#install.packages("GPArotation")
library(GPArotation)

# Load in data file step 4
df4 <- read_excel("../data/codebook-main-step4.xlsx")

# Functions ---------------------------------------------------------------
# Function to extract fit measures for models (only works if 3 models are estimated)
mi.results <- function(x, y, z, type = NULL) {
  
  all.results <- matrix(NA, ncol = 15, nrow = 3)
  
  # Define basic and scaled fit measures
  basic_met <- c("chisq", "df", "pvalue", "rmsea", "cfi")
  scaled_met <- c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.scaled", "cfi.scaled")
  
  # Check if type is "scaled" and modify column names and sel accordingly
  if (is.null(type)) {
    colnames(all.results) <- c(basic_met, 
                               "LRT-pvalue", "diffRMSEA", "diffCFI", "AIC", "BIC", "chisq_change_sig", "rmsea_change", "cfi_change", "aic_change", "bic_change")
    sel <- basic_met
    
  } else if (type == "scaled") {
    colnames(all.results) <- c(scaled_met, 
                               "LRT-pvalue", "diffRMSEA", "diffCFI", "AIC", "BIC", "chisq_change_sig", "rmsea_change", "cfi_change", "aic_change", "bic_change")
    sel <- scaled_met
    
  }
  
  # Save fit measures from models
  all.results[1, sel] <- round(fitmeasures(x, fit.measures = sel), 3)
  all.results[2, sel] <- round(fitmeasures(y, fit.measures = sel), 3)
  all.results[3, sel] <- round(fitmeasures(z, fit.measures = sel), 3)
  
  # Extract fit measures for use in calculations
  chisq <- sel[1]
  pvalue <- sel[3]
  rmsea <- sel[4]
  cfi <- sel[5]

  # Compute difference in RMSEA, CFI for consecutive models
  sel_diff <- c("diffRMSEA", "diffCFI")
  all.results[2, sel_diff] <- round(all.results[2, c(rmsea, cfi)] - all.results[1, c(rmsea, cfi)], 3)
  all.results[3, sel_diff] <- round(all.results[3, c(rmsea, cfi)] - all.results[2, c(rmsea, cfi)], 3)
        
  # Compute the likelihood ratio test (LRT) between the different models
  all.results[2,"LRT-pvalue"] <- round(unlist(lavTestLRT(x,y)["Pr(>Chisq)"])[2],3)
  all.results[3,"LRT-pvalue"] <- round(unlist(lavTestLRT(y,z)["Pr(>Chisq)"])[2],3)
  
  # Extract AIC for each model
  all.results[1, "AIC"] <- round(AIC(x), 1)
  all.results[2, "AIC"] <- round(AIC(y), 1)
  all.results[3, "AIC"] <- round(AIC(z), 1)
  
  # Extract BIC for each model
  all.results[1, "BIC"] <- round(BIC(x), 1)
  all.results[2, "BIC"] <- round(BIC(y), 1)
  all.results[3, "BIC"] <- round(BIC(z), 1)

  # Check if chisquare test (model 1) or chisquare change (model 2-3) is sig
  all.results[1, "chisq_change_sig"] <- as.numeric(all.results[1, pvalue]) < 0.05
  all.results[2,"chisq_change_sig"] <- as.numeric(all.results[2,"LRT-pvalue"]) < 0.05
  all.results[3,"chisq_change_sig"] <- as.numeric(all.results[3,"LRT-pvalue"]) < 0.05
  
  # Does RMSEA change exceed critical value?
  all.results[1, "rmsea_change"] <- as.numeric(all.results[1, rmsea]) > .08
  all.results[2,"rmsea_change"] <- abs(as.numeric(all.results[2,"diffRMSEA"])) >= 0.01
  all.results[3,"rmsea_change"] <- abs(as.numeric(all.results[3,"diffRMSEA"])) >= 0.01
  
  # Does CFI change exceed critical value?
  all.results[1, "cfi_change"] <- as.numeric(all.results[1, cfi]) < 0.95
  all.results[2,"cfi_change"] <- abs(as.numeric(all.results[2,"diffCFI"])) >= 0.01
  all.results[3,"cfi_change"] <- abs(as.numeric(all.results[3,"diffCFI"])) >= 0.01
  
  # Does AIC increase or decrease  
  all.results[1,"aic_change"] <- 0
  all.results[2,"aic_change"] <- sign(as.numeric(all.results[2,"AIC"]) - as.numeric(all.results[1,"AIC"]))
  all.results[3,"aic_change"] <- sign(as.numeric(all.results[3,"AIC"]) - as.numeric(all.results[2,"AIC"]))
  
  # Does BIC increase or decrease 
  all.results[1,"bic_change"] <- 0
  all.results[2,"bic_change"] <- sign(as.numeric(all.results[2,"BIC"]) - as.numeric(all.results[1,"BIC"]))
  all.results[3,"bic_change"] <- sign(as.numeric(all.results[3,"BIC"]) - as.numeric(all.results[2,"BIC"]))
  
  return(all.results)
}

# Function to run parallel analysis and exploratory factor analysis
run.par <- function(vars = vars, df = df, groupvar = groupvar, iters = iters) {
  
  # Parallel analysis total sample
  print("Total sample:")
  par <- fa.parallel(df[,vars], fa="fa", n.iter= iters, quant = .95, plot = FALSE) 
  cat("\n") 
  
  # Cronbach's alpha  
  ca <- psych::alpha(df[,vars], check.keys=TRUE)
  print(paste0("Cronbach's alpha total sample: ",round(ca$total[1],2)))
  cat("\n") 
  
  # Factor structure groups
  #nfac <- unlist(par["nfact"])
  #fit <- fa(df[,vars], nfactors = nfac, rotate="promax")
  #loads <- fit["loadings"][["loadings"]][,1:nfac]
  #if (nfac > 1) {
  #fa.diagram(loads)
  #} else {
    #print(loads)
  #}
    
  # Parallel analysis per group
  for (i in 1:length(unlist(unique(df[,groupvar])))) {
    
    # Select group
    groupno <- unlist(unique(df[,groupvar]))[i]
    df.1 <- df[df[,groupvar] == groupno,]

    # Parallel analysis
    print(paste0("Group = ",groupno))
    par <- fa.parallel(df.1[,vars], fa="fa", n.iter=iters, quant = .95, plot = FALSE) 
  }
}

# Function to bind rows together
bindrows <- function(df) { 
  mydf <- rbind(mydf,df[nrow(df),c(1,2,3,4,5)])
}

# Article 9 ------------------------------------------------------------------------
# Data shared is raw data in .sav file, including .sps code file: https://osf.io/yxvuk/files/
# Study 1; 5 comparisons
url <- 'https://osf.io/mzyh5//?action=download'
filename <- 'article9.1.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article9.1 <- read_sav(filename)

# Comparison 1: 
# Grouping: confederate (black / white)
# Scale: perception of overall positivity
# We cannot find the variables that are used in the scale,
# So we cannot run a MI test. 

# Comparison 2: 
# Scale: post negative affect (DV_sad, DV_hopeless, DV_Discouraged, DV_angry, DV_resentful, DV_annoyed, DV_fatigued, DV_wornout, DV_exhausted, DV_vigor, DV_lively, DV_cheer)
# Grouping: C_CnCI (low and high self-disclosure)

# Create model
model9.1.2 <- 'F1 =~ DV_sad +  DV_hopeless + DV_Discouraged + DV_angry + DV_resentful + DV_annoyed + DV_fatigued + DV_wornout +  DV_exchausted + DV_vigor + DV_lively + DV_cheer'

# Change type of variables to numeric for estimating the model
article9.1$group <- as.numeric(article9.1$C_CnCls)
# Fit the configural invariance model
conf.fit.9.1.2 <- cfa(model9.1.2, article9.1, group = "group")
# Fit the loadings invariance model
load.fit.9.1.2 <- cfa(model9.1.2, article9.1, group = "group", group.equal = "loadings")
# Fit the intercepts invariance model
int.fit.9.1.2 <- cfa(model9.1.2, article9.1,  group = "group", group.equal = c("loadings", "intercepts"))
# Store fit measures and check results
smc.0009.1.02 <- mi.results(conf.fit.9.1.2,load.fit.9.1.2,int.fit.9.1.2)
smc.0009.1.02
# Chisquare test is significant & RMSEA > 0.08 & CFI < 0.95, so we reject configural invariance
# Run parallel analysis
vars <- c("DV_sad","DV_hopeless","DV_Discouraged","DV_angry","DV_resentful","DV_annoyed","DV_fatigued","DV_wornout","DV_exchausted","DV_vigor","DV_lively","DV_cheer")
run.par(vars = vars, df = article9.1, groupvar = "group", iters = 300)

# Comparison 3: 
# Scale: post negative affect (DV_sad, DV_hopeless, DV_Discouraged, DV_angry, DV_resentful, DV_annoyed, DV_fatigued, DV_wornout, DV_exhausted, DV_vigor, DV_lively, DV_cheer)
# Grouping: confederate (black / white)

# Create model
model9.1.3 <- 'F1 =~ DV_sad +  DV_hopeless + DV_Discouraged + DV_angry + DV_resentful + DV_annoyed + DV_fatigued + DV_wornout +  DV_exchausted + DV_vigor + DV_lively + DV_cheer'

# Fit the configural invariance model
conf.fit.9.1.3 <- cfa(model9.1.3, article9.1, group = "ConfederateRace")
# Fit the loadings invariance model
load.fit.9.1.3 <- cfa(model9.1.3, article9.1, group = "ConfederateRace", group.equal = "loadings")
# Fit the intercepts invariance model
int.fit.9.1.3 <- cfa(model9.1.3, article9.1, group = "ConfederateRace", group.equal = c("loadings", "intercepts"))
# Store fit measures and check results
smc.0009.1.03 <- mi.results(conf.fit.9.1.3,load.fit.9.1.3,int.fit.9.1.3)
smc.0009.1.03
# Chisquare test is significant & RMSEA > 0.08 & CFI < 0.95, so we reject configural invariance
# Run parallel analysis
vars <- c("DV_sad","DV_hopeless","DV_Discouraged","DV_angry","DV_resentful","DV_annoyed","DV_fatigued","DV_wornout","DV_exchausted","DV_vigor","DV_lively","DV_cheer")
run.par(vars = vars, df = article9.1, groupvar = "ConfederateRace", iters = 300)

# Comparison 4:
# Scale: Toronto empathy questionnaire 
# Grouping: low and high selfdisclosure
# Create model
model9.1.4 <- 'F1 =~ Empathy1 + Empathy2 + Empathy3 + Empathy4 + Empathy5 + Empathy6 + Empathy7 +
Empathy8 + Empathy9 + Empathy10 + Empathy11 + Empathy12 + Empathy13'
#change type of variables to numeric for estimating the model
article9.1$group <- as.numeric(article9.1$C_CnCls)
# Fit the configural invariance model
conf.fit.9.1.4 <- cfa(model9.1.4, article9.1, group = "group")
# Fit the loadings invariance model
load.fit.9.1.4 <- cfa(model9.1.4, article9.1, group = "group", group.equal = "loadings")
# Fit the intercepts invariance model
int.fit.9.1.4 <- cfa(model9.1.4, article9.1,  group = "group", group.equal = c("loadings", "intercepts"))
# Store fit measures and check results
smc.0009.1.04 <- mi.results(conf.fit.9.1.4,load.fit.9.1.4,int.fit.9.1.4)
smc.0009.1.04
# Chisquare test is significant & RMSEA > 0.08 & CFI < 0.95, so we reject configural invariance
# Run parallel analysis
vars <- c("Empathy1","Empathy2","Empathy3","Empathy4","Empathy5","Empathy6","Empathy7","Empathy8","Empathy9","Empathy10","Empathy11","Empathy12","Empathy13")
run.par(vars = vars, df = article9.1, groupvar = "group", iters = 300)

# Comparison 5:
# Scale: Toronto empathy questionnaire 
# Grouping: confederate (black / white)
# Create model
model9.1.5 <- 'F1 =~ Empathy1 + Empathy2 + Empathy3 + Empathy4 + Empathy5 + Empathy6 + Empathy7 + Empathy8 + Empathy9 + Empathy10 + Empathy11 + Empathy12 + Empathy13'
# Fit the configural invariance model
conf.fit.9.1.5 <- cfa(model9.1.5, article9.1, group = "ConfederateRace")
# Fit the loadings invariance model
load.fit.9.1.5 <- cfa(model9.1.5, article9.1, group = "ConfederateRace", group.equal = "loadings")
# Fit the intercepts invariance model
int.fit.9.1.5 <- cfa(model9.1.5, article9.1,  group = "ConfederateRace", group.equal = c("loadings", "intercepts"))
# store fit measures and check results
smc.0009.1.05 <- mi.results(conf.fit.9.1.5,load.fit.9.1.5,int.fit.9.1.5)
smc.0009.1.05
# Chisquare test is significant & RMSEA > 0.08 & CFI < 0.95, so we reject configural invariance
# Run parallel analysis
vars <- c("Empathy1","Empathy2","Empathy3","Empathy4","Empathy5","Empathy6","Empathy7","Empathy8","Empathy9","Empathy10","Empathy11","Empathy12","Empathy13")
run.par(vars = vars, df = article9.1, groupvar = "ConfederateRace", iters = 300)

# Study 2; 4 comparisons (6-7-8-9)
url <- 'https://osf.io/g85jp//?action=download'
filename <- 'article9.2.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article9.2 <- read_sav(filename)

# Comparison 6: 
# Scale: negative affect after speech but before feedback 
# POMS_prefdbk_hopeless,POMS_prefdbk_discouraged, POMS_prefdbk_angry, POMS_prefdbk_resent, POMS_prefdbk_annyd, POMS_prefdbk_fatigued, POMS_prefdbk_wornout, POMS_prefdbk_exhauste
# Grouping: confederate C_CnCI (low and high self-disclosure)
# Change grouping variable class
article9.2$group <- as.numeric(article9.2$C_CnCl)
# Create model using the variables colnames for the POMS scale prefdbk
model9.2.6 <- NA
for (i in 1:12){
  model9.2.6[i] <- paste0("F1 =~" , colnames(article9.2[,i+19]))
}
# Fit the configural invariance model
conf.fit.9.2.6 <- cfa(model9.2.6, article9.2, group = "group")
# Fit the loadings invariance model
load.fit.9.2.6 <- cfa(model9.2.6, article9.2, group = "group", group.equal = "loadings")
# Fit the intercepts invariance model
int.fit.9.2.6 <- cfa(model9.2.6, article9.2, group = "group", group.equal = c("loadings", "intercepts"))
smc.0009.2.06 <- mi.results(conf.fit.9.2.6,load.fit.9.2.6,int.fit.9.2.6)
smc.0009.2.06
# Chisquare test is significant & RMSEA > 0.08 & CFI < 0.95, so we reject configural invariance
# Run parallel analysis
vars <- c()
for (i in 1:12){
  vars[i] <- colnames(article9.2[,i+19])
}
run.par(vars = vars, df = article9.2, groupvar = "group", iters = 300)

# Comparison 7: 
# Scale: negative affect after speech but before feedback 
# POMS_prefdbk_hopeless,POMS_prefdbk_discouraged, POMS_prefdbk_angry, POMS_prefdbk_resent, POMS_prefdbk_annyd, POMS_prefdbk_fatigued, POMS_prefdbk_wornout, POMS_prefdbk_exhauste, POMS_vigor_prefdbk_R, POMS_lively_prefdbk_R, POMS_cheer_prefdbk_R
# Grouping: confederate confederate (black / white)
# Create model using the variables colnames for the POMS scale prefdbk
model9.2.7 <- NA
for (i in 1:12){
  model9.2.7[i] <- paste0("F1 =~" , colnames(article9.2[,i+19]))
}
# Fit the configural invariance model
conf.fit.9.2.7 <- cfa(model9.2.7, article9.2, group = "RaceCondition")
# Fit the loadings invariance model
load.fit.9.2.7 <- cfa(model9.2.7, article9.2, group = "RaceCondition", group.equal = "loadings")
# Fit the intercepts invariance model
int.fit.9.2.7 <- cfa(model9.2.7, article9.2,group = "RaceCondition", group.equal = c("loadings", "intercepts"))
smc.0009.2.07 <- mi.results(conf.fit.9.2.7,load.fit.9.2.7,int.fit.9.2.7)
smc.0009.2.07
# Chisquare test is significant & RMSEA > 0.08 & CFI < 0.95, so we reject configural invariance
# Run parallel analysis
run.par(vars = vars, df = article9.2, groupvar = "RaceCondition", iters = 300)

# Comparison 8:
# Scale: Toronto empathy questionnaire 
# Grouping: low and high selfdisclosure
# Create model
model9.2.8 <- 'F1 =~ Empathy1 + Empathy2 + Empathy3 + Empathy4 + Empathy5 + Empathy6 + Empathy7 +
                   Empathy8 + Empathy9 + Empathy10 + Empathy11 + Empathy12 + Empathy13'
# Fit the configural invariance model
conf.fit.9.2.8 <- cfa(model9.2.8, article9.2, group = "group")
# Fit the loadings invariance model
load.fit.9.2.8 <- cfa(model9.2.8, article9.2, group = "group", group.equal = "loadings")
# Fit the intercepts invariance model
int.fit.9.2.8 <- cfa(model9.2.8, article9.2,  group = "group", group.equal = c("loadings", "intercepts"))
# store fit measures and check results
smc.0009.2.08 <- mi.results(conf.fit.9.2.8,load.fit.9.2.8,int.fit.9.2.8)
smc.0009.2.08
# Chisquare test is significant & RMSEA > 0.08 & CFI < 0.95, so we reject configural invariance
# Run parallel analysis
vars <- c("Empathy1","Empathy2","Empathy3","Empathy4","Empathy5","Empathy6","Empathy7","Empathy8","Empathy9","Empathy10","Empathy11","Empathy12","Empathy13")
run.par(vars = vars, df = article9.2, groupvar = "group", iters = 300)

# Comparison 9:
# Scale: Toronto empathy questionnaire 
# Grouping: confederate (black / white)
# Create model
model9.2.9 <- 'F1 =~ Empathy1 + Empathy2 + Empathy3 + Empathy4 + Empathy5 + Empathy6 + Empathy7 + Empathy8 + Empathy9 + Empathy10 + Empathy11 + Empathy12 + Empathy13'
# Fit the configural invariance model
conf.fit.9.2.9 <- cfa(model9.2.9, article9.2, group = "RaceCondition")
# Fit the loadings invariance model
load.fit.9.2.9 <- cfa(model9.2.9, article9.2, group = "RaceCondition", group.equal = "loadings")
# Fit the intercepts invariance model
int.fit.9.2.9 <- cfa(model9.2.9, article9.2,  group = "RaceCondition", group.equal = c("loadings", "intercepts"))
# store fit measures and check results
smc.0009.2.09 <- mi.results(conf.fit.9.2.9,load.fit.9.2.9,int.fit.9.2.9)
smc.0009.2.09
# Chisquare test is significant & RMSEA > 0.08 & CFI < 0.95, so we reject configural invariance
# Run parallel analysis
vars <- c("Empathy1","Empathy2","Empathy3","Empathy4","Empathy5","Empathy6","Empathy7","Empathy8","Empathy9","Empathy10","Empathy11","Empathy12","Empathy13")
run.par(vars = vars, df = article9.2, groupvar = "RaceCondition", iters = 300)

# Article 18 -----------------------------------------------------------------------
# Data availability statement states: "All relevant data are within the paper and its Supporting Information files."
# No raw data or correlations between variables reported in the article or in the supplemental material.
# We cannot do a MI test because we have no data.

# Article 20 -----------------------------------------------------------------------
# Raw data is shared as supplemental material: https://doi.org/10.1371/journal.pone.0195982.s001
url <- 'https://doi.org/10.1371/journal.pone.0195982.s001'
filename <- 'article20.xlsx'
GET(url, write_disk(filename, overwrite = TRUE))
article20 <- read_excel(filename)

# Grouping: condom users: condomvaginalsex2
# Variables for scale: self-efficacy sumscore given, which is afterwards dichotomized. No individual items shared
# We cannot do a MI test because we cannot construct a scale.

# Article 56 -----------------------------------------------------------------------
url <- 'https://doi.org/10.1371/journal.pone.0202466.s002'
filename <- 'article56.xlxs'
GET(url, write_disk(filename, overwrite = TRUE))
article56 <- read_excel(filename)

# Comparison 1:
# variables for scale: EIC1, EIC2, EIC3, EIC4, EIC5, EIC6, EIC7, EIC8, EIC9
# Grouping: Gender
#change colnames for first variables
article56.1 <- article56[,c(2:10,47) ]
colnames(article56.1) <- c("EIC1", "EIC2", "EIC3", "EIC4", "EIC5", "EIC6", "EIC7", "EIC8", "EIC9", "Gender")
# Create model using the variables
model.56.1.1 <- NA
for (i in 1:9){
  model.56.1.1[i] <- paste0("F1 =~" , "EIC", i)
}
# Fit the configural invariance model
conf.fit.56.1 <- cfa(model.56.1.1, article56.1 , group = "Gender")
# Fit the loadings invariance model
load.fit.56.1 <- cfa(model.56.1.1, article56.1 , group = "Gender", group.equal = "loadings")
# Fit the intercepts invariance model
int.fit.56.1 <- cfa(model.56.1.1, article56.1 , group = "Gender", group.equal = c("loadings", "intercepts"))
smc.0056.1.01 <- mi.results(conf.fit.56.1,load.fit.56.1,int.fit.56.1)
smc.0056.1.01
# Chisquare test is significant & RMSEA > 0.08 & CFI < 0.95, so we reject configural invariance
# Run parallel analysis
vars <- c("EIC1", "EIC2", "EIC3", "EIC4", "EIC5", "EIC6", "EIC7", "EIC8", "EIC9")
run.par(vars = vars, df = article56.1, groupvar = "Gender", iters = 300)

# Comparison 2:
# variables for scale: EBEI1, EBEI2, EBEI3, EBEI4, EBEI5, EBEI6, EBEI7, EBEI8, EBEI9, EBEI10
# Grouping: Gender
# get the correct variables from original dataframe
article56.2 <- article56[,c(11:20,47) ]
colnames(article56.2) <- c("EBEI1", "EBEI2", "EBEI3", "EBEI4", "EBEI5", "EBEI6", "EBEI7", "EBEI8", "EBEI9", "EBEI10", "Gender")
# Create model using the variables colnames for the POMS scale prefdbk
model.56.1.2 <- NA
for (i in 1:9){
  model.56.1.2[i] <- paste0("F1 =~" , "EBEI", i)
}
# Fit the configural invariance model
conf.fit.56.1.2 <- cfa(model.56.1.2, article56.2 , group = "Gender")
# Fit the loadings invariance model
load.fit.56.1.2 <- cfa(model.56.1.2, article56.2 , group = "Gender", group.equal = "loadings")
# Fit the intercepts invariance model
int.fit.56.1.2 <- cfa(model.56.1.2, article56.2 , group = "Gender", group.equal = c("loadings", "intercepts"))
smc.0056.1.02 <- mi.results(conf.fit.56.1.2,load.fit.56.1.2,int.fit.56.1.2)
smc.0056.1.02
# Chisquare test is significant & RMSEA > 0.08 & CFI < 0.95, so we reject configural invariance
# Run parallel analysis
vars <- c("EBEI1", "EBEI2", "EBEI3", "EBEI4", "EBEI5", "EBEI6", "EBEI7", "EBEI8", "EBEI9", "EBEI10")
run.par(vars = vars, df = article56.2, groupvar = "Gender", iters = 300)

# Comparison 3:
# variables for scale: ECOVP1, ECOVP2, ECOVP3, ECOVP4, ECOVP5, ECOVP6, ECOVP7, ECOVP8, ECOVP9, ECOVP10,
# ECOVP11, ECOVP12, ECOVP13, ECOVP14, ECOVP15, ECOVP16, ECOVP17, ECOVP18, ECOVP19, ECOVP20, ECOVP21, 
# ECOVP22, ECOVP23, ECOVP24, ECOVP25
# Grouping: Gender
# get the correct variables from original dataframe
article56.3 <- article56[,c(21:45,47) ]
colnames(article56.3) <- c("ECOVP1", "ECOVP2", "ECOVP3", "ECOVP4", "ECOVP5", "ECOVP6", "ECOVP7", "ECOVP8", "ECOVP9", "ECOVP10", "ECOVP11", "ECOVP12", "ECOVP13", "ECOVP14", "ECOVP15", "ECOVP16", "ECOVP17", "ECOVP18", "ECOVP19", "ECOVP20", "ECOVP21", "ECOVP22", "ECOVP23", "ECOVP24", "ECOVP25", "Gender")
# Create model using the variables colnames for the POMS scale prefdbk
model.56.1.3 <- NA
for (i in 1:9){
  model.56.1.3[i] <- paste0("F1 =~" , "ECOVP", i)
}
# Fit the configural invariance model
conf.fit.56.1.3 <- cfa(model.56.1.3, article56.3 , group = "Gender")
# Fit the loadings invariance model
load.fit.56.1.3 <- cfa(model.56.1.3, article56.3 , group = "Gender", group.equal = "loadings")
# Fit the intercepts invariance model
int.fit.56.1.3 <- cfa(model.56.1.3, article56.3 , group = "Gender", group.equal = c("loadings", "intercepts"))
smc.0056.1.03 <- mi.results(conf.fit.56.1.3,load.fit.56.1.3,int.fit.56.1.3)
smc.0056.1.03
# Chisquare test is significant & RMSEA > 0.08 & CFI < 0.95, so we reject configural invariance
# Run parallel analysis
vars <- c("ECOVP1", "ECOVP2", "ECOVP3", "ECOVP4", "ECOVP5", "ECOVP6", "ECOVP7", "ECOVP8", "ECOVP9", "ECOVP10", "ECOVP11", "ECOVP12", "ECOVP13", "ECOVP14", "ECOVP15", "ECOVP16", "ECOVP17", "ECOVP18", 
"ECOVP19", "ECOVP20", "ECOVP21", "ECOVP22", "ECOVP23", "ECOVP24", "ECOVP25")
run.par(vars = vars, df = article56.3, groupvar = "Gender", iters = 300)
# Note that in group "M", "the correlation matrix is singular, the pseudo inverse is used for factor.scores"

# Article 74 -----------------------------------------------------------------------
url <- 'https://doi.org/10.1371/journal.pone.0205352.s002'
filename <- 'article74.csv'
GET(url, write_disk(filename, overwrite = TRUE))
article74 <- read.csv2(filename, header=F, na.strings="NA", sep=",")

# variables for scale: V1 V2 V3 (do not forget to delete the first row)
# Grouping: we cannot find the grouping variable.
# Grouping variable as stated in article: A: cow housing pasture and presence of shade; B: cow housing pasture and absence of shade; 
# C: cow housing indoors and presence of shade; D: cow housing indoors and absence of shade.

# We cannot do a MI test because we cannot construct a grouping variable.

# Article 123 ----------------------------------------------------------------------
# The data are shared on this website but we are unable to locate a dataset to download: http://medat.samrc.ac.za/index.php/catalog/35
# We cannot do a MI test because we have no data.

# Article 131 ----------------------------------------------------------------------
url <- 'https://doi.org/10.1371/journal.pone.0211604.s001'
filename <- 'article131.xlsx'
GET(url, write_disk(filename, overwrite = TRUE))
article131 <- read_excel(filename)

# Comparison 1:
# variables for scale: Eq5Dmob, Eq5Dselfc, Eq5Duact, Eq5Dpain, Eq5Dmood
# Grouping: depression
# Create model 
model.131.1.1 <- 'F1 =~ Eq5Dmob +  Eq5Dselfc + Eq5Duact +  Eq5Dpain + Eq5Dmood'
# Fit the configural invariance model
conf.mod.131.1 <- measEq.syntax(model.131.1.1,
                                ID.fac = "std.lv",
                                ID.cat = "Wu",
                                ordered = c("Eq5Dmob", "Eq5Dselfc", "Eq5Duact", "Eq5Dpain", "Eq5Dmood"),
                                group = "depression", 
                                parameterization = "delta", 
                                data = article131,
                                group.equal = "configural") 

conf.fit.131.1 <- cfa(as.character(conf.mod.131.1), article131, group = "depression", estimator = "WLSMV")
# Fit the thresholds + loadings + intercepts invariance model
# Note that with ternary scales Wu and Estabrook suggest using a two-step MI approach.
load.mod.131.1 <- measEq.syntax(model.131.1.1,
                                ID.fac = "std.lv",
                                ID.cat = "Wu",
                                ordered = c("Eq5Dmob", "Eq5Dselfc", "Eq5Duact", "Eq5Dpain", "Eq5Dmood"),
                                group = "depression", 
                                parameterization = "delta", 
                                data = article131,
                                group.equal = c("thresholds", "loadings", "intercepts")) 

load.fit.131.1 <- cfa(as.character(load.mod.131.1), article131, group = "depression", estimator = "WLSMV")
# Store goodness-of-fit measures results
smc.0131.1.01 <- matrix(NA, ncol = 8, nrow = 2)
colnames(smc.0131.1.01) <- c("chisq.scaled","df.scaled","pvalue.scaled","rmsea.scaled", "cfi.scaled", "diffRMSEA", "diffCFI", "lavtestLRT")
smc.0131.1.01[1,1:5] <-round(data.matrix(fitmeasures(conf.fit.131.1,
                                                      fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled"))), digits=3)
smc.0131.1.01[2,1:5] <-round(data.matrix(fitmeasures(load.fit.131.1,
                                                       fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled"))), digits=3)
# Compute the difference in goodness-of-fit measures
smc.0131.1.01[2,6:7] <- smc.0131.1.01[2,][4:5] - smc.0131.1.01[1,][4:5]
# Compute the LRT between the different models and store the p-value result
smc.0131.1.01[2,8] <- lavTestLRT(conf.fit.131.1, load.fit.131.1)$`Pr(>Chisq)`[2]
# Note that AIC and BIC are unavailable with this estimator
smc.0131.1.01
# Chisquare test is significant & RMSEA > 0.08 & CFI < 0.95, so we reject configural invariance
# Run parallel analysis
vars <- c("Eq5Dmob","Eq5Dselfc","Eq5Duact","Eq5Dpain","Eq5Dmood")
run.par(vars = vars, df = article131, groupvar = "depression", iters = 300)

# Comparison 2:
# variables for scale: Eq5Dmob, Eq5Dselfc, Eq5Duact, Eq5Dpain, Eq5Dmood
# Grouping: distress
# Create model 
model.131.1.2 <- 'F1 =~ Eq5Dmob +  Eq5Dselfc + Eq5Duact +  Eq5Dpain + Eq5Dmood'
# Note that since in 1 group (distress = 0) there is a 0 cell problem (non-observed score for the variables Eq5Dmob, selfc, Duact, mood)
# we decided to collapse categories 3 scores in 2 for both groups
article131_collapsed <- article131
article131_collapsed$Eq5Dmob <- ifelse(article131_collapsed$Eq5Dmob >=2, 2, 1)
article131_collapsed$Eq5Dselfc <- ifelse(article131_collapsed$Eq5Dselfc >=2, 2, 1)
article131_collapsed$Eq5Duact <- ifelse(article131_collapsed$Eq5Duact >=2, 2, 1)
article131_collapsed$Eq5Dmood <- ifelse(article131_collapsed$Eq5Dmood >=2, 2, 1)
# Fit the configural invariance model
conf.mod.131.2 <- measEq.syntax(model.131.1.2,
                                      ID.fac = "std.lv",
                                      ID.cat = "Wu",
                                      ordered = c("Eq5Dmob", "Eq5Dselfc", "Eq5Duact", "Eq5Dpain", "Eq5Dmood"),
                                      group = "distress", 
                                      parameterization = "delta", 
                                      data = article131_collapsed,
                                      group.equal = "configural") 
conf.fit.131.2 <- cfa(as.character(conf.mod.131.2), article131_collapsed, group = "distress", estimator = "WLSMV")
# Fit the intercepts invariance model
load.mod.131.2 <- measEq.syntax(model.131.1.2,
                                      ID.fac = "std.lv",
                                      ID.cat = "Wu",
                                      ordered = c("Eq5Dmob", "Eq5Dselfc", "Eq5Duact", "Eq5Dpain", "Eq5Dmood"),
                                      group = "distress", 
                                      parameterization = "delta", 
                                      data = article131_collapsed,
                                      group.equal = c("thresholds", "loadings", "intercepts")) 
load.fit.131.2 <- cfa(as.character(load.mod.131.2), article131_collapsed,  group = "distress", estimator = "WLSMV")
# Store model goodness-of-fit measures results
smc.0131.1.02 <- matrix(NA, ncol = 8, nrow = 2)
colnames(smc.0131.1.02) <- c("chisq.scaled","df.scaled","pvalue.scaled","rmsea.scaled", "cfi.scaled", "diffRMSEA", "diffCFI", "lavtestLRT")
smc.0131.1.02[1,1:5] <-round(data.matrix(fitmeasures(conf.fit.131.2,
                                                      fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled"))), digits=3)
smc.0131.1.02[2,1:5] <-round(data.matrix(fitmeasures(load.fit.131.2,
                                                       fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled"))), digits=3)
# Compute the difference in goodness-of-fit measures
smc.0131.1.02[2,6:7] <- smc.0131.1.02[2,][4:5] - smc.0131.1.02[1,][4:5]
# Compute the LRT between the different models and store the p-value result
smc.0131.1.02[2,8] <- lavTestLRT(conf.fit.131.2, load.fit.131.2)$`Pr(>Chisq)`[2]
# Note that AIC and BIC are unavailable with this estimator
smc.0131.1.02
# Chisquare test is significant & RMSEA > 0.08, so we reject configural invariance
# Run parallel analysis
vars <- c("Eq5Dmob","Eq5Dselfc","Eq5Duact","Eq5Dpain","Eq5Dmood")
run.par(vars = vars, df = article131_collapsed, groupvar = "distress", iters = 300)

# Article 142 ----------------------------------------------------------------------
url <- 'https://osf.io/rfam5//?action=download'
filename <- 'article142.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article142 <- read_sav(filename)

# Unclear what the grouping variable is, maybe a_CondR?
# Factors (number of items):
# - Rutger Alcohol Problem Index (RAPI) (23)
# - Alcohol Use Disorder Identification Test (10)
# - Alcohol self concept scale (5)
# - Implicit Theories About Willpower (6)
# - Positive affect (3)
# - Negative affect (3)
# Sum scores are used for the constructs.

# We cannot construct a scale, and we're not clear on what the grouping variable is, so we cannot conduct a MI test.
# Article 155 ----------------------------------------------------------------------
url <- 'https://doi.org/10.1371/journal.pone.0218242.s004'
filename <- 'article155.dta'
GET(url, write_disk(filename, overwrite = TRUE))
article155 <- read_dta(filename)

# Grouping variable: measurement (0,1,2, corresponding to baseline, immediately post intervention, and 6 weeks after intervention)
# Factors (number of items): 
#- PAID (20)
#- IPQ-R (timeline acute/chronic (6); timeline cyclical (4); timeline consequences (6); personal control (6); treatment control (5); coherence (5); emotional representation (6); own behavior in the past (6); psychological cause (5))
#- DAS-3 (need for special training (5); perceived seriousness (7); value of tight control (6); psychosocial impact (6); patient autonomy (8))
#- Perceptions of partner support (active engagement (5); protective buffering (8); overprotection (6))
#- DES-20 (20)

# Grouping variable is found. All factors are sum scores so we cannot construct a scale and do a MI test.

# Article 199 ----------------------------------------------------------------------
# Data Availability: All relevant data are within the manuscript, Supporting Information files 
# and via the DRYAD repository: https://datadryad.org/review?doi=doi:10.5061/dryad.m2q4c83.
# The data from DRYAD is not available. 
# There is no supplemental material added to the article
# The article does not contain correlations for the items of the WHO-DAS
# So we cannot do a MI test because we have no data

# Article 251 ----------------------------------------------------------------------
# Study 1: one comparison
url <- 'https://osf.io/ukj2n//?action=download'
filename <- 'article251.1.txt'
GET(url, write_disk(filename, overwrite = TRUE))
article251.1 <- read.table(filename, header=TRUE) 

# Grouping variable: counterattitudinal information and control information: variable name is "man"
# Scale: explicit evaluation, variable name is "liking"

# From the article and supplemental materials on OSF (https://osf.io/nfqju/):
# "We will create an explicit rating score indicating a preference for Niffites over 
# Luupites by averaging difference scores of the 4 questions about the groups characteristics"

# Explicit-evaluation scores were computed by subtracting rating scores for the negative-induction group from scores for the positive-induction group.

# The authors constructed the Liking variable like this (https://osf.io/tnfas/):
# compute scores
# Data$c1<-ifelse(Data$cdt==1,(Data$Lpls+Data$Lcor+Data$Lwon+Data$Lgoo)/4,(Data$Npls+Data$Ncor+Data$Nwon+Data$Ngoo)/4)
# Data$c2<-ifelse(Data$cdt==2,(Data$Lpls+Data$Lcor+Data$Lwon+Data$Lgoo)/4,(Data$Npls+Data$Ncor+Data$Nwon+Data$Ngoo)/4)
# Data$liking<-Data$c1-Data$c2 # preference for first group described as pos

# The authors have 4 variables of liking a certain thing (Lpls, Lcor, Lwon, Lgoo), and 4 variables of liking another thing (Npls, Ncor, Nwon, Ngoo). The authors assume each set of 4 variables constitutes a "liking" LV, and combine them into an index. Then they subtract L - N (for cdt=1) and N - L (for cdt=2) to get a final variable score for each participant.
# As we are interested in measurement invariance, we will not combine them into an index but first make difference scores, which we will test for MI.
article251.1$pls <- ifelse(article251.1$cdt==1,(article251.1$Lpls - article251.1$Npls),(article251.1$Npls - article251.1$Lpls))
article251.1$cor <- ifelse(article251.1$cdt==1,(article251.1$Lcor - article251.1$Ncor),(article251.1$Ncor - article251.1$Lcor))
article251.1$won <- ifelse(article251.1$cdt==1,(article251.1$Lwon - article251.1$Nwon),(article251.1$Nwon - article251.1$Lwon))
article251.1$goo <- ifelse(article251.1$cdt==1,(article251.1$Lgoo - article251.1$Ngoo),(article251.1$Ngoo - article251.1$Lgoo))
# Create model 
model.251.1.1 <- 'F1 =~ pls + cor + won + goo'
# Fit the configural invariance model
conf.fit.251.1 <- cfa(model.251.1.1, article251.1 , group = "man")
# Fit the loadings invariance model
load.fit.251.1 <- cfa(model.251.1.1, article251.1 , group = "man", group.equal = "loadings")
# Fit the intercepts invariance model
int.fit.251.1 <- cfa(model.251.1.1, article251.1 , group = "man", group.equal = c("loadings", "intercepts"))
smc.0251.1.01 <- mi.results(conf.fit.251.1,load.fit.251.1,int.fit.251.1)
smc.0251.1.01
# Chisquare non-significant, RMSEA < .08, CFI > .95, configural invariance holds
# Metric: nonsig chisquare change + change in CFI < .01 (and CFI > .95 + RMSEA < .08). AIC and BIC are smaller. Metric invariance holds (but, RMSEA change > .01)
# Scalar: nonsig chisquare change + change in CFI and RMSEA > .01 and AIC is larger. Scalar invariance violated (BIC is ok)

# Study 2: three comparisons
url <- 'https://osf.io/vj8nz//?action=download'
filename <- 'article251.2.txt'
GET(url, write_disk(filename, overwrite = TRUE))
article251.2 <- read.table(filename, header=TRUE) 

# Comparison 2
# Grouping variable is time (pre-post)
# Scale is three self-report items probing likability; friendliness; and trustworthiness

# The way this data is constructed by the authors is by first averaging three pre-scores and then averaging three post-scores (https://osf.io/69rez/):
## Compute Variables for the two targets
#Data$exp_Bobpre<-(Data$qBF_pre+Data$qBL_pre+Data$qBT_pre)/3
#Data$exp_Bobpost<-(Data$qBF_post+Data$qBL_post+Data$qBT_post)/3
#Data$exp_Janpre<-(Data$qJF_pre+Data$qJL_pre+Data$qJT_pre)/3
#Data$exp_Janpost<-(Data$qJF_post+Data$qJL_post+Data$qJT_post)/3

# Then, the authors compute the final scores for the t-test by subtracting pre-pre and post-post: 
### compute EC explicit scores
# Data$exp_pre<-ifelse(Data$EC==1,Data$exp_Bobpre-Data$exp_Janpre,Data$exp_Janpre-Data$exp_Bobpre)
# Data$exp_post<-ifelse(Data$EC==1,Data$exp_Bobpost-Data$exp_Janpost,Data$exp_Janpost-Data$exp_Bobpost)

# The authors state: 
# Explicit ratings were collapsed into one score for Bob and Jan at each time of assessment (Cronbach’s α = .94). Explicit-evaluation scores
#were computed by subtracting rating scores for the negative-induction person from scores for the positive-induction person. 

# We have 12 variables in total:
# friendliness: qBF_pre, qBF_post, qJF_pre, qJF_post
# likeability: qBL_pre, qBL_post, qJL_pre, qJL_post
# trustworthiness: qBT_pre, qBT_post, qJT_pre, qJT_post

# We will also collapse the B (Bob) and J (Jan) scores together for each variable separately
# If EC==1, we will do Bob-Jan. If EC==2, we will do Jan-Bob
article251.2$fpre <- ifelse(article251.2$EC==1, (article251.2$qBF_pre - article251.2$qJF_pre),(article251.2$qJF_pre - article251.2$qBF_pre))
article251.2$lpre <- ifelse(article251.2$EC==1, (article251.2$qBL_pre - article251.2$qJL_pre),(article251.2$qJL_pre - article251.2$qBL_pre))
article251.2$tpre <- ifelse(article251.2$EC==1, (article251.2$qBT_pre - article251.2$qJT_pre),(article251.2$qJT_pre - article251.2$qBT_pre))
article251.2$fpost <- ifelse(article251.2$EC==1, (article251.2$qBF_post - article251.2$qJF_post),(article251.2$qJF_post - article251.2$qBF_post))
article251.2$lpost <- ifelse(article251.2$EC==1, (article251.2$qBL_post - article251.2$qJL_post),(article251.2$qJL_post - article251.2$qBL_post))
article251.2$tpost <- ifelse(article251.2$EC==1, (article251.2$qBT_post - article251.2$qJT_post),(article251.2$qJT_post - article251.2$qBT_post))
# Make dataset for t1
article251.2.t1 <- cbind(article251.2$fpre,article251.2$lpre,article251.2$tpre)
grouping <- rep(0,nrow(article251.2.t1))
article251.2.t1 <- cbind(article251.2.t1,grouping)
# Make dataset for t2
article251.2.t2 <- cbind(article251.2$fpost,article251.2$lpost,article251.2$tpost)
grouping <- rep(1,nrow(article251.2.t2))
article251.2.t2 <- cbind(article251.2.t2,grouping)
# Combine datasets
article251.2.tot <- rbind(article251.2.t1,article251.2.t2)
colnames(article251.2.tot) <- c("friend","like","trust","grouping")
# Construct model
model.251.2.2 <- 'F1 =~ friend + like + trust'
conf.fit.251.2 <- cfa(model.251.2.2, data = article251.2.tot, group = "grouping")
load.fit.251.2 <- cfa(model.251.2.2, article251.2.tot, group = "grouping", group.equal = "loadings")
int.fit.251.2 <- cfa(model.251.2.2, article251.2.tot, group = "grouping", group.equal = c("loadings", "intercepts"))
smc.0251.2.02 <- mi.results(conf.fit.251.2,load.fit.251.2,int.fit.251.2)
smc.0251.2.02
# 3 items, so we must assume configural invariance
# Metric: nonsig chisq change + change CFI = 0 (and CFI > .95 + RMSEA < .08), AIC and BIC smaller (only RMSEA change > .01); metric invariance holds 
# Scalar: nonsig chisq change + change CFI = 0, AIC and BIC smaller, RMSEA > .01 but the fit improves (0.02 to 0), so scalar invariance holds

# Comparison 3
# grouping variable is hypnosis vs relaxation condition at time 1 (variable name is hypnosis)
# Scale is three self-report items probing likability; friendliness; and trustworthiness
# That would make the following variables:

## Compute Variables for the two targets
#Data$exp_Bobpre<-(Data$qBF_pre+Data$qBL_pre+Data$qBT_pre)/3
#Data$exp_Janpre<-(Data$qJF_pre+Data$qJL_pre+Data$qJT_pre)/3

# Compute then the final variable: 
# Data$exp_pre<-ifelse(Data$EC==1,Data$exp_Bobpre-Data$exp_Janpre,Data$exp_Janpre-Data$exp_Bobpre)

# We will not first make an index for Bob and Jan, but analyze the three variables separately. 
model.251.2.3 <- 'F =~ fpre + lpre + tpre'
conf.fit.251.3 <- cfa(model.251.2.3, data = article251.2, group = "hypnosis")
load.fit.251.3 <- cfa(model.251.2.3, article251.2, group = "hypnosis", group.equal = "loadings")
int.fit.251.3 <- cfa(model.251.2.3, article251.2, group = "hypnosis", group.equal = c("loadings", "intercepts"))
smc.0251.2.03 <- mi.results(conf.fit.251.3,load.fit.251.3,int.fit.251.3)
smc.0251.2.03
# 3 items, so we must assume configural invariance
# Metric: nonsig chisq change + change CFI < .01, AIC and BIC smaller (only RMSEA change > .01); metric invariance holds 
# Scalar: nonsig chisq change + change CFI = 0, AIC and BIC smaller, RMSEA > .01 but the fit improves (0.04 to 0), so scalar invariance holds

# Comparison 4
# Grouping variable is hypnosis vs relaxation condition at time 2 (variable name is hypnosis)
# Scale is three self-report items probing likability; friendliness; and trustworthiness
# That would make the following variables:
## Compute Variables for the two targets
#Data$exp_Bobpost<-(Data$qBF_post+Data$qBL_post+Data$qBT_post)/3
#Data$exp_Janpost<-(Data$qJF_post+Data$qJL_post+Data$qJT_post)/3

# And then the final variable:
# Data$exp_post<-ifelse(Data$EC==1,Data$exp_Bobpost-Data$exp_Janpost,Data$exp_Janpost-Data$exp_Bobpost)
# We will not first make an index for Bob and Jan, but analyze the three variables separately. 
model.251.2.4 <- 'F =~ fpost + lpost + tpost'
conf.fit.251.4 <- cfa(model.251.2.4, data = article251.2, group = "hypnosis")
load.fit.251.4 <- cfa(model.251.2.4, article251.2, group = "hypnosis", group.equal = "loadings")
int.fit.251.4 <- cfa(model.251.2.4, article251.2, group = "hypnosis", group.equal = c("loadings", "intercepts"))
smc.0251.2.04 <- mi.results(conf.fit.251.4,load.fit.251.4,int.fit.251.4)
smc.0251.2.04
# 3 items, so we must assume configural invariance
# Metric: nonsig chisq change + CFI and RMSEA change = 0 (and CFI > .95 + RMSEA < .08), AIC and BIC smaller; metric invariance holds 
# Scalar: nonsig chisq change + CFI and RMSEA change = 0, AIC and BIC smaller, so scalar invariance holds

# Article 261 ----------------------------------------------------------------------
url <- 'https://osf.io/ztsq4//?action=download'
filename <- 'article261.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article261 <- read_sav(filename)
# Comparison 1
# Grouping variable: untrained controls and trained participants (condition)
# Scale: Bias blind spot (bbs1 - bbs14)
# From the syntax (https://osf.io/5h7tc/) we learn that the bbs scores are computed by subtracting b-a:
article261$bbs01 <-  as.numeric(article261$bbs_1b-article261$bbs_1a)
article261$bbs02 <-  as.numeric(article261$bbs_2b-article261$bbs_2a)
article261$bbs03 <-  as.numeric(article261$bbs_3b-article261$bbs_3a)
article261$bbs04 <-  as.numeric(article261$bbs_4b-article261$bbs_4a)
article261$bbs05 <-  as.numeric(article261$bbs_5b-article261$bbs_5a)
article261$bbs06 <-  as.numeric(article261$bbs_6b-article261$bbs_6a)
article261$bbs07 <-  as.numeric(article261$bbs_7b-article261$bbs_7a)
article261$bbs08 <-  as.numeric(article261$bbs_8b-article261$bbs_8a)
article261$bbs09 <-  as.numeric(article261$bbs_9b-article261$bbs_9a)
article261$bbs10 <- as.numeric(article261$bbs_10b-article261$bbs_10a)
article261$bbs11 <- as.numeric(article261$bbs_11b-article261$bbs_11a)
article261$bbs12 <- as.numeric(article261$bbs_12b-article261$bbs_12a)
article261$bbs13 <- as.numeric(article261$bbs_13b-article261$bbs_13a)
article261$bbs14 <- as.numeric(article261$bbs_14b-article261$bbs_14a)
# Make grouping variable numeric
article261$condition <- as.numeric(article261$condition)
# Create model 
model.261.1.1 <- 'F =~ bbs01 + bbs02 + bbs03 + bbs04 + bbs05 + bbs06 + bbs07 + bbs08 + bbs09 + bbs10 + bbs11 + bbs12 + bbs13 + bbs14'
# Fit the configural invariance model
conf.fit.261.1 <- cfa(model.261.1.1, data = article261, group = "condition")
# Fit the loadings invariance model
load.fit.261.1 <- cfa(model.261.1.1, article261 , group = "condition", group.equal = "loadings")
# Fit the intercepts invariance model
int.fit.261.1 <- cfa(model.261.1.1, article261 , group = "condition", group.equal = c("loadings", "intercepts"))
smc.0261.1.01 <- mi.results(conf.fit.261.1,load.fit.261.1,int.fit.261.1)
smc.0261.1.01
# Chisquare test is significant & RMSEA > 0.08 & CFI < 0.95, so we reject configural invariance
# Run parallel analysis
vars <- c("bbs01","bbs02","bbs03","bbs04","bbs05","bbs06","bbs07","bbs08","bbs09","bbs10","bbs11","bbs12","bbs13","bbs14")
run.par(vars = vars, df = article261, groupvar = "condition", iters = 300)

# Comparison 2
# Grouping variable: untrained controls and trained participants (condition)
# Scale: NED; correspondence bias (ned1 - ned10)
# Create model 
model.261.1.2 <- 'F =~ ned1 + ned2 + ned3 + ned4 + ned5 + ned6 + ned7 + ned8 + ned9 + ned10'
# Fit the configural invariance model
conf.fit.261.2 <- cfa(model.261.1.2, data = article261, group = "condition")
# Fit the loadings invariance model
load.fit.261.2 <- cfa(model.261.1.2, article261 , group = "condition", group.equal = "loadings")
# Fit the intercepts invariance model
int.fit.261.2 <- cfa(model.261.1.2, article261 , group = "condition", group.equal = c("loadings", "intercepts"))
smc.0261.1.02 <- mi.results(conf.fit.261.2,load.fit.261.2,int.fit.261.2)
smc.0261.1.02
# Chisquare test is significant & RMSEA > 0.08 & CFI < 0.95, so we reject configural invariance
# Run parallel analysis
vars <- c("ned1","ned2","ned3","ned4","ned5","ned6","ned7","ned8","ned9","ned10")
run.par(vars = vars, df = article261, groupvar = "condition", iters = 300)

# Article 301 ----------------------------------------------------------------------
# Study 1
url <- 'https://osf.io/h8cs2//?action=download'
filename <- 'article301.1.csv'
GET(url, write_disk(filename, overwrite = TRUE))
article301.1 <- read.csv2(filename, header=T, na.strings="NA", sep=",")
# Comparison 1
# Grouping variable: message type (moral argument vs practical argument): cond
# Scale: postmessage attitudes towards recycling: att2_1, att2_2, att2_3
# Create model 
model.301.1.1 <- 'F =~ att2_1 + att2_2 + att2_3'
# Fit the configural invariance model
conf.fit.301.1 <- cfa(model.301.1.1, data = article301.1, group = "cond")
# Fit the loadings invariance model
load.fit.301.1 <- cfa(model.301.1.1, article301.1, group = "cond", group.equal = "loadings")
# Fit the intercepts invariance model
int.fit.301.1 <- cfa(model.301.1.1, article301.1, group = "cond", group.equal = c("loadings", "intercepts"))
smc.0301.1.01 <- mi.results(conf.fit.301.1,load.fit.301.1,int.fit.301.1)
smc.0301.1.01
# 3 items, so we must assume configural invariance
# Metric: nonsig chisq change + CFI > .95 + RMSEA < .08 (change = 0), AIC and BIC smaller; metric invariance holds.
# Scalar: sig chisq change, but change CFI < .01 + BIC smaller; however, AIC larger and change in RMSEA > .01, so scalar invariance does not hold.

# Comparison 2
# Grouping variable: political orientation (very liberal; somewhat liberal; neither; somewhat conservative; very conservative); poli_soc, poli_eco
# Scale: postmessage attitudes towards recycling: att2_1, att2_2, att2_3

# Political orientation is measured by two items, the authors state this:
# Since the reliability of these two items was quite strong (alpha = .86), we combined them into one political-orientation scale.

# We combined the two variables by averaging them and rounding (so someone who scored 1.5 becomes 2)
article301.1$pol <- as.numeric(round((article301.1$poli_soc+article301.1$poli_eco)/2))
# Create model 
model.301.1.2 <- 'F =~ att2_1 + att2_2 + att2_3'
# Fit the configural invariance model
conf.fit.301.2 <- cfa(model.301.1.2, data = article301.1, group = "pol")
# Fit the loadings invariance model
load.fit.301.2 <- cfa(model.301.1.2, article301.1 , group = "pol", group.equal = "loadings")
# Fit the intercepts invariance model
int.fit.301.2 <- cfa(model.301.1.2, article301.1, group = "pol", group.equal = c("loadings", "intercepts"))
smc.0301.1.02 <- mi.results(conf.fit.301.2,load.fit.301.2,int.fit.301.2)
smc.0301.1.02
# Three items so we assume configural invariance
# Metric invariance: sig chisq change + RMSEA change > .01, AIC larger; metric invariance rejected (BIC and CFI ok)

# Study 2
url <- 'https://osf.io/cbue5//?action=download'
filename <- 'article301.2.csv'
GET(url, write_disk(filename, overwrite = TRUE))
article301.2 <- read.csv2(filename, header=T, na.strings="NA", sep=",")

# Comparison 3
# Grouping variable: message type (moral argument vs practical argument): cond
# Scale: postmessage attitudes towards marijuana: att2_1, att2_2, att2_3
# Create model 
model.301.2.3 <- 'F =~ att2_1 + att2_2 + att2_3'
# Fit the configural invariance model
conf.fit.301.3 <- cfa(model.301.2.3, data = article301.2, group = "cond")
# Fit the loadings invariance model
load.fit.301.3 <- cfa(model.301.2.3, article301.2, group = "cond", group.equal = "loadings")
# Fit the intercepts invariance model
int.fit.301.3 <- cfa(model.301.2.3, article301.2, group = "cond", group.equal = c("loadings", "intercepts"))
smc.0301.2.03 <- mi.results(conf.fit.301.3,load.fit.301.3,int.fit.301.3)
smc.0301.2.03
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq + CFI > .95 and RMSEA < .08 and change < .01, AIC and BIC lower; metric invariance holds.
# Scalar invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; scalar invariance holds.

# Article 481 ----------------------------------------------------------------------
url <- 'https://osf.io/jcx9r//?action=download'
filename <- 'article481.1.csv'
GET(url, write_disk(filename, overwrite = TRUE))
article481.1 <- read.csv2(filename, header=T, na.strings="NA", sep=",")

# remove the first row with text
article481.1 <- article481.1[-1,]
# grouping variables: agreement (match) and caring (care)
# scale variables: benevolence (benev1 + benev2 + benev3) and integrity (integ1 + integ2 + integ3)
article481.1$benev1 <- as.numeric(article481.1$benev1)
article481.1$benev2 <- as.numeric(article481.1$benev2)
article481.1$benev3 <- as.numeric(article481.1$benev3)
article481.1$integ1 <- as.numeric(article481.1$integ1)
article481.1$integ2 <- as.numeric(article481.1$integ2)
article481.1$integ3 <- as.numeric(article481.1$integ3)
# make different datasets separated by issue
article481.capital <- article481.1[article481.1$issue == "capital",]
article481.abortion <- article481.1[article481.1$issue == "abortion",]
article481.suicide <- article481.1[article481.1$issue == "suicide",]
article481.testing <- article481.1[article481.1$issue == "testing",]
article481.gun <- article481.1[article481.1$issue == "gun",]
# Comparison 1: match - capital - integrity
model.481.1 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.1 <- cfa(model.481.1, data = article481.capital, group = "match")
load.fit.481.1 <- cfa(model.481.1, article481.capital, group = "match", group.equal = "loadings")
int.fit.481.1 <- cfa(model.481.1, article481.capital, group = "match", group.equal = c("loadings", "intercepts"))
smc.0481.1.01 <- mi.results(conf.fit.481.1,load.fit.481.1,int.fit.481.1)
smc.0481.1.01
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; metric invariance holds.
# Scalar invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; scalar invariance holds.
# Comparison 2: match - abortion - integrity
model.481.2 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.2 <- cfa(model.481.2, data = article481.abortion, group = "match")
load.fit.481.2 <- cfa(model.481.2, article481.abortion, group = "match", group.equal = "loadings")
int.fit.481.2 <- cfa(model.481.2, article481.abortion, group = "match", group.equal = c("loadings", "intercepts"))
smc.0481.1.02 <- mi.results(conf.fit.481.2,load.fit.481.2,int.fit.481.2)
smc.0481.1.02
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; metric invariance holds.
# Scalar invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; scalar invariance holds.

# Comparison 3: match - gun - integrity
model.481.3 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.3 <- cfa(model.481.3, data = article481.gun, group = "match")
load.fit.481.3 <- cfa(model.481.3, article481.gun, group = "match", group.equal = "loadings")
int.fit.481.3 <- cfa(model.481.3, article481.gun, group = "match", group.equal = c("loadings", "intercepts"))
smc.0481.1.03 <- mi.results(conf.fit.481.3,load.fit.481.3,int.fit.481.3)
smc.0481.1.03
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; metric invariance holds.
# Scalar invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; scalar invariance holds.

# Comparison 4: match - testing - integrity
model.481.4 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.4 <- cfa(model.481.4, data = article481.testing, group = "match")
load.fit.481.4 <- cfa(model.481.4, article481.testing, group = "match", group.equal = "loadings")
int.fit.481.4 <- cfa(model.481.4, article481.testing, group = "match", group.equal = c("loadings", "intercepts"))
smc.0481.1.04 <- mi.results(conf.fit.481.4,load.fit.481.4,int.fit.481.4)
smc.0481.1.04
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; metric invariance holds.
# Scalar invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; scalar invariance holds.

# Comparison 5: match - suicide - integrity
model.481.5 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.5 <- cfa(model.481.5, data = article481.suicide, group = "match")
load.fit.481.5 <- cfa(model.481.5, article481.suicide, group = "match", group.equal = "loadings")
int.fit.481.5 <- cfa(model.481.5, article481.suicide, group = "match", group.equal = c("loadings", "intercepts"))
smc.0481.1.05 <- mi.results(conf.fit.481.5,load.fit.481.5,int.fit.481.5)
smc.0481.1.05
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI change < .01, AIC and BIC lower; metric invariance holds.
# Scalar invariance: 
# Chisquare nonsignificant, BIC smaller than previous step.
# RMSEA change too large, AIC larger than previous step. CFI changes more than 0.01, but goes from 0.996 to 0.976.
# Criteria are: both significant chisquare change and 2 out of 4 fit measures exceed the criteria.
# As the chisquare is non-significant, we cannot reject scalar invariance; scalar invariance holds. 

# Comparison 6: match - overall - integrity
model.481.6 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.6 <- cfa(model.481.6, data = article481.1, group = "match")
load.fit.481.6 <- cfa(model.481.6, article481.1, group = "match", group.equal = "loadings")
int.fit.481.6 <- cfa(model.481.6, article481.1, group = "match", group.equal = c("loadings", "intercepts"))
smc.0481.1.06 <- mi.results(conf.fit.481.6,load.fit.481.6,int.fit.481.6)
smc.0481.1.06
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI change < .01, AIC and BIC lower; metric invariance holds.
# Scalar invariance: nonsig chisq change + CFI change < .01, BIC lower
# Scalar invariance: RMSEA change is too large (> .01) and AIC is larger than in the previous step. But, 
# Chisquare is nonsignificant so we cannot reject scalar invariance; scalar invariance holds.

# Comparison 7: match - capital - benevolence
model.481.7 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.7 <- cfa(model.481.7, data = article481.capital, group = "match")
load.fit.481.7 <- cfa(model.481.7, article481.capital, group = "match", group.equal = "loadings")
int.fit.481.7 <- cfa(model.481.7, article481.capital, group = "match", group.equal = c("loadings", "intercepts"))
smc.0481.1.07 <- mi.results(conf.fit.481.7,load.fit.481.7,int.fit.481.7)
smc.0481.1.07
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI change < .01, AIC and BIC lower; metric invariance holds.
# Scalar invariance: nonsig chisq change + CFI change < .01, AIC and BIC lower, RMSEA change > .01 but the fit improves; scalar invariance holds.

# Comparison 8: match - abortion - benevolence
model.481.8 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.8 <- cfa(model.481.8, data = article481.abortion, group = "match")
load.fit.481.8 <- cfa(model.481.8, article481.abortion, group = "match", group.equal = "loadings")
int.fit.481.8 <- cfa(model.481.8, article481.abortion, group = "match", group.equal = c("loadings", "intercepts"))
smc.0481.1.08 <- mi.results(conf.fit.481.8,load.fit.481.8,int.fit.481.8)
smc.0481.1.08
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; metric invariance holds.
# Scalar invariance: sig chisq change + CFI and RMSEA change > .01, AIC and BIC larger; scalar invariance rejected

# Comparison 9: match - gun - benevolence
model.481.9 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.9 <- cfa(model.481.9, data = article481.gun, group = "match")
load.fit.481.9 <- cfa(model.481.9, article481.gun, group = "match", group.equal = "loadings")
int.fit.481.9 <- cfa(model.481.9, article481.gun, group = "match", group.equal = c("loadings", "intercepts"))
smc.0481.1.09 <- mi.results(conf.fit.481.9,load.fit.481.9,int.fit.481.9)
smc.0481.1.09
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI and RMSEA change > .01, AIC larger; metric invariance holds because chisq non-sig (BIC ok)
# Scalar invariance: sig chisq change + CFI and RMSEA change > .01, AIC and BIC larger; scalar invariance rejected.

# Comparison 10: match - testing - benevolence
model.481.10 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.10 <- cfa(model.481.10, data = article481.testing, group = "match")
load.fit.481.10 <- cfa(model.481.10, article481.testing, group = "match", group.equal = "loadings")
int.fit.481.10 <- cfa(model.481.10, article481.testing, group = "match", group.equal = c("loadings", "intercepts"))
smc.0481.1.10 <- mi.results(conf.fit.481.10,load.fit.481.10,int.fit.481.10)
smc.0481.1.10
# Three items so we assume configural invariance
# Metric invariance: sig chisq change + CFI and RMSEA change > .01 (with RMSEA being 0.158), AIC larger; metric invariance rejected (BIC ok; CFI decreases but from 0.987 to 0.973).

# Comparison 11: match - suicide - benevolence
model.481.11 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.11 <- cfa(model.481.11, data = article481.suicide, group = "match")
load.fit.481.11 <- cfa(model.481.11, article481.suicide, group = "match", group.equal = "loadings")
int.fit.481.11 <- cfa(model.481.11, article481.suicide, group = "match", group.equal = c("loadings", "intercepts"))
smc.0481.1.11 <- mi.results(conf.fit.481.11,load.fit.481.11,int.fit.481.11)
smc.0481.1.11
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; metric invariance holds.
# Scalar invariance: sig chisq change + CFI and RMSEA change > .01, AIC and BIC larger; scalar invariance rejected.

# Comparison 12: match - overall - benevolence
model.481.12 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.12 <- cfa(model.481.12, data = article481.1, group = "match")
load.fit.481.12 <- cfa(model.481.12, article481.1, group = "match", group.equal = "loadings")
int.fit.481.12 <- cfa(model.481.12, article481.1, group = "match", group.equal = c("loadings", "intercepts"))
smc.0481.1.12 <- mi.results(conf.fit.481.12,load.fit.481.12,int.fit.481.12)
smc.0481.1.12
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI change < .01, BIC lower
# But: RMSEA change > .01, AIC larger. 
# As the chisquare change is not significant, we cannot reject metric invariance; metric invariance holds
# Scalar invariance: sig chisq change + CFI and RMSEA change > .01, AIC and BIC larger; scalar invariance rejected

# Comparison 13: care - capital - integrity
model.481.13 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.13 <- cfa(model.481.13, data = article481.capital, group = "care")
load.fit.481.13 <- cfa(model.481.13, article481.capital, group = "care", group.equal = "loadings")
int.fit.481.13 <- cfa(model.481.13, article481.capital, group = "care", group.equal = c("loadings", "intercepts"))
smc.0481.1.13 <- mi.results(conf.fit.481.13,load.fit.481.13,int.fit.481.13)
smc.0481.1.13
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI change < .01 (and > .95), AIC and BIC lower; metric invariance holds
# Scalar invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; scalar invariance holds.

# Comparison 14: care - abortion - integrity
model.481.14 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.14 <- cfa(model.481.14, data = article481.abortion, group = "care")
load.fit.481.14 <- cfa(model.481.14, article481.abortion, group = "care", group.equal = "loadings")
int.fit.481.14 <- cfa(model.481.14, article481.abortion, group = "care", group.equal = c("loadings", "intercepts"))
smc.0481.1.14 <- mi.results(conf.fit.481.14,load.fit.481.14,int.fit.481.14)
smc.0481.1.14
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI and RMSEA change < .01 (and > .95 and < .08), AIC and BIC lower; metric invariance holds.
# Scalar invariance: nonsig chisq change + CFI change < .01, AIC and BIC lower; scalar invariance holds (but: RMSEA change > .01)

# Comparison 15: care - gun - integrity
model.481.15 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.15 <- cfa(model.481.15, data = article481.gun, group = "care")
load.fit.481.15 <- cfa(model.481.15, article481.gun, group = "care", group.equal = "loadings")
int.fit.481.15 <- cfa(model.481.15, article481.gun, group = "care", group.equal = c("loadings", "intercepts"))
smc.0481.1.15 <- mi.results(conf.fit.481.15,load.fit.481.15,int.fit.481.15)
smc.0481.1.15
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI and RMSEA change < .01 (and > .95 and < .08), AIC and BIC lower; metric invariance holds.
# Scalar invariance: sig chisq change + CFI and RMSEA change > .01, AIC larger; scalar invariance rejected (BIC ok).

# Comparison 16: care - testing - integrity
model.481.16 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.16 <- cfa(model.481.16, data = article481.testing, group = "care")
load.fit.481.16 <- cfa(model.481.16, article481.testing, group = "care", group.equal = "loadings")
int.fit.481.16 <- cfa(model.481.16, article481.testing, group = "care", group.equal = c("loadings", "intercepts"))
smc.0481.1.16 <- mi.results(conf.fit.481.16,load.fit.481.16,int.fit.481.16)
smc.0481.1.16
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI change < .01 (and > .95 and < .08), AIC and BIC lower; metric invariance holds (RMSEA change > 01).
# Scalar invariance: nonsig chisq change + CFI change < .01, RMSEA change > .01 but fit improves, AIC and BIC lower; scalar invariance holds.

# Comparison 17: care - suicide - integrity
model.481.17 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.17 <- cfa(model.481.17, data = article481.suicide, group = "care")
load.fit.481.17 <- cfa(model.481.17, article481.suicide, group = "care", group.equal = "loadings")
int.fit.481.17 <- cfa(model.481.17, article481.suicide, group = "care", group.equal = c("loadings", "intercepts"))
smc.0481.1.17 <- mi.results(conf.fit.481.17,load.fit.481.17,int.fit.481.17)
smc.0481.1.17
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI change < .01, AIC and BIC lower; metric invariance holds (RMSEA change > .01 and RMSEA > .08).
# Scalar invariance: nonsig chisq change + RMSEA change < .01, BIC lower
# But: CFI change > .01, AIC larger
# As the chisquare difference is non-significant, we cannot reject scalar invariance; scalar invariance holds.

# Comparison 18: care - overall - integrity
model.481.18 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.18 <- cfa(model.481.18, data = article481.1, group = "care")
load.fit.481.18 <- cfa(model.481.18, article481.1, group = "care", group.equal = "loadings")
int.fit.481.18 <- cfa(model.481.18, article481.1, group = "care", group.equal = c("loadings", "intercepts"))
smc.0481.1.18 <- mi.results(conf.fit.481.18,load.fit.481.18,int.fit.481.18)
smc.0481.1.18
# Three items so we assume configural invariance
# Metric invariance: sig chisq change + RMSEA change > .01 (and > .08), AIC larger. We reject metric invariance. (CFI change < .01 and BIC lower)

# Comparison 19: care - capital - benevolence
model.481.19 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.19 <- cfa(model.481.19, data = article481.capital, group = "care")
load.fit.481.19 <- cfa(model.481.19, article481.capital, group = "care", group.equal = "loadings")
int.fit.481.19 <- cfa(model.481.19, article481.capital, group = "care", group.equal = c("loadings", "intercepts"))
smc.0481.1.19 <- mi.results(conf.fit.481.19,load.fit.481.19,int.fit.481.19)
smc.0481.1.19
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + BIC lower
# RMSEA change > .01, CFI change > .01, AIC larger. 
# As the chisquare difference is not significant, we cannot reject metric invariance; metric invariance holds
# Scalar invariance: nonsig chisq change + CFI change < .01, AIC and BIC lower; scalar invariance holds (RMSEA change > .01 but fit improves)

# Comparison 20: care - abortion - benevolence
model.481.20 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.20 <- cfa(model.481.20, data = article481.abortion, group = "care")
load.fit.481.20 <- cfa(model.481.20, article481.abortion, group = "care", group.equal = "loadings")
int.fit.481.20 <- cfa(model.481.20, article481.abortion, group = "care", group.equal = c("loadings", "intercepts"))
smc.0481.1.20 <- mi.results(conf.fit.481.20,load.fit.481.20,int.fit.481.20)
smc.0481.1.20
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI change < .01, AIC and BIC lower; metric invariance holds (RMSEA change > .01)
# Scalar invariance: nonsig chisq change + CFI change < .01, BIC lower;
# RMSEA change > .01 and AIC is larger. As the chisquare test is non-sig, we cannot reject scalar invariance; scalar invariance holds.

# Comparison 21: care - gun - benevolence
model.481.21 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.21 <- cfa(model.481.21, data = article481.gun, group = "care")
load.fit.481.21 <- cfa(model.481.21, article481.gun, group = "care", group.equal = "loadings")
int.fit.481.21 <- cfa(model.481.21, article481.gun, group = "care", group.equal = c("loadings", "intercepts"))
smc.0481.1.21 <- mi.results(conf.fit.481.21,load.fit.481.21,int.fit.481.21)
smc.0481.1.21
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + BIC lower
# CFI change > .01, RMSEA change > .01 (and > .08), AIC larger than previous step. Because chisquare is non-sig we cannot reject metric invariance; metric invariance holds.
# Scalar invariance: nonsig chisq change + CFI change < .01, AIC and BIC lower; scalar invariance holds (RMSEA change > .01 but fit improves)

# Comparison 22: care - testing - benevolence
model.481.22 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.22 <- cfa(model.481.22, data = article481.testing, group = "care")
load.fit.481.22 <- cfa(model.481.22, article481.testing, group = "care", group.equal = "loadings")
int.fit.481.22 <- cfa(model.481.22, article481.testing, group = "care", group.equal = c("loadings", "intercepts"))
smc.0481.1.22 <- mi.results(conf.fit.481.22,load.fit.481.22,int.fit.481.22)
smc.0481.1.22
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; metric invariance holds.
# Scalar invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; scalar invariance holds.

# Comparison 23: care - suicide - benevolence
model.481.23 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.23 <- cfa(model.481.23, data = article481.suicide, group = "care")
load.fit.481.23 <- cfa(model.481.23, article481.suicide, group = "care", group.equal = "loadings")
int.fit.481.23 <- cfa(model.481.23, article481.suicide, group = "care", group.equal = c("loadings", "intercepts"))
smc.0481.1.23 <- mi.results(conf.fit.481.23,load.fit.481.23,int.fit.481.23)
smc.0481.1.23
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI change < .01, BIC lower; 
# RMSEA change > .01 and AIC larger; because chisquare is nonsig we cannot reject metric invariance; metric invariance holds.
# Scalar invariance: nonsig chisq change + CFI and RMSEA change < .01, BIC lower; scalar invariance holds (AIC larger)

# Comparison 24: care - overall - benevolence
model.481.24 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.24 <- cfa(model.481.24, data = article481.1, group = "care")
load.fit.481.24 <- cfa(model.481.24, article481.1, group = "care", group.equal = "loadings")
int.fit.481.24 <- cfa(model.481.24, article481.1, group = "care", group.equal = c("loadings", "intercepts"))
smc.0481.1.24 <- mi.results(conf.fit.481.24,load.fit.481.24,int.fit.481.24)
smc.0481.1.24
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI change < .01, BIC lower; 
# RMSEA change > .01, AIC larger; because chisquare nonsig we cannot reject metric invariance; metric invariance holds.
# Scalar invariance: nonsig chisq change + CFI change < .01, AIC and BIC lower; scalar invariance holds (RMSEA change > .01 but fit improves)

# Study 3a + 3b
# Grouping variables: agreement (match) and caring (care)
# Scale variables: integrity based trust and benevolence based trust (benev123, integ123)
url <- 'https://osf.io/fq82m//?action=download'
filename <- 'article481.2.csv'
GET(url, write_disk(filename, overwrite = TRUE))
article481.2 <- read.csv2(filename, header=T, na.strings="NA", sep=",")
# Remove the first row with text
article481.2 <- article481.2[-1,]
# Change values to numeric for items
article481.2[,14:19] <- as.numeric(unlist(article481.2[,17:19]))
# Comparison 25: match - integrity
model.481.25 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.25 <- cfa(model.481.25, data = article481.2, group = "match")
load.fit.481.25 <- cfa(model.481.25, article481.2, group = "match", group.equal = "loadings")
int.fit.481.25 <- cfa(model.481.25, article481.2, group = "match", group.equal = c("loadings", "intercepts"))
smc.0481.2.25 <- mi.results(conf.fit.481.25,load.fit.481.25,int.fit.481.25)
smc.0481.2.25
# Three items so we assume configural invariance
# Metric invariance: sig chisq change + RMSEA change > .01 (RMSEA > .08 too), AIC larger; we reject metric invariance (CFI and BIC ok)

# Comparison 26: care - integrity
model.481.26 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.26 <- cfa(model.481.26, data = article481.2, group = "care")
load.fit.481.26 <- cfa(model.481.26, article481.2, group = "care", group.equal = "loadings")
int.fit.481.26 <- cfa(model.481.26, article481.2, group = "care", group.equal = c("loadings", "intercepts"))
smc.0481.2.26 <- mi.results(conf.fit.481.26,load.fit.481.26,int.fit.481.26)
smc.0481.2.26
# Three items so we assume configural invariance
# Metric invariance: sig chisq change + RMSEA change > .01, AIC larger; we reject metric invariance (CFI and BIC ok)

# Comparison 27: match - benevolence
model.481.27 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.27 <- cfa(model.481.27, data = article481.2, group = "match")
load.fit.481.27 <- cfa(model.481.27, article481.2, group = "match", group.equal = "loadings")
int.fit.481.27 <- cfa(model.481.27, article481.2, group = "match", group.equal = c("loadings", "intercepts"))
smc.0481.2.27 <- mi.results(conf.fit.481.27,load.fit.481.27,int.fit.481.27)
smc.0481.2.27
# Three items so we assume configural invariance
# Metric invariance: sig chisq change + RMSEA change > .01 (and > .08), AIC higher; metric invariance rejected.

# Comparison 28: care - benevolence
model.481.28 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.28 <- cfa(model.481.28, data = article481.2, group = "care")
load.fit.481.28 <- cfa(model.481.28, article481.2, group = "care", group.equal = "loadings")
int.fit.481.28 <- cfa(model.481.28, article481.2, group = "care", group.equal = c("loadings", "intercepts"))
smc.0481.2.28 <- mi.results(conf.fit.481.28,load.fit.481.28,int.fit.481.28)
smc.0481.2.28
# Three items so we assume configural invariance
# Metric invariance: sig chisq change + AIC higher 
# But RMSEA is ok (< 0.08), CFI change is < .01 (and CFI > .95) and BIC is lower; meaning we cannot reject metric invariance
# Scalar invariance: sig chisq change + AIC higher
# But: RMSEA change ok (< .01), CFI ok (< .01) and BIC is ok; we cannot reject scalar invariance; scalar invariance holds.

# Article 521 ----------------------------------------------------------------------
# Comparison 1-6 (Study 1)
url <- 'https://osf.io/4qjah//?action=download'
filename <- 'article521.1.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article521.1 <- read_sav(filename)

# Comparison 1:
# Scale: 5 envy items across domain 1
# Group: tense (past v future)
# Only the student sample
# "The five envy items were collapsed into composite envy scales for each  domain (each population; αs ≥ .78)." 
# Only the composite score is available in the data, we can't construct a scale

# Comparison 2:
# Scale: 5 envy items across domain 1
# Group: tense (past v future)
# Only the student sample
# "The five envy items were collapsed into composite envy scales for each  domain (each population; αs ≥ .78)." 
# Only the composite score is available in the data, we can't construct a scale

# Comparison 3:
# Scale: 5 envy items across domain 1
# Group: tense (past v future)
# Only the student sample
# "The five envy items were collapsed into composite envy scales for each  domain (each population; αs ≥ .78)." 
# Only the composite score is available in the data, we can't construct a scale

# Comparison 4:
# Scale: 5 envy items across domain 1
# Group: tense (past v future)
# Only the student sample
# "The five envy items were collapsed into composite envy scales for each  domain (each population; αs ≥ .78)." 
# Only the composite score is available in the data, we can't construct a scale

# Comparison 5:
# Scale: 5 envy items across domain 1
# Group: tense (past v future)
# Only the student sample
# "The five envy items were collapsed into composite envy scales for each  domain (each population; αs ≥ .78)." 
# Only the composite score is available in the data, we can't construct a scale

# Comparison 1:
# Scale: 5 envy items across all domains combined
# Group: tense (past v future)
# Only the student sample
article521.6 <- article521.1[article521.1$student == 1,]
# Fit model
model.521.1.6 <- 'F =~ Vacation_1envy + Job_1envy + Date_1envy + House_1envy + Car_1envy'
conf.fit.521.1.6 <- cfa(model.521.1.6, data = article521.6, group = "tense")
load.fit.521.1.6 <- cfa(model.521.1.6, article521.6, group = "tense", group.equal = "loadings")
int.fit.521.1.6 <- cfa(model.521.1.6, article521.6, group = "tense", group.equal = c("loadings", "intercepts"))
smc.0521.1.06 <- mi.results(conf.fit.521.1.6,load.fit.521.1.6,int.fit.521.1.6)
smc.0521.1.06
# Configural invariance: non-sig chisquare, RMSEA < .08, CFI > .95; configural invariance holds
# Metric invariance: sig chisq change, RMSEA change is .07, CFI change is -0.019, AIC is larger (BIC is smaller); we reject metric invariance.

# Study 2a (comparison 7)
url <- 'https://osf.io/7gjmk//?action=download'
filename <- 'article521.1.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article521.2 <- read_sav(filename)

# Grouping variable: Tense
# Scale variable: EnvyDV_envy, EnvyDV_jeal, EnvyDV_want, EnvyDV_inf, EnvyDV_hap
model.521.2.7 <- 'F =~  EnvyDV_envy + EnvyDV_jeal + EnvyDV_want + EnvyDV_inf + EnvyDV_hap'
conf.fit.521.1 <- cfa(model.521.2.7 , data = article521.2, group = "Tense")
load.fit.521.1 <- cfa(model.521.2.7 , article521.2, group = "Tense", group.equal = "loadings")
int.fit.521.1 <- cfa(model.521.2.7 , article521.2, group = "Tense", group.equal = c("loadings", "intercepts"))
smc.0521.2.07 <- mi.results(conf.fit.521.1,load.fit.521.1,int.fit.521.1)
smc.0521.2.07
# Chisquare is significant, rmsea > 0.08, CFI < .95, so configural invariance rejected.
# Run parallel analysis
vars <- c("EnvyDV_envy","EnvyDV_jeal","EnvyDV_want","EnvyDV_inf","EnvyDV_hap")
run.par(vars = vars, df = article521.2, groupvar = "Tense", iters = 300)

# Study 2b (comparison 8)
url <- 'https://osf.io/u692w//?action=download'
filename <- 'article521.2.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article521.3 <- read_sav(filename)

# Grouping variable: tense
# Scale variable: EnvyDV_envy, EnvyDV_jeal, EnvyDV_want, EnvyDV_inf, EnvyDV_hap

# The study states the following: 
# A separate paired-samples t test specifically comparing the absolute difference between February 13 and February 14 (M = 0.26, SD = 1.19) 
# with the absolute difference between February 14 and February 15 (M = 0.66, SD = 1.31) revealed a significant difference, 
# t(169) = 3.75, p < .001, d = 0.29, 95% CI for the mean difference = [0.19, 0.61].

# This is how the authors calculate the scores per day:
#article521.2$day1 <- (article521.2$Day1DV_envious + article521.2$Day1DV_jealous + article521.2$Day1DV_want + article521.2$Day1DV_inferior + article521.2$Day1DV_happy)/5
#article521.2$day2 <- (article521.2$Day2DV_envious + article521.2$Day2DV_jealous + article521.2$Day2DV_want + article521.2$Day2DV_inferior + article521.2$Day2DV_happy)/5
#article521.2$day3 <- (article521.2$Day3DV_envious + article521.2$Day3DV_jealous + article521.2$Day3DV_want + article521.2$Day3DV_inferior + article521.2$Day3DV_happy)/5
# They then subtract day2 from day3 (day3-day2) and subtract day1 from day2 (day2-day1) and perform a t-test between these two difference scores.

# However, since we are interested in checking whether these items fit well to the factor, we do not combine them into an index first.
# We make difference scores for each subject for before (t2-t1) and after (t3-t2) and test these for MI. 
before_envious <- abs(article521.3$Day2DV_envious - article521.3$Day1DV_envious)
before_jealous <- abs(article521.3$Day2DV_jealous - article521.3$Day1DV_jealous)
before_want <- abs(article521.3$Day2DV_want - article521.3$Day1DV_want)
before_inferior <- abs(article521.3$Day2DV_inferior - article521.3$Day1DV_inferior)
before_happy <- abs(article521.3$Day2DV_happy - article521.3$Day1DV_happy)
after_envious <- abs(article521.3$Day3DV_envious - article521.3$Day2DV_envious)
after_jealous <- abs(article521.3$Day3DV_jealous - article521.3$Day2DV_jealous)
after_want <- abs(article521.3$Day3DV_want - article521.3$Day2DV_want)
after_inferior <- abs(article521.3$Day3DV_inferior - article521.3$Day2DV_inferior)
after_happy <- abs(article521.3$Day3DV_happy - article521.3$Day2DV_happy)
# Make dataset with only difference scores for before Vday
article521.before <- cbind(before_envious,before_jealous,before_want,before_inferior,before_happy)
grouping <- rep(0,nrow(article521.before))
article521.before <- cbind(article521.before,grouping)
# Make dataset with only difference scores for after Vday
article521.after <- cbind(after_envious,after_jealous,after_want,after_inferior,after_happy)
grouping <- rep(1,nrow(article521.after))
article521.after <- cbind(article521.after,grouping)
# Combine datasets
article521.3.tot <- rbind(article521.before,article521.after)
# Construct and estimate model
model.521.3.8 <- 'F=~ before_envious + before_jealous + before_want + before_inferior + before_happy'
conf.fit.521.2 <- cfa(model.521.3.8, data = article521.3.tot, group = "grouping")
load.fit.521.2 <- cfa(model.521.3.8, article521.3.tot, group = "grouping", group.equal = "loadings")
int.fit.521.2 <- cfa(model.521.3.8, article521.3.tot, group = "grouping", group.equal = c("loadings", "intercepts"))
smc.0521.3.08 <- mi.results(conf.fit.521.2,load.fit.521.2,int.fit.521.2)
smc.0521.3.08
# Chisquare is significant, rmsea > 0.08, CFI < .95, so configural invariance rejected. 
# Run parallel analysis
vars <- c("before_envious","before_jealous","before_want","before_inferior","before_happy")
run.par(vars = vars, df = article521.3.tot, groupvar = "grouping", iters = 300)

# Study 3a' comparisons 9-10
url <- 'https://osf.io/4fpxu//?action=download'
filename <- 'article521.3.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article521.4 <- read_sav(filename)
# Comparison 9
# Grouping variable: past vs distant future (tense)
# Scale variable: Malicious envy: DVfrustrating, DVillwill, DVdislike, DVreplace, DVscream
model.521.4.9 <- 'F =~ DVfrustrating + DVillwill + DVdislike + DVreplace + DVscream'
conf.fit.521.3 <- cfa(model.521.4.9, data = article521.4, group = "tense")
load.fit.521.3 <- cfa(model.521.4.9, article521.4, group = "tense", group.equal = "loadings")
int.fit.521.3 <- cfa(model.521.4.9, article521.4, group = "tense", group.equal = c("loadings", "intercepts"))
smc.0521.4.09 <- mi.results(conf.fit.521.3,load.fit.521.3,int.fit.521.3)
smc.0521.4.09
# Chisquare is significant, rmsea > 0.08, so we reject configural invariance (CFI is ok: 0.976)
# Run parallel analysis
vars <- c("DVfrustrating","DVillwill","DVdislike","DVreplace","DVscream")
run.par(vars = vars, df = article521.4, groupvar = "tense", iters = 300)

# Comparison 10
# Grouping variable: past vs control (tense)
# Scale variable: Benign envy: DVpleas, DVinspire, DVliking, DVtryhard, DVcompliment.
model.521.4.10 <- 'F =~ DVpleas + DVinspire + DVliking + DVtryhard + DVcompliment'
conf.fit.521.4 <- cfa(model.521.4.10, data = article521.4, group = "tense")
load.fit.521.4 <- cfa(model.521.4.10, article521.4, group = "tense", group.equal = "loadings")
int.fit.521.4 <- cfa(model.521.4.10, article521.4, group = "tense", group.equal = c("loadings", "intercepts"))
smc.0521.4.10 <- mi.results(conf.fit.521.4,load.fit.521.4,int.fit.521.4)
smc.0521.4.10
# chisquare is significant, rmsea > 0.08, CFI < .95, so configural invariance rejected. 
vars <- c("DVpleas","DVinspire","DVliking","DVtryhard","DVcompliment")
run.par(vars = vars, df = article521.4, groupvar = "tense", iters = 300)

# Study 4
# Comparison 11-15
url <- 'https://osf.io/6ujnw//?action=download'
filename <- 'article521.4.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article521.5 <- read_sav(filename)

# Comparison 11
# Grouping variable: past vs control (group = 0 for control and group = 1 for past)
# Scale variable: State-trait anxiety inventory (6 items per time point so 12 items)

# The authors constructed the variable StressT1 by taking the average of the 6 anxiety scores. 
# The variable DiffStress is then the difference in StressT2-StressT1. This DiffStress is then t-tested.
# If we would test these average anxiety items for MI (StressT2 and StressT2), we would only have two items per factor. As such
# We decided to compute difference scores per anxiety item for each participant, and perform a MI test for these variables
article521.5$anx1 <- article521.5$Anx1reverseT2 - article521.5$Anx1reverseT1
article521.5$anx2 <- article521.5$Anxiety_2.0 - article521.5$Anxiety_2
article521.5$anx3 <- article521.5$Anxiety_3.0 - article521.5$Anxiety_3
article521.5$anx4 <- article521.5$Anx4reverseT2 - article521.5$Anx4reverseT1
article521.5$anx5 <- article521.5$Anx5reverseT2 - article521.5$Anx5reverseT1
article521.5$anx6 <- article521.5$Anxiety_6.0 - article521.5$Anxiety_6
# We only need group=0 and group=1 for this comparison, and will drop out group=2
article521.5.1 <- article521.5[article521.5[,"GROUP"] != 2,]
range(article521.5.1$GROUP)

# Construct model and estimate
model.521.5 <- 'F =~ anx1 + anx2 + anx3 + anx4 + anx5 + anx6'
conf.fit.521.5 <- cfa(model.521.5, data = article521.5.1, group = "GROUP")
load.fit.521.5 <- cfa(model.521.5, article521.5.1, group = "GROUP", group.equal = "loadings")
int.fit.521.5 <- cfa(model.521.5, article521.5.1, group = "GROUP", group.equal = c("loadings", "intercepts"))
smc.0521.5.11 <- mi.results(conf.fit.521.5,load.fit.521.5,int.fit.521.5)
smc.0521.5.11
# Chisquare is significant, rmsea > 0.08, CFI < 0.95 so we reject configural invariance
vars <- c("anx1","anx2","anx3","anx4","anx5","anx6")
run.par(vars = vars, df = article521.5.1, groupvar = "GROUP", iters = 300)
# We previously received the message that an ultra-Heywood case was detected, parallel analysis does not work for group reliabilities. The number of factors for Group = "0" is 0.

# Comparison 12
# Grouping variable: past vs distant future (group = 2 for future and group = 1 for past)
# Scale variable: State self-esteem scale
# Again, difference scores are calculated. We decided to do this by item and run a MI test on those.
article521.5$se1 <- article521.5$SelfEsteem_1.0 - article521.5$SelfEsteem_1
article521.5$se2 <- article521.5$SelfEsteem_2.0 - article521.5$SelfEsteem_2
article521.5$se3 <- article521.5$SelfEsteem_3.0 - article521.5$SelfEsteem_3
article521.5$se4 <- article521.5$SelfEsteem_4.0 - article521.5$SelfEsteem_4
article521.5$se5 <- article521.5$SelfEsteem_5.0 - article521.5$SelfEsteem_5
article521.5$se6 <- article521.5$SelfEsteem_6.0 - article521.5$SelfEsteem_6
# We only need group=2 and group=1 for this comparison, and will drop out group=0
article521.5.2 <- article521.5[article521.5[,"GROUP"] != 0,]
range(article521.5.2$GROUP)
# Construct model and estimate
model.521.6 <- 'F =~ se1 + se2 + se3 + se4 + se5 + se6'
conf.fit.521.6 <- cfa(model.521.6, data = article521.5.2, group = "GROUP")
load.fit.521.6 <- cfa(model.521.6, article521.5.2, group = "GROUP", group.equal = "loadings")
int.fit.521.6 <- cfa(model.521.6, article521.5.2, group = "GROUP", group.equal = c("loadings", "intercepts"))
smc.0521.5.12 <- mi.results(conf.fit.521.6,load.fit.521.6,int.fit.521.6)
smc.0521.5.12
# Chisquare is significant, rmsea > 0.08, CFI < 0.95 so we reject configural invariance
vars <- c("se1","se2","se3","se4","se5","se6")
run.par(vars = vars, df = article521.5.2, groupvar = "GROUP", iters = 300)

# Comparison 13
# Grouping variable: past vs control (group = 0 for control and group = 1 for past)
# Scale variable: State self-esteem scale
# We only need group=0 and group=1 for this comparison, and will drop out group=2
article521.5.3 <- article521.5[article521.5[,"GROUP"] != 2,]
range(article521.5.3$GROUP)
# Construct model and estimate
model.521.5.13 <- 'F =~ se1 + se2 + se3 + se4 + se5 + se6'
conf.fit.521.7 <- cfa(model.521.5.13, data = article521.5.3, group = "GROUP")
load.fit.521.7 <- cfa(model.521.5.13, article521.5.3, group = "GROUP", group.equal = "loadings")
int.fit.521.7 <- cfa(model.521.5.13, article521.5.3, group = "GROUP", group.equal = c("loadings", "intercepts"))
smc.0521.5.13 <- mi.results(conf.fit.521.7,load.fit.521.7,int.fit.521.7)
smc.0521.5.13
# Chisquare is significant, rmsea > 0.08, CFI < 0.95 so we reject configural invariance
vars <- c("se1","se2","se3","se4","se5","se6")
run.par(vars = vars, df = article521.5.3, groupvar = "GROUP", iters = 300)

# Comparison 14
# Grouping variable: time1 vs time2
# Scale variable: State-trait anxiety
# Make dataset with only anxiety scores for time 1
article521.5.t1 <- cbind(article521.5$Anx1reverseT1,article521.5$Anxiety_2,article521.5$Anxiety_3,article521.5$Anx4reverseT1,article521.5$Anx5reverseT1,article521.5$Anxiety_6)
grouping <- rep(0,nrow(article521.5.t1))
article521.5.t1 <- cbind(article521.5.t1,grouping)
# Make dataset with only anxiety scores for time 2
article521.5.t2 <- cbind(article521.5$Anx1reverseT2,article521.5$Anxiety_2.0,article521.5$Anxiety_3.0,article521.5$Anx4reverseT2,article521.5$Anx5reverseT2,article521.5$Anxiety_6.0)
grouping <- rep(1,nrow(article521.5.t2))
article521.5.t2 <- cbind(article521.5.t2,grouping)
# Combine datasets
article521.5.1.tot <- rbind(article521.5.t1,article521.5.t2)
colnames(article521.5.1.tot) <- c("anx1","anx2","anx3","anx4","anx5","anx6","group")
# The data is ordinal, so we construct syntax and estimate model according to Wu and Estabrook
model.521.8 <- 'F =~ anx1 + anx2 + anx3 + anx4 + anx5 + anx6'
# Syntax and estimate configural model
syntax.521.8.config <- measEq.syntax(model.521.8, 
                                     ID.fac = "std.lv", 
                                     ID.cat = "Wu",
                                     ordered = colnames(article521.5.1.tot[,1:6]),
                                     parameterization = "delta",
                                     data = article521.5.1.tot,
                                     group = "group", 
                                     group.equal = "configural", 
                                     orthogonal = T)

config.521.8 <- cfa(as.character(syntax.521.8.config),
                    article521.5.1.tot, 
                    ordered = colnames(article521.5.1.tot[,1:6]),
                    group = "group", 
                    estimator = "WLSMV")

# Syntax and estimate thresholds model
syntax.521.8.thres <- measEq.syntax(model.521.8, 
                                     ID.fac = "std.lv", 
                                     ID.cat = "Wu",
                                     ordered = colnames(article521.5.1.tot[,1:6]),
                                     parameterization = "delta",
                                     data = article521.5.1.tot,
                                     group = "group", 
                                     group.equal = "thresholds", 
                                     orthogonal = T)

thres.521.8 <- cfa(as.character(syntax.521.8.thres),
                    article521.5.1.tot, 
                    ordered = colnames(article521.5.1.tot[,1:6]),
                    group = "group", 
                    estimator = "WLSMV")

# Syntax and estimate loadings model
syntax.521.8.load <- measEq.syntax(model.521.8, 
                                     ID.fac = "std.lv", 
                                     ID.cat = "Wu",
                                     ordered = colnames(article521.5.1.tot[,1:6]),
                                     parameterization = "delta",
                                     data = article521.5.1.tot,
                                     group = "group", 
                                     group.equal = c("thresholds","loadings"), 
                                     orthogonal = T)

load.521.8 <- cfa(as.character(syntax.521.8.load),
                    article521.5.1.tot, 
                    ordered = colnames(article521.5.1.tot[,1:6]),
                    group = "group", 
                    estimator = "WLSMV")

# Note that we get the scaled estimates because we have ordinal data
smc.0521.5.14 <- mi.results(config.521.8,thres.521.8,load.521.8, type = "scaled")
smc.0521.5.14
# Chisquare is significant, rmsea > 0.08 (CFI is ok), so we reject configural invariance
vars <- c("anx1","anx2","anx3","anx4","anx5","anx6")
run.par(vars = vars, df = article521.5.1.tot, groupvar = "group", iters = 300)

# Comparison 15
# Grouping variable: time1 vs time2
# Scale variable: State self-esteem
# Make dataset with only anxiety scores for time 1
article521.5.t1 <- cbind(article521.5$SelfEsteem_1,article521.5$SelfEsteem_2,article521.5$SelfEsteem_3,article521.5$SelfEsteem_4,article521.5$SelfEsteem_5,article521.5$SelfEsteem_6)
grouping <- rep(0,nrow(article521.5.t1))
article521.5.t1 <- cbind(article521.5.t1,grouping)
# Make dataset with only anxiety scores for time 2
article521.5.t2 <- cbind(article521.5$SelfEsteem_1.0,article521.5$SelfEsteem_2.0,article521.5$SelfEsteem_3.0,article521.5$SelfEsteem_4.0,article521.5$SelfEsteem_5.0,article521.5$SelfEsteem_6.0)
grouping <- rep(1,nrow(article521.5.t2))
article521.5.t2 <- cbind(article521.5.t2,grouping)
# Combine datasets
article521.5.2.tot <- rbind(article521.5.t1,article521.5.t2)
colnames(article521.5.2.tot) <- c("es1","es2","es3","es4","es5","es6","group")
# The data is ordinal, so we construct syntax and estimate model according to Wu and Estabrook
model.521.9 <- 'F =~ es1 + es2 + es3 + es4 + es5 + es6'
# Syntax and estimate configural model
syntax.521.9.config <- measEq.syntax(model.521.9, 
                                     ID.fac = "std.lv", 
                                     ID.cat = "Wu",
                                     ordered = colnames(article521.5.2.tot[,1:6]),
                                     parameterization = "delta",
                                     data = article521.5.2.tot,
                                     group = "group", 
                                     group.equal = "configural", 
                                     orthogonal = T)

config.521.9 <- cfa(as.character(syntax.521.9.config),
                    article521.5.2.tot, 
                    ordered = colnames(article521.5.2.tot[,1:6]),
                    group = "group", 
                    estimator = "WLSMV")

# Syntax and estimate thresholds model
syntax.521.9.thres <- measEq.syntax(model.521.9, 
                                    ID.fac = "std.lv", 
                                    ID.cat = "Wu",
                                    ordered = colnames(article521.5.2.tot[,1:6]),
                                    parameterization = "delta",
                                    data = article521.5.2.tot,
                                    group = "group", 
                                    group.equal = "thresholds", 
                                    orthogonal = T)

thres.521.9 <- cfa(as.character(syntax.521.9.thres),
                   article521.5.2.tot, 
                   ordered = colnames(article521.5.2.tot[,1:6]),
                   group = "group", 
                   estimator = "WLSMV")

# Syntax and estimate loadings model
syntax.521.9.load <- measEq.syntax(model.521.9, 
                                   ID.fac = "std.lv", 
                                   ID.cat = "Wu",
                                   ordered = colnames(article521.5.2.tot[,1:6]),
                                   parameterization = "delta",
                                   data = article521.5.2.tot,
                                   group = "group", 
                                   group.equal = c("thresholds","loadings"), 
                                   orthogonal = T)

load.521.9 <- cfa(as.character(syntax.521.9.load),
                  article521.5.2.tot, 
                  ordered = colnames(article521.5.2.tot[,1:6]),
                  group = "group", 
                  estimator = "WLSMV")

# Note that we get the scaled estimates because we have ordinal data
smc.0521.5.15 <- mi.results(config.521.9,thres.521.9,load.521.9, type = "scaled")
smc.0521.5.15
# Chisquare is significant, rmsea > 0.08 (CFI ok) so we reject configural invariance
vars <- c("es1","es2","es3","es4","es5","es6")
run.par(vars = vars, df = article521.5.2.tot, groupvar = "group", iters = 300)

# Article 661 ----------------------------------------------------------------------
url <- 'https://osf.io/u82mp//?action=download'
filename <- 'article661.csv'
GET(url, write_disk(filename, overwrite = TRUE))
article661 <- read.csv2(filename, header=T, na.strings="NA", sep=",")

# Grouping variable: Consumption shared vs separate (cond_all)
# Scale variable: Perceived closeness (likabe + get_along + close + friends)
model.661 <- 'F =~ likabe + get_along + close + friends'
conf.fit.661 <- cfa(model.661, data = article661, group = "cond_all")
load.fit.661 <- cfa(model.661, article661, group = "cond_all", group.equal = "loadings")
int.fit.661 <- cfa(model.661, article661, group = "cond_all", group.equal = c("loadings", "intercepts"))
smc.0661.1.01 <- mi.results(conf.fit.661,load.fit.661,int.fit.661)
smc.0661.1.01
# Chisquare is significant, rmsea > 0.08 and CFI < 0.95 so we reject configural invariance
vars <- c("likabe","get_along","close","friends")
run.par(vars = vars, df = article661, groupvar = "cond_all", iters = 300)

# Article 811 ----------------------------------------------------------------------
# Study 1
url <- 'https://osf.io/9sg3u//?action=download'
filename <- 'article811.1.csv'
GET(url, write_disk(filename, overwrite = TRUE))
article811.1 <- read.csv2(filename, header=T, na.strings="NA", sep=",")
# Comparison 1
# Grouping variable: control and perspective-taking (condition)
# Scale variable: receptiveness (Q3.receparg + Q4.recepoarg + Q5recepgen + Q6recepogen)
model.811.1.1 <- 'F =~ Q3.receparg + Q4.recepoarg + Q5.recepgen + Q6.recepogen'
conf.fit.811.1.1 <- cfa(model.811.1.1, data = article811.1, group = "condition")
load.fit.811.1.1 <- cfa(model.811.1.1, article811.1, group = "condition", group.equal = "loadings")
int.fit.811.1.1 <- cfa(model.811.1.1, article811.1, group = "condition", group.equal = c("loadings", "intercepts"))
smc.0811.1.01 <- mi.results(conf.fit.811.1.1,load.fit.811.1.1,int.fit.811.1.1)
smc.0811.1.01
# Chisquare is significant, RMSEA > 0.08 and CFI < 0.95 so we reject configural invariance
# Run parallel analysis
vars <- c("Q3.receparg","Q4.recepoarg","Q5.recepgen","Q6.recepogen")
run.par(vars = vars, df = article811.1, groupvar = "condition", iters = 300)

# Comparison 2
# Grouping variable: control and perspective-taking (condition)
# Scale variable: value congruence (Q7.valueco + Q7.valuewv + Q8.valuemoral)
model.811.1.2 <- 'F =~ Q7.valueco + Q7.valuewv + Q8.valuemoral'
conf.fit.811.1.2 <- cfa(model.811.1.2, data = article811.1, group = "condition")
load.fit.811.1.2 <- cfa(model.811.1.2, article811.1, group = "condition", group.equal = "loadings")
int.fit.811.1.2 <- cfa(model.811.1.2, article811.1, group = "condition", group.equal = c("loadings", "intercepts"))
smc.0811.1.02 <- mi.results(conf.fit.811.1.2,load.fit.811.1.2,int.fit.811.1.2)
smc.0811.1.02
# 3 items so we assume configural invariance
# Metric: nonsig chisquare change, change RMSEA and CFI < .01 (and RMSEA < .08 and CFI > .95); lower AIC and BIC; metric invariance holds
# Scalar: nonsig chisquare change, change RMSEA and CFI < .01; lower AIC and BIC; scalar invariance holds.

# Study 2
url <- 'https://osf.io/ymc3h//?action=download'
filename <- 'article811.2.csv'
GET(url, write_disk(filename, overwrite = TRUE))
article811.2 <- read.csv2(filename, header=T, na.strings="NA", sep=",")

# Comparison 3
# Grouping variable: perspective-taking/different-ideology condition + perspective-taking/same-ideology condition + control condition (condition)
# Scale variable: receptiveness (recep1.arg + recep2.arg + recep3.gen + recep4.gen)
model.811.2.1 <- 'F =~ recep1.arg + recep2.arg + recep3.gen + recep4.gen'
conf.fit.811.2.1 <- cfa(model.811.2.1, data = article811.2, group = "condition")
load.fit.811.2.1 <- cfa(model.811.2.1, article811.2, group = "condition", group.equal = "loadings")
int.fit.811.2.1 <- cfa(model.811.2.1, article811.2, group = "condition", group.equal = c("loadings", "intercepts"))
smc.0811.2.01 <- mi.results(conf.fit.811.2.1,load.fit.811.2.1,int.fit.811.2.1)
smc.0811.2.01
# chisquare is significant, rmsea > 0.08 and CFI < 0.95 so we reject configural invariance
# Run parallel analysis
vars <- c("recep1.arg","recep2.arg","recep3.gen","recep4.gen")
run.par(vars = vars, df = article811.2, groupvar = "condition", iters = 300)

# Comparison 4
# Grouping variable: perspective-taking/different-ideology condition + perspective-taking/same-ideology condition + control condition (condition)
# Scale variable: value congruence (Q3.congruent + Q4.fitworldview + Q5.morals)
model.811.2.2 <- 'F =~ Q3.congruent + Q4.fitworldview + Q5.morals'
conf.fit.811.2.2 <- cfa(model.811.2.2, data = article811.2, group = "condition")
load.fit.811.2.2 <- cfa(model.811.2.2, article811.2, group = "condition", group.equal = "loadings")
int.fit.811.2.2 <- cfa(model.811.2.2, article811.2, group = "condition", group.equal = c("loadings", "intercepts"))
smc.0811.2.02 <- mi.results(conf.fit.811.2.2,load.fit.811.2.2,int.fit.811.2.2)
smc.0811.2.02
# 3 items so we assume configural invariance
# Metric: nonsig chisquare change, change CFI < .01 (and >.95); lower AIC and BIC; metric invariance holds (RMSEA change > .01 but still < .08)
# Scalar: nonsig chisquare change, change CFI < .01; lower AIC and BIC; scalar invariance holds (RMSEA change > .01 but fit improves)

# Article 861 ----------------------------------------------------------------------
# Study 1
url <- 'https://osf.io/bdq3m//?action=download'
filename <- 'article861.1.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article861.1 <- read_sav(filename)
# Comparison 1
# Grouping variable: giving condition and getting condition (condition)
# Scaling variable: day-end happiness (DaysEnd_good.1 + DaysEnd_happy.1 + DaysEnd_satisfied.1 + DaysEnd_mood.1)

# The authors seemingly collapse all variables over time and then test them per condition, so we will do the same.
article861.1$daysend.good <- as.numeric((article861.1$DaysEnd_good.1 + article861.1$DaysEnd_good.2 + article861.1$DaysEnd_good.3 + article861.1$DaysEnd_good.4 + article861.1$DaysEnd_good.5)/5)
article861.1$daysend.happy <- as.numeric((article861.1$DaysEnd_happy.1 + article861.1$DaysEnd_happy.2 + article861.1$DaysEnd_happy.3 + article861.1$DaysEnd_happy.4 + article861.1$DaysEnd_happy.5)/5)
article861.1$daysend.satisfied <- as.numeric((article861.1$DaysEnd_satisfied.1 + article861.1$DaysEnd_satisfied.2 + article861.1$DaysEnd_satisfied.3 + article861.1$DaysEnd_satisfied.4 + article861.1$DaysEnd_satisfied.5)/5)
article861.1$daysend.mood <- as.numeric((article861.1$DaysEnd_mood.1 + article861.1$DaysEnd_mood.2 + article861.1$DaysEnd_mood.3 + article861.1$DaysEnd_mood.4 + article861.1$DaysEnd_mood.5)/5)
article861.1$condition <- as.numeric(article861.1$condition)

model.861.1 <- 'F =~ daysend.good + daysend.happy + daysend.satisfied + daysend.mood'
conf.fit.861.1 <- cfa(model.861.1, data = article861.1, group = "condition")
load.fit.861.1 <- cfa(model.861.1, article861.1, group = "condition", group.equal = "loadings")
int.fit.861.1 <- cfa(model.861.1, article861.1, group = "condition", group.equal = c("loadings", "intercepts"))
smc.0861.1.01 <- mi.results(conf.fit.861.1,load.fit.861.1,int.fit.861.1)
smc.0861.1.01
# Configural: Chisquare not significant, RMSEA < .08, CFI > .95. Configural invariance holds
# Metric: Chisquare change not significant, change CFI < .01, AIC and BIC lower. Change RMSEA is too large.
# Three out of four criteria adhere to our criteria (only rmsea change is too large); metric invariance holds
# Scalar: chisquare change not significant, change RMSEA = 0.002, change CFI = -0.002, AIC and BIC both lower; scalar invariance holds

# Comparison 2
# Grouping variable: giving condition and getting condition
# Scaling variable: recalled-happiness
# The authors collapse all variables over time and then test them per condition, so we will do the same.
article861.1$recalled.good <- (article861.1$Recalled_good.1 + article861.1$Recalled_good.2 + article861.1$Recalled_good.3 + article861.1$Recalled_good.4 + article861.1$Recalled_good.5)/5
article861.1$recalled.happy <- (article861.1$Recalled_happy.1 + article861.1$Recalled_happy.2 + article861.1$Recalled_happy.3 + article861.1$Recalled_happy.4 + article861.1$Recalled_happy.5)/5
article861.1$recalled.satisfied <- (article861.1$Recalled_satisfied.1 + article861.1$Recalled_satisfied.2 + article861.1$Recalled_satisfied.3 + article861.1$Recalled_satisfied.4 + article861.1$Recalled_satisfied.5)/5
article861.1$recalled.mood <- (article861.1$Recalled_mood.1 + article861.1$Recalled_mood.2 + article861.1$Recalled_mood.3 + article861.1$Recalled_mood.4 + article861.1$Recalled_mood.5)/5

model.861.2 <- 'F =~ recalled.good + recalled.happy + recalled.satisfied + recalled.mood'
conf.fit.861.2 <- cfa(model.861.2, data = article861.1, group = "condition")
load.fit.861.2 <- cfa(model.861.2, article861.1, group = "condition", group.equal = "loadings")
int.fit.861.2 <- cfa(model.861.2, article861.1, group = "condition", group.equal = c("loadings", "intercepts"))
smc.0861.1.02 <- mi.results(conf.fit.861.2,load.fit.861.2,int.fit.861.2)
smc.0861.1.02
# Chisquare significant, rmsea > .08, so we reject configural invariance.
# Run parallel analysis
vars <- c("recalled.good","recalled.happy","recalled.satisfied","recalled.mood")
run.par(vars = vars, df = article861.1, groupvar = "condition", iters = 300)

# Comparison 3
# Grouping variable: giving condition and getting condition
# Scaling variable: study-happiness
article861.1$study.exciting <- as.numeric((article861.1$Study_exciting.1 + article861.1$Study_exciting.2 + article861.1$Study_exciting.3 + article861.1$Study_exciting.4 + article861.1$Study_exciting.5)/5)
article861.1$study.enjoyable <- as.numeric((article861.1$Study_enjoyable.1 + article861.1$Study_enjoyable.2 + article861.1$Study_enjoyable.3 + article861.1$Study_enjoyable.4 + article861.1$Study_enjoyable.5)/5)
article861.1$study.rewarding <- as.numeric((article861.1$Study_rewarding.1 + article861.1$Study_rewarding.2 + article861.1$Study_rewarding.3 + article861.1$Study_rewarding.4 + article861.1$Study_rewarding.5)/5)

model.861.3 <- 'F =~ study.exciting + study.enjoyable + study.rewarding'
conf.fit.861.3 <- cfa(model.861.3, data = article861.1, group = "condition")
load.fit.861.3 <- cfa(model.861.3, article861.1, group = "condition", group.equal = "loadings")
int.fit.861.3 <- cfa(model.861.3, article861.1, group = "condition", group.equal = c("loadings", "intercepts"))
smc.0861.1.03 <- mi.results(conf.fit.861.3,load.fit.861.3,int.fit.861.3)
smc.0861.1.03
# 3 items so we assume configural invariance
# Metric: sig chisquare change, change RMSEA and CFI > .01; larger AIC; metric invariance rejected

# Comparison 4
# Grouping variable: giving condition and getting condition
# Scaling variable: overall happiness
model.861.4 <- 'F =~ daysend.good + daysend.happy + daysend.satisfied + daysend.mood + recalled.good + recalled.happy + recalled.satisfied + recalled.mood + study.exciting + study.enjoyable + study.rewarding'
conf.fit.861.4 <- cfa(model.861.4, data = article861.1, group = "condition")
load.fit.861.4 <- cfa(model.861.4, article861.1, group = "condition", group.equal = "loadings")
int.fit.861.4 <- cfa(model.861.4, article861.1, group = "condition", group.equal = c("loadings", "intercepts"))
smc.0861.1.04 <- mi.results(conf.fit.861.4,load.fit.861.4,int.fit.861.4)
smc.0861.1.04
# Chisquare significant, rmsea > .08, CFI < .95 so we reject configural invariance
# Run parallel analysis
vars <- c("daysend.good","daysend.happy","daysend.satisfied","daysend.mood","recalled.good","recalled.happy","recalled.satisfied","recalled.mood","study.exciting","study.enjoyable","study.rewarding")
run.par(vars = vars, df = article861.1, groupvar = "condition", iters = 300)

# Comparison 5
# Grouping variable: time1 to time5
# Scaling variable: day-end happiness

# Make 2 datasets, time 1 vs time 5
# t1
article861.1.1 <- cbind(article861.1$DaysEnd_good.1, article861.1$DaysEnd_happy.1, article861.1$DaysEnd_satisfied.1, article861.1$DaysEnd_mood.1,
                        article861.1$Recalled_good.1, article861.1$Recalled_happy.1, article861.1$Recalled_satisfied.1, article861.1$Recalled_mood.1,
                        article861.1$Study_exciting.1, article861.1$Study_enjoyable.1, article861.1$Study_rewarding.1)
grouping <- rep(1,nrow(article861.1.1))
article861.1.1 <- cbind(article861.1.1,grouping)

#t5
article861.1.5 <- cbind(article861.1$DaysEnd_good.5, article861.1$DaysEnd_happy.5, article861.1$DaysEnd_satisfied.5, article861.1$DaysEnd_mood.5,
                        article861.1$Recalled_good.5, article861.1$Recalled_happy.5, article861.1$Recalled_satisfied.5, article861.1$Recalled_mood.5,
                        article861.1$Study_exciting.5, article861.1$Study_enjoyable.5, article861.1$Study_rewarding.5)
grouping <- rep(5,nrow(article861.1.5))
article861.1.5 <- cbind(article861.1.5,grouping)
# Merge datasets
article861.1.tot <- rbind(article861.1.1,article861.1.5)
colnames(article861.1.tot) <- c("end.good","end.happy","end.satis","end.mood","rec.good","rec.happy","rec.satis","rec.mood","stud.ex","stud.en","stud.re","time")
# Scaling variable: day-end happiness
model.861.5 <- 'F =~ end.good + end.happy + end.satis + end.mood'
conf.fit.861.5 <- cfa(model.861.5, data = article861.1.tot, group = "time")
load.fit.861.5 <- cfa(model.861.5, article861.1.tot, group = "time", group.equal = "loadings")
int.fit.861.5 <- cfa(model.861.5, article861.1.tot, group = "time", group.equal = c("loadings", "intercepts"))
smc.0861.1.05 <- mi.results(conf.fit.861.5,load.fit.861.5,int.fit.861.5)
smc.0861.1.05
# Chisquare significant, rmsea > .08 so we reject configural invariance
# Run parallel analysis
vars <- c("end.good","end.happy","end.satis","end.mood")
run.par(vars = vars, df = article861.1.tot, groupvar = "time", iters = 300)

# Comparison 6
# Grouping variable: time1 to time5
# Scaling variable: recall happiness
model.861.6 <- 'F =~ rec.good + rec.happy + rec.satis + rec.mood'
conf.fit.861.6 <- cfa(model.861.6, data = article861.1.tot, group = "time")
load.fit.861.6 <- cfa(model.861.6, article861.1.tot, group = "time", group.equal = "loadings")
int.fit.861.6 <- cfa(model.861.6, article861.1.tot, group = "time", group.equal = c("loadings", "intercepts"))
smc.0861.1.06 <- mi.results(conf.fit.861.6,load.fit.861.6,int.fit.861.6)
smc.0861.1.06
# Chisquare significant, rmsea > .08 so we reject configural invariance
# Run parallel analysis
vars <- c("rec.good","rec.happy","rec.satis","rec.mood")
run.par(vars = vars, df = article861.1.tot, groupvar = "time", iters = 300)

# Comparison 7
# grouping variable: time1 to time5
# scaling variable: study happiness
model.861.7 <- 'F =~ stud.ex + stud.en + stud.re'
conf.fit.861.7 <- cfa(model.861.7, data = article861.1.tot, group = "time")
load.fit.861.7 <- cfa(model.861.7, article861.1.tot, group = "time", group.equal = "loadings")
int.fit.861.7 <- cfa(model.861.7, article861.1.tot, group = "time", group.equal = c("loadings", "intercepts"))
smc.0861.1.07 <- mi.results(conf.fit.861.7,load.fit.861.7,int.fit.861.7)
smc.0861.1.07
# 3 items so we assume configural invariance
# Metric: nonsig chisquare change, change RMSEA and CFI < .01 (and < .08 and > .95); lower AIC and BIC; metric invariance holds
# Scalar: nonsig chisquare change, change CFI < .01; lower AIC and BIC; scalar invariance holds.

# Comparison 8
# Grouping variable: time1 to time5
# Scaling variable: overall happiness
model.861.8 <- 'F =~ end.good + end.happy + end.satis + end.mood + rec.good + rec.happy + rec.satis + rec.mood + stud.ex + stud.en + stud.re'
conf.fit.861.8 <- cfa(model.861.8, data = article861.1.tot, group = "time")
load.fit.861.8 <- cfa(model.861.8, article861.1.tot, group = "time", group.equal = "loadings")
int.fit.861.8 <- cfa(model.861.8, article861.1.tot, group = "time", group.equal = c("loadings", "intercepts"))
smc.0861.1.08 <- mi.results(conf.fit.861.8,load.fit.861.8,int.fit.861.8)
smc.0861.1.08
# Chisquare significant, rmsea > .08, CFI < 0.95 so we reject configural invariance
# Run parallel analysis
vars <- c("end.good","end.happy","end.satis","end.mood","rec.good","rec.happy","rec.satis","rec.mood","stud.ex","stud.en","stud.re")
run.par(vars = vars, df = article861.1.tot, groupvar = "time", iters = 300)

# Study 2
url <- 'https://osf.io/kz8q2//?action=download'
filename <- 'article861.2.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article861.2 <- read_sav(filename)

# Comparison 9
# grouping variable: giving condition and getting condition
# scale variable: happiness block (happy, elated, joy)

# First we collapse all round 1-10 together
article861.2$happy <- as.numeric((article861.2$round1happy + article861.2$round2happy + article861.2$round3happy + article861.2$round4happy + article861.2$round5happy +
                      article861.2$round6happy + article861.2$round7happy + article861.2$round8happy + article861.2$round9happy + article861.2$round10happy)/10)
article861.2$elated <- as.numeric((article861.2$round1elated + article861.2$round2elated + article861.2$round3elated + article861.2$round4elated + article861.2$round5elated +
                                    article861.2$round6elated + article861.2$round7elated + article861.2$round8elated + article861.2$round9elated + article861.2$round10elated)/10)
article861.2$joy <- as.numeric((article861.2$round1joy + article861.2$round2joy + article861.2$round3joy + article861.2$round4joy + article861.2$round5joy +
                                    article861.2$round6joy + article861.2$round7joy + article861.2$round8joy + article861.2$round9joy + article861.2$round10joy)/10)
article861.2$condition <- as.numeric(article861.2$condition)

model.861.9 <- 'F =~ happy + elated + joy'
conf.fit.861.9 <- cfa(model.861.9, data = article861.2, group = "condition")
load.fit.861.9 <- cfa(model.861.9, article861.2, group = "condition", group.equal = "loadings")
int.fit.861.9 <- cfa(model.861.9, article861.2, group = "condition", group.equal = c("loadings", "intercepts"))
smc.0861.2.09 <- mi.results(conf.fit.861.9,load.fit.861.9,int.fit.861.9)
smc.0861.2.09
# 3 items so we assume configural invariance
# Metric: sig chisquare change, change RMSEA > .01 (and > .08); AIC larger, we reject metric invariance (CFI change and BIC ok)

# Comparison 10
# Grouping variable: round 1 to round 10
# Scale variable: happiness block
# First we split the datasets per timepoint 
article861.2.1 <- cbind(article861.2$round1happy,article861.2$round1elated,article861.2$round1joy)
article861.2.10 <- cbind(article861.2$round10happy,article861.2$round10elated,article861.2$round10joy)
article861.2.tot <- rbind(article861.2.1,article861.2.10)
grouping <- rep(c(1,10),each=502); nrow(article861.2.tot) == length(grouping)
article861.2.tot <- cbind(article861.2.tot,grouping)
colnames(article861.2.tot) <- c("happy","elated","joy","time")

model.861.10 <- 'F =~ happy + elated + joy'
conf.fit.861.10 <- cfa(model.861.10, data = article861.2.tot, group = "time")
load.fit.861.10 <- cfa(model.861.10, article861.2.tot, group = "time", group.equal = "loadings")
int.fit.861.10 <- cfa(model.861.10, article861.2.tot, group = "time", group.equal = c("loadings", "intercepts"))
smc.0861.2.10 <- mi.results(conf.fit.861.10,load.fit.861.10,int.fit.861.10)
smc.0861.2.10
# 3 items so we assume configural invariance
# Metric: sig chisquare change, change RMSEA and CFI < .01 (and < .08 and >.95); lower AIC and BIC; metric invariance holds
# Scalar: sig chisquare change, larger AIC, change RMSEA > .01; we reject scalar invariance.

# Article 891 ----------------------------------------------------------------------
# Study 3
url <- 'https://osf.io/yk75c//?action=download'
filename <- 'article891.1.xlsx'
GET(url, write_disk(filename, overwrite = TRUE))
article891.1 <- read_excel(filename, sheet = 5)

# Comparison 1
# Grouping variable: fixed or growth mindset (cond)
# Scale variable: support for resettling refugees (Refugees1 + Refugees2 + Refugees3 + Refugees4)
model.891.1 <- 'F =~ Refugees1 + Refugees2 + Refugees3 + Refugees4'
conf.fit.891.1 <- cfa(model.891.1, data = article891.1, group = "cond")
load.fit.891.1 <- cfa(model.891.1, article891.1, group = "cond", group.equal = "loadings")
int.fit.891.1 <- cfa(model.891.1, article891.1, group = "cond", group.equal = c("loadings", "intercepts"))
smc.0891.3.01 <- mi.results(conf.fit.891.1,load.fit.891.1,int.fit.891.1)
smc.0891.3.01
# Configural: chisquare nonsig, rmsea < .08, CFI > .95; configural holds
# Metric: nonsig chisquare change, change RMSEA and CFI < .01; lower AIC and BIC; metric invariance holds
# Scalar: nonsig chisquare change, change CFI < .01; lower AIC and BIC; scalar invariance holds (RMSEA change > .01 but fit improves).

# Comparison 2
# grouping variable: fixed or growth mindset (cond)
# scale variable: political orientation (political1 + political2 + political3)
model.891.2 <- 'F =~ political1 + political2 + political3'
conf.fit.891.2 <- cfa(model.891.2, data = article891.1, group = "cond")
load.fit.891.2 <- cfa(model.891.2, article891.1, group = "cond", group.equal = "loadings")
int.fit.891.2 <- cfa(model.891.2, article891.1, group = "cond", group.equal = c("loadings", "intercepts"))
smc.0891.3.02 <- mi.results(conf.fit.891.2,load.fit.891.2,int.fit.891.2)
smc.0891.3.02
# 3 items so we assume configural invariance
# metric: nonsig chisquare change, change CFI < .01; lower AIC and BIC; metric invariance holds (change RMSEA > .01)
# scalar: nonsig chisquare change, change CFI < .01; lower AIC and BIC; scalar invariance holds (change RMSEA > .01 but fit improves).

# Study 4
url <- 'https://osf.io/yk75c//?action=download'
filename <- 'article891.2.xlsx'
GET(url, write_disk(filename, overwrite = TRUE))
article891.2 <- read_excel(filename, sheet = 7)

# Comparison 3
# grouping variable: fixed or growth mindset (cond)
# scale variable: participants’ beliefs about how well refugees can assimilate in society (assim1 + assim2 + assim3 + assim4 + assim5)
model.891.3 <- 'F =~ assim1 + assim2 + assim3 + assim4 + assim5'
conf.fit.891.3 <- cfa(model.891.3, data = article891.2, group = "cond")
load.fit.891.3 <- cfa(model.891.3, article891.2, group = "cond", group.equal = "loadings")
int.fit.891.3 <- cfa(model.891.3, article891.2, group = "cond", group.equal = c("loadings", "intercepts"))
smc.0891.4.03 <- mi.results(conf.fit.891.3,load.fit.891.3,int.fit.891.3)
smc.0891.4.03
# Chisquare significant, rmsea > 0.08, so we reject configural invariance
# Run parallel analysis
vars <- c("assim1","assim2","assim3","assim4","assim5")
run.par(vars = vars, df = article891.2, groupvar = "cond", iters = 300)

# Comparison 4
# grouping variable: fixed or growth mindset (cond)
# scale variable: support for resettling refugees (Refugees1 + Refugees2 + Refugees3 + Refugees4)
model.891.4 <- 'F =~ Refugees1 + Refugees2 + Refugees3 + Refugees4'
conf.fit.891.4 <- cfa(model.891.4, data = article891.2, group = "cond")
load.fit.891.4 <- cfa(model.891.4, article891.2, group = "cond", group.equal = "loadings")
int.fit.891.4 <- cfa(model.891.4, article891.2, group = "cond", group.equal = c("loadings", "intercepts"))
smc.0891.4.04 <- mi.results(conf.fit.891.4,load.fit.891.4,int.fit.891.4)
smc.0891.4.04
# Chisquare significant, rmsea > 0.08, so we reject configural invariance
vars <- c("Refugees1","Refugees2","Refugees3","Refugees4")
run.par(vars = vars, df = article891.2, groupvar = "cond", iters = 300)

# Comparison 5
# Grouping variable: fixed or growth mindset (cond)
# Scale variable: political orientation (political1 + political2 + political3)
model.891.5 <- 'F =~ political1 + political2 + political3'
conf.fit.891.5 <- cfa(model.891.5, data = article891.2, group = "cond")
load.fit.891.5 <- cfa(model.891.5, article891.2, group = "cond", group.equal = "loadings")
int.fit.891.5 <- cfa(model.891.5, article891.2, group = "cond", group.equal = c("loadings", "intercepts"))
smc.0891.4.05 <- mi.results(conf.fit.891.5,load.fit.891.5,int.fit.891.5)
smc.0891.4.05
# 3 items so we assume configural invariance
# Metric: nonsig chisquare change, change CFI < .01; lower BIC; 
# but: change RMSEA > .01 and AIC is larger; because chisquare change is non-significant, we cannot reject metric invariance; metric invariance holds
# Scalar: nonsig chisquare change, change CFI < .01; lower AIC and BIC; scalar invariance holds (RMSEA change > .01 but fit improves).

# Article 941 ----------------------------------------------------------------------
# Study 2
url <- 'https://osf.io/4yu27//?action=download'
filename <- 'article941.Rda'
GET(url, write_disk(filename, overwrite = TRUE))
article941 <- load(filename)
article941 <- dat

# Grouping variable: butcher, firefighter, construction worker
# Scaling variable: fair, just, accurate, intelligent

# There are 6 conditions, from the syntax (https://osf.io/49vnu/) we gather that cond1&cond2 / cond3&cond4 / cond5&cond6 belong together.
article941.1 <- article941[which(article941$cond == 1),c("c1.fair","c1.just","c1.accurate","c1.intelligent","cond")]
article941.2 <- article941[which(article941$cond == 2),c("c2.fair","c2.just","c2.accurate","c2.intelligent","cond")]
article941.3 <- article941[which(article941$cond == 3),c("c3.fair","c3.just","c3.accurate","c3.intelligent","cond")]
article941.4 <- article941[which(article941$cond == 4),c("c4.fair","c4.just","c4.accurate","c4.intelligent","cond")]
article941.5 <- article941[which(article941$cond == 5),c("c5.fair","c5.just","c5.accurate","c5.intelligent","cond")]
article941.6 <- article941[which(article941$cond == 6),c("c6.fair","c6.just","c6.accurate","c6.intelligent","cond")]
colnames(article941.1) <- colnames(article941.2) <- colnames(article941.3) <- c("fair","just","accurate","intelligent","cond")
colnames(article941.4) <- colnames(article941.5) <- colnames(article941.6) <- c("fair","just","accurate","intelligent","cond")
article941.tot <- rbind(article941.1,article941.2,article941.3,article941.4,article941.5,article941.6)
article941.tot <- lapply(article941.tot,as.numeric)
article941.tot <- as.data.frame(article941.tot)

model.941 <- 'F =~ fair + just + accurate + intelligent'
conf.fit.941 <- cfa(model.941, data = article941.tot, group = "cond")
load.fit.941 <- cfa(model.941, article941.tot, group = "cond", group.equal = "loadings")
int.fit.941 <- cfa(model.941, article941.tot, group = "cond", group.equal = c("loadings", "intercepts"))
smc.0941.2.01 <- mi.results(conf.fit.941,load.fit.941,int.fit.941)
smc.0941.2.01
# Chisquare significant, rmsea > .08, so we reject configural invariance 
vars <- c("fair","just","accurate","intelligent")
run.par(vars = vars, df = article941.tot, groupvar = "cond", iters = 300)

# Article 1031 ---------------------------------------------------------------------
url <- 'https://osf.io/zjxhw//?action=download'
filename <- 'article1031.csv'
GET(url, write_disk(filename, overwrite = TRUE))
article1031 <- read.csv2(filename, header=T, na.strings="NA", sep=",")
colnames(article1031)

# Comparison 1
# Grouping variable: data collection site (Site)
# Scale variable: Rutger Alcohol Problem Index (RAPI)

# Only the total score for the scaling variable is shared, meaning we cannot construct a scale and cannot do a MI test
# we also checked the other datasets for study 1 here, but all of them only have total scores: https://osf.io/h67mx/files/

# Comparison 2
# Grouping variable: data collection site (Site)
# Scale variable: University Identification Questionnaire

# Only the total score for the scaling variable is shared, meaning we cannot construct a scale and cannot do a MI test
# we also checked the other datasets for study 1 here, but all of them only have total scores: https://osf.io/h67mx/files/
# Article 1081 ---------------------------------------------------------------------
url <- 'https://osf.io/e25yk//?action=download'
filename <- 'article1081.csv'
GET(url, write_disk(filename, overwrite = TRUE))
article1081 <- read.csv2(filename, header=T, na.strings="NA", sep=",")

# Comparison 1
# Grouping variable: Incremental theorists and entity theorists
# Scale variable: Cost perception

# There is a variable "PerceivedCostofHelpSeeking" in the dataset, which according to the values seems to be the combination of the seven perceived cost of receiving help measure as referenced here https://osf.io/knygt/. As such, we cannot construct a scale because we do not have the item scores. No correlations reported in the article either so we can't do a MI test. 
  
# Comparison 2
# Grouping variable: anthropomorphized vs nonanthropomorphized helper (Anthropomorphism)
# Scale variable: Cost perception
  
# There is a variable "PerceivedCostofHelpSeeking" in the dataset, which according to the values seems to be the combination of the seven perceived cost of receiving help measure as referenced here https://osf.io/knygt/. As such, we cannot construct a scale because we do not have the item scores. No correlations reported in the article either so we can't do a MI test. 

# Article 1111 ---------------------------------------------------------------------
# Study 2b
# Comparison 1-3
url <- 'https://osf.io/ev5yq//?action=download'
filename <- 'article1111.1.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article1111.1 <- read_sav(filename)
article1111.1$cond <- as.numeric(article1111.1$cond)

# From the supplemental material (https://journals.sagepub.com/doi/suppl/10.1177/0956797617733829):
# "We labeled the factors “appearance solutions” (plastic surgery, liposuction, diet pills, 
# cosmetics/makeup, facial makeover; Cronbach’s α = .82), “fitness solutions” (gym, health food, 
# fitness equipment, exercise, personal activity tracker; Cronbach’s α = .83), and “other solutions” 
# (batteries, trash bags, light bulbs, plastic utensils; Cronbach’s α = .81)."

# Comparison 1: 
# Grouping variable: control and pathogen threat (cond)
# Scale variable: Fitness solution (health1 + health2 + health3 + health4 + health5)
model.1111.1 <- 'F =~ health1 + health2 + health3 + health4 + health5'
conf.fit.1111.1 <- cfa(model.1111.1, data = article1111.1, group = "cond")
load.fit.1111.1 <- cfa(model.1111.1, article1111.1, group = "cond", group.equal = "loadings")
int.fit.1111.1 <- cfa(model.1111.1, article1111.1, group = "cond", group.equal = c("loadings", "intercepts"))
smc.1111.2.01 <- mi.results(conf.fit.1111.1,load.fit.1111.1,int.fit.1111.1)
smc.1111.2.01
# Chisquare significant, rmsea > .08, so we reject configural invariance
vars <- c("health1","health2","health3","health4","health5")
run.par(vars = vars, df = article1111.1, groupvar = "cond", iters = 300)

# Comparison 2: 
# Grouping variable: control and pathogen threat (cond)
# Scale variable: Appearance solution (app1 + app2 + app3 + app4 + app5)
model.1111.2 <- 'F =~ app1 + app2 + app3 + app4 + app5'
conf.fit.1111.2 <- cfa(model.1111.2, data = article1111.1, group = "cond")
load.fit.1111.2 <- cfa(model.1111.2, article1111.1, group = "cond", group.equal = "loadings")
int.fit.1111.2 <- cfa(model.1111.2, article1111.1, group = "cond", group.equal = c("loadings", "intercepts"))
smc.1111.2.02 <- mi.results(conf.fit.1111.2,load.fit.1111.2,int.fit.1111.2)
smc.1111.2.02
# Chisquare significant, rmsea > .08, CFI < .95, so we reject configural invariance
vars <- c("app1","app2","app3","app4","app5")
run.par(vars = vars, df = article1111.1, groupvar = "cond", iters = 300)

# Comparison 3: 
# Grouping variable: control and pathogen threat (cond)
# Scale variable: Other solution (control1 + control2 + control3 + control4)
model.1111.3 <- 'F =~ control1 + control2 + control3 + control4'
conf.fit.1111.3 <- cfa(model.1111.3, data = article1111.1, group = "cond")
load.fit.1111.3 <- cfa(model.1111.3, article1111.1, group = "cond", group.equal = "loadings")
int.fit.1111.3 <- cfa(model.1111.3, article1111.1, group = "cond", group.equal = c("loadings", "intercepts"))
smc.1111.2.03 <- mi.results(conf.fit.1111.3,load.fit.1111.3,int.fit.1111.3)
smc.1111.2.03
# Chisquare non-significant, rmsea < .08, CFI > .95, so configural invariance holds
# Nonsig chisquare difference, RMSEA and CFI difference is 0, AIC and BIC are smaller; metric invariance holds
# Nonsig chisquare difference, RMSEA and CFI difference is 0, AIC and BIC are smaller; scalar invariance holds

# Study 3
# Comparison 4-6
url <- 'https://osf.io/5fubx//?action=download'
filename <- 'article1111.2.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article1111.2 <- read_sav(filename)
article1111.2$condition <- as.numeric(article1111.2$condition)

# From the manuscript
# In the second task, participants rated their liking for (−5 = dislike, 5 = like) and likelihood of purchasing (0 = not at all, 10 = very) 17 consumer products (chosen based on a pretest reported in the Supplemental Material). These consisted of appearance products (liposuction, cosmetic plastic surgery, diet pills, cosmetics/makeup, blemish cream, facial makeover), hygiene products (soap, shampoo, toothpaste, toilet paper, dental floss, antiseptic hand wipes), and unrelated household products (batteries, light bulbs, aluminum foil, music files/CDs, plastic utensils). 

# From the supplemental material (https://journals.sagepub.com/doi/suppl/10.1177/0956797617733829):
# Scores on the liking and purchase likelihood ratings correlated highly within product items (rs > .55), and thus three overall composites were created for appearance (Cronbach’s α = .91), hygiene (Cronbach’s α = .88), and household (Cronbach’s α = .81) products.

# Comparison 4: 
# Grouping variable: control and pathogen threat (condition)
# Scale variable: Appearance; the dataset has all 6 items and all items have a liking/purchasing answer, so 12 items in total we decided not to combine them like the supplemental material does but to test them all separately
# liposuct1 + liposuct2 + psurgery1 + psurgery2 + dietpills1 + dietpills2 + cosmetic1 + cosmetic2 + blemcream1 + blemcream2 + fmakeover1 + fmakeover2
model.1111.4 <- 'F =~ liposuct1 + liposuct2 + psurgery1 + psurgery2 + dietpills1 + dietpills2 + cosmetic1 + cosmetic2 + blemcream1 + blemcream2 + fmakeover1 + fmakeover2'
conf.fit.1111.4 <- cfa(model.1111.4, data = article1111.2, group = "condition")
load.fit.1111.4 <- cfa(model.1111.4, article1111.2, group = "condition", group.equal = "loadings")
int.fit.1111.4 <- cfa(model.1111.4, article1111.2, group = "condition", group.equal = c("loadings", "intercepts"))
smc.1111.3.04 <- mi.results(conf.fit.1111.4,load.fit.1111.4,int.fit.1111.4)
smc.1111.3.04
# Chisquare significant, rmsea > .08, CFI < .95, so we reject configural invariance
vars <- c("liposuct1","liposuct2","psurgery1","psurgery2","dietpills1","dietpills2","cosmetic1","cosmetic2","blemcream1","blemcream2","fmakeover1","fmakeover2")
run.par(vars = vars, df = article1111.2, groupvar = "condition", iters = 300)

# Comparison 5: 
# Grouping variable: control and pathogen threat (condition)
# Scale variable: Hygiene; here we also take the 6 items and leave them split for liking and purchase, so 12 in total
# Soap1 + soap2 + shampoo1 + shampoo2 + tooth1 + tooth2 + toilet1 + toilet2 + floss1 + floss2 + handwipe1 + handwipe2
model.1111.5 <- 'F =~ soap1 + soap2 + shampoo1 + shampoo2 + tooth1 + tooth2 + toilet1 + toilet2 + floss1 + floss2 + handwipe1 + handwipe2'
conf.fit.1111.5 <- cfa(model.1111.5, data = article1111.2, group = "condition")
load.fit.1111.5 <- cfa(model.1111.5, article1111.2, group = "condition", group.equal = "loadings")
int.fit.1111.5 <- cfa(model.1111.5, article1111.2, group = "condition", group.equal = c("loadings", "intercepts"))
smc.1111.3.05 <- mi.results(conf.fit.1111.5,load.fit.1111.5,int.fit.1111.5)
smc.1111.3.05
# Chisquare significant, rmsea > .08, CFI < .95, so we reject configural invariance
vars <- c("soap1","soap2","shampoo1","shampoo2","tooth1","tooth2","toilet1","toilet2","floss1","floss2","handwipe1","handwipe2")
run.par(vars = vars, df = article1111.2, groupvar = "condition", iters = 300)

# Comparison 6: 
# Grouping variable: control and pathogen threat (condition)
# Scale variable: Household (5 items, split in liking and purchase, so 10 in total)
# Batteries1 + batteries2 + lightbulb1 + lightbulb2 + alumfoil1 + alumfoil2 + music1 + music2 + utensils1 + utensils2
model.1111.6 <- 'F =~ batteries1 + batteries2 + lightbulb1 + lightbulb2 + alumfoil1 + alumfoil2 + music1 + music2 + utensils1 + utensils2'
conf.fit.1111.6 <- cfa(model.1111.6, data = article1111.2, group = "condition")
load.fit.1111.6 <- cfa(model.1111.6, article1111.2, group = "condition", group.equal = "loadings")
int.fit.1111.6 <- cfa(model.1111.6, article1111.2, group = "condition", group.equal = c("loadings", "intercepts"))
smc.1111.3.06 <- mi.results(conf.fit.1111.6,load.fit.1111.6,int.fit.1111.6)
smc.1111.3.06
# Chisquare significant, rmsea > .08, CFI < .95, so we reject configural invariance
vars <- c("batteries1","batteries2","lightbulb1","lightbulb2","alumfoil1","alumfoil2","music1","music2","utensils1","utensils2")
run.par(vars = vars, df = article1111.2, groupvar = "condition", iters = 300)

# Study 4
# Comparisons 7-9
url <- 'https://osf.io/k2tb6//?action=download'
filename <- 'article1111.3.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article1111.3 <- read_sav(filename)
article1111.3$cond <- as.numeric(article1111.3$cond)

# From the manuscript
# In the second task, participants rated their liking for (−5 = dislike, 5 = like) and interest in purchasing, using, or engaging in (−5 = extremely disinterested, 5 = extremely interested) 28 consumer products or activities (chosen on the basis of a pretest reported in the Supplemental Material). These items related to appearance (liposuction, cosmetic plastic surgery, diet pills, cosmetics/makeup, blemish cream, facial makeover, hair dye), hygiene (shampoo, toothbrush, mouthwash, dental floss, hand sanitizer, deodorant, razors), physical fitness (fitness equipment, working out at a gym, cardio machine, exercise, lifting weights, treadmill, exercise mat), and  the household (batteries, light bulbs, aluminum foil, plates, extension cord, lamp, clock). 

# From the supplemental material
# In the main study, liking and interest items correlated highly (all αs > .81), as they did in the pre-test, and so these were averaged for each product. We next repeated the principal axis factor analysis with promax rotation. Results mapped directly onto the pre-test findings. For the later analyses, we therefore created composites for appearance (Cronbach’s α = .83), hygiene (Cronbach’s α = .79), physical fitness (Cronbach’s α = .90) and household products (Cronbach’s α = .89) products. Additionally, two separate composites for head appearance (Cronbach’s α = .82) and body appearance (Cronbach’s α = .78) were created.

# Comparison 7
# Grouping variable: control and pathogen threat (cond)
# Scale variable: Appearance
# liposuct1 + liposuct2 + psurgery1 + psurgery2 + dietpills1 + dietpills2 + cosmetic1 + cosmetic2 + blemcream1 + blemcream2 + fmakeover1 + fmakeover2 + hairdye1 + hairdye2
model.1111.7 <- 'F =~ liposuct1 + liposuct2 + psurgery1 + psurgery2 + dietpills1 + dietpills2 + cosmetic1 + cosmetic2 + blemcream1 + blemcream2 + fmakeover1 + fmakeover2 + hairdye1 + hairdye2'
conf.fit.1111.7 <- cfa(model.1111.7, data = article1111.3, group = "cond")
load.fit.1111.7 <- cfa(model.1111.7, article1111.3, group = "cond", group.equal = "loadings")
int.fit.1111.7 <- cfa(model.1111.7, article1111.3, group = "cond", group.equal = c("loadings", "intercepts"))
smc.1111.4.07 <- mi.results(conf.fit.1111.7,load.fit.1111.7,int.fit.1111.7)
smc.1111.4.07
# Chisquare significant, rmsea > .08, CFI < .95, so we reject configural invariance
vars <- c("liposuct1","liposuct2","psurgery1","psurgery2","dietpills1","dietpills2","cosmetic1","cosmetic2","blemcream1","blemcream2","fmakeover1","fmakeover2","hairdye1","hairdye2")
run.par(vars = vars, df = article1111.3, groupvar = "cond", iters = 300)

# Comparison 8
# Grouping variable: control and pathogen threat
# Scale variable: Hygiene
# shampoo1 + shampoo2 + toothbrush1 + toothbrush2 + mouthwash1 + mouthwash2 + floss1 + floss2 + sanitize1 + sanitize2 + deodorant1 + deodorant2 + razors1 + razors2
model.1111.8 <- 'F =~ shampoo1 + shampoo2 + toothbrush1 + toothbrush2 + mouthwash1 + mouthwash2 + floss1 + floss2 + sanitize1 + sanitize2 + deodorant1 + deodorant2 + razors1 + razors2'
conf.fit.1111.8 <- cfa(model.1111.8, data = article1111.3, group = "cond")
load.fit.1111.8 <- cfa(model.1111.8, article1111.3, group = "cond", group.equal = "loadings")
int.fit.1111.8 <- cfa(model.1111.8, article1111.3, group = "cond", group.equal = c("loadings", "intercepts"))
smc.1111.4.08 <- mi.results(conf.fit.1111.8,load.fit.1111.8,int.fit.1111.8)
smc.1111.4.08
# Chisquare significant, rmsea > .08, CFI < .95, so we reject configural invariance
vars <- c("shampoo1","shampoo2","toothbrush1","toothbrush2","mouthwash1","mouthwash2","floss1","floss2","sanitize1","sanitize2","deodorant1","deodorant2","razors1","razors2")
run.par(vars = vars, df = article1111.3, groupvar = "cond", iters = 300)

# Comparison 9
# Grouping variable: control and pathogen threat (cond)
# Scale variable: Household products
# batteries1 + batteries2 + light1 + light2 + foil1 + foil2 + plates1 + plates2 + cord1 + cord2 + lamp1 + lamp2 + clock1 + clock2
model.1111.9 <- 'F =~ batteries1 + batteries2 + light1 + light2 + foil1 + foil2 + plates1 + plates2 + cord1 + cord2 + lamp1 + lamp2 + clock1 + clock2'
conf.fit.1111.9 <- cfa(model.1111.9, data = article1111.3, group = "cond")
load.fit.1111.9 <- cfa(model.1111.9, article1111.3, group = "cond", group.equal = "loadings")
int.fit.1111.9 <- cfa(model.1111.9, article1111.3, group = "cond", group.equal = c("loadings", "intercepts"))
smc.1111.4.09 <- mi.results(conf.fit.1111.9,load.fit.1111.9,int.fit.1111.9)
smc.1111.4.09
# Chisquare significant, rmsea > .08, CFI < .95, so we reject configural invariance
vars <- c("batteries1","batteries2","light1","light2","foil1","foil2","plates1","plates2","cord1","cord2","lamp1","lamp2","clock1","clock2")
run.par(vars = vars, df = article1111.3, groupvar = "cond", iters = 300)

# Study 5a
url <- 'https://osf.io/n6sua//?action=download'
filename <- 'article1111.4.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article1111.4 <- read_sav(filename)
article1111.4$condition <- as.numeric(article1111.4$condition)

# From the manuscript
# A composite for the Body-Esteem–Appearance subscale was created by reverse-scoring 
# positive items, producing a composite with higher values indicating insecurity about one’s appearance (Cronbach’s α = .93). 

# Comparison 10
# Grouping variable: pathogen-threat condition and property-crime condition (condition)
# Scale variable: body esteem appearance composites be_1R + be_2R + BE_3 + BE_4 + BE_5 + BE_6 + be_7r + BE_8 + BE_9 + be_10R
model.1111.10 <- 'F =~ be_1R + be_2R + BE_3 + BE_4 + BE_5 + BE_6 + be_7R + BE_8 + BE_9 + be_10R'
conf.fit.1111.10 <- cfa(model.1111.10, data = article1111.4, group = "condition")
load.fit.1111.10 <- cfa(model.1111.10, article1111.4, group = "condition", group.equal = "loadings")
int.fit.1111.10 <- cfa(model.1111.10, article1111.4, group = "condition", group.equal = c("loadings", "intercepts"))
smc.1111.5.10 <- mi.results(conf.fit.1111.10,load.fit.1111.10,int.fit.1111.10)
smc.1111.5.10
# Chisquare significant, rmsea > .08, CFI < .95, so we reject configural invariance
vars <- c("be_1R","be_2R","BE_3","BE_4","BE_5","BE_6","be_7R","BE_8","BE_9","be_10R")
run.par(vars = vars, df = article1111.4, groupvar = "condition", iters = 300)

# Study 5b
url <- 'https://osf.io/e6cyf//?action=download'
filename <- 'article1111.5.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article1111.5 <- read_sav(filename)
article1111.5$condition <- as.numeric(article1111.5$condition)

# Comparison 11
# Grouping variable: pathogen-threat condition and property-crime condition (condition)
# Scale variable: body esteem appearance composites be_1R + be_2R + BE_3 + BE_4 + BE_5 + BE_6 + be_7r + BE_8 + BE_9 + be_10R
model.1111.11 <- 'F =~ be_1R + be_2R + BE_3 + BE_4 + BE_5 + BE_6 + be_7R + BE_8 + BE_9 + be_10R'
conf.fit.1111.11 <- cfa(model.1111.11, data = article1111.5, group = "condition")
load.fit.1111.11 <- cfa(model.1111.11, article1111.5, group = "condition", group.equal = "loadings")
int.fit.1111.11 <- cfa(model.1111.11, article1111.5, group = "condition", group.equal = c("loadings", "intercepts"))
smc.1111.6.11 <- mi.results(conf.fit.1111.11,load.fit.1111.11,int.fit.1111.11)
smc.1111.6.11
# Chisquare significant, rmsea > .08, CFI < .95, so we reject configural invariance
vars <- c("be_1R","be_2R","BE_3","BE_4","BE_5","BE_6","be_7R","BE_8","BE_9","be_10R")
run.par(vars = vars, df = article1111.5, groupvar = "condition", iters = 300)

# Article 1151 ---------------------------------------------------------------------
url <- 'https://osf.io/7ngqz//?action=download'
filename <- 'article1151.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article1151 <- read_sav(filename)
article1151$Condition <- as.numeric(article1151$Condition)

# Grouping variable: imagined interacting either with a straight male stranger or with a gay male stranger (Condition)
# Scale variable: worry about having things in common with man CommonWor_1 + CommonWor_2 + CommonWor_3)
model.1151 <- 'F =~ CommonWor_1 + CommonWor_2 + CommonWor_3'
conf.fit.1151 <- cfa(model.1151, data = article1151, group = "Condition")
load.fit.1151 <- cfa(model.1151, article1151, group = "Condition", group.equal = "loadings")
int.fit.1151 <- cfa(model.1151, article1151, group = "Condition", group.equal = c("loadings", "intercepts"))
smc.1151.1.01 <- mi.results(conf.fit.1151,load.fit.1151,int.fit.1151)
smc.1151.1.01
# 3 items so we assume configural invariance
# Metric: nonsig chisquare change, change RMSEA and CFI < .01 (and < .08 and > .95); lower AIC and BIC; metric invariance holds
# Scalar: sig chisquare change, change RMSEA and CFI > .01; larger AIC; scalar invariance rejected (BIC ok)

# Article 1251 ---------------------------------------------------------------------
# Study 1-6
url <- 'https://osf.io/zrbq6//?action=download'
filename <- 'article1251.xlsx'
GET(url, write_disk(filename, overwrite = TRUE))
article1251 <- read_excel(filename)

# Comparison 1
# Grouping variable: advice type: approximate-chance advice vs. certain advice (Advice)
# Scale variable: Advice evaluation (Knowledgeable + Competent + Credible + Trust + SeekAdvice + Persuasive + Accurate)
# the authors seem to ignore the nesting of the data, that each individual contributes more rows of data to the study.
model.1251 <- 'F =~ Knowledgeable + Competent + Credible + Trust + SeekAdvice + Persuasive + Accurate'
conf.fit.1251 <- cfa(model.1251, data = article1251, group = "Advice")
load.fit.1251 <- cfa(model.1251, article1251, group = "Advice", group.equal = "loadings")
int.fit.1251 <- cfa(model.1251, article1251, group = "Advice", group.equal = c("loadings", "intercepts"))
smc.1251.1.01 <- mi.results(conf.fit.1251,load.fit.1251,int.fit.1251)
smc.1251.1.01
# Chisquare is significant, RMSEA > .08, so we reject configural invariance
art <- article1251[!is.na(article1251$Advice),]
vars <- c("Knowledgeable","Competent","Credible","Trust","SeekAdvice","Persuasive","Accurate")
run.par(vars = vars, df = art, groupvar = "Advice", iters = 300)

# Study 7
# Comparison 2
# Grouping variable: advice type: approximate-chance advice vs. certain advice
# Scale variable: Advice evaluation (Knowledgeable + Competent + Credible + Trust + SeekAdvice + Persuasive + Accurate)

# The OSF (https://osf.io/ew34q/) only contains data for study1-6, we can't find those for study 7.

# Article 1261 ---------------------------------------------------------------------
# Study 5
url <- 'https://osf.io/bfj8n//?action=download'
filename <- 'article1261.xlsx'
GET(url, write_disk(filename, overwrite = TRUE))
article1261.1 <- read_excel(filename, sheet = 7)

# Comparison 1
# Grouping variable: performer: present; absent (condition)
# Scale variable: perceived-skill-acquisition scale (scale1_improve + scale2_prepare + scale3_learn)
model.1261.1 <- 'F =~ scale1_improve + scale2_prepare + scale3_learn'
conf.fit.1261.1 <- cfa(model.1261.1, data = article1261.1, group = "condition")
load.fit.1261.1 <- cfa(model.1261.1, article1261.1, group = "condition", group.equal = "loadings")
int.fit.1261.1 <- cfa(model.1261.1, article1261.1, group = "condition", group.equal = c("loadings", "intercepts"))
smc.1261.5.01 <- mi.results(conf.fit.1261.1,load.fit.1261.1,int.fit.1261.1)
smc.1261.5.01
# 3 items so we assume configural invariance
# Metric: sig chisquare change, change RMSEA > .01 (and > .08); larger AIC; metric invariance rejected (BIC ok and CFI change ok and > .95)

# Comparison 2
# Grouping variable: exposure: low; high (exposures)
# Scale variable: perceived-skill-acquisition scale (scale1_improve + scale2_prepare + scale3_learn)
model.1261.2 <- 'F =~ scale1_improve + scale2_prepare + scale3_learn'
conf.fit.1261.2 <- cfa(model.1261.2, data = article1261.1, group = "exposures")
load.fit.1261.2 <- cfa(model.1261.2, article1261.1, group = "exposures", group.equal = "loadings")
int.fit.1261.2 <- cfa(model.1261.2, article1261.1, group = "exposures", group.equal = c("loadings", "intercepts"))
smc.1261.5.02 <- mi.results(conf.fit.1261.2,load.fit.1261.2,int.fit.1261.2)
smc.1261.5.02
# 3 items so we assume configural invariance
# Metric: nonsig chisquare change, change CFI < .01 (and >.95); lower AIC and BIC; metric invariance holds (RMSEA change > .01 but <.08)
# Scalar: nonsig chisquare change, change RMSEA and CFI < .01; lower BIC; scalar invariance holds (only AIC larger)

# Study 6
article1261.2 <- read_excel(filename, sheet = 8, col_names = T)
colnames(article1261.2) <- article1261.2[1,] # use first row as colnames
article1261.2 <- article1261.2[-1,] # remove first row
article1261.2$condition <- as.numeric(article1261.2$condition)

# From the manuscript
# Comparison 1
# Grouping variable: one of three kinds of debiasing task (condition)
# Scale variable: perceived-skill-acquisition scale (scale1_improve_t1 + scale2_prepare_t1 + scale3_learn_t1)

# The authors are interested in the main effect of condition, which means that they are interested
# in the difference in Y scores for condition=1-2-3 averaged across time. We will therefore also average across time for the three items
article1261.2$improve <- (as.numeric(article1261.2$scale1_improve_t1) + as.numeric(article1261.2$scale1_improve_t2))/2
article1261.2$prepare <- (as.numeric(article1261.2$scale2_prepare_t1) + as.numeric(article1261.2$scale2_prepare_t2))/2
article1261.2$learn <- (as.numeric(article1261.2$scale3_learn_t1) + as.numeric(article1261.2$scale3_learn_t2))/2

model.1261.3 <- 'F =~ improve + prepare + learn'
conf.fit.1261.3 <- cfa(model.1261.3, data = article1261.2, group = "condition")
load.fit.1261.3 <- cfa(model.1261.3, article1261.2, group = "condition", group.equal = "loadings")
int.fit.1261.3 <- cfa(model.1261.3, article1261.2, group = "condition", group.equal = c("loadings", "intercepts"))
smc.1261.6.03 <- mi.results(conf.fit.1261.3,load.fit.1261.3,int.fit.1261.3)
smc.1261.6.03
# 3 items so we assume configural invariance
# Metric: nonsig chisquare change, change CFI < .01 (and <.95); lower AIC and BIC; metric invariance holds (change RMSEA > .01 but < .08)
# Scalar: nonsig chisquare change, change CFI < .01; lower AIC and BIC; scalar invariance holds (change RMSEA > .01)

# Comparison 2
# Grouping variable: time: perceived learning at Time 1; before the debiasing task; and perceived learning at Time 2; after the debiasing task
# Scale variable: perceived-skill-acquisition scale 

# Make dataset for t1
article1261.2.t1 <- cbind(article1261.2$scale1_improve_t1,article1261.2$scale2_prepare_t1,article1261.2$scale3_learn_t1)
grouping <- rep(0,nrow(article1261.2.t1))
article1261.2.t1 <- cbind(article1261.2.t1,grouping)
# Make dataset for t2
article1261.2.t2 <- cbind(article1261.2$scale1_improve_t2,article1261.2$scale2_prepare_t2,article1261.2$scale3_learn_t2)
grouping <- rep(1,nrow(article1261.2.t2))
article1261.2.t2 <- cbind(article1261.2.t2,grouping)
# Combine datasets
article1261.2.tot <- rbind(article1261.2.t1,article1261.2.t2)
colnames(article1261.2.tot) <- c("improve","prepare","learn","condition")

model.1261.4 <- 'F =~ improve + prepare + learn'
conf.fit.1261.4 <- cfa(model.1261.4, data = article1261.2.tot, group = "condition")
load.fit.1261.4 <- cfa(model.1261.4, article1261.2.tot, group = "condition", group.equal = "loadings")
int.fit.1261.4 <- cfa(model.1261.4, article1261.2.tot, group = "condition", group.equal = c("loadings", "intercepts"))
smc.1261.6.04 <- mi.results(conf.fit.1261.4,load.fit.1261.4,int.fit.1261.4)
smc.1261.6.04
# 3 items so we assume configural invariance
# Metric: nonsig chisquare change, change CFI < .01 (and >.95); lower AIC and BIC; metric invariance holds (change RMSEA > .01 but < .08)
# Scalar: nonsig chisquare change, change RMSEA and CFI < .01; lower AIC and BIC; scalar invariance holds.

# Article 1361 ---------------------------------------------------------------------
# Study 2
url <- 'https://osf.io/xk3ny//?action=download'
filename <- 'article1361.1.csv'
GET(url, write_disk(filename, overwrite = TRUE))
article1361.1 <- read.csv2(filename, header=T, na.strings="NA", sep=",")

# Comparison 1
# Grouping variable: inspired-by; inspired-to; or a neutral control condition (condinsp)
# Scale variable: Dawkins' belief in god scale DBIG_1 + DBIG_2 + DBIG_3 + DBIG_4 + DBIG_5 + DBIG_6 + DBIG_7
model.1361.1 <- 'F =~ DBIG_1 + DBIG_2 + DBIG_3 + DBIG_4 + DBIG_5 + DBIG_6 + DBIG_7'

# Fit the configural invariance model
conf.mod.1361.1 <- measEq.syntax(model.1361.1,
                                ID.fac = "std.lv",
                                ID.cat = "Wu",
                                ordered = c("DBIG_1","DBIG_2","DBIG_3","DBIG_4","DBIG_5","DBIG_6","DBIG_7"),
                                group = "condinsp", 
                                parameterization = "delta", 
                                data = article1361.1,
                                group.equal = "configural") 

conf.fit.1361.1 <- cfa(as.character(conf.mod.1361.1), article1361.1, group = "condinsp", estimator = "WLSMV")

# Fit the thresholds invariance model
thres.mod.1361.1 <- measEq.syntax(model.1361.1,
                                 ID.fac = "std.lv",
                                 ID.cat = "Wu",
                                 ordered = c("DBIG_1","DBIG_2","DBIG_3","DBIG_4","DBIG_5","DBIG_6","DBIG_7"),
                                 group = "condinsp", 
                                 parameterization = "delta", 
                                 data = article1361.1,
                                 group.equal = c("thresholds")) 

thres.fit.1361.1 <- cfa(as.character(thres.mod.1361.1), article1361.1, group = "condinsp", estimator = "WLSMV")

# Fit the loadingsinvariance model
load.mod.1361.1 <- measEq.syntax(model.1361.1,
                                ID.fac = "std.lv",
                                ID.cat = "Wu",
                                ordered = c("DBIG_1","DBIG_2","DBIG_3","DBIG_4","DBIG_5","DBIG_6","DBIG_7"),
                                group = "condinsp", 
                                parameterization = "delta", 
                                data = article1361.1,
                                group.equal = c("thresholds", "loadings")) 

load.fit.1361.1 <- cfa(as.character(load.mod.1361.1), article1361.1, group = "condinsp", estimator = "WLSMV")
smc.1361.2.01 <- mi.results(conf.fit.1361.1,thres.fit.1361.1,load.fit.1361.1, type = "scaled")
smc.1361.2.01
# chisquare significant, RMSEA > .08, CFI < .95, so we reject configural invariance
vars <- c("DBIG_1","DBIG_2","DBIG_3","DBIG_4","DBIG_5","DBIG_6","DBIG_7")
run.par(vars = vars, df = article1361.1, groupvar = "condinsp", iters = 300)

# Study 3
# Comparisons 2-4
url <- 'https://osf.io/2n93e//?action=download'
filename <- 'article1361.2.csv'
GET(url, write_disk(filename, overwrite = TRUE))
article1361.2 <- read.csv2(filename, header=T, na.strings="NA", sep=",")
# Comparison 2
# Grouping variable: inspired-by; awe; or neutral control condition (condition)
# Scale variable: intolerance to uncertainty (AmbigIntol_1 + AmbigIntol_2 + AmbigIntol_3 + AmbigIntol_4 + AmbigIntol_5 + AmbigIntol_6 + AmbigIntol_7 + AmbigIntol_8 + AmbigIntol_9)
model.1361.2 <- 'F =~ AmbigIntol_1 + AmbigIntol_2 + AmbigIntol_3 + AmbigIntol_4 + AmbigIntol_5 + AmbigIntol_6 + AmbigIntol_7 + AmbigIntol_8 + AmbigIntol_9'
conf.fit.1361.2 <- cfa(model.1361.2, data = article1361.2, group = "condition")
load.fit.1361.2 <- cfa(model.1361.2, article1361.2, group = "condition", group.equal = "loadings")
int.fit.1361.2 <- cfa(model.1361.2, article1361.2, group = "condition", group.equal = c("loadings", "intercepts"))
smc.1361.3.02 <- mi.results(conf.fit.1361.2,load.fit.1361.2,int.fit.1361.2)
smc.1361.3.02
# chisquare significant, CFI < .95, so we reject configural invariance
vars <- c("AmbigIntol_1","AmbigIntol_2","AmbigIntol_3","AmbigIntol_4","AmbigIntol_5","AmbigIntol_6","AmbigIntol_7","AmbigIntol_8","AmbigIntol_9")
run.par(vars = vars, df = article1361.2, groupvar = "condition", iters = 300)

# Comparison 3
# Grouping variable: inspired-by; awe; or neutral control condition (condition)
# Scale variable: transcendence (Pekala1r + Pekala2 + Pekala3 + Pekala4r for cond1 and cond3, Q332 + Q333 + Q334 + Q335 for cond2)
# Make dataset for cond1 and cond3
keep1 <- c("Pekala1r","Pekala2","Pekala3","Pekala4r","condition")
cond1 <- subset(article1361.2,condition==1,select=keep1)
cond3 <- subset(article1361.2,condition==3,select=keep1)
# Make dataset for cond2
keep2 <- c("Q332","Q333","Q334","Q335","condition")
cond2 <- subset(article1361.2,condition==2,select=keep2)
colnames(cond2) <- c("Pekala1r","Pekala2","Pekala3","Pekala4r","condition")
# Combine datasets
article1361.2.2 <- rbind(cond1,cond2,cond3)

model.1361.3 <- 'F =~ Pekala1r + Pekala2 + Pekala3 + Pekala4r'
conf.fit.1361.3 <- cfa(model.1361.3, data = article1361.2.2, group = "condition")
load.fit.1361.3 <- cfa(model.1361.3, article1361.2.2, group = "condition", group.equal = "loadings")
int.fit.1361.3 <- cfa(model.1361.3, article1361.2.2, group = "condition", group.equal = c("loadings", "intercepts"))
smc.1361.3.03 <- mi.results(conf.fit.1361.3,load.fit.1361.3,int.fit.1361.3)
smc.1361.3.03
# Chisquare significant, RMSEA > 0.08, CFI < .95, so we reject configural invariance
vars <- c("Pekala1r","Pekala2","Pekala3","Pekala4r")
run.par(vars = vars, df = article1361.2.2, groupvar = "condition", iters = 300)

# Comparison 4
# Grouping variable: inspired-by; awe; or neutral control condition (condition)
# Scale variable: Connectedness (condition 1 and 3 answered "soco1W","soco2W","soco3W","soco4W" / condition 2 answered "soco1A","soco2A","soco3A","soco4A")
# Make dataset for cond1 and cond3
keep1 <- c("soco1W","soco2W","soco3W","soco4W","condition")
cond1 <- subset(article1361.2,condition==1,select=keep1)
cond3 <- subset(article1361.2,condition==3,select=keep1)
# Make dataset for cond2
keep2 <- c("soco1A","soco2A","soco3A","soco4A","condition")
cond2 <- subset(article1361.2,condition==2,select=keep2)
colnames(cond2) <- c("soco1W","soco2W","soco3W","soco4W","condition")
# Combine datasets
article1361.2.1 <- rbind(cond1,cond2,cond3)
colnames(article1361.2.1) <- c("soc1","soc2","soc3","soc4","condition")
model.1361.4 <- 'F =~ soc1 + soc2 + soc3 + soc4'
conf.fit.1361.4 <- cfa(model.1361.4, data = article1361.2.1, group = "condition")
load.fit.1361.4 <- cfa(model.1361.4, article1361.2.1, group = "condition", group.equal = "loadings")
int.fit.1361.4 <- cfa(model.1361.4, article1361.2.1, group = "condition", group.equal = c("loadings", "intercepts"))
smc.1361.3.04 <- mi.results(conf.fit.1361.4,load.fit.1361.4,int.fit.1361.4)
smc.1361.3.04
# Chisquare significant, RMSEA > 0.08, CFI < .95, so we reject configural invariance
vars <- c("soc1","soc2","soc3","soc4")
run.par(vars = vars, df = article1361.2.1, groupvar = "condition", iters = 300)

# Study 4
url <- 'https://osf.io/uwg4k//?action=download'
filename <- 'article1361.3.csv'
GET(url, write_disk(filename, overwrite = TRUE))
article1361.3 <- read.csv2(filename, header=T, na.strings="NA", sep=",")
# Comparison 5
# Grouping variable: inspiration or control (IV_video)
# Scale variable: Enjoyment; enjoyable + good + feelgood + moving + inspired + skeptical_reversed
model.1361.5 <- 'F =~ enjoyable + good + feelgood + moving + inspired + skeptical_reversed'
conf.fit.1361.5 <- cfa(model.1361.5, data = article1361.3, group = "IV_video")
load.fit.1361.5 <- cfa(model.1361.5, article1361.3, group = "IV_video", group.equal = "loadings")
int.fit.1361.5 <- cfa(model.1361.5, article1361.3, group = "IV_video", group.equal = c("loadings", "intercepts"))
smc.1361.4.05 <- mi.results(conf.fit.1361.5,load.fit.1361.5,int.fit.1361.5)
smc.1361.4.05
# Chisquare significant, RMSEA > 0.08, CFI < .95, so we reject configural invariance
vars <- c("enjoyable","good","feelgood","moving","inspired","skeptical_reversed")
#run.par(vars = vars, df = article1361.3, groupvar = "IV_video", iters = 300)
# Error: An ultra-Heywood case was detected. Examine the results carefully

# Article 1391 ---------------------------------------------------------------------
# Study 4
url <- 'https://osf.io/tucvj///?action=download'
filename <- 'article1391.xlsx'
GET(url, write_disk(filename, overwrite = TRUE))
article1391 <- read_excel(filename)

# Comparison 1
# Grouping variable: simple elaborate and complex elaborate (condition)
# Scale variable: Fluency (fluent_easy + fluent_quick + fluent_enjoy)
model.1391.1 <- 'F =~ fluent_easy + fluent_quick + fluent_enjoy'
conf.fit.1391.1 <- cfa(model.1391.1, data = article1391, group = "condition")
load.fit.1391.1 <- cfa(model.1391.1, article1391, group = "condition", group.equal = "loadings")
int.fit.1391.1 <- cfa(model.1391.1, article1391, group = "condition", group.equal = c("loadings", "intercepts"))
smc.1391.4.01 <- mi.results(conf.fit.1391.1,load.fit.1391.1,int.fit.1391.1)
smc.1391.4.01
# 3 items so we assume configural invariance
# Metric: nonsig chisquare change, change CFI < .01; lower BIC; 
# change RMSEA > .01 (but still < .08), AIC larger. Chisquare change is nonsignificant so we cannot reject metric invariance; metric invariance holds 
# Scalar: nonsig chisquare change, change RMSEA and CFI < .01; lower BIC; scalar invariance holds (AIC is larger)

# Comparison 2
# Grouping variable: within-group 10 times (day)
# Scale variable: Fluency (fluent_easy + fluent_quick + fluent_enjoy)
model.1391.2 <- 'F =~ fluent_easy + fluent_quick + fluent_enjoy'
conf.fit.1391.2 <- cfa(model.1391.2, data = article1391, group = "day")
load.fit.1391.2 <- cfa(model.1391.2, article1391, group = "day", group.equal = "loadings")
int.fit.1391.2 <- cfa(model.1391.2, article1391, group = "day", group.equal = c("loadings", "intercepts"))
smc.1391.4.02 <- mi.results(conf.fit.1391.2,load.fit.1391.2,int.fit.1391.2)
smc.1391.4.02
# 3 items so we assume configural invariance
# Metric: nonsig chisquare change, change RMSEA and CFI < .01; lower AIC and BIC; metric invariance holds
# Scalar: nonsig chisquare change, change CFI < .01; lower AIC and BIC; scalar invariance holds (change RMSEA > .01 but still < .08)

# Comparison 3
# Grouping variable: simple elaborate and complex elaborate (condition)
# Scale variable: Flow (flow1 + flow2 + flow3 + flow4 + flow5 + flow6 + flow7 + flow8 + flow9 + flow10)
model.1391.3 <- 'F =~ flow1 + flow2 + flow3 + flow4 + flow5 + flow6 + flow7 + flow8 + flow9 + flow10'
conf.fit.1391.3 <- cfa(model.1391.3, data = article1391, group = "condition")
load.fit.1391.3 <- cfa(model.1391.3, article1391, group = "condition", group.equal = "loadings")
int.fit.1391.3 <- cfa(model.1391.3, article1391, group = "condition", group.equal = c("loadings", "intercepts"))
smc.1391.4.03 <- mi.results(conf.fit.1391.3,load.fit.1391.3,int.fit.1391.3)
smc.1391.4.03
# Chisquare significant, rmsea > .08, CFI <.95 so we reject configural invariance
vars <- c("flow1","flow2","flow3","flow4","flow5","flow6","flow7","flow8","flow9","flow10")
run.par(vars = vars, df = article1391, groupvar = "condition", iters = 300)

# Comparison 4
# Grouping variable: within-group 10 times (day)
# Scale variable: Flow (flow1 + flow2 + flow3 + flow4 + flow5 + flow6 + flow7 + flow8 + flow9 + flow10)
model.1391.4 <- 'F =~ flow1 + flow2 + flow3 + flow4 + flow5 + flow6 + flow7 + flow8 + flow9 + flow10'
conf.fit.1391.4 <- cfa(model.1391.4, data = article1391, group = "day")
load.fit.1391.4 <- cfa(model.1391.4, article1391, group = "day", group.equal = "loadings")
int.fit.1391.4 <- cfa(model.1391.4, article1391, group = "day", group.equal = c("loadings", "intercepts"))
smc.1391.4.04 <- mi.results(conf.fit.1391.4,load.fit.1391.4,int.fit.1391.4)
smc.1391.4.04
# Chisquare significant, rmsea > .08, CFI <.95 so we reject configural invariance
vars <- c("flow1","flow2","flow3","flow4","flow5","flow6","flow7","flow8","flow9","flow10")
run.par(vars = vars, df = article1391, groupvar = "day", iters = 300)

# Article 1421 ---------------------------------------------------------------------
# Study 3
url <- 'https://osf.io/bw72f//?action=download'
filename <- 'article1421.1.xlsx'
GET(url, write_disk(filename, overwrite = TRUE))
article1421.1 <- read_excel(filename)

# Comparison 1
# Grouping variable: charity condition or investment condition (Type)
# Scaling variable: Subjective Preference (PersonallyCare + ReflectViews + ObjectiveMoreImportant)
model.1421.1 <- 'F =~ PersonallyCare + ReflectViews + ObjectiveMoreImportant'
conf.fit.1421.1 <- cfa(model.1421.1, data = article1421.1, group = "Type")
load.fit.1421.1 <- cfa(model.1421.1, article1421.1, group = "Type", group.equal = "loadings")
int.fit.1421.1 <- cfa(model.1421.1, article1421.1, group = "Type", group.equal = c("loadings", "intercepts"))
smc.1421.3.01 <- mi.results(conf.fit.1421.1,load.fit.1421.1,int.fit.1421.1)
smc.1421.3.01
# 3 items so we assume configural invariance
# Metric: nonsig chisquare change, change RMSEA and CFI < .01 (and < .08 and > .95); lower AIC and BIC; metric invariance holds
# Scalar: sig chisquare change, change RMSEA and CFI > .01; larger AIC; scalar invariance rejected (BIC ok)

# Study 5
url <- 'https://osf.io/fmbng//?action=download'
filename <- 'article1421.2.xlsx'
GET(url, write_disk(filename, overwrite = TRUE))
article1421.2 <- read_excel(filename, col_names=T)

# Comparison 2
# Grouping variable: choice: most effective option vs. least effective option (Choice)
# Scaling variable: perceived decision quality (Appropriat + Responsibl + Thoughtful)
model.1421.2 <- 'F =~ Appropriat + Responsibl + Thoughtful'
conf.fit.1421.2 <- cfa(model.1421.2, data = article1421.2, group = "Choice")
load.fit.1421.2 <- cfa(model.1421.2, article1421.2, group = "Choice", group.equal = "loadings")
int.fit.1421.2 <- cfa(model.1421.2, article1421.2, group = "Choice", group.equal = c("loadings", "intercepts"))
smc.1421.5.02 <- mi.results(conf.fit.1421.2,load.fit.1421.2,int.fit.1421.2)
smc.1421.5.02
# 3 items so we assume configural invariance
# Metric: nonsig chisquare change, change RMSEA and CFI < .01 (and < .08 and >.95); lower AIC and BIC; metric invariance holds
# Scalar: nonsig chisquare change, change RMSEA and CFI < .01; lower AIC and BIC; scalar invariance holds.

# Comparison 3
# Grouping variable: choice: most effective option vs. least effective choice (Choice)
# Scaling variable: perceived altruism (Selfish + Ethical + Good)
model.1421.3 <- 'F =~ Selfish + Ethical + Good'
conf.fit.1421.3 <- cfa(model.1421.3, data = article1421.2, group = "Choice")
load.fit.1421.3 <- cfa(model.1421.3, article1421.2, group = "Choice", group.equal = "loadings")
int.fit.1421.3 <- cfa(model.1421.3, article1421.2, group = "Choice", group.equal = c("loadings", "intercepts"))
smc.1421.5.03 <- mi.results(conf.fit.1421.3,load.fit.1421.3,int.fit.1421.3)
smc.1421.5.03
# 3 items so we assume configural invariance
# Metric: nonsig chisquare change, change CFI < .01 (RMSEA < .08 and CFI > .95); lower AIC and BIC; metric invariance holds 
# Scalar: sig chisquare change, change RMSEA and CFI > .01; larger AIC; scalar invariance rejected (BIC ok)

# Article 1501 ---------------------------------------------------------------------
# Study 1
# Comparisons 1-3
url <- 'https://osf.io/t3rhv//?action=download'
filename <- 'article1501.1.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article1501.1 <- read_sav(filename)
article1501.1$targrel <- as.numeric(article1501.1$targrel)

# Comparison 1
# Grouping variable: religious target and nonreligious target (targrel)
# Scale variable: Reproductive strategy (Faithful + Family + multsex + LongTerm + promisc + InvCh + DedParent)
# Got the correct variables from the syntax: https://osf.io/wt8jc/
model.1501.1 <- 'F =~ Faithful + Family + multsex + LongTerm + promisc + InvCh + DedParent'
conf.fit.1501.1 <- cfa(model.1501.1, data = article1501.1, group = "targrel")
load.fit.1501.1 <- cfa(model.1501.1, article1501.1, group = "targrel", group.equal = "loadings")
int.fit.1501.1 <- cfa(model.1501.1, article1501.1, group = "targrel", group.equal = c("loadings", "intercepts"))
smc.1501.1.01 <- mi.results(conf.fit.1501.1,load.fit.1501.1,int.fit.1501.1)
smc.1501.1.01
# Chisquare is significant, RMSEA > .08, CFI < .95, so we reject configural invariance
vars <- c("Faithful","Family","multsex","LongTerm","promisc","InvCh","DedParent")
run.par(vars = vars, df = article1501.1, groupvar = "targrel", iters = 300)

# Comparison 2
# Grouping variable: religious target and nonreligious target (targrel)
# Scale variable: Non-aggression (aggressive + violent + temper)
model.1501.2 <- 'F =~ aggressive + violent + temper'
conf.fit.1501.2 <- cfa(model.1501.2, data = article1501.1, group = "targrel")
load.fit.1501.2 <- cfa(model.1501.2, article1501.1, group = "targrel", group.equal = "loadings")
int.fit.1501.2 <- cfa(model.1501.2, article1501.1, group = "targrel", group.equal = c("loadings", "intercepts"))
smc.1501.1.02 <- mi.results(conf.fit.1501.2,load.fit.1501.2,int.fit.1501.2)
smc.1501.1.02
# 3 items so we assume configural invariance
# Metric: nonsig chisquare change, change CFI < .01; lower AIC and BIC; metric invariance holds (RMSEA change > .01 but still < .08)
# Scalar: sig chisquare change, change RMSEA > .01; larger AIC; scalar invariance rejected (CFI BIC ok)

# Comparison 3
# Grouping variable: religious target and nonreligious target
# Scale variable: Mate value (MV1_1 + MV1_2 + MV1_3 + MV1_6 + MV1_7)
model.1501.3 <- 'F =~ MV1_1 + MV1_2 + MV1_3 + MV1_6 + MV1_7'
conf.fit.1501.3 <- cfa(model.1501.3, data = article1501.1, group = "targrel")
load.fit.1501.3 <- cfa(model.1501.3, article1501.1, group = "targrel", group.equal = "loadings")
int.fit.1501.3 <- cfa(model.1501.3, article1501.1, group = "targrel", group.equal = c("loadings", "intercepts"))
smc.1501.1.03 <- mi.results(conf.fit.1501.3,load.fit.1501.3,int.fit.1501.3)
smc.1501.1.03
# Chisquare is significant, RMSEA > .08, so we reject configural invariance
vars <- c("MV1_1","MV1_2","MV1_3","MV1_6","MV1_7")
run.par(vars = vars, df = article1501.1, groupvar = "targrel", iters = 300)

# Study 2
# Comparisons 4-12
url <- 'https://osf.io/8f7wa//?action=download'
filename <- 'article1501.2.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article1501.2 <- read_sav(filename)
article1501.2$targrel <- as.numeric(article1501.2$targrel)
article1501.2$targstrat <- as.numeric(article1501.2$targstrat)
# Syntax here: https://osf.io/b9pnm/

# Comparison 4
# Grouping variable: religious target and nonreligious target (targrel)
# Scale variable: reproductive strategy (CS1 + CS2 + CS3 + CS4 + CS5 + CS6 + CS7)
model.1501.4 <- 'F =~ CS1 + CS2 + CS3 + CS4 + CS5 + CS6 + CS7'
conf.fit.1501.4 <- cfa(model.1501.4, data = article1501.2, group = "targrel")
load.fit.1501.4 <- cfa(model.1501.4, article1501.2, group = "targrel", group.equal = "loadings")
int.fit.1501.4 <- cfa(model.1501.4, article1501.2, group = "targrel", group.equal = c("loadings", "intercepts"))
smc.1501.2.04 <- mi.results(conf.fit.1501.4,load.fit.1501.4,int.fit.1501.4)
smc.1501.2.04
# Chisquare is significant, rmsea > .08 CFI < .95, so we reject configural invariance
vars <- c("CS1","CS2","CS3","CS4","CS5","CS6","CS7")
run.par(vars = vars, df = article1501.2, groupvar = "targrel", iters = 300)

# Comparison 5
# Grouping variable: committed strategies vs noncommitted strategies (targstrat)
# Scale variable: reproductive strategy (CS1 + CS2 + CS3 + CS4 + CS5 + CS6 + CS7)
model.1501.5 <- 'F =~ CS1 + CS2 + CS3 + CS4 + CS5 + CS6 + CS7'
conf.fit.1501.5 <- cfa(model.1501.5, data = article1501.2, group = "targstrat")
load.fit.1501.5 <- cfa(model.1501.5, article1501.2, group = "targstrat", group.equal = "loadings")
int.fit.1501.5 <- cfa(model.1501.5, article1501.2, group = "targstrat", group.equal = c("loadings", "intercepts"))
smc.1501.2.05 <- mi.results(conf.fit.1501.5,load.fit.1501.5,int.fit.1501.5)
smc.1501.2.05
# Chisquare is significant, rmsea > .08 CFI < .95, so we reject configural invariance
vars <- c("CS1","CS2","CS3","CS4","CS5","CS6","CS7")
run.par(vars = vars, df = article1501.2, groupvar = "targstrat", iters = 300)

# Comparison 6
# Grouping variable: committed strategies vs noncommitted strategies (targstrat)
# Scale variable: nonimpulsivity (IMP1 + IMP2 + IMP3 + IMP4 + IMP5 + IMP6 + IMP7)
model.1501.6 <- 'F =~ IMP1 + IMP2 + IMP3 + IMP4 + IMP5 + IMP6 + IMP7'
conf.fit.1501.6 <- cfa(model.1501.6, data = article1501.2, group = "targstrat")
load.fit.1501.6 <- cfa(model.1501.6, article1501.2, group = "targstrat", group.equal = "loadings")
int.fit.1501.6 <- cfa(model.1501.6, article1501.2, group = "targstrat", group.equal = c("loadings", "intercepts"))
smc.1501.2.06 <- mi.results(conf.fit.1501.6,load.fit.1501.6,int.fit.1501.6)
smc.1501.2.06
# Chisquare is significant, rmsea > .08 CFI < .95, so we reject configural invariance
vars <- c("IMP1","IMP2","IMP3","IMP4","IMP5","IMP6","IMP7")
run.par(vars = vars, df = article1501.2, groupvar = "targstrat", iters = 300)

# Comparison 7
# Grouping variable: religious target and nonreligious target (targrel)
# Scale variable: nonopportunistic behavior (OB1 + OB2 + OB3)
model.1501.7 <- 'F =~ OB1 + OB2 + OB3'
conf.fit.1501.7 <- cfa(model.1501.7, data = article1501.2, group = "targrel")
load.fit.1501.7 <- cfa(model.1501.7, article1501.2, group = "targrel", group.equal = "loadings", std.lv=TRUE)
int.fit.1501.7 <- cfa(model.1501.7, article1501.2, group = "targrel", group.equal = c("loadings", "intercepts"), std.lv=TRUE)
smc.1501.2.07 <- mi.results(conf.fit.1501.7,load.fit.1501.7,int.fit.1501.7)
smc.1501.2.07
# 3 items so we assume configural invariance
# Metric: nonsig chisquare change, change RMSEA and CFI < .01 (and < 08 and >.95); lower AIC and BIC; metric invariance holds
# Scalar: sig chisquare change, change RMSEA > .01; larger AIC; scalar invariance rejected (CFI BIC ok)

# Comparison 8
# Grouping variable: committed strategies vs noncommitted strategies (targstrat)
# Scale variable: nonopportunistic behavior (OB1 + OB2 + OB3)
model.1501.8 <- 'F =~ OB1 + OB2 + OB3'
conf.fit.1501.8 <- cfa(model.1501.8, data = article1501.2, group = "targstrat")
load.fit.1501.8 <- cfa(model.1501.8, article1501.2, group = "targstrat", group.equal = "loadings", std.lv=TRUE)
int.fit.1501.8 <- cfa(model.1501.8, article1501.2, group = "targstrat", group.equal = c("loadings", "intercepts"), std.lv=TRUE)
smc.1501.2.08 <- mi.results(conf.fit.1501.8,load.fit.1501.8,int.fit.1501.8)
smc.1501.2.08
# 3 items so we assume configural invariance
# metric: sig chisquare change, change RMSEA > .01 (and > .08); larger AIC; metric invariance rejected (CFI BIC ok)

# Comparison 9
# Grouping variable: religious target and nonreligious target (targrel)
# Scale variable: hopeful ecology (ECOL1 + ECOL2 + ECOL3)
model.1501.9 <- 'F =~ ECOL1 + ECOL2 + ECOL3'
conf.fit.1501.9 <- cfa(model.1501.9, data = article1501.2, group = "targrel")
load.fit.1501.9 <- cfa(model.1501.9, article1501.2, group = "targrel", group.equal = "loadings")
int.fit.1501.9 <- cfa(model.1501.9, article1501.2, group = "targrel", group.equal = c("loadings", "intercepts"))
smc.1501.2.09 <- mi.results(conf.fit.1501.9,load.fit.1501.9,int.fit.1501.9)
smc.1501.2.09
# 3 items so we assume configural invariance
# Metric: nonsig chisquare change, change CFI < .01 (and >.95); lower BIC;
# But: larger AIC, RMSEA change > .01 (and > .08); chisquare nonsignificant so we cannot reject metric invariance; metric invariance holds
# scalar: nonsig chisquare change, change CFI < .01; lower AIC and BIC; scalar invariance holds (change RMSEA > .01 but fit improves)

# Comparison 10
# Grouping variable: committed strategies vs noncommitted strategies (targstrat)
# Scale variable: hopeful ecology (ECOL1 + ECOL2 + ECOL3)
model.1501.10 <- 'F =~ ECOL1 + ECOL2 + ECOL3'
conf.fit.1501.10 <- cfa(model.1501.10, data = article1501.2, group = "targstrat")
load.fit.1501.10 <- cfa(model.1501.10, article1501.2, group = "targstrat", group.equal = "loadings")
int.fit.1501.10 <- cfa(model.1501.10, article1501.2, group = "targstrat", group.equal = c("loadings", "intercepts"))
smc.1501.2.10 <- mi.results(conf.fit.1501.10,load.fit.1501.10,int.fit.1501.10)
smc.1501.2.10
# 3 items so we assume configural invariance
# Metric: nonsig chisquare change, change RMSEA and CFI < .01 (and < .08 and > .95); lower AIC and BIC; metric invariance holds
# Scalar: sig chisquare change, change RMSEA and CFI > .01; larger AIC and BIC; scalar invariance rejected

# Comparison 11
# Grouping variable: religious target and nonreligious target (targrel)
# Scale variable: trust (TR1 + TR2 + TR3 + TR4 + TR5 + TR6)
model.1501.11 <- 'F =~ TR1 + TR2 + TR3 + TR4 + TR5 + TR6'
conf.fit.1501.11 <- cfa(model.1501.11, data = article1501.2, group = "targrel")
load.fit.1501.11 <- cfa(model.1501.11, article1501.2, group = "targrel", group.equal = "loadings")
int.fit.1501.11 <- cfa(model.1501.11, article1501.2, group = "targrel", group.equal = c("loadings", "intercepts"))
smc.1501.2.11 <- mi.results(conf.fit.1501.11,load.fit.1501.11,int.fit.1501.11)
smc.1501.2.11
# Chisquare non-significant, RMSEA < .08, CFI > .95 so we do not reject configural invariance
# Metric: nonsig chisquare change, change in RMSEA < .01 (and < .08), change in CFI < .01 (and >.95), AIC and BIC are lower, we do not reject metric invariance
# Scalar: nonsig chisquare change, change in RMSEA < .01, change in CFI < .01, AIC and BIC are lower, we do not reject scalar invariance

# Comparison 12
# Grouping variable: committed strategies vs noncommitted strategies (targstrat)
# Scale variable: trust (TR1 + TR2 + TR3 + TR4 + TR5 + TR6)
model.1501.12 <- 'F =~ TR1 + TR2 + TR3 + TR4 + TR5 + TR6'
conf.fit.1501.12 <- cfa(model.1501.12, data = article1501.2, group = "targstrat")
load.fit.1501.12 <- cfa(model.1501.12, article1501.2, group = "targstrat", group.equal = "loadings")
int.fit.1501.12 <- cfa(model.1501.12, article1501.2, group = "targstrat", group.equal = c("loadings", "intercepts"))
smc.1501.2.12 <- mi.results(conf.fit.1501.12,load.fit.1501.12,int.fit.1501.12)
smc.1501.2.12
# Chisquare non-significant, RMSEA < .08, CFI > .95 so we do not reject configural invariance
# Metric: nonsig chisquare change, change in RMSEA < .01 (and < .08), change in CFI < .01 (and > .95), AIC and BIC are lower, we do not reject metric invariance
# Scalar: nonsig chisquare change, change in RMSEA < .01, change in CFI < .01, AIC and BIC are lower, we do not reject scalar invariance

# Study 3
# Comparisons 13-18
url <- 'https://osf.io/uh2ce//?action=download'
filename <- 'article1501.3.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article1501.3 <- read_sav(filename)
article1501.3$targrel <- as.numeric(article1501.3$Targrel)

# Comparison 13
# Grouping variable: religious target and nonreligious target (targrel)
# Scale variable: Reproductive strategy (CommStr_1 + CommStr_2 + CommStr_3 + CommStr_4 + CommStr_5 + CommStr_6 + CommStr_7)
model.1501.13 <- 'F =~ CommStr_1 + CommStr_2 + CommStr_3 + CommStr_4 + CommStr_5 + CommStr_6 + CommStr_7'
conf.fit.1501.13 <- cfa(model.1501.13, data = article1501.3, group = "targrel")
load.fit.1501.13 <- cfa(model.1501.13, article1501.3, group = "targrel", group.equal = "loadings")
int.fit.1501.13 <- cfa(model.1501.13, article1501.3, group = "targrel", group.equal = c("loadings", "intercepts"))
smc.1501.3.13 <- mi.results(conf.fit.1501.13,load.fit.1501.13,int.fit.1501.13)
smc.1501.3.13
# Chisquare significant, RMSEA > .08, CFI < .95, so we reject configural invariance
vars <- c("CommStr_1","CommStr_2","CommStr_3","CommStr_4","CommStr_5","CommStr_6","CommStr_7")
run.par(vars = vars, df = article1501.3, groupvar = "targrel", iters = 300)

# Comparison 14
# Grouping variable: religious target and nonreligious target (targrel)
# Scale variable: Nonimpulsivity (Impulsive_1 + Impulsive_2 + Impulsive_3 + Impulsive_4 + Impulsive_5 + Impulsive_6 + Impulsive_7 + Impulsive_8)
# The syntax includes Impulsive_5 for this scale, but every participant answered 6 on that question because it was an attention check. As such, we have no variance on Impulsive_5 and cannot include it. As it is not an item that actually adds information to the scale, we decided to estimate the model without that variable. 
model.1501.14 <- 'F =~ Impulsive_1 + Impulsive_2 + Impulsive_3 + Impulsive_4 + Impulsive_6 + Impulsive_7 + Impulsive_8'
conf.fit.1501.14 <- cfa(model.1501.14, data = article1501.3, group = "targrel")
load.fit.1501.14 <- cfa(model.1501.14, article1501.3, group = "targrel", group.equal = "loadings")
int.fit.1501.14 <- cfa(model.1501.14, article1501.3, group = "targrel", group.equal = c("loadings", "intercepts"))
smc.1501.3.14 <- mi.results(conf.fit.1501.14,load.fit.1501.14,int.fit.1501.14)
smc.1501.3.14
# Chisquare significant, RMSEA > .08, CFI < .95, so we reject configural invariance
vars <- c("Impulsive_1","Impulsive_2","Impulsive_3","Impulsive_4","Impulsive_6","Impulsive_7","Impulsive_8")
run.par(vars = vars, df = article1501.3, groupvar = "targrel", iters = 300)

# Comparison 15
# Grouping variable: religious target and nonreligious target (targrel)
# Scale variable: Nonopportunistic behaviour (Opportunistic_1 + Opportunistic_2 + Opportunistic_3)
model.1501.15 <- 'F =~ Opportunistic_1 + Opportunistic_2 + Opportunistic_3'
conf.fit.1501.15 <- cfa(model.1501.15, data = article1501.3, group = "targrel")
load.fit.1501.15 <- cfa(model.1501.15, article1501.3, group = "targrel", group.equal = "loadings")
int.fit.1501.15 <- cfa(model.1501.15, article1501.3, group = "targrel", group.equal = c("loadings", "intercepts"))
smc.1501.3.15 <- mi.results(conf.fit.1501.15,load.fit.1501.15,int.fit.1501.15)
smc.1501.3.15
# 3 items so we assume configural invariance
# Metric: sig chisquare change, change RMSEA > .01 (and > .08); larger AIC; metric invariance rejected (CFI change and BIC ok)

# Comparison 16
# Grouping variable: religious target and nonreligious target (targrel)
# Scale variable: Hopeful ecology (Ecology_1 + Ecology_2 + Ecology_3)
model.1501.16 <- 'F =~ Ecology_1 + Ecology_2 + Ecology_3'
conf.fit.1501.16 <- cfa(model.1501.16, data = article1501.3, group = "targrel")
load.fit.1501.16 <- cfa(model.1501.16, article1501.3, group = "targrel", group.equal = "loadings")
int.fit.1501.16 <- cfa(model.1501.16, article1501.3, group = "targrel", group.equal = c("loadings", "intercepts"))
smc.1501.3.16 <- mi.results(conf.fit.1501.16,load.fit.1501.16,int.fit.1501.16)
smc.1501.3.16
# 3 items so we assume configural invariance
# Metric: nonsig chisquare change, change RMSEA and CFI < .01 (and < .08 and > .95); lower AIC and BIC; metric invariance holds
# Scalar: nonsig chisquare change, change CFI < .01; lower AIC and BIC; scalar invariance holds (change RMSEA > .01)

# Comparison 17
# Grouping variable: religious target and nonreligious target (targrel)
# Scale variable: General Trust (Trust_1 + Trust_2 + Trust_3 + Trust_4 + Trust_5 + Trust_6 + Trust_7 + Trust_8 + Trust_9 + Trust_10)
model.1501.17 <- 'F =~ Trust_1 + Trust_2 + Trust_3 + Trust_4 + Trust_5 + Trust_6 + Trust_7 + Trust_8 + Trust_9 + Trust_10'
conf.fit.1501.17 <- cfa(model.1501.17, data = article1501.3, group = "targrel")
load.fit.1501.17 <- cfa(model.1501.17, article1501.3, group = "targrel", group.equal = "loadings")
int.fit.1501.17 <- cfa(model.1501.17, article1501.3, group = "targrel", group.equal = c("loadings", "intercepts"))
smc.1501.3.17 <- mi.results(conf.fit.1501.17,load.fit.1501.17,int.fit.1501.17)
smc.1501.3.17
# Chisquare significant, RMSEA > .08 so we reject configural invariance
vars <- c("Trust_1","Trust_2","Trust_3","Trust_4","Trust_5","Trust_6","Trust_7","Trust_8","Trust_9","Trust_10")
run.par(vars = vars, df = article1501.3, groupvar = "targrel", iters = 300)

# Comparison 18
# Grouping variable: religious target and nonreligious target (targrel)
# Scale variable: Accountancy Trust (AccTrust_11 + AccTrust_12 + AccTrust_13 + AccTrust_14 + AccTrust_15)
model.1501.18 <- 'F =~ AccTrust_11 + AccTrust_12 + AccTrust_13 + AccTrust_14 + AccTrust_15'
conf.fit.1501.18 <- cfa(model.1501.18, data = article1501.3, group = "targrel")
load.fit.1501.18 <- cfa(model.1501.18, article1501.3, group = "targrel", group.equal = "loadings")
int.fit.1501.18 <- cfa(model.1501.18, article1501.3, group = "targrel", group.equal = c("loadings", "intercepts"))
smc.1501.3.18 <- mi.results(conf.fit.1501.18,load.fit.1501.18,int.fit.1501.18)
smc.1501.3.18
# Chisquare significant, RMSEA > .08, CFI < .95, so we reject configural invariance
vars <- c("AccTrust_11","AccTrust_12","AccTrust_13","AccTrust_14","AccTrust_15")
run.par(vars = vars, df = article1501.3, groupvar = "targrel", iters = 300)

# Article 1681 ---------------------------------------------------------------------
# Study 1a (comparison 1-4)
url <- 'https://osf.io/k948v//?action=download'
filename <- 'article1681.1.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article1681.1 <- read_sav(filename)
# 1 = ethical
# 2 = unethical
# 3 = neutral
article1681.1$Condition <- as.numeric(article1681.1$Condition)

# Comparison 1
# Grouping variable: unethical v. ethical v. neutral
# Scale variable: Mind-Attribution scale - Self-dehumanization (DV_1 + DV_2 + DV_3 + DV_4 + DV_5 + DV_6 + DV_7 + DV_8 + DV_9 + DV_10)
model.1681.1 <- 'F =~ DV_1 + DV_2 + DV_3 + DV_4 + DV_5 + DV_6 + DV_7 + DV_8 + DV_9 + DV_10'
conf.fit.1681.1 <- cfa(model.1681.1, data = article1681.1, group = "Condition")
load.fit.1681.1 <- cfa(model.1681.1, article1681.1, group = "Condition", group.equal = "loadings")
int.fit.1681.1 <- cfa(model.1681.1, article1681.1, group = "Condition", group.equal = c("loadings", "intercepts"))
smc.1681.1.01 <- mi.results(conf.fit.1681.1,load.fit.1681.1,int.fit.1681.1)
smc.1681.1.01
# Chisquare significant, RMSEA > .08, CFI < .95, so we reject configural invariance
vars <- c("DV_1","DV_2","DV_3","DV_4","DV_5","DV_6","DV_7","DV_8","DV_9","DV_10")
run.par(vars = vars, df = article1681.1, groupvar = "Condition", iters = 300)

# Comparison 2
# Grouping variable: unethical v. neutral
art <- article1681.1[article1681.1$Condition != 1,]
# Scale variable: Mind-Attribution scale - Self-dehumanization (DV_1 + DV_2 + DV_3 + DV_4 + DV_5 + DV_6 + DV_7 + DV_8 + DV_9 + DV_10)
conf.fit.1681.1 <- cfa(model.1681.1, data = art, group = "Condition")
load.fit.1681.1 <- cfa(model.1681.1, art, group = "Condition", group.equal = "loadings")
int.fit.1681.1 <- cfa(model.1681.1, art, group = "Condition", group.equal = c("loadings", "intercepts"))
smc.1681.1.02 <- mi.results(conf.fit.1681.1,load.fit.1681.1,int.fit.1681.1)
smc.1681.1.02
# Chisquare significant, RMSEA > .08, CFI < .95, so we reject configural invariance
vars <- c("DV_1","DV_2","DV_3","DV_4","DV_5","DV_6","DV_7","DV_8","DV_9","DV_10")
run.par(vars = vars, df = art, groupvar = "Condition", iters = 300)

# Comparison 3
# Grouping variable: unethical v. ethical
art <- article1681.1[article1681.1$Condition != 3,]
# Scale variable: Mind-Attribution scale - Self-dehumanization (DV_1 + DV_2 + DV_3 + DV_4 + DV_5 + DV_6 + DV_7 + DV_8 + DV_9 + DV_10)
conf.fit.1681.1 <- cfa(model.1681.1, data = art, group = "Condition")
load.fit.1681.1 <- cfa(model.1681.1, art, group = "Condition", group.equal = "loadings")
int.fit.1681.1 <- cfa(model.1681.1, art, group = "Condition", group.equal = c("loadings", "intercepts"))
smc.1681.1.03 <- mi.results(conf.fit.1681.1,load.fit.1681.1,int.fit.1681.1)
smc.1681.1.03
# Chisquare significant, RMSEA > .08, CFI < .95, so we reject configural invariance
vars <- c("DV_1","DV_2","DV_3","DV_4","DV_5","DV_6","DV_7","DV_8","DV_9","DV_10")
run.par(vars = vars, df = art, groupvar = "Condition", iters = 300)

# Comparison 4
# Grouping variable: ethical v. neutral
art <- article1681.1[article1681.1$Condition != 2,]
# Scale variable: Mind-Attribution scale - Self-dehumanization (DV_1 + DV_2 + DV_3 + DV_4 + DV_5 + DV_6 + DV_7 + DV_8 + DV_9 + DV_10)
conf.fit.1681.1 <- cfa(model.1681.1, data = art, group = "Condition")
load.fit.1681.1 <- cfa(model.1681.1, art, group = "Condition", group.equal = "loadings")
int.fit.1681.1 <- cfa(model.1681.1, art, group = "Condition", group.equal = c("loadings", "intercepts"))
smc.1681.1.04 <- mi.results(conf.fit.1681.1,load.fit.1681.1,int.fit.1681.1)
smc.1681.1.04
# Chisquare significant, RMSEA > .08, CFI < .95, so we reject configural invariance
vars <- c("DV_1","DV_2","DV_3","DV_4","DV_5","DV_6","DV_7","DV_8","DV_9","DV_10")
run.par(vars = vars, df = art, groupvar = "Condition", iters = 300)

# Study 1b (comparisons 5-8)
url <- 'https://osf.io/9ejm6//?action=download'
filename <- 'article1681.2.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article1681.2 <- read_sav(filename)
# 1 = ethical
# 2 = unethical
# 3 = neutral
article1681.2$Condition <- as.numeric(article1681.2$Condition)

# Comparison 5
# Grouping variable: neutral v. lying v. honesty
# Scale variable: Mind-Attribution scale - Self-dehumanization (DV_1 + DV_2 + DV_3 + DV_4 + DV_5 + DV_6 + DV_7 + DV_8 + DV_9 + DV_10)
model.1681.2 <- 'F =~ DV_1 + DV_2 + DV_3 + DV_4 + DV_5 + DV_6 + DV_7 + DV_8 + DV_9 + DV_10'
conf.fit.1681.2 <- cfa(model.1681.2, data = article1681.2, group = "Condition")
load.fit.1681.2 <- cfa(model.1681.2, article1681.2, group = "Condition", group.equal = "loadings")
int.fit.1681.2 <- cfa(model.1681.2, article1681.2, group = "Condition", group.equal = c("loadings", "intercepts"))
smc.1681.2.05 <- mi.results(conf.fit.1681.2,load.fit.1681.2,int.fit.1681.2)
smc.1681.2.05
# Chisquare significant, RMSEA > .08, CFI < .95, so we reject configural invariance
vars <- c("DV_1","DV_2","DV_3","DV_4","DV_5","DV_6","DV_7","DV_8","DV_9","DV_10")
run.par(vars = vars, df = article1681.2, groupvar = "Condition", iters = 300)

# Comparison 6
# Grouping variable: neutral v. lying
art <- article1681.2[article1681.2$Condition != 1,]
# Scale variable: Mind-Attribution scale - Self-dehumanization (DV_1 + DV_2 + DV_3 + DV_4 + DV_5 + DV_6 + DV_7 + DV_8 + DV_9 + DV_10)
conf.fit.1681.1 <- cfa(model.1681.1, data = art, group = "Condition")
load.fit.1681.1 <- cfa(model.1681.1, art, group = "Condition", group.equal = "loadings")
int.fit.1681.1 <- cfa(model.1681.1, art, group = "Condition", group.equal = c("loadings", "intercepts"))
smc.1681.2.06 <- mi.results(conf.fit.1681.1,load.fit.1681.1,int.fit.1681.1)
smc.1681.2.06
# Chisquare significant, RMSEA > .08, CFI < .95, so we reject configural invariance
vars <- c("DV_1","DV_2","DV_3","DV_4","DV_5","DV_6","DV_7","DV_8","DV_9","DV_10")
run.par(vars = vars, df = art, groupvar = "Condition", iters = 300)

# Comparison 7
# Grouping variable:neutral v. honesty
art <- article1681.2[article1681.2$Condition != 2,]
# Scale variable: Mind-Attribution scale - Self-dehumanization (DV_1 + DV_2 + DV_3 + DV_4 + DV_5 + DV_6 + DV_7 + DV_8 + DV_9 + DV_10)
conf.fit.1681.1 <- cfa(model.1681.1, data = art, group = "Condition")
load.fit.1681.1 <- cfa(model.1681.1, art, group = "Condition", group.equal = "loadings")
int.fit.1681.1 <- cfa(model.1681.1, art, group = "Condition", group.equal = c("loadings", "intercepts"))
smc.1681.2.07 <- mi.results(conf.fit.1681.1,load.fit.1681.1,int.fit.1681.1)
smc.1681.2.07
# Chisquare significant, RMSEA > .08, CFI < .95, so we reject configural invariance
vars <- c("DV_1","DV_2","DV_3","DV_4","DV_5","DV_6","DV_7","DV_8","DV_9","DV_10")
run.par(vars = vars, df = art, groupvar = "Condition", iters = 300)

# Comparison 8
# Grouping variable: lying v. honesty
art <- article1681.2[article1681.2$Condition != 3,]
# Scale variable: Mind-Attribution scale - Self-dehumanization (DV_1 + DV_2 + DV_3 + DV_4 + DV_5 + DV_6 + DV_7 + DV_8 + DV_9 + DV_10)
conf.fit.1681.1 <- cfa(model.1681.1, data = art, group = "Condition")
load.fit.1681.1 <- cfa(model.1681.1, art, group = "Condition", group.equal = "loadings")
int.fit.1681.1 <- cfa(model.1681.1, art, group = "Condition", group.equal = c("loadings", "intercepts"))
smc.1681.2.08 <- mi.results(conf.fit.1681.1,load.fit.1681.1,int.fit.1681.1)
smc.1681.2.08
# Chisquare significant, RMSEA > .08, CFI < .95, so we reject configural invariance
vars <- c("DV_1","DV_2","DV_3","DV_4","DV_5","DV_6","DV_7","DV_8","DV_9","DV_10")
run.par(vars = vars, df = art, groupvar = "Condition", iters = 300)

# Study 1c (Comparisons 9-12)
url <- 'https://osf.io/ys6h8//?action=download'
filename <- 'article1681.3.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article1681.3 <- read_sav(filename)
# 1 neutral
# 2 ethical
# 3 unethical
article1681.3$Cond <- as.numeric(article1681.3$Cond)

# Comparison 9
# Grouping variable: neutral v. unethical v. ethical
# Scale variable: Mind-Attribution scale - Self-dehumanization (DV_1 + DV_2 + DV_3 + DV_4 + DV_5 + DV_6 + DV_7 + DV_8 + DV_9 + DV_10)
model.1681.3 <- 'F =~ DV_1 + DV_2 + DV_3 + DV_4 + DV_5 + DV_6 + DV_7 + DV_8 + DV_9 + DV_10'
conf.fit.1681.3 <- cfa(model.1681.3, data = article1681.3, group = "Cond")
load.fit.1681.3 <- cfa(model.1681.3, article1681.3, group = "Cond", group.equal = "loadings")
int.fit.1681.3 <- cfa(model.1681.3, article1681.3, group = "Cond", group.equal = c("loadings", "intercepts"))
smc.1681.3.09 <- mi.results(conf.fit.1681.3,load.fit.1681.3,int.fit.1681.3)
smc.1681.3.09
# Chisquare significant, RMSEA > .08, CFI < .95, so we reject configural invariance
vars <- c("DV_1","DV_2","DV_3","DV_4","DV_5","DV_6","DV_7","DV_8","DV_9","DV_10")
run.par(vars = vars, df = article1681.3, groupvar = "Cond", iters = 300)

# Comparison 10
# Grouping variable: unethical v. ethical
art <- article1681.3[article1681.3$Cond != 0,]
# Scale variable: Mind-Attribution scale - Self-dehumanization (DV_1 + DV_2 + DV_3 + DV_4 + DV_5 + DV_6 + DV_7 + DV_8 + DV_9 + DV_10)
conf.fit.1681.1 <- cfa(model.1681.1, data = art, group = "Cond")
load.fit.1681.1 <- cfa(model.1681.1, art, group = "Cond", group.equal = "loadings")
int.fit.1681.1 <- cfa(model.1681.1, art, group = "Cond", group.equal = c("loadings", "intercepts"))
smc.1681.3.10 <- mi.results(conf.fit.1681.1,load.fit.1681.1,int.fit.1681.1)
smc.1681.3.10
# Chisquare significant, RMSEA > .08, CFI < .95, so we reject configural invariance
vars <- c("DV_1","DV_2","DV_3","DV_4","DV_5","DV_6","DV_7","DV_8","DV_9","DV_10")
run.par(vars = vars, df = art, groupvar = "Cond", iters = 300)

# Comparison 11
# Grouping variable: unethical v. neutral
art <- article1681.3[article1681.3$Cond != 1,]
# Scale variable: Mind-Attribution scale - Self-dehumanization (DV_1 + DV_2 + DV_3 + DV_4 + DV_5 + DV_6 + DV_7 + DV_8 + DV_9 + DV_10)
conf.fit.1681.1 <- cfa(model.1681.1, data = art, group = "Cond")
load.fit.1681.1 <- cfa(model.1681.1, art, group = "Cond", group.equal = "loadings")
int.fit.1681.1 <- cfa(model.1681.1, art, group = "Cond", group.equal = c("loadings", "intercepts"))
smc.1681.3.11 <- mi.results(conf.fit.1681.1,load.fit.1681.1,int.fit.1681.1)
smc.1681.3.11
# Chisquare significant, RMSEA > .08, CFI < .95, so we reject configural invariance
vars <- c("DV_1","DV_2","DV_3","DV_4","DV_5","DV_6","DV_7","DV_8","DV_9","DV_10")
run.par(vars = vars, df = art, groupvar = "Cond", iters = 300)

# Comparison 12
# Grouping variable: ethical v. neutral
art <- article1681.3[article1681.3$Cond != 2,]
# Scale variable: Mind-Attribution scale - Self-dehumanization (DV_1 + DV_2 + DV_3 + DV_4 + DV_5 + DV_6 + DV_7 + DV_8 + DV_9 + DV_10)
conf.fit.1681.1 <- cfa(model.1681.1, data = art, group = "Cond")
load.fit.1681.1 <- cfa(model.1681.1, art, group = "Cond", group.equal = "loadings")
int.fit.1681.1 <- cfa(model.1681.1, art, group = "Cond", group.equal = c("loadings", "intercepts"))
smc.1681.3.12 <- mi.results(conf.fit.1681.1,load.fit.1681.1,int.fit.1681.1)
smc.1681.3.12
# Chisquare significant, RMSEA > .08, CFI < .95, so we reject configural invariance
vars <- c("DV_1","DV_2","DV_3","DV_4","DV_5","DV_6","DV_7","DV_8","DV_9","DV_10")
run.par(vars = vars, df = art, groupvar = "Cond", iters = 300)

# Study 1d (Comparisons 13-16)
url <- 'https://osf.io/tnjz8//?action=download'
filename <- 'article1681.4.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article1681.4 <- read_sav(filename)
# 1 ethical
# 2 unethical
# 3 neutral
article1681.4$Condition <- as.numeric(article1681.4$Condition)

# Comparison 13
# Grouping variable: negative v. unethical v. ethical
# Scale variable: Mind-Attribution scale - Self-dehumanization (DV_1 + DV_2 + DV_3 + DV_4 + DV_5 + DV_6 + DV_7 + DV_8 + DV_9 + DV_10)
model.1681.4 <- 'F =~ DV_1 + DV_2 + DV_3 + DV_4 + DV_5 + DV_6 + DV_7 + DV_8 + DV_9 + DV_10'
conf.fit.1681.4 <- cfa(model.1681.4, data = article1681.4, group = "Condition")
load.fit.1681.4 <- cfa(model.1681.4, article1681.4, group = "Condition", group.equal = "loadings")
int.fit.1681.4 <- cfa(model.1681.4, article1681.4, group = "Condition", group.equal = c("loadings", "intercepts"))
smc.1681.4.13 <- mi.results(conf.fit.1681.4,load.fit.1681.4,int.fit.1681.4)
smc.1681.4.13
# Chisquare significant, RMSEA > .08, CFI < .95, so we reject configural invariance
vars <- c("DV_1","DV_2","DV_3","DV_4","DV_5","DV_6","DV_7","DV_8","DV_9","DV_10")
run.par(vars = vars, df = article1681.4, groupvar = "Condition", iters = 300)

# Comparison 14
# Grouping variable: unethical v. ethical
art <- article1681.4[article1681.4$Condition != 3,]
# Scale variable: Mind-Attribution scale - Self-dehumanization (DV_1 + DV_2 + DV_3 + DV_4 + DV_5 + DV_6 + DV_7 + DV_8 + DV_9 + DV_10)
conf.fit.1681.1 <- cfa(model.1681.4, data = art, group = "Condition")
load.fit.1681.1 <- cfa(model.1681.4, art, group = "Condition", group.equal = "loadings")
int.fit.1681.1 <- cfa(model.1681.4, art, group = "Condition", group.equal = c("loadings", "intercepts"))
smc.1681.4.14 <- mi.results(conf.fit.1681.1,load.fit.1681.1,int.fit.1681.1)
smc.1681.4.14
# Chisquare significant, RMSEA > .08, CFI < .95, so we reject configural invariance
vars <- c("DV_1","DV_2","DV_3","DV_4","DV_5","DV_6","DV_7","DV_8","DV_9","DV_10")
run.par(vars = vars, df = art, groupvar = "Condition", iters = 300)

# Comparison 15
# Grouping variable: unethical v. negative
art <- article1681.4[article1681.4$Condition != 1,]
# Scale variable: Mind-Attribution scale - Self-dehumanization (DV_1 + DV_2 + DV_3 + DV_4 + DV_5 + DV_6 + DV_7 + DV_8 + DV_9 + DV_10)
conf.fit.1681.1 <- cfa(model.1681.4, data = art, group = "Condition")
load.fit.1681.1 <- cfa(model.1681.4, art, group = "Condition", group.equal = "loadings")
int.fit.1681.1 <- cfa(model.1681.4, art, group = "Condition", group.equal = c("loadings", "intercepts"))
smc.1681.4.15 <- mi.results(conf.fit.1681.1,load.fit.1681.1,int.fit.1681.1)
smc.1681.4.15
# Chisquare significant, RMSEA > .08, CFI < .95, so we reject configural invariance
vars <- c("DV_1","DV_2","DV_3","DV_4","DV_5","DV_6","DV_7","DV_8","DV_9","DV_10")
run.par(vars = vars, df = art, groupvar = "Condition", iters = 300)

# Comparison 16
# Grouping variable: ethical v. negative
art <- article1681.4[article1681.4$Condition != 2,]
# Scale variable: Mind-Attribution scale - Self-dehumanization (DV_1 + DV_2 + DV_3 + DV_4 + DV_5 + DV_6 + DV_7 + DV_8 + DV_9 + DV_10)
conf.fit.1681.1 <- cfa(model.1681.4, data = art, group = "Condition")
load.fit.1681.1 <- cfa(model.1681.4, art, group = "Condition", group.equal = "loadings")
int.fit.1681.1 <- cfa(model.1681.4, art, group = "Condition", group.equal = c("loadings", "intercepts"))
smc.1681.4.16 <- mi.results(conf.fit.1681.1,load.fit.1681.1,int.fit.1681.1)
smc.1681.4.16
# Chisquare significant, RMSEA > .08, CFI < .95, so we reject configural invariance
vars <- c("DV_1","DV_2","DV_3","DV_4","DV_5","DV_6","DV_7","DV_8","DV_9","DV_10")
run.par(vars = vars, df = art, groupvar = "Condition", iters = 300)

# Study 2b
url <- 'https://osf.io/j2uwq//?action=download'
filename <- 'article1681.5.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article1681.5 <- read_sav(filename)
article1681.5$cond <- as.numeric(article1681.5$cond)

# Comparison 5
# Grouping variable: neutral condition and self-dehumanized condition (cond)
# Scale variable: Mind-Attribution scale - Self-dehumanization (MC_1 + MC_2 + MC_3 + MC_4 + MC_5 + MC_6 + MC_7 + MC_8 + MC_9 + MC_10)
model.1681.5 <- 'F =~ MC_1 + MC_2 + MC_3 + MC_4 + MC_5 + MC_6 + MC_7 + MC_8 + MC_9 + MC_10'
conf.fit.1681.5 <- cfa(model.1681.5, data = article1681.5, group = "cond")
load.fit.1681.5 <- cfa(model.1681.5, article1681.5, group = "cond", group.equal = "loadings")
int.fit.1681.5 <- cfa(model.1681.5, article1681.5, group = "cond", group.equal = c("loadings", "intercepts"))
smc.1681.5.17 <- mi.results(conf.fit.1681.5,load.fit.1681.5,int.fit.1681.5)
smc.1681.5.17
# Chisquare significant, RMSEA > .08, CFI < .95, so we reject configural invariance
vars <- c("MC_1","MC_2","MC_3","MC_4","MC_5","MC_6","MC_7","MC_8","MC_9","MC_10")
run.par(vars = vars, df = article1681.5, groupvar = "cond", iters = 300)

# Article 1721 ---------------------------------------------------------------------
# Study 1
# Comparison 1-3
# Supplemental material includes code and a csv file that is uploaded (https://journals.sagepub.com/doi/suppl/10.1177/0956797618764621)
article1721.1 <- read.csv(url("https://madata.bib.uni-mannheim.de/266/1/yoga.csv"), header = T, sep = ",")

# Comparison 1
# Grouping variable: yoga condition and control condition (cond)
# Scale variable: self-centrality (imp01 + imp02 + imp03 + imp04)
model.1721.1 <- 'F =~ imp01 + imp02 + imp03 + imp04'
conf.fit.1721.1 <- cfa(model.1721.1, data = article1721.1, group = "cond")
load.fit.1721.1 <- cfa(model.1721.1, article1721.1, group = "cond", group.equal = "loadings")
int.fit.1721.1 <- cfa(model.1721.1, article1721.1, group = "cond", group.equal = c("loadings", "intercepts"))
smc.1721.1.01 <- mi.results(conf.fit.1721.1,load.fit.1721.1,int.fit.1721.1)
smc.1721.1.01
# Chisquare significant, RMSEA> .08, so we reject configural invariance
vars <- c("imp01","imp02","imp03","imp04")
run.par(vars = vars, df = article1721.1, groupvar = "cond", iters = 300)

# Comparison 2
# Grouping variable: yoga condition and control condition (cond)
# Scale variable: self-enhancement: better than average (bta01 + bta02 + bta03 + bta04)
model.1721.2 <- 'F =~ bta01 + bta02 + bta03 + bta04'
conf.fit.1721.2 <- cfa(model.1721.2, data = article1721.1, group = "cond", std.lv=TRUE)
load.fit.1721.2 <- cfa(model.1721.2, article1721.1, group = "cond", group.equal = "loadings", std.lv=TRUE)
int.fit.1721.2 <- cfa(model.1721.2, article1721.1, group = "cond", group.equal = c("loadings", "intercepts"), std.lv=TRUE)
smc.1721.1.02 <- mi.results(conf.fit.1721.2,load.fit.1721.2,int.fit.1721.2)
smc.1721.1.02
# Chisquare significant, RMSEA> .08, so we reject configural invariance
vars <- c("bta01","bta02","bta03","bta04")
run.par(vars = vars, df = article1721.1, groupvar = "cond", iters = 300)

# Comparison 3
# Grouping variable: yoga condition and control condition (cond)
# Scale variable: self-enhancement: communal narcissism (cni01 + cni02 + cni03 + cni04)
model.1721.3 <- 'F =~ cni01 + cni02 + cni03 + cni04'
conf.fit.1721.3 <- cfa(model.1721.3, data = article1721.1, group = "cond")
load.fit.1721.3 <- cfa(model.1721.3, article1721.1, group = "cond", group.equal = "loadings")
int.fit.1721.3 <- cfa(model.1721.3, article1721.1, group = "cond", group.equal = c("loadings", "intercepts"))
smc.1721.1.03 <- mi.results(conf.fit.1721.3,load.fit.1721.3,int.fit.1721.3)
smc.1721.1.03
# Chisquare significant, RMSEA> .08, so we reject configural invariance
vars <- c("cni01","cni02","cni03","cni04")
run.par(vars = vars, df = article1721.1, groupvar = "cond", iters = 300)

# Study 2
# Comparison 4-9
# Supplemental material includes code and a csv file that is uploaded (https://journals.sagepub.com/doi/suppl/10.1177/0956797618764621)
article1721.2 <- read.csv(url("https://madata.bib.uni-mannheim.de/266/2/meditation.csv"), header = T, sep = ",")

# Comparison 4
# Grouping variable: control condition and meditation condition (cond)
# Scale variable: self-centrality (imp01 + imp02 +imp03 +imp04 +imp05 +imp06 +imp07 +imp08 +imp09 +imp10)
model.1721.4 <- 'F =~ imp01 + imp02 +imp03 +imp04 +imp05 +imp06 +imp07 +imp08 +imp09 +imp10'
conf.fit.1721.4 <- cfa(model.1721.4, data = article1721.2, group = "cond")
load.fit.1721.4 <- cfa(model.1721.4, article1721.2, group = "cond", group.equal = "loadings")
int.fit.1721.4 <- cfa(model.1721.4, article1721.2, group = "cond", group.equal = c("loadings", "intercepts"))
smc.1721.2.04 <- mi.results(conf.fit.1721.4,load.fit.1721.4,int.fit.1721.4)
smc.1721.2.04
# Chisquare significant, RMSEA> .08, CFI < .95 so we reject configural invariance
vars <- c("imp01","imp02","imp03","imp04","imp05","imp06","imp07","imp08","imp09","imp10")
run.par(vars = vars, df = article1721.2, groupvar = "cond", iters = 300)

# Comparison 5
# Grouping variable: control condition and meditation condition (cond)
# Scale variable: self-enhancement: better than average (bta01 + bta02 + bta03 + bta04 + bta05 + bta06 + bta07 + bta08 + bta09 + bta10)
model.1721.5 <- 'F =~ bta01 + bta02 + bta03 + bta04 + bta05 + bta06 + bta07 + bta08 + bta09 + bta10'
conf.fit.1721.5 <- cfa(model.1721.5, data = article1721.2, group = "cond")
load.fit.1721.5 <- cfa(model.1721.5, article1721.2, group = "cond", group.equal = "loadings")
int.fit.1721.5 <- cfa(model.1721.5, article1721.2, group = "cond", group.equal = c("loadings", "intercepts"))
smc.1721.2.05 <- mi.results(conf.fit.1721.5,load.fit.1721.5,int.fit.1721.5)
smc.1721.2.05
# Chisquare significant, RMSEA> .08, CFI < .95 so we reject configural invariance
vars <- c("bta01","bta02","bta03","bta04","bta05","bta06","bta07","bta08","bta09","bta10")
run.par(vars = vars, df = article1721.2, groupvar = "cond", iters = 300)

# Comparison 6
# Grouping variable: control condition and meditation condition (cond)
# Scale variable: self-enhancement: communal narcissism (cni01 + cni02 + cni03 + cni04 + cni05 + cni06 + cni07 + cni08 + cni09 + cni10 + cni11 + cni12 + cni13 + cni14 + cni15 + cni16)
model.1721.6 <- 'F =~ cni01 + cni02 + cni03 + cni04 + cni05 + cni06 + cni07 + cni08 + cni09 + cni10 + cni11 + cni12 + cni13 + cni14 + cni15 + cni16'
conf.fit.1721.6 <- cfa(model.1721.6, data = article1721.2, group = "cond")
load.fit.1721.6 <- cfa(model.1721.6, article1721.2, group = "cond", group.equal = "loadings")
int.fit.1721.6 <- cfa(model.1721.6, article1721.2, group = "cond", group.equal = c("loadings", "intercepts"))
smc.1721.2.06 <- mi.results(conf.fit.1721.6,load.fit.1721.6,int.fit.1721.6)
smc.1721.2.06
# Chisquare significant, RMSEA> .08, CFI < .95 so we reject configural invariance
vars <- c("cni01","cni02","cni03","cni04","cni05","cni06","cni07","cni08","cni09","cni10","cni11","cni12","cni13","cni14","cni15","cni16")
run.par(vars = vars, df = article1721.2, groupvar = "cond", iters = 300)

# Comparison 7
# Grouping variable: control condition and meditation condition (cond)
# Scale variable: self-esteem (rse01 + rse02r + rse03 + rse04 + rse05r + rse06r + rse07 + rse08r + rse09r + rse10)
model.1721.7 <- 'F =~ rse01 + rse02r + rse03 + rse04 + rse05r + rse06r + rse07 + rse08r + rse09r + rse10'
conf.fit.1721.7 <- cfa(model.1721.7, data = article1721.2, group = "cond")
load.fit.1721.7 <- cfa(model.1721.7, article1721.2, group = "cond", group.equal = "loadings")
int.fit.1721.7 <- cfa(model.1721.7, article1721.2, group = "cond", group.equal = c("loadings", "intercepts"))
smc.1721.2.07 <- mi.results(conf.fit.1721.7,load.fit.1721.7,int.fit.1721.7)
smc.1721.2.07
# Chisquare significant, RMSEA> .08, CFI < .95 so we reject configural invariance
vars <- c("rse01","rse02r","rse03","rse04","rse05r","rse06r","rse07","rse08r","rse09r","rse10")
run.par(vars = vars, df = article1721.2, groupvar = "cond", iters = 300)

# Comparison 8
# Grouping variable: control condition and meditation condition (cond)
# Scale variable: well-being: hedonic (aff01 + aff02 + aff03 + aff04 + aff05r + aff06r + aff07r + aff08r + aff09r)
model.1721.8 <- 'F =~ aff01 + aff02 + aff03 + aff04 + aff05r + aff06r + aff07r + aff08r + aff09r'
conf.fit.1721.8 <- cfa(model.1721.8, data = article1721.2, group = "cond")
load.fit.1721.8 <- cfa(model.1721.8, article1721.2, group = "cond", group.equal = "loadings")
int.fit.1721.8 <- cfa(model.1721.8, article1721.2, group = "cond", group.equal = c("loadings", "intercepts"))
smc.1721.2.08 <- mi.results(conf.fit.1721.8,load.fit.1721.8,int.fit.1721.8)
smc.1721.2.08
# Chisquare significant, RMSEA> .08, CFI < .95 so we reject configural invariance
vars <- c("aff01","aff02","aff03","aff04","aff05r","aff06r","aff07r","aff08r","aff09r")
run.par(vars = vars, df = article1721.2, groupvar = "cond", iters = 300)

# Comparison 9
# Grouping variable: control condition and meditation condition (cond)
# Scale variable: well-being: eudemonic (eud01 + eud02r + eud03r + eud04 + eud05 + eud06r + eud07r + eud08 + eud09 + eud10r + eud011 + eud12r)
model.1721.9 <- 'F =~ eud01 + eud02r + eud03r + eud04 + eud05 + eud06r + eud07r + eud08 + eud09 + eud10r + eud011 + eud12r'
conf.fit.1721.9 <- cfa(model.1721.9, data = article1721.2, group = "cond")
load.fit.1721.9 <- cfa(model.1721.9, article1721.2, group = "cond", group.equal = "loadings")
int.fit.1721.9 <- cfa(model.1721.9, article1721.2, group = "cond", group.equal = c("loadings", "intercepts"))
smc.1721.2.09 <- mi.results(conf.fit.1721.9,load.fit.1721.9,int.fit.1721.9)
smc.1721.2.09
# Chisquare significant, RMSEA> .08, CFI < .95 so we reject configural invariance
vars <- c("eud01","eud02r","eud03r","eud04","eud05","eud06r","eud07r","eud08","eud09","eud10r","eud011","eud12r")
run.par(vars = vars, df = article1721.2, groupvar = "cond", iters = 300)

# Article 1861 ---------------------------------------------------------------------
# Study 1
url <- 'https://osf.io/tc7ys//?action=download'
filename <- 'article1861.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article1861 <- read_sav(filename)
# Syntax here: https://osf.io/khbvp/

# Remove rows that do not have the grouping variable 
article1861$cgender <- as.numeric(article1861$cgender)
article1861_collapsed <- article1861[which(!is.na(article1861$cgender)),]

# Comparison 1
# Grouping variable: gender (boy and girl): cgender
# Scale variable: communal orientation (comm1 + comm2 + comm3 + comm4)

# Create model 
model1861.1 <- 'F =~ comm1 + comm2 + comm3 + comm4'

# Note that since in 1 group (cgender = 1) there is a 0 cell problem 
# Non-observed score for the variables comm1 with value 5 --> we collapse value 5 into value 4 in both groups
article1861_collapsed["comm1"][article1861_collapsed["comm1"] == 5] <- 4
# Non-observed score for the variables comm3 with value 4&5 --> we collapse value 4&5 into value 3 in both groups
article1861_collapsed["comm3"][article1861_collapsed["comm3"] == 5] <- 3
article1861_collapsed["comm3"][article1861_collapsed["comm3"] == 4] <- 3
# Non-observed score for the variables comm4 with value 5 --> we collapse value 5 into value 4 in both groups
article1861_collapsed["comm4"][article1861_collapsed["comm4"] == 5] <- 4

# Fit the configural invariance model
conf.mod.1861.1 <- measEq.syntax(model1861.1,
                                ID.fac = "std.lv",
                                ID.cat = "Wu",
                                ordered = c("comm1", "comm2", "comm3", "comm4"),
                                group = "cgender", 
                                parameterization = "delta", 
                                data = article1861_collapsed,
                                group.equal = "configural") 
conf.fit.1861.1 <- cfa(as.character(conf.mod.1861.1), article1861_collapsed, group = "cgender", estimator = "WLSMV")

# Fit the thresholds invariance model
thres.mod.1861.1 <- measEq.syntax(model1861.1,
                                 ID.fac = "std.lv",
                                 ID.cat = "Wu",
                                 ordered = c("comm1", "comm2", "comm3", "comm4"),
                                 group = "cgender", 
                                 parameterization = "delta", 
                                 data = article1861_collapsed,
                                 group.equal = c("thresholds")) 
thres.fit.1861.1 <- cfa(as.character(thres.mod.1861.1), article1861_collapsed, group = "cgender", estimator = "WLSMV")

# Fit the loadingsinvariance model
load.mod.1861.1 <- measEq.syntax(model1861.1,
                                ID.fac = "std.lv",
                                ID.cat = "Wu",
                                ordered = c("comm1", "comm2", "comm3", "comm4"),
                                group = "cgender", 
                                parameterization = "delta", 
                                data = article1861_collapsed,
                                group.equal = c("thresholds", "loadings")) 
load.fit.1861.1 <- cfa(as.character(load.mod.1861.1), article1861_collapsed, group = "cgender", estimator = "WLSMV")

smc.1861.1.01 <- mi.results(conf.fit.1861.1,thres.fit.1861.1,load.fit.1861.1, type = "scaled")
smc.1861.1.01
# Step 4
# Non-significant chisquare, RMSEA < .08 and CFI > .95 so configural invariance holds
# Metric: RMSEA change is 0, CFI change is 0, AIC and BIC are not estimated due to not estimating a loglikelihood. Metric invariance holds
# Scalar: RMSEA change is 0, CFI change is 0, AIC and BIC are not estimated due to not estimating a loglikelihood. Scalar invariance holds

# Comparison 2
# Grouping variable: gender (boy and girl): cgender
# Scale variable: agentic value (agen1 + agen3 + agen4)
# The authors mention that they exclude agen2 from analyses

# Create model 
model1861.2 <- 'F =~ agen1 + agen3 + agen4'

# Fit the configural invariance model
conf.mod.1861.2 <- measEq.syntax(model1861.2,
                                 ID.fac = "std.lv",
                                 ID.cat = "Wu",
                                 ordered = c("agen1", "agen3", "agen4"),
                                 group = "cgender", 
                                 parameterization = "delta", 
                                 data = article1861_collapsed,
                                 group.equal = "configural") 
conf.fit.1861.2 <- cfa(as.character(conf.mod.1861.2), article1861_collapsed, group = "cgender", estimator = "WLSMV")

# Fit the thresholds invariance model
thres.mod.1861.2 <- measEq.syntax(model1861.2,
                                  ID.fac = "std.lv",
                                  ID.cat = "Wu",
                                  ordered = c("agen1", "agen3", "agen4"),
                                  group = "cgender", 
                                  parameterization = "delta", 
                                  data = article1861_collapsed,
                                  group.equal = c("thresholds")) 
thres.fit.1861.2 <- cfa(as.character(thres.mod.1861.2), article1861_collapsed, group = "cgender", estimator = "WLSMV")

# Fit the loadingsinvariance model
load.mod.1861.2 <- measEq.syntax(model1861.2,
                                 ID.fac = "std.lv",
                                 ID.cat = "Wu",
                                 ordered = c("agen1", "agen3", "agen4"),
                                 group = "cgender", 
                                 parameterization = "delta", 
                                 data = article1861_collapsed,
                                 group.equal = c("thresholds", "loadings")) 
load.fit.1861.2 <- cfa(as.character(load.mod.1861.2), article1861_collapsed, group = "cgender", estimator = "WLSMV")

smc.1861.1.02 <- mi.results(conf.fit.1861.2,thres.fit.1861.2,load.fit.1861.2, type = "scaled")
smc.1861.1.02

# Step 4
# 3 items so we assume configural invariance
# Metric: nonsig chisquare change, change CFI < .01 (and >.95), AIC and BIC not estimated. Thresholds invariance holds
# Scalar: nonsig chisquare change, change CFI < .01, change RMSEA > .01 (but fit better), AIC and BIC not estimated. Threshold + loading invariance holds.

# Comparison 3
# Grouping variable: gender (boy and girl): cgender
# Scale variable: Explicit gender identification (ident1_bl + ident3_b + ident2_REV + Idnet4_REV)
# Create model 
model1861.3 <- 'F =~ ident1_bl + ident3_bl + ident2_REV + Idnet4_REV'

# Note that since in 1 group (cgender = 1) there is a 0 cell problem 
# non-observed score for the variables ident3_bl and Idnet4_REV with value 1 --> we collapse value 1 into value 2 in both groups
article1861_collapsed["ident3_bl"][article1861_collapsed["ident3_bl"] == 1] <- 2
article1861_collapsed["Idnet4_REV"][article1861_collapsed["Idnet4_REV"] == 1] <- 2

# Fit the configural invariance model
conf.mod.1861.3 <- measEq.syntax(model1861.3,
                                 ID.fac = "std.lv",
                                 ID.cat = "Wu",
                                 ordered = c("ident1_bl", "ident3_bl", "ident2_REV", "Idnet4_REV"),
                                 group = "cgender", 
                                 parameterization = "delta", 
                                 data = article1861_collapsed,
                                 group.equal = "configural") 
conf.fit.1861.3 <- cfa(as.character(conf.mod.1861.3), article1861_collapsed, group = "cgender", estimator = "WLSMV")

# Fit the thresholds invariance model
thres.mod.1861.3 <- measEq.syntax(model1861.3,
                                  ID.fac = "std.lv",
                                  ID.cat = "Wu",
                                  ordered = c("ident1_bl", "ident3_bl", "ident2_REV", "Idnet4_REV"),
                                  group = "cgender",  
                                  parameterization = "delta", 
                                  data = article1861_collapsed,
                                  group.equal = c("thresholds")) 
thres.fit.1861.3 <- cfa(as.character(thres.mod.1861.3), article1861_collapsed, group = "cgender", estimator = "WLSMV")

# Fit the loadingsinvariance model
load.mod.1861.3 <- measEq.syntax(model1861.3,
                                 ID.fac = "std.lv",
                                 ID.cat = "Wu",
                                 ordered = c("ident1_bl", "ident3_bl", "ident2_REV", "Idnet4_REV"),
                                 group = "cgender", 
                                 parameterization = "delta", 
                                 data = article1861_collapsed,
                                 group.equal = c("thresholds", "loadings")) 
load.fit.1861.3 <- cfa(as.character(load.mod.1861.3), article1861_collapsed, group = "cgender", estimator = "WLSMV")

smc.1861.1.03 <- mi.results(conf.fit.1861.3,thres.fit.1861.3,load.fit.1861.3, type = "scaled")
smc.1861.1.03
# Chisquare significant, RMSEA > .08, so we reject configural invariance
vars <- c("ident1_bl", "ident3_bl", "ident2_REV", "Idnet4_REV")
run.par(vars = vars, df = article1861_collapsed, groupvar = "cgender", iters = 300)

# Article 1931 ---------------------------------------------------------------------
# Study 1
# Comparisons 1-2
url <- 'https://osf.io/sfq6a//?action=download'
filename <- 'article1931.1.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article1931.1 <- read_sav(filename)
article1931.1$ViralOutrage <- as.numeric(article1931.1$ViralOutrage)
article1931.1$TargetCommenter <- as.numeric(article1931.1$TargetCommenter)
# Syntax https://osf.io/g2348/ 

# Comparison 1
# Grouping variable: viral vs non-viral outrage (ViralOutrage)
# Scale variable: Evaluation of target / Impression of commenter (Neg1 + Neg2 + Pos1 + Pos2)
model1931.1 <- 'F=~ Neg1 + Neg2 + Pos1 + Pos2' 

conf.fit.1931.1 <- cfa(model1931.1, data = article1931.1, group = "ViralOutrage")
load.fit.1931.1 <- cfa(model1931.1, article1931.1, group = "ViralOutrage", group.equal = "loadings")
int.fit.1931.1 <- cfa(model1931.1, article1931.1, group = "ViralOutrage", group.equal = c("loadings", "intercepts"))
smc.1931.1.01 <- mi.results(conf.fit.1931.1,load.fit.1931.1,int.fit.1931.1)
smc.1931.1.01
# Chisquare significant, RMSEA > .08, CFI < .95 so we reject configural invariance
vars <- c("Neg1","Neg2","Pos1","Pos2")
run.par(vars = vars, df = article1931.1, groupvar = "ViralOutrage", iters = 300)

# Comparison 2
# Grouping variable: target commenter: first or last (TargetCommenter)
# Scale variable: Evaluation of target / Impression of commenter (Neg1 + Neg2 + Pos1 + Pos2)
conf.fit.1931.2 <- cfa(model1931.1, data = article1931.1, group = "TargetCommenter")
load.fit.1931.2 <- cfa(model1931.1, article1931.1, group = "TargetCommenter", group.equal = "loadings")
int.fit.1931.2 <- cfa(model1931.1, article1931.1, group = "TargetCommenter", group.equal = c("loadings", "intercepts"))
smc.1931.1.02 <- mi.results(conf.fit.1931.2,load.fit.1931.2,int.fit.1931.2)
smc.1931.1.02
# Chisquare significant, RMSEA > .08, CFI < .95 so we reject configural invariance
vars <- c("Neg1","Neg2","Pos1","Pos2")
run.par(vars = vars, df = article1931.1, groupvar = "TargetCommenter", iters = 300)

# Study 2
# Comparisons 3-4
url <- 'https://osf.io/jbam4//?action=download'
filename <- 'article1931.2.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article1931.2 <- read_sav(filename)
article1931.2$ViralOutrage <- as.numeric(article1931.2$ViralOutrage)
article1931.2$ResponseType <- as.numeric(article1931.2$ResponseType)

# Comparison 3
# Grouping variable: viral vs non-viral outrage (ViralOutrage)
# Scale variable: Evaluation of target / Impression of commenter (Neg1 + Neg2 + Pos1 + Pos2)
conf.fit.1931.3 <- cfa(model1931.1, data = article1931.2, group = "ViralOutrage")
load.fit.1931.3 <- cfa(model1931.1, article1931.2, group = "ViralOutrage", group.equal = "loadings")
int.fit.1931.3 <- cfa(model1931.1, article1931.2, group = "ViralOutrage", group.equal = c("loadings", "intercepts"))
smc.1931.2.03 <- mi.results(conf.fit.1931.3,load.fit.1931.3,int.fit.1931.3)
smc.1931.2.03
# Chisquare significant, RMSEA > .08, so we reject configural invariance
vars <- c("Neg1","Neg2","Pos1","Pos2")
run.par(vars = vars, df = article1931.2, groupvar = "ViralOutrage", iters = 300)

# Comparison 4
# Grouping variable: response type: control; anonymous; or upvoting (ResponseType)
# Scale variable: Evaluation of target / Impression of commenter (Neg1 + Neg2 + Pos1 + Pos2)
conf.fit.1931.4 <- cfa(model1931.1, data = article1931.2, group = "ResponseType")
load.fit.1931.4 <- cfa(model1931.1, article1931.2, group = "ResponseType", group.equal = "loadings")
int.fit.1931.4 <- cfa(model1931.1, article1931.2, group = "ResponseType", group.equal = c("loadings", "intercepts"))
smc.1931.2.04 <- mi.results(conf.fit.1931.4,load.fit.1931.4,int.fit.1931.4)
smc.1931.2.04
# Chisquare significant, RMSEA > .08 so we reject configural invariance
# CFI ok (0.961)
vars <- c("Neg1","Neg2","Pos1","Pos2")
run.par(vars = vars, df = article1931.2, groupvar = "ResponseType", iters = 300)

# Study 3
# Comparisons 5-6
url <- 'https://osf.io/pfzc6//?action=download'
filename <- 'article1931.3.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article1931.3 <- read_sav(filename)
article1931.3$ViralOutrage <- as.numeric(article1931.3$ViralOutrage)
article1931.3$Surprise <- as.numeric(article1931.3$Surprise)

# Comparison 5
# Grouping variable: viral vs non-viral outrage (ViralOutrage)
# Scale variable: Evaluation of target / Impression of commenter (Neg1 + Neg2 + Pos1 + Pos2)
conf.fit.1931.5 <- cfa(model1931.1, data = article1931.3, group = "ViralOutrage")
load.fit.1931.5 <- cfa(model1931.1, article1931.3, group = "ViralOutrage", group.equal = "loadings")
int.fit.1931.5 <- cfa(model1931.1, article1931.3, group = "ViralOutrage", group.equal = c("loadings", "intercepts"))
smc.1931.3.05 <- mi.results(conf.fit.1931.5,load.fit.1931.5,int.fit.1931.5)
smc.1931.3.05
# Chisquare significant, RMSEA > .08, CFI < .95 so we reject configural invariance
vars <- c("Neg1","Neg2","Pos1","Pos2")
run.par(vars = vars, df = article1931.3, groupvar = "ViralOutrage", iters = 300)

# Comparison 6
# Grouping variable: surprise: control; no surprise; or surprise (Surprise)
# Scale variable: Evaluation of target / Impression of commenter (Neg1 + Neg2 + Pos1 + Pos2)
conf.fit.1931.6 <- cfa(model1931.1, data = article1931.3, group = "Surprise")
load.fit.1931.6 <- cfa(model1931.1, article1931.3, group = "Surprise", group.equal = "loadings")
int.fit.1931.6 <- cfa(model1931.1, article1931.3, group = "Surprise", group.equal = c("loadings", "intercepts"))
smc.1931.3.06 <- mi.results(conf.fit.1931.6,load.fit.1931.6,int.fit.1931.6)
smc.1931.3.06
# Chisquare significant, RMSEA > .08, CFI < .95 so we reject configural invariance
vars <- c("Neg1","Neg2","Pos1","Pos2")
run.par(vars = vars, df = article1931.3, groupvar = "Surprise", iters = 300)

# Study 4a
# Comparisons 7-12
url <- 'https://osf.io/924gy//?action=download'
filename <- 'article1931.4.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article1931.4 <- read_sav(filename)
article1931.4$ViralOutrage <- as.numeric(article1931.4$ViralOutrage)
article1931.4$Offender <- as.numeric(article1931.4$Offender)

# Comparison 7
# Grouping variable: viral vs non-viral outrage (ViralOutrage)
# Scale variable: Evaluation of target / Impression of commenter (Neg1 + Neg2 + Pos1 + Pos2)
conf.fit.1931.7 <- cfa(model1931.1, data = article1931.4, group = "ViralOutrage")
load.fit.1931.7 <- cfa(model1931.1, article1931.4, group = "ViralOutrage", group.equal = "loadings")
int.fit.1931.7 <- cfa(model1931.1, article1931.4, group = "ViralOutrage", group.equal = c("loadings", "intercepts"))
smc.1931.4.07 <- mi.results(conf.fit.1931.7,load.fit.1931.7,int.fit.1931.7)
smc.1931.4.07
# Chisquare significant, RMSEA > .08, CFI < .95 so we reject configural invariance
vars <- c("Neg1","Neg2","Pos1","Pos2")
run.par(vars = vars, df = article1931.4, groupvar = "ViralOutrage", iters = 300)

# Comparison 8
# Grouping variable: high-status-offender condition (Offender)
# Scale variable: Evaluation of target / Impression of commenter (Neg1 + Neg2 + Pos1 + Pos2)
conf.fit.1931.8 <- cfa(model1931.1, data = article1931.4, group = "Offender")
load.fit.1931.8 <- cfa(model1931.1, article1931.4, group = "Offender", group.equal = "loadings")
int.fit.1931.8 <- cfa(model1931.1, article1931.4, group = "Offender", group.equal = c("loadings", "intercepts"))
smc.1931.4.08 <- mi.results(conf.fit.1931.8,load.fit.1931.8,int.fit.1931.8)
smc.1931.4.08
# Chisquare significant, RMSEA > .08, CFI < .95 so we reject configural invariance
vars <- c("Neg1","Neg2","Pos1","Pos2")
run.par(vars = vars, df = article1931.4, groupvar = "Offender", iters = 300)

# Comparison 9
# Grouping variable: viral vs non-viral outrage (ViralOutrage)
# Scale variable: Perceived as harmful (Harm1 + Harm2 + Harm3 + Harm4)
model1931.2 <- 'F=~ Harm1 + Harm2 + Harm3 + Harm4' 

conf.fit.1931.9 <- cfa(model1931.2, data = article1931.4, group = "ViralOutrage")
load.fit.1931.9 <- cfa(model1931.2, article1931.4, group = "ViralOutrage", group.equal = "loadings")
int.fit.1931.9 <- cfa(model1931.2, article1931.4, group = "ViralOutrage", group.equal = c("loadings", "intercepts"))
smc.1931.4.09 <- mi.results(conf.fit.1931.9,load.fit.1931.9,int.fit.1931.9)
smc.1931.4.09
# Config: nonsig chisquare, RMSEA < .08, CFI > .95, config holds
# Metric invariance: nonsig chisq change + CFI change < .01, AIC and BIC lower; metric invariance holds. (RMSEA > .01 but fit improves)
# Scalar invariance: nonsig chisq change + CFI change < .01, AIC and BIC lower; scalar invariance holds. (RMSEA > .01 but fit improves)

# Comparison 10
# Grouping variable: high-status-offender condition (Offender)
# Scale variable: Perceived as harmful (Harm1 + Harm2 + Harm3 + Harm4)
conf.fit.1931.10 <- cfa(model1931.2, data = article1931.4, group = "Offender")
load.fit.1931.10 <- cfa(model1931.2, article1931.4, group = "Offender", group.equal = "loadings")
int.fit.1931.10 <- cfa(model1931.2, article1931.4, group = "Offender", group.equal = c("loadings", "intercepts"))
smc.1931.4.10 <- mi.results(conf.fit.1931.10,load.fit.1931.10,int.fit.1931.10)
smc.1931.4.10
# Config: nonsig chisquare, RMSEA < .08, CFI > .95, config holds
# Metric invariance: RMSEA change > .01 & AIC higher, but chisq change not significant, and CFI change < .01, and BIC lower. Metric invariance holds.
# Scalar invariance: sig chisq change, AIC and BIC higher, RMSEA and CFI change > .01: scalar invariance rejected

# Comparison 11
# Grouping variable: viral vs non-viral outrage (ViralOutrage)
# Scale variable: Perceived to be vulnerable to criticism (Vul1 + Vul2 + Vul3 + Vul4)
model1931.3 <- 'F=~ Vul1 + Vul2 + Vul3 + Vul4' 
conf.fit.1931.11 <- cfa(model1931.3, data = article1931.4, group = "ViralOutrage")
load.fit.1931.11 <- cfa(model1931.3, article1931.4, group = "ViralOutrage", group.equal = "loadings")
int.fit.1931.11 <- cfa(model1931.3, article1931.4, group = "ViralOutrage", group.equal = c("loadings", "intercepts"))
smc.1931.4.11 <- mi.results(conf.fit.1931.11,load.fit.1931.11,int.fit.1931.11)
smc.1931.4.11
# Chisquare significant, RMSEA > .08, CFI < .95 so we reject configural invariance
vars <- c("Vul1","Vul2","Vul3","Vul4")
run.par(vars = vars, df = article1931.4, groupvar = "ViralOutrage", iters = 300)

# Comparison 12
# Grouping variable: high-status-offender condition (Offender)
# Scale variable: Perceived to be vulnerable to criticism (Vul1 + Vul2 + Vul3 + Vul4)
conf.fit.1931.12 <- cfa(model1931.3, data = article1931.4, group = "Offender")
load.fit.1931.12 <- cfa(model1931.3, article1931.4, group = "Offender", group.equal = "loadings")
int.fit.1931.12 <- cfa(model1931.3, article1931.4, group = "Offender", group.equal = c("loadings", "intercepts"))
smc.1931.4.12 <- mi.results(conf.fit.1931.12,load.fit.1931.12,int.fit.1931.12)
smc.1931.4.12
# Chisquare significant, RMSEA > .08, CFI < .95 so we reject configural invariance
vars <- c("Vul1","Vul2","Vul3","Vul4")
run.par(vars = vars, df = article1931.4, groupvar = "Offender", iters = 300)

# Study 4b
# Comparisons 13-16
url <- 'https://osf.io/ndfpx//?action=download'
filename <- 'article1931.5.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article1931.5 <- read_sav(filename)
article1931.5$ViralOutrage <- as.numeric(article1931.5$ViralOutrage)
article1931.5$Offender <- as.numeric(article1931.5$Offender)

# Comparison 13
# Grouping variable: viral vs non-viral outrage (ViralOutrage)
# Scale variable: Evaluation of target / Impression of commenter (Neg1 + Neg2 + Pos1 + Pos2)
conf.fit.1931.13 <- cfa(model1931.1, data = article1931.5, group = "ViralOutrage")
load.fit.1931.13 <- cfa(model1931.1, article1931.5, group = "ViralOutrage", group.equal = "loadings")
int.fit.1931.13 <- cfa(model1931.1, article1931.5, group = "ViralOutrage", group.equal = c("loadings", "intercepts"))
smc.1931.5.13 <- mi.results(conf.fit.1931.13,load.fit.1931.13,int.fit.1931.13)
smc.1931.5.13
# Chisquare significant, RMSEA > .08, CFI < .95 so we reject configural invariance
vars <- c("Neg1","Neg2","Pos1","Pos2")
run.par(vars = vars, df = article1931.5, groupvar = "ViralOutrage", iters = 300)

# Comparison 14
# Grouping variable: viral vs non-viral outrage (ViralOutrage)
# Scale variable: Perceived as harmful (Harm1 + Harm2 + Harm3 + Harm4)
conf.fit.1931.14 <- cfa(model1931.2, data = article1931.5, group = "ViralOutrage")
load.fit.1931.14 <- cfa(model1931.2, article1931.5, group = "ViralOutrage", group.equal = "loadings")
int.fit.1931.14 <- cfa(model1931.2, article1931.5, group = "ViralOutrage", group.equal = c("loadings", "intercepts"))
smc.1931.5.14 <- mi.results(conf.fit.1931.14,load.fit.1931.14,int.fit.1931.14)
smc.1931.5.14
# Chisquare non-significant, RMSEA < .08, CFI > .95, so configural invariance holds
# Metric: change in RMSEA is > .01 but the RMSEA decreases (0.028 to 0 so the fit becomes better), so it adheres to cutoff
# Metric: change in CFI < .01, AIC is smaller, BIC is smaller; metric invariance holds
# Scalar: change in RMSEA < .01, change in CFI < .01, AIC and BIC are lower; scalar invariance holds

# Comparison 15
# Grouping variable: offender: unsympathetic or control (Offender)
# Scale variable: Evaluation of target / Impression of commenter (Neg1 + Neg2 + Pos1 + Pos2)
conf.fit.1931.15 <- cfa(model1931.1, data = article1931.5, group = "Offender")
load.fit.1931.15 <- cfa(model1931.1, article1931.5, group = "Offender", group.equal = "loadings")
int.fit.1931.15 <- cfa(model1931.1, article1931.5, group = "Offender", group.equal = c("loadings", "intercepts"))
smc.1931.5.15 <- mi.results(conf.fit.1931.15,load.fit.1931.15,int.fit.1931.15)
smc.1931.5.15
# Chisquare significant, RMSEA > .08, CFI < .95 so we reject configural invariance
vars <- c("Neg1","Neg2","Pos1","Pos2")
run.par(vars = vars, df = article1931.5, groupvar = "Offender", iters = 300)

# Comparison 16
# Grouping variable: offender: unsympathetic or control (Offender)
# Scale variable: Perceived as harmful (Harm1 + Harm2 + Harm3 + Harm4)
conf.fit.1931.16 <- cfa(model1931.2, data = article1931.5, group = "Offender")
load.fit.1931.16 <- cfa(model1931.2, article1931.5, group = "Offender", group.equal = "loadings")
int.fit.1931.16 <- cfa(model1931.2, article1931.5, group = "Offender", group.equal = c("loadings", "intercepts"))
smc.1931.5.16 <- mi.results(conf.fit.1931.16,load.fit.1931.16,int.fit.1931.16)
smc.1931.5.16
# Chisquare non-significant, RMSEA < .08, CFI > .95, so configural invariance holds
# Metric: change in CFI < .01, BIC is smaller but:
# change in RMSEA is > .01 AIC is larger, significant chisq: metric invariance is rejected.

# Article 2091 ---------------------------------------------------------------------
# Study 1b
url <- 'https://osf.io/jpyrd//?action=download'
filename <- 'article2091.xlsx'
GET(url, write_disk(filename, overwrite = TRUE))
article2091 <- read_excel(filename)

# Comparison 1
# Grouping variable: multi-tasking or single-tasking (Multi1)
# Scale variable: quality (coder1detail + coder1quality + coder1effort + coder2detail + coder2quality + coder2effort)

# From the manuscript: 
# Two coders, who were blind to the hypothesis and conditions, coded the quality of the notes. 
# The coders evaluated participants’ notes on (a) how detailed, thorough, and comprehensive the notes were; 
# (b) their overall clarity; and (c) how much effort they thought the participant put into writing the notes. 
# All items were measured on a scale from 1 to 7 and were highly correlated (α = .97). 
# Therefore, we collapsed these measures to form a single measure of quality. 
# Further, the two coders’ ratings were highly correlated (r = .91, p < .001) and were therefore averaged, 
# resulting in a single quality score for each participant.

# We decided to combine the item scores for the two coders first, leaving us with one item for detail, one for quality, and one for effort
article2091$detail <- (article2091$coder1detail + article2091$coder2detail)/2
article2091$quality <- (article2091$coder1quality + article2091$coder2quality)/2
article2091$effort <- (article2091$coder1effort + article2091$coder2effort)/2

model2091 <- 'F=~ detail + quality + effort' 

conf.fit.2091 <- cfa(model2091, data = article2091, group = "Multi1")
load.fit.2091 <- cfa(model2091, article2091, group = "Multi1", group.equal = "loadings")
int.fit.2091 <- cfa(model2091, article2091, group = "Multi1", group.equal = c("loadings", "intercepts"))
smc.2091.1.01 <- mi.results(conf.fit.2091,load.fit.2091,int.fit.2091)
smc.2091.1.01
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI change < .01, BIC lower; 
# But: RMSEA change > .10 and AIC larger. Metric invariance holds because we cannot reject chisquare test. 
# Scalar invariance: nonsig chisq change + CFI change < .01, AIC and BIC lower; scalar invariance holds. (RMSEA > .01 but fit improves)


# Make list of results ----------------------------------------------------
# This is Supplement C

# Select all comparisons
start_idx <- which(ls() == "smc.0009.1.02")
end_idx <- which(ls() == "smc.2091.1.01")
selected_vars <- ls()[start_idx:end_idx]

# Put all results in a list
dfs.list <- Filter(function(x) is(x, "matrix"), mget(selected_vars))

# Assign results to list
res <- lapply(dfs.list, bindrows)
str(res)

# Bind list together
res <- rbindlist(list(res))

# Transpose results
res_step4 <- data.frame(t(round(res,3)))

# Add row names to column
res_step4 <- cbind(row.names(res_step4), res_step4)

# Extract relevant columns from codesheet step 4
# Comparisons we tested
par_test <- df4$mitest_step4

# Number of factors extracted for complete sample
par_f <- as.integer(df4$f_total)

# Number of factors extracted for group
par_f_group <- df4$f_group
par_f_group[par_f_group == "NA"] <- NA
df_par <- as.data.frame(cbind(par_test, par_f, par_f_group))

# Remove rows we do not test for MI
df_par <- df_par[df_par$par_test == 1,]

# Bind columns to results from step 4
res_step4 <- cbind(res_step4, df_par$par_f, df_par$par_f_group)

# Add column names 
colnames(res_step4) <- c("SMC", "Chi-square", "df", "p", "RMSEA", "CFI", "F total","F group")

# Make APA table and save to file
tab <- nice_table(res_step4, short = TRUE)

# Print table
flextable::save_as_docx(tab, path = "../submission/SupplementC-unedited.docx")
