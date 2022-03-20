### CODE FOR MAIN STUDY SYSTEMATIC REVIEW MEASUREMENT INVARIANCE ##
### This is code to perform step 4 of our main study; reproducing results for studies that DID NOT report on MI

rm(list = ls()) # clear workspace
require("httr") # to load data from OSF into R
require("haven") # to load sav files into R
require("readxl") # to load xlsx files into R
require("lavaan") # to test for measurement invariance
require("semTools") # construct syntax for thresholds

# function to extract fit measures for models (only works if 3 models are estimated)
mi.results <- function(x,y,z) {
  
  all.results <- matrix(NA, ncol = 11, nrow = 3)
  colnames(all.results) <- c("chisq","df","pvalue", "rmsea", "cfi", "srmr", 
                             "diffRMSEA", "diffCFI", "lavtestLRT-pvalue", "AIC", "BIC")
  
  # save fit measures from models
  all.results[1,1:6] <- round(data.matrix(fitmeasures(x, fit.measures = c("chisq","df","pvalue","rmsea", "cfi", "srmr"))), digits=3)
  all.results[2,1:6] <- round(data.matrix(fitmeasures(y, fit.measures = c("chisq","df","pvalue","rmsea", "cfi", "srmr"))), digits=3)
  all.results[3,1:6] <- round(data.matrix(fitmeasures(z, fit.measures = c("chisq","df","pvalue","rmsea", "cfi", "srmr"))), digits=3)
  
  # compute the difference in goodness-of-fit measures
  all.results[2,7:8] <- all.results[2,4:5] - all.results[1,4:5]
  all.results[3,7:8] <- all.results[3,4:5] - all.results[2,4:5]
  
  # Compute the LRT between the different models and store the p-value result
  all.results[2,9] <- lavTestLRT(x,y)$`Pr(>Chisq)`[2]
  all.results[3,9] <- lavTestLRT(y,z)$`Pr(>Chisq)`[2]
  
  all.results[1,10] <- AIC(x)
  all.results[2,10] <- AIC(y)
  all.results[3,10] <- AIC(z)
  
  all.results[1,11] <- BIC(x)
  all.results[2,11] <- BIC(y)
  all.results[3,11] <- BIC(z)
  
  return(all.results)
}

# function to extract scaled fit measures for models (only works if 3 models are estimated)
mi.results.sc <- function(x,y,z) {
  
  all.results <- matrix(NA, ncol = 11, nrow = 3)
  colnames(all.results) <- c("chisq.sc","df.sc","pvalue.sc", "rmsea.sc", "cfi.sc", "srmr.sc", 
                             "diffRMSEA", "diffCFI", "lavtestLRT-pvalue", "AIC", "BIC")
  
  # save fit measures from models
  all.results[1,1:6] <- round(data.matrix(fitmeasures(x, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled","rmsea.scaled", "cfi.scaled", "srmr"))), digits=3)
  all.results[2,1:6] <- round(data.matrix(fitmeasures(y, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled","rmsea.scaled", "cfi.scaled", "srmr"))), digits=3)
  all.results[3,1:6] <- round(data.matrix(fitmeasures(z, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled","rmsea.scaled", "cfi.scaled", "srmr"))), digits=3)
  
  # compute the difference in goodness-of-fit measures
  all.results[2,7:8] <- all.results[2,4:5] - all.results[1,4:5]
  all.results[3,7:8] <- all.results[3,4:5] - all.results[2,4:5]
  
  # Compute the LRT between the different models and store the p-value result
  all.results[2,9] <- lavTestLRT(x,y)$`Pr(>Chisq)`[2]
  all.results[3,9] <- lavTestLRT(y,z)$`Pr(>Chisq)`[2]
  
  all.results[1,10] <- AIC(x)
  all.results[2,10] <- AIC(y)
  all.results[3,10] <- AIC(z)
  
  all.results[1,11] <- BIC(x)
  all.results[2,11] <- BIC(y)
  all.results[3,11] <- BIC(z)
  
  return(all.results)
}

# Article 9: Leitner --------------------------------------------------------------------------
# Data shared is raw data in .sav file, including .sps code file: https://osf.io/yxvuk/files/
# study 1; 3 comparisons
url <- 'https://osf.io/mzyh5//?action=download'
filename <- '../article9.1.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article9.1 <- read_sav(filename)

# comparison 1: 
# grouping: confederate (black / white)
# we cannot find the variables that are used in the perception of overall positivity scale,
# so we cannot run a MI test. 

# comparison 2: 
# variables for scale: post negative affect (DV_sad, DV_hopeless, DV_Discouraged, DV_angry, DV_resentful, DV_annoyed, DV_fatigued, DV_wornout, DV_exhausted, DV_vigor, DV_lively, DV_cheer)
# grouping: C_CnCI (low and high self-disclosure)

#Create model
model9.1 <- 'F1 =~ DV_sad +  DV_hopeless + DV_Discouraged + DV_angry + DV_resentful + DV_annoyed + DV_fatigued +
                  DV_wornout +  DV_exchausted + DV_vigor + DV_lively + DV_cheer'

#change type of variables to numeric for estimating the model
article9.1$group <- as.numeric(article9.1$C_CnCls)

# Fit the configural invariance model
conf.fit.9.1.2 <- cfa(model9.1, article9.1, group = "group")

# Fit the loadings invariance model
load.fit.9.1.2 <- cfa(model9.1, article9.1, group = "group", group.equal = "loadings")

# Fit the intercepts invariance model
int.fit.9.1.2 <- cfa(model9.1, article9.1,  group = "group", group.equal = c("loadings", "intercepts"))

# store fit measures and check results
all.results.9.1.2 <- mi.results(conf.fit.9.1.2,load.fit.9.1.2,int.fit.9.1.2)
all.results.9.1.2

# Step 4
# chisquare test is significant & RMSEA > 0.08 & CFI < 0.95, so we reject configural invariance

# comparison 3: 
# variables for scale: post negative affect (DV_sad, DV_hopeless, DV_Discouraged, DV_angry, DV_resentful, DV_annoyed, DV_fatigued, DV_wornout, DV_exhausted, DV_vigor, DV_lively, DV_cheer)
# grouping: confederate (black / white)

# Fit the configural invariance model
conf.fit.9.1.3 <- cfa(model9.1, article9.1, group = "ConfederateRace")
# Fit the loadings invariance model
load.fit.9.1.3 <- cfa(model9.1, article9.1, group = "ConfederateRace", group.equal = "loadings")
# Fit the intercepts invariance model
int.fit.9.1.3 <- cfa(model9.1, article9.1, group = "ConfederateRace", group.equal = c("loadings", "intercepts"))

# store fit measures and check results
all.results.9.1.3 <- mi.results(conf.fit.9.1.3,load.fit.9.1.3,int.fit.9.1.3)
all.results.9.1.3

# Step 4
# chisquare test is significant & RMSEA > 0.08 & CFI < 0.95, so we reject configural invariance

# study 2; 2 comparisons (4&5)

# comparison 4: 
# variables for scale: negative affect after speech but before feedback 
# POMS_prefdbk_hopeless,POMS_prefdbk_discouraged, POMS_prefdbk_angry, POMS_prefdbk_resent, POMS_prefdbk_annyd, POMS_prefdbk_fatigued, POMS_prefdbk_wornout, POMS_prefdbk_exhauste
# grouping: confederate C_CnCI (low and high self-disclosure)
url <- 'https://osf.io/g85jp//?action=download'
filename <- '../article7.2.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article7.2 <- read_sav(filename)

#change grouping variable class
article7.2$group <- as.numeric(article7.2$C_CnCl)

#Create model using the variables colnames for the POMS scale prefdbk
model9.2 <- NA
for (i in 1:12){
  model9.2[i] <- paste0("F1 =~" , colnames(article7.2[,i+19]))
}

# Fit the configural invariance model
conf.fit.9.2.1<- cfa(model9.2, article7.2, group = "group")

# Fit the loadings invariance model
load.fit.9.2.1 <- cfa(model9.2, article7.2, group = "group", group.equal = "loadings")

# Fit the intercepts invariance model
int.fit.9.2.1 <- cfa(model9.2, article7.2,group = "group", group.equal = c("loadings", "intercepts"))

all.results.9.2.1 <- mi.results(conf.fit.9.2.1,load.fit.9.2.1,int.fit.9.2.1)

all.results.9.2.1

# Step 4
# chisquare test is significant & RMSEA > 0.08 & CFI < 0.95, so we reject configural invariance

# comparison 5: 
# variables for scale: negative affect after speech but before feedback 
# POMS_prefdbk_hopeless,POMS_prefdbk_discouraged, POMS_prefdbk_angry, POMS_prefdbk_resent, POMS_prefdbk_annyd, POMS_prefdbk_fatigued, POMS_prefdbk_wornout, POMS_prefdbk_exhauste, POMS_vigor_prefdbk_R, POMS_lively_prefdbk_R, POMS_cheer_prefdbk_R
# grouping: confederate confederate (black / white)

# Fit the configural invariance model
conf.fit.9.2.2 <- cfa(model9.2, article7.2, group = "RaceCondition")

# Fit the loadings invariance model
load.fit.9.2.2 <- cfa(model9.2, article7.2, group = "RaceCondition", group.equal = "loadings")

# Fit the intercepts invariance model
int.fit.9.2.2 <- cfa(model9.2, article7.2,group = "RaceCondition", group.equal = c("loadings", "intercepts"))

all.results.9.2.2 <- mi.results(conf.fit.9.2.2,load.fit.9.2.2,int.fit.9.2.2)
all.results.9.2.2

# Step 4
# chisquare test is significant & RMSEA > 0.08 & CFI < 0.95, so we reject configural invariance

# Article 18: Bisallah ------------------------------------------------------------------------
# Data availability statement states: "All relevant data are within the paper and its Supporting Information files.".
# No raw data or correlations between variables reported in the article or in the supplemental material.
# We cannot do a MI test because we have no data.

# Article 20: Chang ---------------------------------------------------------------------------
# Raw data is shared as supplemental material: https://doi.org/10.1371/journal.pone.0195982.s001
url <- 'https://doi.org/10.1371/journal.pone.0195982.s001'
filename <- '../data/data-main/article20.xlsx'
GET(url, write_disk(filename, overwrite = TRUE))
article20 <- read_excel(filename)

# grouping: condom users: condomvaginalsex2
# variables for scale: self-efficacy sumscore given, which is afterwards dichotomized. No individual items shared
# We cannot do a MI test because we cannot construct a scale.



# Article 56: AlMahmoud -----------------------------------------------------------------------
url <- 'https://doi.org/10.1371/journal.pone.0202466.s002'
filename <- '../article56.xlxs'
GET(url, write_disk(filename, overwrite = TRUE))
article56 <- read_excel(filename)

# comparison 1:
# variables for scale: EIC1, EIC2, EIC3, EIC4, EIC5, EIC6, EIC7, EIC8, EIC9
# grouping: Gender

#change colnames for first variables
article56.1 <- article56[,c(2:10,47) ]
colnames(article56.1) <- c("EIC1", "EIC2", "EIC3", "EIC4", "EIC5", "EIC6", "EIC7", "EIC8", "EIC9", "Gender")

#Create model using the variables
model.56.1 <- NA
for (i in 1:9){
  model.56.1[i] <- paste0("F1 =~" , "EIC", i)
}

# Fit the configural invariance model
conf.fit.56.1 <- cfa(model.56.1, article56.1 , group = "Gender")

# Fit the loadings invariance model
load.fit.56.1 <- cfa(model.56.1, article56.1 , group = "Gender", group.equal = "loadings")

# Fit the intercepts invariance model
int.fit.56.1 <- cfa(model.56.1, article56.1 , group = "Gender", group.equal = c("loadings", "intercepts"))

all.results.56.1 <- mi.results(conf.fit.56.1,load.fit.56.1,int.fit.56.1)
all.results.56.1

# Step 4
# chisquare test is significant & RMSEA > 0.08 & CFI < 0.95, so we reject configural invariance

# comparison 2:
# variables for scale: EBEI1, EBEI2, EBEI3, EBEI4, EBEI5, EBEI6, EBEI7, EBEI8, EBEI9, EBEI10
# grouping: Gender

# get the correct variables from original dataframe
article56.2 <- article56[,c(11:20,47) ]
colnames(article56.2) <- c("EBEI1", "EBEI2", "EBEI3", "EBEI4", "EBEI5", "EBEI6", "EBEI7", "EBEI8", "EBEI9", "EBEI10", "Gender")

#Create model using the variables colnames for the POMS scale prefdbk
model.56.2 <- NA
for (i in 1:9){
  model.56.2[i] <- paste0("F1 =~" , "EBEI", i)
}

# Fit the configural invariance model
conf.fit.56.2 <- cfa(model.56.2, article56.2 , group = "Gender")

# Fit the loadings invariance model
load.fit.56.2 <- cfa(model.56.2, article56.2 , group = "Gender", group.equal = "loadings")

# Fit the intercepts invariance model
int.fit.56.2 <- cfa(model.56.2, article56.2 , group = "Gender", group.equal = c("loadings", "intercepts"))

all.results.56.2 <- mi.results(conf.fit.56.2,load.fit.56.2,int.fit.56.2)
all.results.56.2

# Step 4
# chisquare test is significant & RMSEA > 0.08 & CFI < 0.95, so we reject configural invariance

# comparison 3:s
# variables for scale: ECOVP1, ECOVP2, ECOVP3, ECOVP4, ECOVP5, ECOVP6, ECOVP7, ECOVP8, ECOVP9, ECOVP10,
# ECOVP11, ECOVP12, ECOVP13, ECOVP14, ECOVP15, ECOVP16, ECOVP17, ECOVP18, ECOVP19, ECOVP20, ECOVP21, 
# ECOVP22, ECOVP23, ECOVP24, ECOVP25
# grouping: Gender

# get the correct variables from original dataframe
article56.3 <- article56[,c(21:45,47) ]

colnames(article56.3) <- c("ECOVP1", "ECOVP2", "ECOVP3", "ECOVP4", "ECOVP5", "ECOVP6", "ECOVP7", "ECOVP8", "ECOVP9", "ECOVP10",
                           "ECOVP11", "ECOVP12", "ECOVP13", "ECOVP14", "ECOVP15", "ECOVP16", "ECOVP17", "ECOVP18" , 
                           "ECOVP19", "ECOVP20", "ECOVP21", "ECOVP22", "ECOVP23", "ECOVP24", "ECOVP25", "Gender")

#Create model using the variables colnames for the POMS scale prefdbk
model.56.3 <- NA
for (i in 1:9){
  model.56.3[i] <- paste0("F1 =~" , "ECOVP", i)
}

# Fit the configural invariance model
conf.fit.56.3 <- cfa(model.56.3, article56.3 , group = "Gender")

# Fit the loadings invariance model
load.fit.56.3 <- cfa(model.56.3, article56.3 , group = "Gender", group.equal = "loadings")

# Fit the intercepts invariance model
int.fit.56.3 <- cfa(model.56.3, article56.3 , group = "Gender", group.equal = c("loadings", "intercepts"))

all.results.56.3 <- mi.results(conf.fit.56.3,load.fit.56.3,int.fit.56.3)
all.results.56.3

# Step 4
# chisquare test is significant & RMSEA > 0.08 & CFI < 0.95, so we reject configural invariance

# Article 74: Cardoso -------------------------------------------------------------------------
url <- 'https://doi.org/10.1371/journal.pone.0205352.s002'
filename <- '../article74.csv'
GET(url, write_disk(filename, overwrite = TRUE))
article74 <- read.csv2(filename, header=F, na.strings="NA", sep=",")

# variables for scale: V1 V2 V3 (do not forget to delete the first row)
# grouping: we cannot find the grouping variable.
# Grouping variable as stated in article: A: cow housing pasture and presence of shade; B: cow housing pasture and absence of shade; 
# C: cow housing indoors and presence of shade; D: cow housing indoors and absence of shade.

# We cannot do a MI test because we cannot construct a grouping variable.

# Article 123: Jewkes -------------------------------------------------------------------------
# The data are shared on this website but we are unable to locate a dataset to download: http://medat.samrc.ac.za/index.php/catalog/35
# We cannot do a MI test because we have no data.


# Article 131: Senanayake ---------------------------------------------------------------------
url <- 'https://doi.org/10.1371/journal.pone.0211604.s001'
filename <- '../article131.xlsx'
GET(url, write_disk(filename, overwrite = TRUE))
article131 <- read_excel(filename)

# comparison 1:
# variables for scale: Eq5Dmob, Eq5Dselfc, Eq5Duact, Eq5Dpain, Eq5Dmood
# grouping: depression

#Create model 
model.131.1 <- 'F1 =~ Eq5Dmob +  Eq5Dselfc + Eq5Duact +  Eq5Dpain + Eq5Dmood'

# Fit the configural invariance model
conf.mod.131.1 <- measEq.syntax(model.131.1,
                                ID.fac = "std.lv",
                                ID.cat = "Wu",
                                ordered = c("Eq5Dmob", "Eq5Dselfc", "Eq5Duact", "Eq5Dpain", "Eq5Dmood"),
                                group = "depression", 
                                parameterization = "delta", 
                                data = article131,
                                group.equal = "configural") 

conf.fit.131.1 <- cfa(as.character(conf.mod.131.1), article131, group = "depression", estimator = "WLSMV")

# Fit the thresholds + loadings + intercepts invariance model
load.mod.131.1 <- measEq.syntax(model.131.1,
                                ID.fac = "std.lv",
                                ID.cat = "Wu",
                                ordered = c("Eq5Dmob", "Eq5Dselfc", "Eq5Duact", "Eq5Dpain", "Eq5Dmood"),
                                group = "depression", 
                                parameterization = "delta", 
                                data = article131,
                                group.equal = c("thresholds", "loadings", "intercepts")) 

load.fit.131.1 <- cfa(as.character(load.mod.131.1), article131, group = "depression", estimator = "WLSMV")

# Store goodness-of-fit measures results
all.results.131.1 <- matrix(NA, ncol = 11, nrow = 2)
colnames(all.results.131.1) <- c("chisq.sc","df.sc","pvalue.sc","rmsea.sc", "cfi.sc", "srmr.sc", "diffRMSEA", "diffCFI","lavtestLRT", "AIC", "BIC")

all.results.131.1[1,1:6] <-round(data.matrix(fitmeasures(conf.fit.131.1,
                                                       fit.measures = c("chisq.scaled","df.scaled","pvalue", 
                                                                        "rmsea.scaled", "cfi.scaled", "srmr"))), digits=3)

all.results.131.1[2,1:6] <-round(data.matrix(fitmeasures(load.fit.131.1,
                                                       fit.measures = c("chisq.scaled","df.scaled","pvalue", 
                                                                        "rmsea.scaled", "cfi.scaled", "srmr"))), digits=3)

# Compute the difference in goodness-of-fit measures
all.results.131.1[2,7:8] <- all.results.131.1[2,][4:5] - all.results.131.1[1,][4:5]

# Compute the LRT between the different models and store the p-value result
all.results.131.1[2,9] <- lavTestLRT(conf.fit.131.1, load.fit.131.1)$`Pr(>Chisq)`[2]

# Note that AIC and BIC are unavailable with this estimator
all.results.131.1

# Step 4
# chisquare test is significant & RMSEA > 0.08 & CFI < 0.95, so we reject configural invariance

# comparison 2:
# variables for scale: Eq5Dmob, Eq5Dselfc, Eq5Duact, Eq5Dpain, Eq5Dmood
# grouping: distress

#Create model 
model.131.2 <- 'F1 =~ Eq5Dmob +  Eq5Dselfc + Eq5Duact +  Eq5Dpain + Eq5Dmood'

# Note that since in 1 group (distress = 0) there is a 0 cell problem (non-observed score for the variables Eq5Dmob, selfc, Duact, mood)
# we decided to collapse categories 3 scores in 2 for both groups
article131_collapsed <- article131
article131_collapsed$Eq5Dmob <- ifelse(article131_collapsed$Eq5Dmob >=2, 2, 1)
article131_collapsed$Eq5Dselfc <- ifelse(article131_collapsed$Eq5Dselfc >=2, 2, 1)
article131_collapsed$Eq5Duact <- ifelse(article131_collapsed$Eq5Duact >=2, 2, 1)
article131_collapsed$Eq5Dmood <- ifelse(article131_collapsed$Eq5Dmood >=2, 2, 1)

# Fit the configural invariance model
conf.mod.131.2 <- measEq.syntax(model.131.2,
                                      ID.fac = "std.lv",
                                      ID.cat = "Wu",
                                      ordered = c("Eq5Dmob", "Eq5Dselfc", "Eq5Duact", "Eq5Dpain", "Eq5Dmood"),
                                      group = "distress", 
                                      parameterization = "delta", 
                                      data = article131_collapsed,
                                      group.equal = "configural") 

conf.fit.131.2 <- cfa(as.character(conf.mod.131.2), article131_collapsed, group = "distress", estimator = "WLSMV")

# Fit the intercepts invariance model
load.mod.131.2 <- measEq.syntax(model.131.2,
                                      ID.fac = "std.lv",
                                      ID.cat = "Wu",
                                      ordered = c("Eq5Dmob", "Eq5Dselfc", "Eq5Duact", "Eq5Dpain", "Eq5Dmood"),
                                      group = "distress", 
                                      parameterization = "delta", 
                                      data = article131_collapsed,
                                      group.equal = c("thresholds", "loadings", "intercepts")) 

load.fit.131.2 <- cfa(as.character(load.mod.131.2), article131_collapsed,  group = "distress", estimator = "WLSMV")

# Store model goodness-of-fit measures results
all.results.131.2 <- matrix(NA, ncol = 11, nrow = 2)
colnames(all.results.131.2) <- c("chisq.sc","df.sc","pvalue.sc","rmsea.sc", "cfi.sc", "srmr.sc", "diffRMSEA", "diffCFI","lavtestLRT", "AIC", "BIC")

all.results.131.2[1,1:6] <-round(data.matrix(fitmeasures(conf.fit.131.2,
                                                         fit.measures = c("chisq.scaled","df.scaled","pvalue", 
                                                                          "rmsea.scaled", "cfi.scaled", "srmr"))), digits=3)

all.results.131.2[2,1:6] <-round(data.matrix(fitmeasures(load.fit.131.2,
                                                                fit.measures = c("chisq.scaled","df.scaled","pvalue", 
                                                                                 "rmsea.scaled", "cfi.scaled", "srmr"))), digits=3)


# Compute the difference in goodness-of-fit measures
all.results.131.2[2,7:8] <- all.results.131.2[2,][4:5] - all.results.131.2[1,][4:5]

# Compute the LRT between the different models and store the p-value result
all.results.131.2[2,9] <- lavTestLRT(conf.fit.131.2, load.fit.131.2)$`Pr(>Chisq)`[2]

# Note that AIC and BIC are unavailable with this estimator
all.results.131.2

# Step 4
# chisquare test is significant & RMSEA > 0.08, so we reject configural invariance


# Article 142: Lingren ------------------------------------------------------------------------
url <- 'https://osf.io/rfam5//?action=download'
filename <- '../data/data-main/article142.sav'
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


# Article 155: van Puffelen -------------------------------------------------------------------
url <- 'https://doi.org/10.1371/journal.pone.0218242.s004'
filename <- '../data/data-main/article155.dta'
GET(url, write_disk(filename, overwrite = TRUE))
article155 <- read_dta(filename)

# grouping variable: measurement (0,1,2, corresponding to baseline, immediately post intervention, and 6 weeks after intervention)
# factors (number of items): 
#- PAID (20)
#- IPQ-R (timeline acute/chronic (6); timeline cyclical (4); timeline consequences (6); personal control (6); treatment control (5); coherence (5); emotional representation (6); own behavior in the past (6); psychological cause (5))
#- DAS-3 (need for special training (5); perceived seriousness (7); value of tight control (6); psychosocial impact (6); patient autonomy (8))
#- Perceptions of partner support (active engagement (5); protective buffering (8); overprotection (6))
#- DES-20 (20)

# Grouping variable is found. All factors are sum scores so we cannot construct a scale and do a MI test.


# Article 199: Goswami ------------------------------------------------------------------------
# Data Availability: All relevant data are within the manuscript, Supporting Information files 
# and via the DRYAD repository: https://datadryad.org/review?doi=doi:10.5061/dryad.m2q4c83.
# The data from DRYAD is not available. 
# There is no supplemental material added to the article
# The article does not contain correlations for the items of the WHO-DAS
# So we cannot do a MI test because we have no data


# Article 251: van Dessel -------------------------------------------------
# Study 1: one comparison
url <- 'https://osf.io/ukj2n//?action=download'
filename <- '../data/data-main/article251.1.txt'
GET(url, write_disk(filename, overwrite = TRUE))
article251.1 <- read.table(filename, header=TRUE) 

# Grouping variable: counterattitudinal information and control information: variable name is "man"
# Scale: explicit evaluation, variable name is "liking"

# From the article and supplemental materials on OSF (https://osf.io/nfqju/):

# "We will create an explicit rating score indicating a preference for Niffites over 
# Luupites by averageing difference scores of the 4 questions about the groups characteristics"

# Explicit-evaluation scores were computed by subtracting rating scores for the negative-induction group 
# from scores for the positive-induction group.

# The authors constructed the Liking variable like this (https://osf.io/tnfas/):
# compute scores
# Data$c1<-ifelse(Data$cdt==1,(Data$Lpls+Data$Lcor+Data$Lwon+Data$Lgoo)/4,(Data$Npls+Data$Ncor+Data$Nwon+Data$Ngoo)/4)
# Data$c2<-ifelse(Data$cdt==2,(Data$Lpls+Data$Lcor+Data$Lwon+Data$Lgoo)/4,(Data$Npls+Data$Ncor+Data$Nwon+Data$Ngoo)/4)
# Data$liking<-Data$c1-Data$c2 # preference for first group described as pos

# The authors have 4 variables of liking a certain thing (Lpls, Lcor, Lwon, Lgoo), and 4 variables of liking another thing (Npls, Ncor, Nwon, Ngoo)
# The authors assume each set of 4 variables constitutes a "liking" LV, and combine them into an index. 
# Then they subbtract L - N (for cdt=1) and N - L (for cdt=2) to get a final variable score for each participant.
# As we are interested in measurement invariance, we will not combine them into an index but first make difference scores, which we will test for MI.

article251.1$pls <- ifelse(article251.1$cdt==1,(article251.1$Lpls - article251.1$Npls),(article251.1$Npls - article251.1$Lpls))
article251.1$cor <- ifelse(article251.1$cdt==1,(article251.1$Lcor - article251.1$Ncor),(article251.1$Ncor - article251.1$Lcor))
article251.1$won <- ifelse(article251.1$cdt==1,(article251.1$Lwon - article251.1$Nwon),(article251.1$Nwon - article251.1$Lwon))
article251.1$goo <- ifelse(article251.1$cdt==1,(article251.1$Lgoo - article251.1$Ngoo),(article251.1$Ngoo - article251.1$Lgoo))

#Create model 
model.251.1 <- 'F1 =~ pls + cor + won + goo'

# Fit the configural invariance model
conf.fit.251.1 <- cfa(model.251.1, article251.1 , group = "man")

# Fit the loadings invariance model
load.fit.251.1 <- cfa(model.251.1, article251.1 , group = "man", group.equal = "loadings")

# Fit the intercepts invariance model
int.fit.251.1 <- cfa(model.251.1, article251.1 , group = "man", group.equal = c("loadings", "intercepts"))

all.results.251.1 <- mi.results(conf.fit.251.1,load.fit.251.1,int.fit.251.1)
all.results.251.1

# Step 4
# Chisquare non-significant, RMSEA < .08, CFI > .95, configural invariance holds
# Metric: nonsig chisquare change + change in CFI < .01. AIC and BIC are smaller. Metric invariance holds (but, RMSEA change > .01)
# Scalar: nonsig chisquare change + change in CFI and RMSEA > .01 and AIC is larger. Scalar invariance violated (BIC is ok)

# Study 2: three comparisons
url <- 'https://osf.io/vj8nz//?action=download'
filename <- '../data/data-main/article251.2.txt'
GET(url, write_disk(filename, overwrite = TRUE))
article251.2 <- read.table(filename, header=TRUE) 

# comparison 2
# grouping variable is time (pre-post)
# scale is three self-report items probing likability; friendliness; and trustworthiness

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

# the authors state: 
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

# make dataset for t1
article251.2.t1 <- cbind(article251.2$fpre,article251.2$lpre,article251.2$tpre)
grouping <- rep(0,nrow(article251.2.t1))
article251.2.t1 <- cbind(article251.2.t1,grouping)

# make dataset for t2
article251.2.t2 <- cbind(article251.2$fpost,article251.2$lpost,article251.2$tpost)
grouping <- rep(1,nrow(article251.2.t2))
article251.2.t2 <- cbind(article251.2.t2,grouping)

# combine datasets
article251.2.tot <- rbind(article251.2.t1,article251.2.t2)
colnames(article251.2.tot) <- c("friend","like","trust","grouping")

model.251.2 <- 'F =~ friend + like + trust'
conf.fit.251.2 <- cfa(model.251.2, data = article251.2.tot, group = "grouping")
load.fit.251.2 <- cfa(model.251.2, article251.2.tot, group = "grouping", group.equal = "loadings")
int.fit.251.2 <- cfa(model.251.2, article251.2.tot, group = "grouping", group.equal = c("loadings", "intercepts"))
all.results.251.2 <- mi.results(conf.fit.251.2,load.fit.251.2,int.fit.251.2)
all.results.251.2

# Step 4
# 3 items, so we must assume configural invariance
# Metric: nonsig chisq change + change CFI = 0, AIC and BIC smaller; metric invariance holds (but! change RMSEA > .01)
# Scalar: nonsig chisq change + change CFI = 0, AIC and BIC smaller, RMSEA > .01 but the fit improves (0.02 to 0), so scalar invariance holds


# comparison 3
# grouping variable is hypnosis vs relaxation condition at time 1 (variable name is hypnosis)
# scale is three self-report items probing likability; friendliness; and trustworthiness
# That would make the following variables:

## Compute Variables for the two targets
#Data$exp_Bobpre<-(Data$qBF_pre+Data$qBL_pre+Data$qBT_pre)/3
#Data$exp_Janpre<-(Data$qJF_pre+Data$qJL_pre+Data$qJT_pre)/3

# and then the final variable: 
# Data$exp_pre<-ifelse(Data$EC==1,Data$exp_Bobpre-Data$exp_Janpre,Data$exp_Janpre-Data$exp_Bobpre)

# We will not first make an index for Bob and Jan, but analyze the three variables separately. 
model.251.3 <- 'F =~ fpre + lpre + tpre'
conf.fit.251.3 <- cfa(model.251.3, data = article251.2, group = "hypnosis")
load.fit.251.3 <- cfa(model.251.3, article251.2, group = "hypnosis", group.equal = "loadings")
int.fit.251.3 <- cfa(model.251.3, article251.2, group = "hypnosis", group.equal = c("loadings", "intercepts"))
all.results.251.3 <- mi.results(conf.fit.251.3,load.fit.251.3,int.fit.251.3)
all.results.251.3

# Step 4
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI change < .01, AIC and BIC lower; metric invariance holds. (but: RMSEA > .01)
# Scalar invariance: nonsig chisq change + CFI change < .01, AIC and BIC lower; RMSEA change is >.01 but fit improves (.041 to 0); scalar invariance holds.

# comparison 4
# grouping variable is hypnosis vs relaxation condition at time 2 (variable name is hypnosis)
# scale is three self-report items probing likability; friendliness; and trustworthiness
# that would make the following variables:
## Compute Variables for the two targets
#Data$exp_Bobpost<-(Data$qBF_post+Data$qBL_post+Data$qBT_post)/3
#Data$exp_Janpost<-(Data$qJF_post+Data$qJL_post+Data$qJT_post)/3

# and then the final variable:
# Data$exp_post<-ifelse(Data$EC==1,Data$exp_Bobpost-Data$exp_Janpost,Data$exp_Janpost-Data$exp_Bobpost)
# We will not first make an index for Bob and Jan, but analyze the three variables separately. 
model.251.4 <- 'F =~ fpost + lpost + tpost'
conf.fit.251.4 <- cfa(model.251.4, data = article251.2, group = "hypnosis")
load.fit.251.4 <- cfa(model.251.4, article251.2, group = "hypnosis", group.equal = "loadings")
int.fit.251.4 <- cfa(model.251.4, article251.2, group = "hypnosis", group.equal = c("loadings", "intercepts"))
all.results.251.4 <- mi.results(conf.fit.251.4,load.fit.251.4,int.fit.251.4)
all.results.251.4

# Step 4
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; metric invariance holds. 
# Scalar invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; scalar invariance holds.

# Article 261: Sellier ----------------------------------------------------
url <- 'https://osf.io/ztsq4//?action=download'
filename <- '../article261.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article261 <- read_sav(filename)

# comparison 1
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

# make grouping variable numeric
article261$condition <- as.numeric(article261$condition)

#Create model 
model.261.1 <- 'F =~ bbs01 + bbs02 + bbs03 + bbs04 + bbs05 + bbs06 + bbs07 + bbs08 + bbs09 + bbs10 + bbs11 + bbs12 + bbs13 + bbs14'

# Fit the configural invariance model
conf.fit.261.1 <- cfa(model.261.1, data = article261, group = "condition")

# Fit the loadings invariance model
load.fit.261.1 <- cfa(model.261.1, article261 , group = "condition", group.equal = "loadings")

# Fit the intercepts invariance model
int.fit.261.1 <- cfa(model.261.1, article261 , group = "condition", group.equal = c("loadings", "intercepts"))

all.results.261.1 <- mi.results(conf.fit.261.1,load.fit.261.1,int.fit.261.1)
all.results.261.1

# Step 4
# chisquare test is significant & RMSEA > 0.08 & CFI < 0.95, so we reject configural invariance

# comparison 2
# Grouping variable: untrained controls and trained participants (condition)
# Scale: NED; correspondence bias (ned1 - ned10)

#Create model 
model.261.2 <- 'F =~ ned1 + ned2 + ned3 + ned4 + ned5 + ned6 + ned7 + ned8 + ned9 + ned10'

# Fit the configural invariance model
conf.fit.261.2 <- cfa(model.261.2, data = article261, group = "condition")

# Fit the loadings invariance model
load.fit.261.2 <- cfa(model.261.2, article261 , group = "condition", group.equal = "loadings")

# Fit the intercepts invariance model
int.fit.261.2 <- cfa(model.261.2, article261 , group = "condition", group.equal = c("loadings", "intercepts"))

all.results.261.2 <- mi.results(conf.fit.261.2,load.fit.261.2,int.fit.261.2)
all.results.261.2

# Step 4
# chisquare test is significant & RMSEA > 0.08 & CFI < 0.95, so we reject configural invariance

# Article 301: Luttrell ---------------------------------------------------
# Study 1
url <- 'https://osf.io/h8cs2//?action=download'
filename <- '../article301.1.csv'
GET(url, write_disk(filename, overwrite = TRUE))
article301.1 <- read.csv2(filename, header=T, na.strings="NA", sep=",")

# comparison 1
# Grouping variable: message type (moral argument vs practical argument): cond
# Scale: postmessage attitudes towards recycling: att2_1, att2_2, att2_3

#Create model 
model.301.1 <- 'F =~ att2_1 + att2_2 + att2_3'

# Fit the configural invariance model
conf.fit.301.1 <- cfa(model.301.1, data = article301.1, group = "cond")

# Fit the loadings invariance model
load.fit.301.1 <- cfa(model.301.1, article301.1, group = "cond", group.equal = "loadings")

# Fit the intercepts invariance model
int.fit.301.1 <- cfa(model.301.1, article301.1, group = "cond", group.equal = c("loadings", "intercepts"))

all.results.301.1 <- mi.results(conf.fit.301.1,load.fit.301.1,int.fit.301.1)
all.results.301.1

# Step 4
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; metric invariance holds.
# Scalar invariance: sig chisq change + RMSEA change > .01. AIC is larger; scalar invariance rejected. (BIC and CFI ok)

# comparison 2
# Grouping variable: political orientation (very liberal; somewhat liberal; neither; somewhat conservative; very conservative); poli_soc, poli_eco
# Scale: postmessage attitudes towards recycling: att2_1, att2_2, att2_3

# Political orientation is measured by two items, the authors state this:
# Since the reliability of these two items was quite strong (alpha = .86), we combined them into one political-orientation scale.

# We combined the two variables by averaging them and rounding (so someone who scored 1.5 becomes 2)
article301.1$pol <- as.numeric(round((article301.1$poli_soc+article301.1$poli_eco)/2))

# Fit the configural invariance model
conf.fit.301.2 <- cfa(model.301.1, data = article301.1, group = "pol")

# Fit the loadings invariance model
load.fit.301.2 <- cfa(model.301.1, article301.1 , group = "pol", group.equal = "loadings")

# Fit the intercepts invariance model
int.fit.301.2 <- cfa(model.301.1, article301.1, group = "pol", group.equal = c("loadings", "intercepts"))

all.results.301.2 <- mi.results(conf.fit.301.2,load.fit.301.2,int.fit.301.2)
all.results.301.2

# Step 4
# Three items so we assume configural invariance
# Metric invariance: sig chisq change + RMSEA change > .01, AIC larger; metric invariance rejected (BIC and CFI ok)

# Study 2
url <- 'https://osf.io/cbue5//?action=download'
filename <- '../article301.2.csv'
GET(url, write_disk(filename, overwrite = TRUE))
article301.2 <- read.csv2(filename, header=T, na.strings="NA", sep=",")

# comparison 3
# Grouping variable: message type (moral argument vs practical argument): cond
# Scale: postmessage attitudes towards marijuana: att2_1, att2_2, att2_3

#Create model 
model.301.3 <- 'F =~ att2_1 + att2_2 + att2_3'

# Fit the configural invariance model
conf.fit.301.3 <- cfa(model.301.3, data = article301.2, group = "cond")

# Fit the loadings invariance model
load.fit.301.3 <- cfa(model.301.3, article301.2, group = "cond", group.equal = "loadings")

# Fit the intercepts invariance model
int.fit.301.3 <- cfa(model.301.3, article301.2, group = "cond", group.equal = c("loadings", "intercepts"))

all.results.301.3 <- mi.results(conf.fit.301.3,load.fit.301.3,int.fit.301.3)
all.results.301.3

# Step 4
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; metric invariance holds.
# Scalar invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; scalar invariance holds.


# Article 481: Zlatev -----------------------------------------------------
url <- 'https://osf.io/jcx9r//?action=download'
filename <- '../article481.1.csv'
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

# comparison 1: match - capital - integrity
model.481.1 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.1 <- cfa(model.481.1, data = article481.capital, group = "match")
load.fit.481.1 <- cfa(model.481.1, article481.capital, group = "match", group.equal = "loadings")
int.fit.481.1 <- cfa(model.481.1, article481.capital, group = "match", group.equal = c("loadings", "intercepts"))
all.results.481.1 <- mi.results(conf.fit.481.1,load.fit.481.1,int.fit.481.1)
all.results.481.1

# Step 4
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; metric invariance holds.
# Scalar invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; scalar invariance holds.

# comparison 2: match - abortion - integrity
model.481.2 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.2 <- cfa(model.481.2, data = article481.abortion, group = "match")
load.fit.481.2 <- cfa(model.481.2, article481.abortion, group = "match", group.equal = "loadings")
int.fit.481.2 <- cfa(model.481.2, article481.abortion, group = "match", group.equal = c("loadings", "intercepts"))
all.results.481.2 <- mi.results(conf.fit.481.2,load.fit.481.2,int.fit.481.2)
all.results.481.2

# Step 4
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; metric invariance holds.
# Scalar invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; scalar invariance holds.

# comparison 3: match - gun - integrity
model.481.3 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.3 <- cfa(model.481.3, data = article481.gun, group = "match")
load.fit.481.3 <- cfa(model.481.3, article481.gun, group = "match", group.equal = "loadings")
int.fit.481.3 <- cfa(model.481.3, article481.gun, group = "match", group.equal = c("loadings", "intercepts"))
all.results.481.3 <- mi.results(conf.fit.481.3,load.fit.481.3,int.fit.481.3)
all.results.481.3

# Step 4
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; metric invariance holds.
# Scalar invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; scalar invariance holds.

# comparison 4: match - testing - integrity
model.481.4 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.4 <- cfa(model.481.4, data = article481.testing, group = "match")
load.fit.481.4 <- cfa(model.481.4, article481.testing, group = "match", group.equal = "loadings")
int.fit.481.4 <- cfa(model.481.4, article481.testing, group = "match", group.equal = c("loadings", "intercepts"))
all.results.481.4 <- mi.results(conf.fit.481.4,load.fit.481.4,int.fit.481.4)
all.results.481.4

# Step 4
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; metric invariance holds.
# Scalar invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; scalar invariance holds.

# comparison 5: match - suicide - integrity
model.481.5 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.5 <- cfa(model.481.5, data = article481.suicide, group = "match")
load.fit.481.5 <- cfa(model.481.5, article481.suicide, group = "match", group.equal = "loadings")
int.fit.481.5 <- cfa(model.481.5, article481.suicide, group = "match", group.equal = c("loadings", "intercepts"))
all.results.481.5 <- mi.results(conf.fit.481.5,load.fit.481.5,int.fit.481.5)
all.results.481.5

# Step 4
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI change < .01, AIC and BIC lower; metric invariance holds.
# Scalar invariance: 
# Chisquare nonsignificant, BIC smaller than previous step
# CFI and RMSEA change too large, AIC larger than previous step
# Criteria are: both significant chisquare change and 2 out of 4 fit measures exceed the criteria.
# As the chisquare is non-significant, we cannot reject scalar invariance; scalar invariance holds. 

# comparison 6: match - overall - integrity
model.481.6 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.6 <- cfa(model.481.6, data = article481.1, group = "match")
load.fit.481.6 <- cfa(model.481.6, article481.1, group = "match", group.equal = "loadings")
int.fit.481.6 <- cfa(model.481.6, article481.1, group = "match", group.equal = c("loadings", "intercepts"))
all.results.481.6 <- mi.results(conf.fit.481.6,load.fit.481.6,int.fit.481.6)
all.results.481.6

# Step 4
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI change < .01, AIC and BIC lower; metric invariance holds.
# Scalar invariance: nonsig chisq change + CFI change < .01, BIC lower
# Scalar invariance: RMSEA change is too large (> .01) and AIC is larger than in the previous step. But, 
# Chisquare is nonsignificant so we cannot reject scalar invariance; scalar invariance holds.

# comparison 7: match - capital - benevolence
model.481.7 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.7 <- cfa(model.481.7, data = article481.capital, group = "match")
load.fit.481.7 <- cfa(model.481.7, article481.capital, group = "match", group.equal = "loadings")
int.fit.481.7 <- cfa(model.481.7, article481.capital, group = "match", group.equal = c("loadings", "intercepts"))
all.results.481.7 <- mi.results(conf.fit.481.7,load.fit.481.7,int.fit.481.7)
all.results.481.7

# Step 4
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI change < .01, AIC and BIC lower; metric invariance holds.
# Scalar invariance: nonsig chisq change + CFI change < .01, AIC and BIC lower, RMSEA change > .01 but the fit improves; scalar invariance holds.

# comparison 8: match - abortion - benevolence
model.481.8 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.8 <- cfa(model.481.8, data = article481.abortion, group = "match")
load.fit.481.8 <- cfa(model.481.8, article481.abortion, group = "match", group.equal = "loadings")
int.fit.481.8 <- cfa(model.481.8, article481.abortion, group = "match", group.equal = c("loadings", "intercepts"))
all.results.481.8 <- mi.results(conf.fit.481.8,load.fit.481.8,int.fit.481.8)
all.results.481.8

# Step 4
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; metric invariance holds.
# Scalar invariance: sig chisq change + CFI and RMSEA change > .01, AIC and BIC larger; scalar invariance rejected


# comparison 9: match - gun - benevolence
model.481.9 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.9 <- cfa(model.481.9, data = article481.gun, group = "match")
load.fit.481.9 <- cfa(model.481.9, article481.gun, group = "match", group.equal = "loadings")
int.fit.481.9 <- cfa(model.481.9, article481.gun, group = "match", group.equal = c("loadings", "intercepts"))
all.results.481.9 <- mi.results(conf.fit.481.9,load.fit.481.9,int.fit.481.9)
all.results.481.9

# Step 4
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI and RMSEA change > .01, AIC larger; metric invariance rejected (BIC ok)

# comparison 10: match - testing - benevolence
model.481.10 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.10 <- cfa(model.481.10, data = article481.testing, group = "match")
load.fit.481.10 <- cfa(model.481.10, article481.testing, group = "match", group.equal = "loadings")
int.fit.481.10 <- cfa(model.481.10, article481.testing, group = "match", group.equal = c("loadings", "intercepts"))
all.results.481.10 <- mi.results(conf.fit.481.10,load.fit.481.10,int.fit.481.10)
all.results.481.10

# Step 4
# Three items so we assume configural invariance
# Metric invariance: sig chisq change + CFI and RMSEA change > .01, AIC larger; metric invariance rejected (BIC ok)

# comparison 11: match - suicide - benevolence
model.481.11 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.11 <- cfa(model.481.11, data = article481.suicide, group = "match")
load.fit.481.11 <- cfa(model.481.11, article481.suicide, group = "match", group.equal = "loadings")
int.fit.481.11 <- cfa(model.481.11, article481.suicide, group = "match", group.equal = c("loadings", "intercepts"))
all.results.481.11 <- mi.results(conf.fit.481.11,load.fit.481.11,int.fit.481.11)
all.results.481.11

# Step 4
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; metric invariance holds.
# Scalar invariance: sig chisq change + CFI and RMSEA change > .01, AIC and BIC larger; scalar invariance rejected.

# comparison 12: match - overall - benevolence
model.481.12 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.12 <- cfa(model.481.12, data = article481.1, group = "match")
load.fit.481.12 <- cfa(model.481.12, article481.1, group = "match", group.equal = "loadings")
int.fit.481.12 <- cfa(model.481.12, article481.1, group = "match", group.equal = c("loadings", "intercepts"))
all.results.481.12 <- mi.results(conf.fit.481.12,load.fit.481.12,int.fit.481.12)
all.results.481.12

# Step 4
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI change < .01, BIC lower
# But: RMSEA change > .01, AIC larger. 
# As the chisquare change is not significant, we cannot reject metric invariance; metric invariance holds
# Scalar invariance: sig chisq change + CFI and RMSEA change > .01, AIC and BIC larger; scalar invariance rejected

# comparison 13: care - capital - integrity
model.481.13 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.13 <- cfa(model.481.13, data = article481.capital, group = "care")
load.fit.481.13 <- cfa(model.481.13, article481.capital, group = "care", group.equal = "loadings")
int.fit.481.13 <- cfa(model.481.13, article481.capital, group = "care", group.equal = c("loadings", "intercepts"))
all.results.481.13 <- mi.results(conf.fit.481.13,load.fit.481.13,int.fit.481.13)
all.results.481.13

# Step 4
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI change < .01, AIC and BIC lower; metric invariance holds (but RMSEA change > .01).
# Scalar invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; scalar invariance holds.

# comparison 14: care - abortion - integrity
model.481.14 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.14 <- cfa(model.481.14, data = article481.abortion, group = "care")
load.fit.481.14 <- cfa(model.481.14, article481.abortion, group = "care", group.equal = "loadings")
int.fit.481.14 <- cfa(model.481.14, article481.abortion, group = "care", group.equal = c("loadings", "intercepts"))
all.results.481.14 <- mi.results(conf.fit.481.14,load.fit.481.14,int.fit.481.14)
all.results.481.14

# Step 4
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; metric invariance holds.
# Scalar invariance: nonsig chisq change + CFI change < .01, AIC and BIC lower; scalar invariance holds (but: RMSEA change > .01)

# comparison 15: care - gun - integrity
model.481.15 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.15 <- cfa(model.481.15, data = article481.gun, group = "care")
load.fit.481.15 <- cfa(model.481.15, article481.gun, group = "care", group.equal = "loadings")
int.fit.481.15 <- cfa(model.481.15, article481.gun, group = "care", group.equal = c("loadings", "intercepts"))
all.results.481.15 <- mi.results(conf.fit.481.15,load.fit.481.15,int.fit.481.15)
all.results.481.15

# Step 4
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; metric invariance holds.
# Scalar invariance: sig chisq change + CFI and RMSEA change > .01, AIC larger; scalar invariance rejected (BIC ok).

# comparison 16: care - testing - integrity
model.481.16 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.16 <- cfa(model.481.16, data = article481.testing, group = "care")
load.fit.481.16 <- cfa(model.481.16, article481.testing, group = "care", group.equal = "loadings")
int.fit.481.16 <- cfa(model.481.16, article481.testing, group = "care", group.equal = c("loadings", "intercepts"))
all.results.481.16 <- mi.results(conf.fit.481.16,load.fit.481.16,int.fit.481.16)
all.results.481.16

# Step 4
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI change < .01, AIC and BIC lower; metric invariance holds (RMSEA change > 01).
# Scalar invariance: nonsig chisq change + CFI change < .01, RMSEA change > .01 but fit improves, AIC and BIC lower; scalar invariance holds.

# comparison 17: care - suicide - integrity
model.481.17 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.17 <- cfa(model.481.17, data = article481.suicide, group = "care")
load.fit.481.17 <- cfa(model.481.17, article481.suicide, group = "care", group.equal = "loadings")
int.fit.481.17 <- cfa(model.481.17, article481.suicide, group = "care", group.equal = c("loadings", "intercepts"))
all.results.481.17 <- mi.results(conf.fit.481.17,load.fit.481.17,int.fit.481.17)
all.results.481.17

# Step 4
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI change < .01, AIC and BIC lower; metric invariance holds (RMSEA change > .01).
# Scalar invariance: nonsig chisq change + RMSEA change < .01, BIC lower
# But: CFI change > .01, AIC larger
# As the chisquare difference is non-significant, we cannot reject scalar invariance; scalar invariance holds.

# comparison 18: care - overall - integrity
model.481.18 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.18 <- cfa(model.481.18, data = article481.1, group = "care")
load.fit.481.18 <- cfa(model.481.18, article481.1, group = "care", group.equal = "loadings")
int.fit.481.18 <- cfa(model.481.18, article481.1, group = "care", group.equal = c("loadings", "intercepts"))
all.results.481.18 <- mi.results(conf.fit.481.18,load.fit.481.18,int.fit.481.18)
all.results.481.18

# Step 4
# Three items so we assume configural invariance
# Metric invariance: sig chisq change + RMSEA change > .01, AIC larger. We reject metric invariance. (CFI change < .01 and BIC lower)

# comparison 19: care - capital - benevolence
model.481.19 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.19 <- cfa(model.481.19, data = article481.capital, group = "care")
load.fit.481.19 <- cfa(model.481.19, article481.capital, group = "care", group.equal = "loadings")
int.fit.481.19 <- cfa(model.481.19, article481.capital, group = "care", group.equal = c("loadings", "intercepts"))
all.results.481.19 <- mi.results(conf.fit.481.19,load.fit.481.19,int.fit.481.19)
all.results.481.19

# Step 4
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + BIC lower
# RMSEA change > .01, CFI change > .01, AIC larger. 
# As the chisquare difference is not significant, we cannot reject metric invariance; metric invariance holds
# Scalar invariance: nonsig chisq change + CFI change < .01, AIC and BIC lower; scalar invariance holds (RMSEA change > .01 but fit improves)

# comparison 20: care - abortion - benevolence
model.481.20 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.20 <- cfa(model.481.20, data = article481.abortion, group = "care")
load.fit.481.20 <- cfa(model.481.20, article481.abortion, group = "care", group.equal = "loadings")
int.fit.481.20 <- cfa(model.481.20, article481.abortion, group = "care", group.equal = c("loadings", "intercepts"))
all.results.481.20 <- mi.results(conf.fit.481.20,load.fit.481.20,int.fit.481.20)
all.results.481.20

# Step 4
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI change < .01, AIC and BIC lower; metric invariance holds (RMSEA change > .01)
# Scalar invariance: nonsig chisq change + CFI change < .01, BIC lower;
# RMSEA change > .01 and AIC is larger. As the chisquare test is non-sig, we cannot reject scalar invariance; scalar invariance holds.

# comparison 21: care - gun - benevolence
model.481.21 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.21 <- cfa(model.481.21, data = article481.gun, group = "care")
load.fit.481.21 <- cfa(model.481.21, article481.gun, group = "care", group.equal = "loadings")
int.fit.481.21 <- cfa(model.481.21, article481.gun, group = "care", group.equal = c("loadings", "intercepts"))
all.results.481.21 <- mi.results(conf.fit.481.21,load.fit.481.21,int.fit.481.21)
all.results.481.21

# Step 4
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + BIC lower
# CFI change > .01, RMSEA change > .01, AIC larger than previous step. Because chisquare is non-sig we cannot reject metric invariance; metric invariance holds.
# Scalar invariance: nonsig chisq change + CFI change < .01, AIC and BIC lower; scalar invariance holds (RMSEA change > .01 but fit improves)

# comparison 22: care - testing - benevolence
model.481.22 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.22 <- cfa(model.481.22, data = article481.testing, group = "care")
load.fit.481.22 <- cfa(model.481.22, article481.testing, group = "care", group.equal = "loadings")
int.fit.481.22 <- cfa(model.481.22, article481.testing, group = "care", group.equal = c("loadings", "intercepts"))
all.results.481.22 <- mi.results(conf.fit.481.22,load.fit.481.22,int.fit.481.22)
all.results.481.22

# Step 4
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; metric invariance holds.
# Scalar invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; scalar invariance holds.

# comparison 23: care - suicide - benevolence
model.481.23 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.23 <- cfa(model.481.23, data = article481.suicide, group = "care")
load.fit.481.23 <- cfa(model.481.23, article481.suicide, group = "care", group.equal = "loadings")
int.fit.481.23 <- cfa(model.481.23, article481.suicide, group = "care", group.equal = c("loadings", "intercepts"))
all.results.481.23 <- mi.results(conf.fit.481.23,load.fit.481.23,int.fit.481.23)
all.results.481.23

# Step 4
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI change < .01, BIC lower; 
# RMSEA change > .01 and AIC larger; because chisquare is nonsig we cannot reject metric invariance; metric invariance holds.
# Scalar invariance: nonsig chisq change + CFI and RMSEA change < .01, BIC lower; scalar invariance holds (AIC larger)

# comparison 24: care - overall - benevolence
model.481.24 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.24 <- cfa(model.481.24, data = article481.1, group = "care")
load.fit.481.24 <- cfa(model.481.24, article481.1, group = "care", group.equal = "loadings")
int.fit.481.24 <- cfa(model.481.24, article481.1, group = "care", group.equal = c("loadings", "intercepts"))
all.results.481.24 <- mi.results(conf.fit.481.24,load.fit.481.24,int.fit.481.24)
all.results.481.24

# Step 4
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI change < .01, BIC lower; 
# RMSEA change > .01, AIC larger; because chisquare nonsig we cannot reject metric invariance; metric invariance holds.
# Scalar invariance: nonsig chisq change + CFI change < .01, AIC and BIC lower; scalar invariance holds (RMSEA change > .01 but fit improves)

# Study 3a + 3b
# grouping variables: agreement (match) and caring (care)
# scale variables: integrity based trust and benevolence based trust (benev123, integ123)
url <- 'https://osf.io/fq82m//?action=download'
filename <- '../article481.2.csv'
GET(url, write_disk(filename, overwrite = TRUE))
article481.2 <- read.csv2(filename, header=T, na.strings="NA", sep=",")

# remove the first row with text
article481.2 <- article481.2[-1,]

# comparison 25: match - integrity
model.481.25 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.25 <- cfa(model.481.25, data = article481.2, group = "match")
load.fit.481.25 <- cfa(model.481.25, article481.2, group = "match", group.equal = "loadings")
int.fit.481.25 <- cfa(model.481.25, article481.2, group = "match", group.equal = c("loadings", "intercepts"))
all.results.481.25 <- mi.results(conf.fit.481.25,load.fit.481.25,int.fit.481.25)
all.results.481.25

# Step 4
# Three items so we assume configural invariance
# Metric invariance: sig chisq change + RMSEA change > .01, AIC larger; we reject metric invariance (CFI and BIC ok)

# comparison 26: care - integrity
model.481.26 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.26 <- cfa(model.481.26, data = article481.2, group = "care")
load.fit.481.26 <- cfa(model.481.26, article481.2, group = "care", group.equal = "loadings")
int.fit.481.26 <- cfa(model.481.26, article481.2, group = "care", group.equal = c("loadings", "intercepts"))
all.results.481.26 <- mi.results(conf.fit.481.26,load.fit.481.26,int.fit.481.26)
all.results.481.26

# Step 4
# Three items so we assume configural invariance
# Metric invariance: sig chisq change + RMSEA change > .01, AIC larger; we reject metric invariance (CFI and BIC ok)

# comparison 27: match - benevolence
model.481.27 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.27 <- cfa(model.481.27, data = article481.2, group = "match")
load.fit.481.27 <- cfa(model.481.27, article481.2, group = "match", group.equal = "loadings")
int.fit.481.27 <- cfa(model.481.27, article481.2, group = "match", group.equal = c("loadings", "intercepts"))
all.results.481.27 <- mi.results(conf.fit.481.27,load.fit.481.27,int.fit.481.27)
all.results.481.27

# Step 4
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI change < .01, AIC and BIC lower; metric invariance holds (RMSEA change > .01).
# Scalar invariance: sig chisq change + RMSEA change > .01, AIC larger; we reject scalar invariance (CFI and BIC ok)

# comparison 28: care - benevolence
model.481.28 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.28 <- cfa(model.481.28, data = article481.2, group = "care")
load.fit.481.28 <- cfa(model.481.28, article481.2, group = "care", group.equal = "loadings")
int.fit.481.28 <- cfa(model.481.28, article481.2, group = "care", group.equal = c("loadings", "intercepts"))
all.results.481.28 <- mi.results(conf.fit.481.28,load.fit.481.28,int.fit.481.28)
all.results.481.28

# Step 4
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI and RMSEA change < .01, AIC and BIC lower; metric invariance holds.
# Scalar invariance: sig chisq change + RMSEA change > .01, AIC larger; we reject scalar invariance (CFI and BIC ok)

# Article 521: Kristal ----------------------------------------------------
# comparison 1 (study 2a)
url <- 'https://osf.io/7gjmk//?action=download'
filename <- '../article521.1.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article521.1 <- read_sav(filename)

# Grouping variable: Tense
# Scale variable: EnvyDV_envy, EnvyDV_jeal, EnvyDV_want, EnvyDV_inf, EnvyDV_hap
model.521.1 <- 'F =~  EnvyDV_envy + EnvyDV_jeal + EnvyDV_want + EnvyDV_inf + EnvyDV_hap'
conf.fit.521.1 <- cfa(model.521.1, data = article521.1, group = "Tense")
load.fit.521.1 <- cfa(model.521.1, article521.1, group = "Tense", group.equal = "loadings")
int.fit.521.1 <- cfa(model.521.1, article521.1, group = "Tense", group.equal = c("loadings", "intercepts"))
all.results.521.1 <- mi.results(conf.fit.521.1,load.fit.521.1,int.fit.521.1)
all.results.521.1

# Step 4
# chisquare is significant, rmsea > 0.08, CFI < .95, so configural invariance rejected.

# comparison 2 (study 2b)
url <- 'https://osf.io/u692w//?action=download'
filename <- '../article521.2.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article521.2 <- read_sav(filename)

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

# ask Damiano: should we remove the absolute from this code or not?
before_envious <- abs(article521.2$Day2DV_envious - article521.2$Day1DV_envious)
before_jealous <- abs(article521.2$Day2DV_jealous - article521.2$Day1DV_jealous)
before_want <- abs(article521.2$Day2DV_want - article521.2$Day1DV_want)
before_inferior <- abs(article521.2$Day2DV_inferior - article521.2$Day1DV_inferior)
before_happy <- abs(article521.2$Day2DV_happy - article521.2$Day1DV_happy)
after_envious <- abs(article521.2$Day3DV_envious - article521.2$Day2DV_envious)
after_jealous <- abs(article521.2$Day3DV_jealous - article521.2$Day2DV_jealous)
after_want <- abs(article521.2$Day3DV_want - article521.2$Day2DV_want)
after_inferior <- abs(article521.2$Day3DV_inferior - article521.2$Day2DV_inferior)
after_happy <- abs(article521.2$Day3DV_happy - article521.2$Day2DV_happy)

# make dataset with only difference scores for before Vday
article521.before <- cbind(before_envious,before_jealous,before_want,before_inferior,before_happy)
grouping <- rep(0,nrow(article521.before))
article521.before <- cbind(article521.before,grouping)

# make dataset with only difference scores for after Vday
article521.after <- cbind(after_envious,after_jealous,after_want,after_inferior,after_happy)
grouping <- rep(1,nrow(article521.after))
article521.after <- cbind(article521.after,grouping)

# combine datasets
article521.2.tot <- rbind(article521.before,article521.after)

# construct and estimate model
model.521.2 <- 'F=~ before_envious + before_jealous + before_want + before_inferior + before_happy'
conf.fit.521.2 <- cfa(model.521.2, data = article521.2.tot, group = "grouping")
load.fit.521.2 <- cfa(model.521.2, article521.2.tot, group = "grouping", group.equal = "loadings")
int.fit.521.2 <- cfa(model.521.2, article521.2.tot, group = "grouping", group.equal = c("loadings", "intercepts"))
all.results.521.2 <- mi.results(conf.fit.521.2,load.fit.521.2,int.fit.521.2)
all.results.521.2

# Step 4
# chisquare is significant, rmsea > 0.08, CFI < .95, so configural invariance rejected. 

# comparison 3 & 4 (study 3a)
url <- 'https://osf.io/4fpxu//?action=download'
filename <- '../article521.3.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article521.3 <- read_sav(filename)

# comparison 3
# Grouping variable: past vs distant future (tense)
# Scale variable: Malicious envy: DVfrustrating, DVillwill, DVdislike, DVreplace, DVscream
model.521.3 <- 'F =~ DVfrustrating + DVillwill + DVdislike + DVreplace + DVscream'
conf.fit.521.3 <- cfa(model.521.3, data = article521.3, group = "tense")
load.fit.521.3 <- cfa(model.521.3, article521.3, group = "tense", group.equal = "loadings")
int.fit.521.3 <- cfa(model.521.3, article521.3, group = "tense", group.equal = c("loadings", "intercepts"))
all.results.521.3 <- mi.results(conf.fit.521.3,load.fit.521.3,int.fit.521.3)
all.results.521.3

# Step 4
# chisquare is significant, rmsea > 0.08, so we reject configural invariance
# CFI is ok (0.976)

# comparison 4
# Grouping variable: past vs control (tense)
# Scale variable: Benign envy: DVpleas, DVinspire, DVliking, DVtryhard, DVcompliment.
model.521.4 <- 'F =~ DVpleas + DVinspire + DVliking + DVtryhard + DVcompliment'
conf.fit.521.4 <- cfa(model.521.4, data = article521.3, group = "tense")
load.fit.521.4 <- cfa(model.521.4, article521.3, group = "tense", group.equal = "loadings")
int.fit.521.4 <- cfa(model.521.4, article521.3, group = "tense", group.equal = c("loadings", "intercepts"))
all.results.521.4 <- mi.results(conf.fit.521.4,load.fit.521.4,int.fit.521.4)
all.results.521.4

# Step 4
# chisquare is significant, rmsea > 0.08, CFI < .95, so configural invariance rejected. 

# Study 4
# comparison 5-10
url <- 'https://osf.io/6ujnw//?action=download'
filename <- '../article521.4.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article521.4 <- read_sav(filename)

# comparison 5
# Grouping variable: past vs control (group = 0 for control and group = 1 for past)
# Scale variable: State-trait anxiety inventory (6 items per time point so 12 items)

# The authors constructed the variable StressT1 by taking the average of the 6 anxiety scores. 
# The variable DiffStress is then the difference in StressT2-StressT1. This DiffStress is then t-tested.
# If we would test these average anxiety items for MI (StressT2 and StressT2), we would only have two items per factor. As such
# we decided to compute difference scores per anxiety item for each participant, and perform a MI test for these variables

article521.4$anx1 <- article521.4$Anx1reverseT2 - article521.4$Anx1reverseT1
article521.4$anx2 <- article521.4$Anxiety_2.0 - article521.4$Anxiety_2
article521.4$anx3 <- article521.4$Anxiety_3.0 - article521.4$Anxiety_3
article521.4$anx4 <- article521.4$Anx4reverseT2 - article521.4$Anx4reverseT1
article521.4$anx5 <- article521.4$Anx5reverseT2 - article521.4$Anx5reverseT1
article521.4$anx6 <- article521.4$Anxiety_6.0 - article521.4$Anxiety_6

# we only need group=0 and group=1 for this comparison, and will drop out group=2
article521.4.1 <- article521.4[article521.4[,"GROUP"] != 2,]
range(article521.4.1$GROUP)

# construct model and estimate
model.521.5 <- 'F =~ anx1 + anx2 + anx3 + anx4 + anx5 + anx6'
conf.fit.521.5 <- cfa(model.521.5, data = article521.4.1, group = "GROUP")
load.fit.521.5 <- cfa(model.521.5, article521.4.1, group = "GROUP", group.equal = "loadings")
int.fit.521.5 <- cfa(model.521.5, article521.4.1, group = "GROUP", group.equal = c("loadings", "intercepts"))
all.results.521.5 <- mi.results(conf.fit.521.5,load.fit.521.5,int.fit.521.5)
all.results.521.5

# Step 4
# chisquare is significant, rmsea > 0.08, CFI < 0.95 so we reject configural invariance

# comparison 6
# Grouping variable: past vs distant future (group = 2 for future and group = 1 for past)
# Scale variable: State self-esteem scale
# Again, difference scores are calculated. We decided to do this by item and run a MI test on those.
article521.4$se1 <- article521.4$SelfEsteem_1.0 - SelfEsteem_1
article521.4$se2 <- article521.4$SelfEsteem_2.0 - SelfEsteem_2
article521.4$se3 <- article521.4$SelfEsteem_3.0 - SelfEsteem_3
article521.4$se4 <- article521.4$SelfEsteem_4.0 - SelfEsteem_4
article521.4$se5 <- article521.4$SelfEsteem_5.0 - SelfEsteem_5
article521.4$se6 <- article521.4$SelfEsteem_6.0 - SelfEsteem_6

# we only need group=2 and group=1 for this comparison, and will drop out group=0
article521.4.2 <- article521.4[article521.4[,"GROUP"] != 0,]
range(article521.4.2$GROUP)

# construct model and estimate
model.521.6 <- 'F =~ se1 + se2 + se3 + se4 + se5 + se6'
conf.fit.521.6 <- cfa(model.521.6, data = article521.4.2, group = "GROUP")
load.fit.521.6 <- cfa(model.521.6, article521.4.2, group = "GROUP", group.equal = "loadings")
int.fit.521.6 <- cfa(model.521.6, article521.4.2, group = "GROUP", group.equal = c("loadings", "intercepts"))
all.results.521.6 <- mi.results(conf.fit.521.6,load.fit.521.6,int.fit.521.6)
all.results.521.6

# Step 4
# chisquare is significant, rmsea > 0.08, CFI < 0.95 so we reject configural invariance

# comparison 7
# Grouping variable: past vs control (group = 0 for control and group = 1 for past)
# Scale variable: State self-esteem scale
# we only need group=0 and group=1 for this comparison, and will drop out group=2
article521.4.3 <- article521.4[article521.4[,"GROUP"] != 2,]
range(article521.4.3$GROUP)

# construct model and estimate
model.521.7 <- 'F =~ se1 + se2 + se3 + se4 + se5 + se6'
conf.fit.521.7 <- cfa(model.521.7, data = article521.4.3, group = "GROUP")
load.fit.521.7 <- cfa(model.521.7, article521.4.3, group = "GROUP", group.equal = "loadings")
int.fit.521.7 <- cfa(model.521.7, article521.4.3, group = "GROUP", group.equal = c("loadings", "intercepts"))
all.results.521.7 <- mi.results(conf.fit.521.7,load.fit.521.7,int.fit.521.7)
all.results.521.7

# Step 4
# chisquare is significant, rmsea > 0.08, CFI < 0.95 so we reject configural invariance

# comparison 8
# Grouping variable: time1 vs time2
# Scale variable: State-trait anxiety

# make dataset with only anxiety scores for time 1
article521.4.t1 <- cbind(article521.4$Anx1reverseT1,article521.4$Anxiety_2,article521.4$Anxiety_3,article521.4$Anx4reverseT1,article521.4$Anx5reverseT1,article521.4$Anxiety_6)
grouping <- rep(0,nrow(article521.4.t1))
article521.4.t1 <- cbind(article521.4.t1,grouping)

# make dataset with only anxiety scores for time 2
article521.4.t2 <- cbind(article521.4$Anx1reverseT2,article521.4$Anxiety_2.0,article521.4$Anxiety_3.0,article521.4$Anx4reverseT2,article521.4$Anx5reverseT2,article521.4$Anxiety_6.0)
grouping <- rep(1,nrow(article521.4.t2))
article521.4.t2 <- cbind(article521.4.t2,grouping)

# combine datasets
article521.4.1.tot <- rbind(article521.4.t1,article521.4.t2)
colnames(article521.4.1.tot) <- c("anx1","anx2","anx3","anx4","anx5","anx6","group")

# the data is ordinal, so we construct syntax and estimate model according to Wu and Estabrook
model.521.8 <- 'F =~ anx1 + anx2 + anx3 + anx4 + anx5 + anx6'

# syntax and estimate configural model
syntax.521.8.config <- measEq.syntax(model.521.8, 
                                     ID.fac = "std.lv", 
                                     ID.cat = "Wu",
                                     ordered = colnames(article521.4.1.tot[,1:6]),
                                     parameterization = "delta",
                                     data = article521.4.1.tot,
                                     group = "group", 
                                     group.equal = "configural", 
                                     orthogonal = T)

config.521.8 <- cfa(as.character(syntax.521.8.config),
                    article521.4.1.tot, 
                    ordered = colnames(article521.4.1.tot[,1:6]),
                    group = "group", 
                    estimator = "WLSMV")

# syntax and estimate thresholds model
syntax.521.8.thres <- measEq.syntax(model.521.8, 
                                     ID.fac = "std.lv", 
                                     ID.cat = "Wu",
                                     ordered = colnames(article521.4.1.tot[,1:6]),
                                     parameterization = "delta",
                                     data = article521.4.1.tot,
                                     group = "group", 
                                     group.equal = "thresholds", 
                                     orthogonal = T)

thres.521.8 <- cfa(as.character(syntax.521.8.thres),
                    article521.4.1.tot, 
                    ordered = colnames(article521.4.1.tot[,1:6]),
                    group = "group", 
                    estimator = "WLSMV")

# syntax and estimate loadings model
syntax.521.8.load <- measEq.syntax(model.521.8, 
                                     ID.fac = "std.lv", 
                                     ID.cat = "Wu",
                                     ordered = colnames(article521.4.1.tot[,1:6]),
                                     parameterization = "delta",
                                     data = article521.4.1.tot,
                                     group = "group", 
                                     group.equal = c("thresholds","loadings"), 
                                     orthogonal = T)

load.521.8 <- cfa(as.character(syntax.521.8.load),
                    article521.4.1.tot, 
                    ordered = colnames(article521.4.1.tot[,1:6]),
                    group = "group", 
                    estimator = "WLSMV")

# note that we get the scaled estimates because we have ordinal data
all.results.521.8 <- mi.results.sc(config.521.8,thres.521.8,load.521.8)
all.results.521.8

# Step 4
# chisquare is significant, rmsea > 0.08 so we reject configural invariance
# CFI is ok (0.968)

# comparison 9
# Grouping variable: time1 vs time2
# Scale variable: State self-esteem
# make dataset with only anxiety scores for time 1
article521.5.t1 <- cbind(article521.4$SelfEsteem_1,article521.4$SelfEsteem_2,article521.4$SelfEsteem_3,article521.4$SelfEsteem_4,article521.4$SelfEsteem_5,article521.4$SelfEsteem_6)
grouping <- rep(0,nrow(article521.5.t1))
article521.5.t1 <- cbind(article521.5.t1,grouping)

# make dataset with only anxiety scores for time 2
article521.5.t2 <- cbind(article521.4$SelfEsteem_1.0,article521.4$SelfEsteem_2.0,article521.4$SelfEsteem_3.0,article521.4$SelfEsteem_4.0,article521.4$SelfEsteem_5.0,article521.4$SelfEsteem_6.0)
grouping <- rep(1,nrow(article521.5.t2))
article521.5.t2 <- cbind(article521.5.t2,grouping)

# combine datasets
article521.4.2.tot <- rbind(article521.5.t1,article521.5.t2)
colnames(article521.4.2.tot) <- c("es1","es2","es3","es4","es5","es6","group")

# the data is ordinal, so we construct syntax and estimate model according to Wu and Estabrook
model.521.9 <- 'F =~ es1 + es2 + es3 + es4 + es5 + es6'

# syntax and estimate configural model
syntax.521.9.config <- measEq.syntax(model.521.9, 
                                     ID.fac = "std.lv", 
                                     ID.cat = "Wu",
                                     ordered = colnames(article521.4.2.tot[,1:6]),
                                     parameterization = "delta",
                                     data = article521.4.2.tot,
                                     group = "group", 
                                     group.equal = "configural", 
                                     orthogonal = T)

config.521.9 <- cfa(as.character(syntax.521.9.config),
                    article521.4.2.tot, 
                    ordered = colnames(article521.4.2.tot[,1:6]),
                    group = "group", 
                    estimator = "WLSMV")

# syntax and estimate thresholds model
syntax.521.9.thres <- measEq.syntax(model.521.9, 
                                    ID.fac = "std.lv", 
                                    ID.cat = "Wu",
                                    ordered = colnames(article521.4.2.tot[,1:6]),
                                    parameterization = "delta",
                                    data = article521.4.2.tot,
                                    group = "group", 
                                    group.equal = "thresholds", 
                                    orthogonal = T)

thres.521.9 <- cfa(as.character(syntax.521.9.thres),
                   article521.4.2.tot, 
                   ordered = colnames(article521.4.2.tot[,1:6]),
                   group = "group", 
                   estimator = "WLSMV")

# syntax and estimate loadings model
syntax.521.9.load <- measEq.syntax(model.521.9, 
                                   ID.fac = "std.lv", 
                                   ID.cat = "Wu",
                                   ordered = colnames(article521.4.2.tot[,1:6]),
                                   parameterization = "delta",
                                   data = article521.4.2.tot,
                                   group = "group", 
                                   group.equal = c("thresholds","loadings"), 
                                   orthogonal = T)

load.521.9 <- cfa(as.character(syntax.521.9.load),
                  article521.4.2.tot, 
                  ordered = colnames(article521.4.2.tot[,1:6]),
                  group = "group", 
                  estimator = "WLSMV")

# note that we get the scaled estimates because we have ordinal data
all.results.521.9 <- mi.results.sc(config.521.9,thres.521.9,load.521.9)
all.results.521.9

# Step 4
# chisquare is significant, rmsea > 0.08 so we reject configural invariance
# CFI is ok (0.983)

# Article 661: Woolley ----------------------------------------------------
url <- 'https://osf.io/u82mp//?action=download'
filename <- '../article661.csv'
GET(url, write_disk(filename, overwrite = TRUE))
article661 <- read.csv2(filename, header=T, na.strings="NA", sep=",")

# Grouping variable: Consumption shared vs separate (cond_all)
# Scale variable: Perceived closeness (likabe + get_along + close + friends)
# construct model and estimate
model.661 <- 'F =~ likabe + get_along + close + friends'
conf.fit.661 <- cfa(model.661, data = article661, group = "cond_all")
load.fit.661 <- cfa(model.661, article661, group = "cond_all", group.equal = "loadings")
int.fit.661 <- cfa(model.661, article661, group = "cond_all", group.equal = c("loadings", "intercepts"))
all.results.661 <- mi.results(conf.fit.661,load.fit.661,int.fit.661)
all.results.661

# Step 4
# chisquare is significant, rmsea > 0.08 and CFI < 0.95 so we reject configural invariance

# Article 791: Sun --------------------------------------------------------

# before checking and finishing this one: please check the codebook first, and then go to the article
# to find the effect we want to test. We may have miscoded this study? 
# it seems to be an ESM study

url <- 'https://osf.io/v25y7//?action=download'
filename <- '../article791.csv'
GET(url, write_disk(filename, overwrite = TRUE))
article791 <- read.csv2(filename, header=T, na.strings="NA", sep=",")

# remove all irrelevant variables from dataframe
article791 <- article791[,c(5,14,15,16)]
colnames(article791)[c(5,14,15,16)]

# Grouping variable: time point (four timepoints; ear.PRO03.w1)
# Scale variable: Modified Big 5 - Neuroticism (worried, relaxed_r, depressed; ear.BFI19.w1 + ear.BFI09r.w1 + ear.BFI04.w1)

# the data is ordinal, so we construct syntax and estimate model according to Wu and Estabrook
model.791 <- 'F =~ ear.BFI19.w1 + ear.BFI09r.w1 + ear.BFI04.w1'

# syntax and estimate configural model
syntax.791.config <- measEq.syntax(model.791, 
                                     ID.fac = "std.lv", 
                                     ID.cat = "Wu",
                                     ordered = colnames(article791)[c(5,14,15,16)],
                                     parameterization = "delta",
                                     data = article791,
                                     group = "ear.PRO03.w1", 
                                     group.equal = "configural", 
                                     orthogonal = T)

config.791 <- cfa(as.character(syntax.791.config),
                  article791, 
                    ordered = c("ear.BFI19.w1,ear.BFI09r.w1,ear.BFI04.w1"),
                    group = "ear.PRO03.w1", 
                    estimator = "WLSMV")

# syntax and estimate thresholds model
syntax.791.thres <- measEq.syntax(model.791, 
                                    ID.fac = "std.lv", 
                                    ID.cat = "Wu",
                                    ordered = c("ear.BFI19.w1,ear.BFI09r.w1,ear.BFI04.w1"),
                                    parameterization = "delta",
                                    data = article791,
                                    group = "ear.PRO03.w1", 
                                    group.equal = "thresholds", 
                                    orthogonal = T)

thres.791 <- cfa(as.character(syntax.791.thres),
                   article791, 
                   ordered = c("ear.BFI19.w1,ear.BFI09r.w1,ear.BFI04.w1"),
                   group = "ear.PRO03.w1", 
                   estimator = "WLSMV")

# syntax and estimate loadings model
syntax.791.load <- measEq.syntax(model.791, 
                                   ID.fac = "std.lv", 
                                   ID.cat = "Wu",
                                   ordered = c("ear.BFI19.w1,ear.BFI09r.w1,ear.BFI04.w1"),
                                   parameterization = "delta",
                                   data = article791,
                                   group = "ear.PRO03.w1", 
                                   group.equal = c("thresholds","loadings"), 
                                   orthogonal = T)

load.791 <- cfa(as.character(syntax.791.load),
                  article791, 
                  ordered = c("ear.BFI19.w1,ear.BFI09r.w1,ear.BFI04.w1"),
                  group = "ear.PRO03.w1", 
                  estimator = "WLSMV")

# note that we get the scaled estimates because we have ordinal data
all.results.791 <- mi.results.sc(config.791,thres.791,load.791)
all.results.791


# Article 811: Catapano -----------------------------------------------------
# Study 1
url <- 'https://osf.io/9sg3u//?action=download'
filename <- '../article811.1.csv'
GET(url, write_disk(filename, overwrite = TRUE))
article811.1 <- read.csv2(filename, header=T, na.strings="NA", sep=",")

# Comparison 1
# Grouping variable: control and perspective-taking (condition)
# Scale variable: receptiveness (Q3.receparg + Q4.recepoarg + Q5recepgen + Q6recepogen)
model.811.1.1 <- 'F =~ Q3.receparg + Q4.recepoarg + Q5.recepgen + Q6.recepogen'
conf.fit.811.1.1 <- cfa(model.811.1.1, data = article811.1, group = "condition")
load.fit.811.1.1 <- cfa(model.811.1.1, article811.1, group = "condition", group.equal = "loadings")
int.fit.811.1.1 <- cfa(model.811.1.1, article811.1, group = "condition", group.equal = c("loadings", "intercepts"))
all.results.811.1.1 <- mi.results(conf.fit.811.1.1,load.fit.811.1.1,int.fit.811.1.1)
all.results.811.1.1

# Step 4
# chisquare is significant, rmsea > 0.08 and CFI < 0.95 so we reject configural invariance

# Comparison 2
# Grouping variable: control and perspective-taking (condition)
# Scale variable: value congruence (Q7.valueco + Q7.valuewv + Q8.valuemoral)
model.811.1.2 <- 'F =~ Q7.valueco + Q7.valuewv + Q8.valuemoral'
conf.fit.811.1.2 <- cfa(model.811.1.2, data = article811.1, group = "condition")
load.fit.811.1.2 <- cfa(model.811.1.2, article811.1, group = "condition", group.equal = "loadings")
int.fit.811.1.2 <- cfa(model.811.1.2, article811.1, group = "condition", group.equal = c("loadings", "intercepts"))
all.results.811.1.2 <- mi.results(conf.fit.811.1.2,load.fit.811.1.2,int.fit.811.1.2)
all.results.811.1.2

# Step 4
# 3 items so we assume configural invariance
# metric: nonsig chisquare change, change RMSEA and CFI < .01; lower AIC and BIC; metric invariance holds
# scalar: nonsig chisquare change, change RMSEA and CFI < .01; lower AIC and BIC; scalar invariance holds.

# Study 2
url <- 'https://osf.io/ymc3h//?action=download'
filename <- '../article811.2.csv'
GET(url, write_disk(filename, overwrite = TRUE))
article811.2 <- read.csv2(filename, header=T, na.strings="NA", sep=",")

# Comparison 3
# Grouping variable: perspective-taking/different-ideology condition + perspective-taking/same-ideology condition + control condition (condition)
# Scale variable: receptiveness (recep1.arg + recep2.arg + recep3.gen + recep4.gen)
model.811.2.1 <- 'F =~ recep1.arg + recep2.arg + recep3.gen + recep4.gen'
conf.fit.811.2.1 <- cfa(model.811.2.1, data = article811.2, group = "condition")
load.fit.811.2.1 <- cfa(model.811.2.1, article811.2, group = "condition", group.equal = "loadings")
int.fit.811.2.1 <- cfa(model.811.2.1, article811.2, group = "condition", group.equal = c("loadings", "intercepts"))
all.results.811.2.1 <- mi.results(conf.fit.811.2.1,load.fit.811.2.1,int.fit.811.2.1)
all.results.811.2.1

# Step 4
# chisquare is significant, rmsea > 0.08 and CFI < 0.95 so we reject configural invariance

# Comparison 4
# Grouping variable: perspective-taking/different-ideology condition + perspective-taking/same-ideology condition + control condition (condition)
# Scale variable: value congruence (Q3.congruent + Q4.fitworldview + Q5.morals)
model.811.2.2 <- 'F =~ Q3.congruent + Q4.fitworldview + Q5.morals'
conf.fit.811.2.2 <- cfa(model.811.2.2, data = article811.2, group = "condition")
load.fit.811.2.2 <- cfa(model.811.2.2, article811.2, group = "condition", group.equal = "loadings")
int.fit.811.2.2 <- cfa(model.811.2.2, article811.2, group = "condition", group.equal = c("loadings", "intercepts"))
all.results.811.2.2 <- mi.results(conf.fit.811.2.2,load.fit.811.2.2,int.fit.811.2.2)
all.results.811.2.2

# Step 4
# 3 items so we assume configural invariance
# metric: nonsig chisquare change, change CFI < .01; lower AIC and BIC; metric invariance holds (RMSEA change > .01)
# scalar: nonsig chisquare change, change CFI < .01; lower AIC and BIC; scalar invariance holds (RMSEA change > .01 but fit improves)



# Article 861: Obrien -----------------------------------------------------
# Study 1
url <- 'https://osf.io/bdq3m//?action=download'
filename <- '../article861.1.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article861.1 <- read_sav(filename)

# comparison 1
# grouping variable: giving condition and getting condition (condition)
# scaling variable: day-end happiness (DaysEnd_good.1 + DaysEnd_happy.1 + DaysEnd_satisfied.1 + DaysEnd_mood.1)

# the authors seemingly collapse all variables over time and then test them per condition, so we will do the same.
article861.1$daysend.good <- as.numeric((article861.1$DaysEnd_good.1 + article861.1$DaysEnd_good.2 + article861.1$DaysEnd_good.3 + article861.1$DaysEnd_good.4 + article861.1$DaysEnd_good.5)/5)
article861.1$daysend.happy <- as.numeric((article861.1$DaysEnd_happy.1 + article861.1$DaysEnd_happy.2 + article861.1$DaysEnd_happy.3 + article861.1$DaysEnd_happy.4 + article861.1$DaysEnd_happy.5)/5)
article861.1$daysend.satisfied <- as.numeric((article861.1$DaysEnd_satisfied.1 + article861.1$DaysEnd_satisfied.2 + article861.1$DaysEnd_satisfied.3 + article861.1$DaysEnd_satisfied.4 + article861.1$DaysEnd_satisfied.5)/5)
article861.1$daysend.mood <- as.numeric((article861.1$DaysEnd_mood.1 + article861.1$DaysEnd_mood.2 + article861.1$DaysEnd_mood.3 + article861.1$DaysEnd_mood.4 + article861.1$DaysEnd_mood.5)/5)
article861.1$condition <- as.numeric(article861.1$condition)

model.861.1 <- 'F =~ daysend.good + daysend.happy + daysend.satisfied + daysend.mood'
conf.fit.861.1 <- cfa(model.861.1, data = article861.1, group = "condition")
load.fit.861.1 <- cfa(model.861.1, article861.1, group = "condition", group.equal = "loadings")
int.fit.861.1 <- cfa(model.861.1, article861.1, group = "condition", group.equal = c("loadings", "intercepts"))
all.results.861.1 <- mi.results(conf.fit.861.1,load.fit.861.1,int.fit.861.1)
all.results.861.1

# Step 4
# Configural: chisquare not significant, rmsea < .08, CFI > .95. Configural invariance holds
# Metric: chisquare change not significant, change rmsea = 0.033, change CFI = -0.003, AIC is lower than config, BIC is lower than config
# meaning three out of four criteria adhere to our criteria (only rmsea change is too large); metric invariance holds
# Scalar: chisquare change not significant, change RMSEA = 0.002, change CFI = -0.002, AIC and BIC both lower; scalar invariance holds

# comparison 2
# grouping variable: giving condition and getting condition
# scaling variable: recalled-happiness

# the authors collapse all variables over time and then test them per condition, so we will do the same.
article861.1$recalled.good <- (article861.1$Recalled_good.1 + article861.1$Recalled_good.2 + article861.1$Recalled_good.3 + article861.1$Recalled_good.4 + article861.1$Recalled_good.5)/5
article861.1$recalled.happy <- (article861.1$Recalled_happy.1 + article861.1$Recalled_happy.2 + article861.1$Recalled_happy.3 + article861.1$Recalled_happy.4 + article861.1$Recalled_happy.5)/5
article861.1$recalled.satisfied <- (article861.1$Recalled_satisfied.1 + article861.1$Recalled_satisfied.2 + article861.1$Recalled_satisfied.3 + article861.1$Recalled_satisfied.4 + article861.1$Recalled_satisfied.5)/5
article861.1$recalled.mood <- (article861.1$Recalled_mood.1 + article861.1$Recalled_mood.2 + article861.1$Recalled_mood.3 + article861.1$Recalled_mood.4 + article861.1$Recalled_mood.5)/5

model.861.2 <- 'F =~ recalled.good + recalled.happy + recalled.satisfied + recalled.mood'
conf.fit.861.2 <- cfa(model.861.2, data = article861.1, group = "condition")
load.fit.861.2 <- cfa(model.861.2, article861.1, group = "condition", group.equal = "loadings")
int.fit.861.2 <- cfa(model.861.2, article861.1, group = "condition", group.equal = c("loadings", "intercepts"))
all.results.861.2 <- mi.results(conf.fit.861.2,load.fit.861.2,int.fit.861.2)
all.results.861.2

# Step 4
# chisquare significant, rmsea > .08, so we reject configural invariance
# CFI is ok (0.982)

# comparison 3
# grouping variable: giving condition and getting condition
# scaling variable: study-happiness
article861.1$study.exciting <- as.numeric((article861.1$Study_exciting.1 + article861.1$Study_exciting.2 + article861.1$Study_exciting.3 + article861.1$Study_exciting.4 + article861.1$Study_exciting.5)/5)
article861.1$study.enjoyable <- as.numeric((article861.1$Study_enjoyable.1 + article861.1$Study_enjoyable.2 + article861.1$Study_enjoyable.3 + article861.1$Study_enjoyable.4 + article861.1$Study_enjoyable.5)/5)
article861.1$study.rewarding <- as.numeric((article861.1$Study_rewarding.1 + article861.1$Study_rewarding.2 + article861.1$Study_rewarding.3 + article861.1$Study_rewarding.4 + article861.1$Study_rewarding.5)/5)

model.861.3 <- 'F =~ study.exciting + study.enjoyable + study.rewarding'
conf.fit.861.3 <- cfa(model.861.3, data = article861.1, group = "condition")
load.fit.861.3 <- cfa(model.861.3, article861.1, group = "condition", group.equal = "loadings")
int.fit.861.3 <- cfa(model.861.3, article861.1, group = "condition", group.equal = c("loadings", "intercepts"))
all.results.861.3 <- mi.results(conf.fit.861.3,load.fit.861.3,int.fit.861.3)
all.results.861.3

# Step 4
# 3 items so we assume configural invariance
# metric: sig chisquare change, change RMSEA and CFI > .01; larger AIC; metric invariance rejected

# comparison 4
# grouping variable: giving condition and getting condition
# scaling variable: overall happiness
model.861.4 <- 'F =~ daysend.good + daysend.happy + daysend.satisfied + daysend.mood + recalled.good + recalled.happy + recalled.satisfied + recalled.mood + study.exciting + study.enjoyable + study.rewarding'
conf.fit.861.4 <- cfa(model.861.4, data = article861.1, group = "condition")
load.fit.861.4 <- cfa(model.861.4, article861.1, group = "condition", group.equal = "loadings")
int.fit.861.4 <- cfa(model.861.4, article861.1, group = "condition", group.equal = c("loadings", "intercepts"))
all.results.861.4 <- mi.results(conf.fit.861.4,load.fit.861.4,int.fit.861.4)
all.results.861.4

# Step 4
# chisquare significant, rmsea > .08, CFI < .95 so we reject configural invariance

# comparison 5
# grouping variable: time1 to time5
# scaling variable: day-end happiness

# make 5 datasets, each for a timepoint
# t1
article861.1.1 <- cbind(article861.1$DaysEnd_good.1, article861.1$DaysEnd_happy.1, article861.1$DaysEnd_satisfied.1, article861.1$DaysEnd_mood.1,
                        article861.1$Recalled_good.1, article861.1$Recalled_happy.1, article861.1$Recalled_satisfied.1, article861.1$Recalled_mood.1,
                        article861.1$Study_exciting.1, article861.1$Study_enjoyable.1, article861.1$Study_rewarding.1)
grouping <- rep(1,nrow(article861.1.1))
article861.1.1 <- cbind(article861.1.1,grouping)

#t2
article861.1.2 <- cbind(article861.1$DaysEnd_good.2, article861.1$DaysEnd_happy.2, article861.1$DaysEnd_satisfied.2, article861.1$DaysEnd_mood.2,
                        article861.1$Recalled_good.2, article861.1$Recalled_happy.2, article861.1$Recalled_satisfied.2, article861.1$Recalled_mood.2,
                        article861.1$Study_exciting.2, article861.1$Study_enjoyable.2, article861.1$Study_rewarding.2)
grouping <- rep(2,nrow(article861.1.2))
article861.1.2 <- cbind(article861.1.2,grouping)

#t3
article861.1.3 <- cbind(article861.1$DaysEnd_good.3, article861.1$DaysEnd_happy.3, article861.1$DaysEnd_satisfied.3, article861.1$DaysEnd_mood.3,
                        article861.1$Recalled_good.3, article861.1$Recalled_happy.3, article861.1$Recalled_satisfied.3, article861.1$Recalled_mood.3,
                        article861.1$Study_exciting.3, article861.1$Study_enjoyable.3, article861.1$Study_rewarding.3)
grouping <- rep(3,nrow(article861.1.3))
article861.1.3 <- cbind(article861.1.3,grouping)

#t4
article861.1.4 <- cbind(article861.1$DaysEnd_good.4, article861.1$DaysEnd_happy.4, article861.1$DaysEnd_satisfied.4, article861.1$DaysEnd_mood.4,
                        article861.1$Recalled_good.4, article861.1$Recalled_happy.4, article861.1$Recalled_satisfied.4, article861.1$Recalled_mood.4,
                        article861.1$Study_exciting.4, article861.1$Study_enjoyable.4, article861.1$Study_rewarding.4)
grouping <- rep(4,nrow(article861.1.4))
article861.1.4 <- cbind(article861.1.4,grouping)

#t5
article861.1.5 <- cbind(article861.1$DaysEnd_good.5, article861.1$DaysEnd_happy.5, article861.1$DaysEnd_satisfied.5, article861.1$DaysEnd_mood.5,
                        article861.1$Recalled_good.5, article861.1$Recalled_happy.5, article861.1$Recalled_satisfied.5, article861.1$Recalled_mood.5,
                        article861.1$Study_exciting.5, article861.1$Study_enjoyable.5, article861.1$Study_rewarding.5)
grouping <- rep(5,nrow(article861.1.5))
article861.1.5 <- cbind(article861.1.5,grouping)

# merge datasets
article861.1.tot <- rbind(article861.1.1,article861.1.2,article861.1.3,article861.1.4,article861.1.5)
colnames(article861.1.tot) <- c("end.good","end.happy","end.satis","end.mood","rec.good","rec.happy","rec.satis","rec.mood","stud.ex","stud.en","stud.re","time")

# scaling variable: day-end happiness
model.861.5 <- 'F =~ end.good + end.happy + end.satis + end.mood'
conf.fit.861.5 <- cfa(model.861.5, data = article861.1.tot, group = "time")
load.fit.861.5 <- cfa(model.861.5, article861.1.tot, group = "time", group.equal = "loadings")
int.fit.861.5 <- cfa(model.861.5, article861.1.tot, group = "time", group.equal = c("loadings", "intercepts"))
all.results.861.5 <- mi.results(conf.fit.861.5,load.fit.861.5,int.fit.861.5)
all.results.861.5

# Step 4
# chisquare significant, rmsea > .08 so we reject configural invariance
# CFI ok (0.996)

# comparison 6
# grouping variable: time1 to time5
# scaling variable: recall happiness
model.861.6 <- 'F =~ rec.good + rec.happy + rec.satis + rec.mood'
conf.fit.861.6 <- cfa(model.861.6, data = article861.1.tot, group = "time")
load.fit.861.6 <- cfa(model.861.6, article861.1.tot, group = "time", group.equal = "loadings")
int.fit.861.6 <- cfa(model.861.6, article861.1.tot, group = "time", group.equal = c("loadings", "intercepts"))
all.results.861.6 <- mi.results(conf.fit.861.6,load.fit.861.6,int.fit.861.6)
all.results.861.6

# Step 4
# chisquare significant, rmsea > .08 so we reject configural invariance
# CFI ok (0.996)

# comparison 7
# grouping variable: time1 to time5
# scaling variable: study happiness
model.861.7 <- 'F =~ stud.ex + stud.en + stud.re'
conf.fit.861.7 <- cfa(model.861.7, data = article861.1.tot, group = "time")
load.fit.861.7 <- cfa(model.861.7, article861.1.tot, group = "time", group.equal = "loadings")
int.fit.861.7 <- cfa(model.861.7, article861.1.tot, group = "time", group.equal = c("loadings", "intercepts"))
all.results.861.7 <- mi.results(conf.fit.861.7,load.fit.861.7,int.fit.861.7)
all.results.861.7

# Step 4
# 3 items so we assume configural invariance
# metric: nonsig chisquare change, change RMSEA and CFI < .01; lower AIC and BIC; metric invariance holds
# scalar: nonsig chisquare change, change RMSEA and CFI < .01; lower AIC and BIC; scalar invariance holds.


# comparison 8
# grouping variable: time1 to time5
# scaling variable: overall happiness
model.861.8 <- 'F =~ end.good + end.happy + end.satis + end.mood + rec.good + rec.happy + rec.satis + rec.mood + stud.ex + stud.en + stud.re'
conf.fit.861.8 <- cfa(model.861.8, data = article861.1.tot, group = "time")
load.fit.861.8 <- cfa(model.861.8, article861.1.tot, group = "time", group.equal = "loadings")
int.fit.861.8 <- cfa(model.861.8, article861.1.tot, group = "time", group.equal = c("loadings", "intercepts"))
all.results.861.8 <- mi.results(conf.fit.861.8,load.fit.861.8,int.fit.861.8)
all.results.861.8

# Step 4
# chisquare significant, rmsea > .08, CFI < 0.95 so we reject configural invariance

# Study 2
url <- 'https://osf.io/kz8q2//?action=download'
filename <- '../article861.2.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article861.2 <- read_sav(filename)

# comparison 9
# grouping variable: giving condition and getting condition
# scale variable: happiness block (happy, elated, joy)

# first we collapse all round 1-10 together
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
all.results.861.9 <- mi.results(conf.fit.861.9,load.fit.861.9,int.fit.861.9)
all.results.861.9

# Step 4
# 3 items so we assume configural invariance
# metric: sig chisquare change, change RMSEA > .01; AIC larger, we reject metric invariance (CFI change and BIC ok)

# comparison 10
# grouping variable: round 1 to round 10
# scale variable: happiness block

# first we split the datasets per timepoint 
article861.2.1 <- cbind(article861.2$round1happy,article861.2$round1elated,article861.2$round1joy)
article861.2.2 <- cbind(article861.2$round2happy,article861.2$round2elated,article861.2$round2joy)
article861.2.3 <- cbind(article861.2$round3happy,article861.2$round3elated,article861.2$round3joy)
article861.2.4 <- cbind(article861.2$round4happy,article861.2$round4elated,article861.2$round4joy)
article861.2.5 <- cbind(article861.2$round5happy,article861.2$round5elated,article861.2$round5joy)
article861.2.6 <- cbind(article861.2$round6happy,article861.2$round6elated,article861.2$round6joy)
article861.2.7 <- cbind(article861.2$round7happy,article861.2$round7elated,article861.2$round7joy)
article861.2.8 <- cbind(article861.2$round8happy,article861.2$round8elated,article861.2$round8joy)
article861.2.9 <- cbind(article861.2$round9happy,article861.2$round9elated,article861.2$round9joy)
article861.2.10 <- cbind(article861.2$round10happy,article861.2$round10elated,article861.2$round10joy)
article861.2.tot <- rbind(article861.2.1,article861.2.2,article861.2.3,article861.2.4,article861.2.5,
                          article861.2.6,article861.2.7,article861.2.8,article861.2.9,article861.2.10)
grouping <- rep(1:10,each=502); nrow(article861.2.tot) == length(grouping)
article861.2.tot <- cbind(article861.2.tot,grouping)
colnames(article861.2.tot) <- c("happy","elated","joy","time")

model.861.10 <- 'F =~ happy + elated + joy'
conf.fit.861.10 <- cfa(model.861.10, data = article861.2.tot, group = "time")
load.fit.861.10 <- cfa(model.861.10, article861.2.tot, group = "time", group.equal = "loadings")
int.fit.861.10 <- cfa(model.861.10, article861.2.tot, group = "time", group.equal = c("loadings", "intercepts"))
all.results.861.10 <- mi.results(conf.fit.861.10,load.fit.861.10,int.fit.861.10)
all.results.861.10

# Step 4
# 3 items so we assume configural invariance
# metric: nonsig chisquare change, change RMSEA and CFI < .01; lower AIC and BIC; metric invariance holds
# scalar: nonsig chisquare change, change CFI < .01; lower AIC and BIC; scalar invariance holds (change RMSEA > .01).



# Article 891: Madan ------------------------------------------------------
# Study 3
url <- 'https://osf.io/yk75c//?action=download'
filename <- '../article891.1.xlsx'
GET(url, write_disk(filename, overwrite = TRUE))
article891.1 <- read_excel(filename, sheet = 5)

# comparison 1
# grouping variable: fixed or growth mindset (cond)
# scale variable: support for resettling refugees (Refugees1 + Refugees2 + Refugees3 + Refugees4)
model.891.1 <- 'F =~ Refugees1 + Refugees2 + Refugees3 + Refugees4'
conf.fit.891.1 <- cfa(model.891.1, data = article891.1, group = "cond")
load.fit.891.1 <- cfa(model.891.1, article891.1, group = "cond", group.equal = "loadings")
int.fit.891.1 <- cfa(model.891.1, article891.1, group = "cond", group.equal = c("loadings", "intercepts"))
all.results.891.1 <- mi.results(conf.fit.891.1,load.fit.891.1,int.fit.891.1)
all.results.891.1

# Step 4
# Configural: chisquare nonsig, rmsea < .08, CFI > .95; configural holds
# metric: nonsig chisquare change, change RMSEA and CFI < .01; lower AIC and BIC; metric invariance holds
# scalar: nonsig chisquare change, change CFI < .01; lower AIC and BIC; scalar invariance holds (RMSEA change > .01 but fit improves).


# comparison 2
# grouping variable: fixed or growth mindset (cond)
# scale variable: political orientation (political1 + political2 + political3)
model.891.2 <- 'F =~ political1 + political2 + political3'
conf.fit.891.2 <- cfa(model.891.2, data = article891.1, group = "cond")
load.fit.891.2 <- cfa(model.891.2, article891.1, group = "cond", group.equal = "loadings")
int.fit.891.2 <- cfa(model.891.2, article891.1, group = "cond", group.equal = c("loadings", "intercepts"))
all.results.891.2 <- mi.results(conf.fit.891.2,load.fit.891.2,int.fit.891.2)
all.results.891.2

# Step 4
# 3 items so we assume configural invariance
# metric: nonsig chisquare change, change CFI < .01; lower AIC and BIC; metric invariance holds (change RMSEA > .01)
# scalar: nonsig chisquare change, change CFI < .01; lower AIC and BIC; scalar invariance holds (change RMSEA > .01 but fit improves).


# Study 4
url <- 'https://osf.io/yk75c//?action=download'
filename <- '../article891.2.xlsx'
GET(url, write_disk(filename, overwrite = TRUE))
article891.2 <- read_excel(filename, sheet = 7)

# comparison 3
# grouping variable: fixed or growth mindset (cond)
# scale variable: participants’ beliefs about how well refugees can assimilate in society (assim1 + assim2 + assim3 + assim4 + assim5)
model.891.3 <- 'F =~ assim1 + assim2 + assim3 + assim4 + assim5'
conf.fit.891.3 <- cfa(model.891.3, data = article891.2, group = "cond")
load.fit.891.3 <- cfa(model.891.3, article891.2, group = "cond", group.equal = "loadings")
int.fit.891.3 <- cfa(model.891.3, article891.2, group = "cond", group.equal = c("loadings", "intercepts"))
all.results.891.3 <- mi.results(conf.fit.891.3,load.fit.891.3,int.fit.891.3)
all.results.891.3

# Step 4
# chisquare significant, rmsea > 0.08, so we reject configural invariance
# CFI is ok (0.986)

# comparison 4
# grouping variable: fixed or growth mindset (cond)
# scale variable: support for resettling refugees (Refugees1 + Refugees2 + Refugees3 + Refugees4)
model.891.4 <- 'F =~ Refugees1 + Refugees2 + Refugees3 + Refugees4'
conf.fit.891.4 <- cfa(model.891.4, data = article891.2, group = "cond")
load.fit.891.4 <- cfa(model.891.4, article891.2, group = "cond", group.equal = "loadings")
int.fit.891.4 <- cfa(model.891.4, article891.2, group = "cond", group.equal = c("loadings", "intercepts"))
all.results.891.4 <- mi.results(conf.fit.891.4,load.fit.891.4,int.fit.891.4)
all.results.891.4

# Step 4
# chisquare significant, rmsea > 0.08, so we reject configural invariance
# CFI is ok (0.980)

# comparison 5
# grouping variable: fixed or growth mindset (cond)
# scale variable: political orientation (political1 + political2 + political3)
model.891.5 <- 'F =~ political1 + political2 + political3'
conf.fit.891.5 <- cfa(model.891.5, data = article891.2, group = "cond")
load.fit.891.5 <- cfa(model.891.5, article891.2, group = "cond", group.equal = "loadings")
int.fit.891.5 <- cfa(model.891.5, article891.2, group = "cond", group.equal = c("loadings", "intercepts"))
all.results.891.5 <- mi.results(conf.fit.891.5,load.fit.891.5,int.fit.891.5)
all.results.891.5

# Step 4
# 3 items so we assume configural invariance
# metric: nonsig chisquare change, change CFI < .01; lower BIC; 
# but: change RMSEA > .01 and AIC is larger; because chisquare change is non-significant, we cannot reject metric invariance; metric invariance holds
# scalar: nonsig chisquare change, change CFI < .01; lower AIC and BIC; scalar invariance holds (RMSEA change > .01 but fit improves).


# Study 5 - THESE ARE THE ORANGE ROWS WHICH I THINK WE SHOULD REMOVE
url <- 'https://osf.io/yk75c//?action=download'
filename <- '../article891.3.xlsx'
GET(url, write_disk(filename, overwrite = TRUE))
article891.3 <- read_excel(filename, sheet = 9)

# comparison 6
# grouping variable: fixed or growth mindset fixed or growth mindset (average of growthpersonality1 + growthpersonality2 + growthpersonality3 as found in syntax here https://osf.io/4ey2h/)
# scale variable: belief that refugees can assimilate in American society (can1 + can2 + can3 + can4 + can5)
article891.3$cond <- (article891.3$growthpersonality1 + article891.3$growthpersonality2 + article891.3$growthpersonality3)/3
# they use this variable in a regression, so they don't have dummy groups. Should we delete all rows from study 5 from all our codebooks then
# since we only used categorical grouping variables?

model.891.6 <- 'F =~ can1 + can2 + can3 + can4 + can5'
conf.fit.891.6 <- cfa(model.891.6, data = article891.3, group = "cond")
load.fit.891.6 <- cfa(model.891.6, article891.3, group = "cond", group.equal = "loadings")
int.fit.891.6 <- cfa(model.891.6, article891.3, group = "cond", group.equal = c("loadings", "intercepts"))
all.results.891.6 <- mi.results(conf.fit.891.6,load.fit.891.6,int.fit.891.6)
all.results.891.6

# comparison 7
# grouping variable: fixed or growth mindset (average of growthpersonality1 + growthpersonality2 + growthpersonality3 as found in syntax here https://osf.io/4ey2h/)
# scale variable: belief that refugees should assimilate in American society (should1 + should2 + should3 + should4 + should5)
model.891.7 <- 'F =~ should1 + should2 + should3 + should4 + should5'
conf.fit.891.7 <- cfa(model.891.7, data = article891.3, group = "cond")
load.fit.891.7 <- cfa(model.891.7, article891.3, group = "cond", group.equal = "loadings")
int.fit.891.7 <- cfa(model.891.7, article891.3, group = "cond", group.equal = c("loadings", "intercepts"))
all.results.891.7 <- mi.results(conf.fit.891.7,load.fit.891.7,int.fit.891.7)
all.results.891.7

# comparison 8
# grouping variable: fixed or growth mindset (average of growthpersonality1 + growthpersonality2 + growthpersonality3 as found in syntax here https://osf.io/4ey2h/)
# scale variable: political orientation (political1 + political2 + political3)
model.891.8 <- 'F =~ political1 + political2 + political3'
conf.fit.891.8 <- cfa(model.891.8, data = article891.3, group = "cond")
load.fit.891.8 <- cfa(model.891.8, article891.3, group = "cond", group.equal = "loadings")
int.fit.891.8 <- cfa(model.891.8, article891.3, group = "cond", group.equal = c("loadings", "intercepts"))
all.results.891.8 <- mi.results(conf.fit.891.8,load.fit.891.8,int.fit.891.8)
all.results.891.8


# Article 941: Cao --------------------------------------------------------
# Study 2
url <- 'https://osf.io/4yu27//?action=download'
filename <- '../article941.Rda'
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
all.results.941 <- mi.results(conf.fit.941,load.fit.941,int.fit.941)
all.results.941

# Step 4
# chisquare significant, rmsea > .08, so we reject configural invariance
# CFI ok (0.974)


# Article 1031: Bartholow -------------------------------------------------
url <- 'https://osf.io/zjxhw//?action=download'
filename <- '../article1031.csv'
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


# Article 1081: Kim -------------------------------------------------------
url <- 'https://osf.io/e25yk//?action=download'
filename <- '../article1081.csv'
GET(url, write_disk(filename, overwrite = TRUE))
article1081 <- read.csv2(filename, header=T, na.strings="NA", sep=",")

# Comparison 1
# Grouping variable: Incremental theorists and entity theorists
# Scale variable: Cost perception

# There is a variable "PerceivedCostofHelpSeeking" in the dataset, which according to the values seems to be the combination of the 
# seven perceived cost of receiving help measure as referenced here https://osf.io/knygt/. As such, we cannot construct a scale
# because we do not have the item scores. No correlations reported in the article either so we can't do a MI test. 
  
# Comparison 2
# Grouping variable: anthropomorphized vs nonanthropomorphized helper (Anthropomorphism)
# Scale variable: Cost perception
  
# There is a variable "PerceivedCostofHelpSeeking" in the dataset, which according to the values seems to be the combination of the 
# seven perceived cost of receiving help measure as referenced here https://osf.io/knygt/. As such, we cannot construct a scale
# because we do not have the item scores. No correlations reported in the article either so we can't do a MI test. 


# Article 1111: Ackerman --------------------------------------------------
# Study 2b
# Comparison 1-3
url <- 'https://osf.io/ev5yq//?action=download'
filename <- '../article1111.1.sav'
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
all.results.1111.1 <- mi.results(conf.fit.1111.1,load.fit.1111.1,int.fit.1111.1)
all.results.1111.1

# Step 4
# Chisquare significant, rmsea > .08, so we reject configural invariance
# CFI is ok (0.953)

# Comparison 2: 
# Grouping variable: control and pathogen threat (cond)
# Scale variable: Appearance solution (app1 + app2 + app3 + app4 + app5)
model.1111.2 <- 'F =~ app1 + app2 + app3 + app4 + app5'
conf.fit.1111.2 <- cfa(model.1111.2, data = article1111.1, group = "cond")
load.fit.1111.2 <- cfa(model.1111.2, article1111.1, group = "cond", group.equal = "loadings")
int.fit.1111.2 <- cfa(model.1111.2, article1111.1, group = "cond", group.equal = c("loadings", "intercepts"))
all.results.1111.2 <- mi.results(conf.fit.1111.2,load.fit.1111.2,int.fit.1111.2)
all.results.1111.2

# Step 4
# Chisquare significant, rmsea > .08, CFI < .95, so we reject configural invariance

# Comparison 3: 
# Grouping variable: control and pathogen threat (cond)
# Scale variable: Other solution (control1 + control2 + control3 + control4)
model.1111.3 <- 'F =~ control1 + control2 + control3 + control4'
conf.fit.1111.3 <- cfa(model.1111.3, data = article1111.1, group = "cond")
load.fit.1111.3 <- cfa(model.1111.3, article1111.1, group = "cond", group.equal = "loadings")
int.fit.1111.3 <- cfa(model.1111.3, article1111.1, group = "cond", group.equal = c("loadings", "intercepts"))
all.results.1111.3 <- mi.results(conf.fit.1111.3,load.fit.1111.3,int.fit.1111.3)
all.results.1111.3

# Step 4
# Chisquare non-significant, rmsea < .08, CFI > .95, so configural invariance holds
# Nonsig chisquare difference, RMSEA and CFI difference is 0, AIC and BIC are smaller; metric invariance holds
# Nonsig chisquare difference, RMSEA and CFI difference is 0, AIC and BIC are smaller; scalar invariance holds

# Study 3
# Comparison 4-6
url <- 'https://osf.io/5fubx//?action=download'
filename <- '../article1111.2.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article1111.2 <- read_sav(filename)
article1111.2$condition <- as.numeric(article1111.2$condition)

# From the manuscript
# In the second task, participants rated their liking for (−5 = dislike, 5 = like) and likelihood of purchasing (0 = not at all, 10 = very) 
# 17 consumer products (chosen based on a pretest reported in the Supplemental Material). These consisted of 
# appearance products (liposuction, cosmetic plastic surgery, diet pills, cosmetics/makeup, blemish cream, facial makeover), 
# hygiene products (soap, shampoo, toothpaste, toilet paper, dental floss, antiseptic hand wipes), and 
# unrelated household products (batteries, light bulbs, aluminum foil, music files/CDs, plastic utensils). 

# From the supplemental material (https://journals.sagepub.com/doi/suppl/10.1177/0956797617733829):
# Scores on the liking and purchase likelihood ratings correlated highly within product items (rs > .55), and thus three 
# overall composites were created for appearance (Cronbach’s α = .91), hygiene (Cronbach’s α = .88), and 
# household (Cronbach’s α = .81) products.

# Comparison 4: 
# Grouping variable: control and pathogen threat (condition)
# Scale variable: Appearance; the dataset has all 6 items and all items have a liking/purchasing answer, so 12 items in total
# we decided not to combine them like the supplemental material does but to test them all separately
# liposuct1 + liposuct2 + psurgery1 + psurgery2 + dietpills1 + dietpills2 + cosmetic1 + cosmetic2 + blemcream1 + blemcream2 + fmakeover1 + fmakeover2
model.1111.4 <- 'F =~ liposuct1 + liposuct2 + psurgery1 + psurgery2 + dietpills1 + dietpills2 + cosmetic1 + cosmetic2 + blemcream1 + blemcream2 + fmakeover1 + fmakeover2'
conf.fit.1111.4 <- cfa(model.1111.4, data = article1111.2, group = "condition")
load.fit.1111.4 <- cfa(model.1111.4, article1111.2, group = "condition", group.equal = "loadings")
int.fit.1111.4 <- cfa(model.1111.4, article1111.2, group = "condition", group.equal = c("loadings", "intercepts"))
all.results.1111.4 <- mi.results(conf.fit.1111.4,load.fit.1111.4,int.fit.1111.4)
all.results.1111.4

# Step 4
# Chisquare significant, rmsea > .08, CFI < .95, so we reject configural invariance

# Comparison 5: 
# Grouping variable: control and pathogen threat (condition)
# Scale variable: Hygiene; here we also take the 6 items and leave them split for liking and purchase, so 12 in total
# soap1 + soap2 + shampoo1 + shampoo2 + tooth1 + tooth2 + toilet1 + toilet2 + floss1 + floss2 + handwipe1 + handwipe2
model.1111.5 <- 'F =~ soap1 + soap2 + shampoo1 + shampoo2 + tooth1 + tooth2 + toilet1 + toilet2 + floss1 + floss2 + handwipe1 + handwipe2'
conf.fit.1111.5 <- cfa(model.1111.5, data = article1111.2, group = "condition")
load.fit.1111.5 <- cfa(model.1111.5, article1111.2, group = "condition", group.equal = "loadings")
int.fit.1111.5 <- cfa(model.1111.5, article1111.2, group = "condition", group.equal = c("loadings", "intercepts"))
all.results.1111.5 <- mi.results(conf.fit.1111.5,load.fit.1111.5,int.fit.1111.5)
all.results.1111.5

# Step 4
# Chisquare significant, rmsea > .08, CFI < .95, so we reject configural invariance

# Comparison 6: 
# Grouping variable: control and pathogen threat (condition)
# Scale variable: Household (5 items, split in liking and purchase, so 10 in total)
# batteries1 + batteries2 + lightbulb1 + lightbulb2 + alumfoil1 + alumfoil2 + music1 + music2 + utensils1 + utensils2
model.1111.6 <- 'F =~ batteries1 + batteries2 + lightbulb1 + lightbulb2 + alumfoil1 + alumfoil2 + music1 + music2 + utensils1 + utensils2'
conf.fit.1111.6 <- cfa(model.1111.6, data = article1111.2, group = "condition")
load.fit.1111.6 <- cfa(model.1111.6, article1111.2, group = "condition", group.equal = "loadings")
int.fit.1111.6 <- cfa(model.1111.6, article1111.2, group = "condition", group.equal = c("loadings", "intercepts"))
all.results.1111.6 <- mi.results(conf.fit.1111.6,load.fit.1111.6,int.fit.1111.6)
all.results.1111.6

# Step 4
# Chisquare significant, rmsea > .08, CFI < .95, so we reject configural invariance

# Study 4
# Comparisons 7-9
url <- 'https://osf.io/k2tb6//?action=download'
filename <- '../article1111.3.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article1111.3 <- read_sav(filename)
article1111.3$cond <- as.numeric(article1111.3$cond)

# From the manuscript
# In the second task, participants rated their liking for (−5 = dislike, 5 = like) and interest in purchasing, using, or engaging in 
# (−5 = extremely disinterested, 5 = extremely interested) 28 consumer products or activities (chosen on the basis of a pretest reported in 
# the Supplemental Material). These items related to appearance (liposuction, cosmetic plastic surgery, diet pills, cosmetics/makeup, 
# blemish cream, facial makeover, hair dye), hygiene (shampoo, toothbrush, mouthwash, dental floss, hand sanitizer, deodorant, razors), 
# physical fitness (fitness equipment, working out at a gym, cardio machine, exercise, lifting weights, treadmill, exercise mat), and 
# the household (batteries, light bulbs, aluminum foil, plates, extension cord, lamp, clock). 

# From the supplemental material
# In the main study, liking and interest items correlated highly (all αs > .81), as they did in the pre-test, and so these were averaged 
# for each product. We next repeated the principal axis factor analysis with promax rotation. Results mapped directly onto the pre-test 
# findings. For the later analyses, we therefore created composites for appearance (Cronbach’s α = .83), hygiene (Cronbach’s α = .79), 
# physical fitness (Cronbach’s α = .90) and household products (Cronbach’s α = .89) products. Additionally, two separate composites for
#head appearance (Cronbach’s α = .82) and body appearance (Cronbach’s α = .78) were created.

# Comparison 7
# Grouping variable: control and pathogen threat (cond)
# Scale variable: Appearance
# liposuct1 + liposuct2 + psurgery1 + psurgery2 + dietpills1 + dietpills2 + cosmetic1 + cosmetic2 + blemcream1 + blemcream2 + 
# fmakeover1 + fmakeover2 + hairdye1 + hairdye2
model.1111.7 <- 'F =~ liposuct1 + liposuct2 + psurgery1 + psurgery2 + dietpills1 + dietpills2 + cosmetic1 + cosmetic2 + blemcream1 + blemcream2 + fmakeover1 + fmakeover2 + hairdye1 + hairdye2'
conf.fit.1111.7 <- cfa(model.1111.7, data = article1111.3, group = "cond")
load.fit.1111.7 <- cfa(model.1111.7, article1111.3, group = "cond", group.equal = "loadings")
int.fit.1111.7 <- cfa(model.1111.7, article1111.3, group = "cond", group.equal = c("loadings", "intercepts"))
all.results.1111.7 <- mi.results(conf.fit.1111.7,load.fit.1111.7,int.fit.1111.7)
all.results.1111.7

# Step 4
# Chisquare significant, rmsea > .08, CFI < .95, so we reject configural invariance

# Comparison 8
# Grouping variable: control and pathogen threat
# Scale variable: Hygiene
# shampoo1 + shampoo2 + toothbrush1 + toothbrush2 + mouthwash1 + mouthwash2 + floss1 + floss2 + sanitize1 + sanitize2 + deodorant1 + 
# deodorant2 + razors1 + razors2
model.1111.8 <- 'F =~ shampoo1 + shampoo2 + toothbrush1 + toothbrush2 + mouthwash1 + mouthwash2 + floss1 + floss2 + sanitize1 + sanitize2 + deodorant1 + deodorant2 + razors1 + razors2'
conf.fit.1111.8 <- cfa(model.1111.8, data = article1111.3, group = "cond")
load.fit.1111.8 <- cfa(model.1111.8, article1111.3, group = "cond", group.equal = "loadings")
int.fit.1111.8 <- cfa(model.1111.8, article1111.3, group = "cond", group.equal = c("loadings", "intercepts"))
all.results.1111.8 <- mi.results(conf.fit.1111.8,load.fit.1111.8,int.fit.1111.8)
all.results.1111.8

# Step 4
# Chisquare significant, rmsea > .08, CFI < .95, so we reject configural invariance

# Comparison 9
# Grouping variable: control and pathogen threat (cond)
# Scale variable: Household products
# batteries1 + batteries2 + light1 + light2 + foil1 + foil2 + plates1 + plates2 + cord1 + cord2 + lamp1 + lamp2 + clock1 + clock2
model.1111.9 <- 'F =~ batteries1 + batteries2 + light1 + light2 + foil1 + foil2 + plates1 + plates2 + cord1 + cord2 + lamp1 + lamp2 + clock1 + clock2'
conf.fit.1111.9 <- cfa(model.1111.9, data = article1111.3, group = "cond")
load.fit.1111.9 <- cfa(model.1111.9, article1111.3, group = "cond", group.equal = "loadings")
int.fit.1111.9 <- cfa(model.1111.9, article1111.3, group = "cond", group.equal = c("loadings", "intercepts"))
all.results.1111.9 <- mi.results(conf.fit.1111.9,load.fit.1111.9,int.fit.1111.9)
all.results.1111.9

# Step 4
# Chisquare significant, rmsea > .08, CFI < .95, so we reject configural invariance

# Study 5a
url <- 'https://osf.io/n6sua//?action=download'
filename <- '../article1111.4.sav'
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
all.results.1111.10 <- mi.results(conf.fit.1111.10,load.fit.1111.10,int.fit.1111.10)
all.results.1111.10

# Step 4
# Chisquare significant, rmsea > .08, CFI < .95, so we reject configural invariance

# Study 5b
url <- 'https://osf.io/e6cyf//?action=download'
filename <- '../article1111.5.sav'
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
all.results.1111.11 <- mi.results(conf.fit.1111.11,load.fit.1111.11,int.fit.1111.11)
all.results.1111.11

# Step 4
# Chisquare significant, rmsea > .08, CFI < .95, so we reject configural invariance


# Article 1151: Russell ---------------------------------------------------
url <- 'https://osf.io/7ngqz//?action=download'
filename <- '../article1151.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article1151 <- read_sav(filename)
article1151$Condition <- as.numeric(article1151$Condition)

# Grouping variable: imagined interacting either with a straight male stranger or with a gay male stranger (Condition)
# Scale variable: worry about having things in common with man CommonWor_1 + CommonWor_2 + CommonWor_3)
model.1151 <- 'F =~ CommonWor_1 + CommonWor_2 + CommonWor_3'
conf.fit.1151 <- cfa(model.1151, data = article1151, group = "Condition")
load.fit.1151 <- cfa(model.1151, article1151, group = "Condition", group.equal = "loadings")
int.fit.1151 <- cfa(model.1151, article1151, group = "Condition", group.equal = c("loadings", "intercepts"))
all.results.1151 <- mi.results(conf.fit.1151,load.fit.1151,int.fit.1151)
all.results.1151

# Step 4
# 3 items so we assume configural invariance
# metric: nonsig chisquare change, change RMSEA and CFI < .01; lower AIC and BIC; metric invariance holds
# scalar: sig chisquare change, change RMSEA and CFI > .01; larger AIC; scalar invariance rejected (BIC ok)



# Article 1251: Gaertig ---------------------------------------------------
# Study 1-6
url <- 'https://osf.io/zrbq6//?action=download'
filename <- '../data/data-main/article1251.xlsx'
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
all.results.1251 <- mi.results(conf.fit.1251,load.fit.1251,int.fit.1251)
all.results.1251

# Step 4
# Chisquare is significant, RMSEA > .08, so we reject configural invariance
# CFI is ok (0.984)

# Study 7
# Comparison 2
# Grouping variable: advice type: approximate-chance advice vs. certain advice
# Scale variable: Advice evaluation (Knowledgeable + Competent + Credible + Trust + SeekAdvice + Persuasive + Accurate)

# The OSF (https://osf.io/ew34q/) only contains data for study1-6, we can't find those for study 7.


# Article 1261: Kardas ----------------------------------------------------
# Study 5
url <- 'https://osf.io/bfj8n//?action=download'
filename <- '../data/data-main/article1261.xlsx'
GET(url, write_disk(filename, overwrite = TRUE))
article1261.1 <- read_excel(filename, sheet = 7)

# Comparison 1
# Grouping variable: performer: present; absent (condition)
# Scale variable: perceived-skill-acquisition scale (scale1_improve + scale2_prepare + scale3_learn)
model.1261.1 <- 'F =~ scale1_improve + scale2_prepare + scale3_learn'
conf.fit.1261.1 <- cfa(model.1261.1, data = article1261.1, group = "condition")
load.fit.1261.1 <- cfa(model.1261.1, article1261.1, group = "condition", group.equal = "loadings")
int.fit.1261.1 <- cfa(model.1261.1, article1261.1, group = "condition", group.equal = c("loadings", "intercepts"))
all.results.1261.1 <- mi.results(conf.fit.1261.1,load.fit.1261.1,int.fit.1261.1)
all.results.1261.1

# Step 4
# 3 items so we assume configural invariance
# metric: sig chisquare change, change RMSEA > .01; larger AIC; metric invariance rejected (BIC and CFI change ok)


# Comparison 2
# Grouping variable: exposure: low; high (exposures)
# Scale variable: perceived-skill-acquisition scale (scale1_improve + scale2_prepare + scale3_learn)
model.1261.2 <- 'F =~ scale1_improve + scale2_prepare + scale3_learn'
conf.fit.1261.2 <- cfa(model.1261.2, data = article1261.1, group = "exposures")
load.fit.1261.2 <- cfa(model.1261.2, article1261.1, group = "exposures", group.equal = "loadings")
int.fit.1261.2 <- cfa(model.1261.2, article1261.1, group = "exposures", group.equal = c("loadings", "intercepts"))
all.results.1261.2 <- mi.results(conf.fit.1261.2,load.fit.1261.2,int.fit.1261.2)
all.results.1261.2

# Step 4
# 3 items so we assume configural invariance
# metric: nonsig chisquare change, change CFI < .01; lower AIC and BIC; metric invariance holds (RMSEA change > .01)
# scalar: nonsig chisquare change, change RMSEA and CFI < .01; lower BIC; scalar invariance holds (AIC larger)


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
all.results.1261.3 <- mi.results(conf.fit.1261.3,load.fit.1261.3,int.fit.1261.3)
all.results.1261.3

# Step 4
# 3 items so we assume configural invariance
# metric: nonsig chisquare change, change CFI < .01; lower AIC and BIC; metric invariance holds (change RMSEA > .01)
# scalar: nonsig chisquare change, change CFI < .01; lower AIC and BIC; scalar invariance holds (change RMSEA > .01)


# Comparison 2
# Grouping variable: time: perceived learning at Time 1; before the debiasing task; and perceived learning at Time 2; after the debiasing task
# Scale variable: perceived-skill-acquisition scale 

# make dataset for t1
article1261.2.t1 <- cbind(article1261.2$scale1_improve_t1,article1261.2$scale2_prepare_t1,article1261.2$scale3_learn_t1)
grouping <- rep(0,nrow(article1261.2.t1))
article1261.2.t1 <- cbind(article1261.2.t1,grouping)

# make dataset for t2
article1261.2.t2 <- cbind(article1261.2$scale1_improve_t2,article1261.2$scale2_prepare_t2,article1261.2$scale3_learn_t2)
grouping <- rep(1,nrow(article1261.2.t2))
article1261.2.t2 <- cbind(article1261.2.t2,grouping)

# combine datasets
article1261.2.tot <- rbind(article1261.2.t1,article1261.2.t2)
colnames(article1261.2.tot) <- c("improve","prepare","learn","condition")

model.1261.4 <- 'F =~ improve + prepare + learn'
conf.fit.1261.4 <- cfa(model.1261.4, data = article1261.2.tot, group = "condition")
load.fit.1261.4 <- cfa(model.1261.4, article1261.2.tot, group = "condition", group.equal = "loadings")
int.fit.1261.4 <- cfa(model.1261.4, article1261.2.tot, group = "condition", group.equal = c("loadings", "intercepts"))
all.results.1261.4 <- mi.results(conf.fit.1261.4,load.fit.1261.4,int.fit.1261.4)
all.results.1261.4

# Step 4
# 3 items so we assume configural invariance
# metric: nonsig chisquare change, change CFI < .01; lower AIC and BIC; metric invariance holds (change RMSEA > .01)
# scalar: nonsig chisquare change, change RMSEA and CFI < .01; lower AIC and BIC; scalar invariance holds.


# Article 1361: Critcher --------------------------------------------------
# Study 2
url <- 'https://osf.io/xk3ny//?action=download'
filename <- '../article1361.1.csv'
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

all.results.1361 <- mi.results.sc(conf.fit.1361.1,thres.fit.1361.1,load.fit.1361.1)
all.results.1361

# Step 4
# chisquare significant, RMSEA > .08, CFI < .95, so we reject configural invariance

# Study 3
# Comparisons 2-4
url <- 'https://osf.io/2n93e//?action=download'
filename <- '../article1361.2.csv'
GET(url, write_disk(filename, overwrite = TRUE))
article1361.2 <- read.csv2(filename, header=T, na.strings="NA", sep=",")

# Comparison 2
# Grouping variable: inspired-by; awe; or neutral control condition (condition)
# Scale variable: intolerance to uncertainty (AmbigIntol_1 + AmbigIntol_2 + AmbigIntol_3 + AmbigIntol_4 + AmbigIntol_5 + AmbigIntol_6 + AmbigIntol_7 + AmbigIntol_8 + AmbigIntol_9)
model.1361.2 <- 'F =~ AmbigIntol_1 + AmbigIntol_2 + AmbigIntol_3 + AmbigIntol_4 + AmbigIntol_5 + AmbigIntol_6 + AmbigIntol_7 + AmbigIntol_8 + AmbigIntol_9'
conf.fit.1361.2 <- cfa(model.1361.2, data = article1361.2, group = "condition")
load.fit.1361.2 <- cfa(model.1361.2, article1361.2, group = "condition", group.equal = "loadings")
int.fit.1361.2 <- cfa(model.1361.2, article1361.2, group = "condition", group.equal = c("loadings", "intercepts"))
all.results.1361.2 <- mi.results(conf.fit.1361.2,load.fit.1361.2,int.fit.1361.2)
all.results.1361.2

# Step 4
# chisquare significant, CFI < .95, so we reject configural invariance
# RMSEA ok (0.065)

# Comparison 3
# Grouping variable: inspired-by; awe; or neutral control condition (condition)
# Scale variable: transcendence (Pekala1r + Pekala2 + Pekala3 + Pekala4r for cond1 and cond3, Q332 + Q333 + Q334 + Q335 for cond2)
# make dataset for cond1 and cond3
keep1 <- c("Pekala1r","Pekala2","Pekala3","Pekala4r","condition")
cond1 <- subset(article1361.2,condition==1,select=keep1)
cond3 <- subset(article1361.2,condition==3,select=keep1)

# make dataset for cond2
keep2 <- c("Q332","Q333","Q334","Q335","condition")
cond2 <- subset(article1361.2,condition==2,select=keep2)
colnames(cond2) <- c("Pekala1r","Pekala2","Pekala3","Pekala4r","condition")

# combine datasets
article1361.2.2 <- rbind(cond1,cond2,cond3)

model.1361.3 <- 'F =~ Pekala1r + Pekala2 + Pekala3 + Pekala4r'
conf.fit.1361.3 <- cfa(model.1361.3, data = article1361.2.2, group = "condition")
load.fit.1361.3 <- cfa(model.1361.3, article1361.2.2, group = "condition", group.equal = "loadings")
int.fit.1361.3 <- cfa(model.1361.3, article1361.2.2, group = "condition", group.equal = c("loadings", "intercepts"))
all.results.1361.3 <- mi.results(conf.fit.1361.3,load.fit.1361.3,int.fit.1361.3)
all.results.1361.3

# Step 4
# chisquare significant, RMSEA > 0.08, CFI < .95, so we reject configural invariance

# Comparison 4
# Grouping variable: inspired-by; awe; or neutral control condition (condition)
# Scale variable: Connectedness (condition 1 and 3 answered "soco1W","soco2W","soco3W","soco4W" / condition 2 answered "soco1A","soco2A","soco3A","soco4A")

# make dataset for cond1 and cond3
keep1 <- c("soco1W","soco2W","soco3W","soco4W","condition")
cond1 <- subset(article1361.2,condition==1,select=keep1)
cond3 <- subset(article1361.2,condition==3,select=keep1)

# make dataset for cond2
keep2 <- c("soco1A","soco2A","soco3A","soco4A","condition")
cond2 <- subset(article1361.2,condition==2,select=keep2)
colnames(cond2) <- c("soco1W","soco2W","soco3W","soco4W","condition")

# combine datasets
article1361.2.1 <- rbind(cond1,cond2,cond3)
colnames(article1361.2.1) <- c("soc1","soc2","soc3","soc4","condition")

model.1361.4 <- 'F =~ soc1 + soc2 + soc3 + soc4'
conf.fit.1361.4 <- cfa(model.1361.4, data = article1361.2.1, group = "condition")
load.fit.1361.4 <- cfa(model.1361.4, article1361.2.1, group = "condition", group.equal = "loadings")
int.fit.1361.4 <- cfa(model.1361.4, article1361.2.1, group = "condition", group.equal = c("loadings", "intercepts"))
all.results.1361.4 <- mi.results(conf.fit.1361.4,load.fit.1361.4,int.fit.1361.4)
all.results.1361.4

# Step 4
# chisquare significant, RMSEA > 0.08, CFI < .95, so we reject configural invariance

# Study 4
url <- 'https://osf.io/uwg4k//?action=download'
filename <- '../article1361.3.csv'
GET(url, write_disk(filename, overwrite = TRUE))
article1361.3 <- read.csv2(filename, header=T, na.strings="NA", sep=",")

# Comparison 5
# Grouping variable: inspiration or control (IV_video)
# Scale variable: Enjoyment; enjoyable + good + feelgood + moving + inspired + skeptical_reversed
model.1361.5 <- 'F =~ enjoyable + good + feelgood + moving + inspired + skeptical_reversed'
conf.fit.1361.5 <- cfa(model.1361.5, data = article1361.3, group = "IV_video")
load.fit.1361.5 <- cfa(model.1361.5, article1361.3, group = "IV_video", group.equal = "loadings")
int.fit.1361.5 <- cfa(model.1361.5, article1361.3, group = "IV_video", group.equal = c("loadings", "intercepts"))
all.results.1361.5 <- mi.results(conf.fit.1361.5,load.fit.1361.5,int.fit.1361.5)
all.results.1361.5

# Step 4
# chisquare significant, RMSEA > 0.08, CFI < .95, so we reject configural invariance


# Article 1391: Oconnor ---------------------------------------------------
# Study 4
url <- 'https://osf.io/tucvj///?action=download'
filename <- '../data/data-main/article1391.xlsx'
GET(url, write_disk(filename, overwrite = TRUE))
article1391 <- read_excel(filename)

# Comparison 1
# Grouping variable: simple elaborate and complex elaborate (condition)
# Scale variable: Fluency (fluent_easy + fluent_quick + fluent_enjoy)
model.1391.1 <- 'F =~ fluent_easy + fluent_quick + fluent_enjoy'
conf.fit.1391.1 <- cfa(model.1391.1, data = article1391, group = "condition")
load.fit.1391.1 <- cfa(model.1391.1, article1391, group = "condition", group.equal = "loadings")
int.fit.1391.1 <- cfa(model.1391.1, article1391, group = "condition", group.equal = c("loadings", "intercepts"))
all.results.1391.1 <- mi.results(conf.fit.1391.1,load.fit.1391.1,int.fit.1391.1)
all.results.1391.1

# Step 4
# 3 items so we assume configural invariance
# metric: nonsig chisquare change, change CFI < .01; lower BIC; 
# but: change RMSEA > .01 and AIC is larger. Chisquare change is nonsignificant so we cannot reject metric invariance; metric invariance holds 
# scalar: nonsig chisquare change, change RMSEA and CFI < .01; lower BIC; scalar invariance holds (AIC is larger)

# Comparison 2
# Grouping variable: within-group 10 times (day)
# Scale variable: Fluency (fluent_easy + fluent_quick + fluent_enjoy)
model.1391.2 <- 'F =~ fluent_easy + fluent_quick + fluent_enjoy'
conf.fit.1391.2 <- cfa(model.1391.2, data = article1391, group = "day")
load.fit.1391.2 <- cfa(model.1391.2, article1391, group = "day", group.equal = "loadings")
int.fit.1391.2 <- cfa(model.1391.2, article1391, group = "day", group.equal = c("loadings", "intercepts"))
all.results.1391.2 <- mi.results(conf.fit.1391.2,load.fit.1391.2,int.fit.1391.2)
all.results.1391.2

# Step 4
# 3 items so we assume configural invariance
# metric: nonsig chisquare change, change RMSEA and CFI < .01; lower AIC and BIC; metric invariance holds
# scalar: nonsig chisquare change, change CFI < .01; lower AIC and BIC; scalar invariance holds (change RMSEA > .01)


# Comparison 3
# Grouping variable: simple elaborate and complex elaborate (condition)
# Scale variable: Flow (flow1 + flow2 + flow3 + flow4 + flow5 + flow6 + flow7 + flow8 + flow9 + flow10)
model.1391.3 <- 'F =~ flow1 + flow2 + flow3 + flow4 + flow5 + flow6 + flow7 + flow8 + flow9 + flow10'
conf.fit.1391.3 <- cfa(model.1391.3, data = article1391, group = "condition")
load.fit.1391.3 <- cfa(model.1391.3, article1391, group = "condition", group.equal = "loadings")
int.fit.1391.3 <- cfa(model.1391.3, article1391, group = "condition", group.equal = c("loadings", "intercepts"))
all.results.1391.3 <- mi.results(conf.fit.1391.3,load.fit.1391.3,int.fit.1391.3)
all.results.1391.3

# Step 4
# chisquare significant, rmsea > .08, CFI <.95 so we reject configural invariance

# Comparison 4
# Grouping variable: within-group 10 times (day)
# Scale variable: Flow (flow1 + flow2 + flow3 + flow4 + flow5 + flow6 + flow7 + flow8 + flow9 + flow10)
model.1391.4 <- 'F =~ flow1 + flow2 + flow3 + flow4 + flow5 + flow6 + flow7 + flow8 + flow9 + flow10'
conf.fit.1391.4 <- cfa(model.1391.4, data = article1391, group = "day")
load.fit.1391.4 <- cfa(model.1391.4, article1391, group = "day", group.equal = "loadings")
int.fit.1391.4 <- cfa(model.1391.4, article1391, group = "day", group.equal = c("loadings", "intercepts"))
all.results.1391.4 <- mi.results(conf.fit.1391.4,load.fit.1391.4,int.fit.1391.4)
all.results.1391.4

# Step 4
# chisquare significant, rmsea > .08, CFI <.95 so we reject configural invariance


# Article 1421: Berman ----------------------------------------------------
# Study 3
url <- 'https://osf.io/bw72f//?action=download'
filename <- '../data/data-main/article1421.1.xlsx'
GET(url, write_disk(filename, overwrite = TRUE))
article1421.1 <- read_excel(filename)

# Comparison 1
# Grouping variable: charity condition or investment condition (Type)
# Scaling variable: Subjective Preference (PersonallyCare + ReflectViews + ObjectiveMoreImportant)
model.1421.1 <- 'F =~ PersonallyCare + ReflectViews + ObjectiveMoreImportant'
conf.fit.1421.1 <- cfa(model.1421.1, data = article1421.1, group = "Type")
load.fit.1421.1 <- cfa(model.1421.1, article1421.1, group = "Type", group.equal = "loadings")
int.fit.1421.1 <- cfa(model.1421.1, article1421.1, group = "Type", group.equal = c("loadings", "intercepts"))
all.results.1421.1 <- mi.results(conf.fit.1421.1,load.fit.1421.1,int.fit.1421.1)
all.results.1421.1

# Step 4
# 3 items so we assume configural invariance
# metric: nonsig chisquare change, change RMSEA and CFI < .01; lower AIC and BIC; metric invariance holds
# scalar: sig chisquare change, change RMSEA and CFI > .01; larger AIC; scalar invariance rejected (BIC ok)


# Study 5
url <- 'https://osf.io/fmbng//?action=download'
filename <- '../data/data-main/article1421.2.xlsx'
GET(url, write_disk(filename, overwrite = TRUE))
article1421.2 <- read_excel(filename, col_names=T)

# Comparison 2
# Grouping variable: choice: most effective option vs. least effective option (Choice)
# Scaling variable: perceived decision quality (Appropriat + Responsibl + Thoughtful)
model.1421.2 <- 'F =~ Appropriat + Responsibl + Thoughtful'
conf.fit.1421.2 <- cfa(model.1421.2, data = article1421.2, group = "Choice")
load.fit.1421.2 <- cfa(model.1421.2, article1421.2, group = "Choice", group.equal = "loadings")
int.fit.1421.2 <- cfa(model.1421.2, article1421.2, group = "Choice", group.equal = c("loadings", "intercepts"))
all.results.1421.2 <- mi.results(conf.fit.1421.2,load.fit.1421.2,int.fit.1421.2)
all.results.1421.2

# Step 4
# 3 items so we assume configural invariance
# metric: nonsig chisquare change, change RMSEA and CFI < .01; lower AIC and BIC; metric invariance holds
# scalar: nonsig chisquare change, change RMSEA and CFI < .01; lower AIC and BIC; scalar invariance holds.


# Comparison 3
# Grouping variable: choice: most effective option vs. least effective choice (Choice)
# Scaling variable: perceived altruism (Selfish + Ethical + Good)
model.1421.3 <- 'F =~ Selfish + Ethical + Good'
conf.fit.1421.3 <- cfa(model.1421.3, data = article1421.2, group = "Choice")
load.fit.1421.3 <- cfa(model.1421.3, article1421.2, group = "Choice", group.equal = "loadings")
int.fit.1421.3 <- cfa(model.1421.3, article1421.2, group = "Choice", group.equal = c("loadings", "intercepts"))
all.results.1421.3 <- mi.results(conf.fit.1421.3,load.fit.1421.3,int.fit.1421.3)
all.results.1421.3

# Step 4
# 3 items so we assume configural invariance
# metric: nonsig chisquare change, change CFI < .01; lower AIC and BIC; metric invariance holds (change RMSEA > .01)
# scalar: sig chisquare change, change RMSEA and CFI > .01; larger AIC; scalar invariance rejected (BIC ok)


# Article 1501: Moon ------------------------------------------------------
# Study 1
# Comparisons 1-3
url <- 'https://osf.io/t3rhv//?action=download'
filename <- '../article1501.1.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article1501.1 <- read_sav(filename)
article1501.1$targrel <- as.numeric(article1501.1$targrel)

# Comparison 1
# Grouping variable: religious target and nonreligious target (targrel)
# Scale variable: Reproductive strategy (Faithful + Family + multsex + LongTerm + promisc + InvCh + DedParent)
# got the correct variables from the syntax: https://osf.io/wt8jc/
model.1501.1 <- 'F =~ Faithful + Family + multsex + LongTerm + promisc + InvCh + DedParent'
conf.fit.1501.1 <- cfa(model.1501.1, data = article1501.1, group = "targrel")
load.fit.1501.1 <- cfa(model.1501.1, article1501.1, group = "targrel", group.equal = "loadings")
int.fit.1501.1 <- cfa(model.1501.1, article1501.1, group = "targrel", group.equal = c("loadings", "intercepts"))
all.results.1501.1 <- mi.results(conf.fit.1501.1,load.fit.1501.1,int.fit.1501.1)
all.results.1501.1

# Step 4
# Chisquare is significant, RMSEA > .08, CFI < .95, so we reject configural invariance

# Comparison 2
# Grouping variable: religious target and nonreligious target (targrel)
# Scale variable: Non-aggression (aggressive + violent + temper)
model.1501.2 <- 'F =~ aggressive + violent + temper'
conf.fit.1501.2 <- cfa(model.1501.2, data = article1501.1, group = "targrel")
load.fit.1501.2 <- cfa(model.1501.2, article1501.1, group = "targrel", group.equal = "loadings")
int.fit.1501.2 <- cfa(model.1501.2, article1501.1, group = "targrel", group.equal = c("loadings", "intercepts"))
all.results.1501.2 <- mi.results(conf.fit.1501.2,load.fit.1501.2,int.fit.1501.2)
all.results.1501.2

# Step 4
# 3 items so we assume configural invariance
# metric: nonsig chisquare change, change CFI < .01; lower AIC and BIC; metric invariance holds (RMSEA change > .01)
# scalar: sig chisquare change, change RMSEA > .01; larger AIC scalar invariance rejected (CFI BIC ok)


# Comparison 3
# Grouping variable: religious target and nonreligious target
# Scale variable: Mate value (MV1_1 + MV1_2 + MV1_3 + MV1_6 + MV1_7)
model.1501.3 <- 'F =~ MV1_1 + MV1_2 + MV1_3 + MV1_6 + MV1_7'
conf.fit.1501.3 <- cfa(model.1501.3, data = article1501.1, group = "targrel")
load.fit.1501.3 <- cfa(model.1501.3, article1501.1, group = "targrel", group.equal = "loadings")
int.fit.1501.3 <- cfa(model.1501.3, article1501.1, group = "targrel", group.equal = c("loadings", "intercepts"))
all.results.1501.3 <- mi.results(conf.fit.1501.3,load.fit.1501.3,int.fit.1501.3)
all.results.1501.3

# Step 4
# Chisquare is significant, RMSEA > .08, so we reject configural invariance
# CFI is ok (0.981)


# Study 2
# Comparisons 4-12
url <- 'https://osf.io/8f7wa//?action=download'
filename <- '../article1501.2.sav'
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
all.results.1501.4 <- mi.results(conf.fit.1501.4,load.fit.1501.4,int.fit.1501.4)
all.results.1501.4

# Step 4
# Chisquare is significant, rmsea > .08 CFI < .95, so we reject configural invariance

# Comparison 5
# Grouping variable: committed strategies vs noncommitted strategies (targstrat)
# Scale variable: reproductive strategy (CS1 + CS2 + CS3 + CS4 + CS5 + CS6 + CS7)
model.1501.5 <- 'F =~ CS1 + CS2 + CS3 + CS4 + CS5 + CS6 + CS7'
conf.fit.1501.5 <- cfa(model.1501.5, data = article1501.2, group = "targstrat")
load.fit.1501.5 <- cfa(model.1501.5, article1501.2, group = "targstrat", group.equal = "loadings")
int.fit.1501.5 <- cfa(model.1501.5, article1501.2, group = "targstrat", group.equal = c("loadings", "intercepts"))
all.results.1501.5 <- mi.results(conf.fit.1501.5,load.fit.1501.5,int.fit.1501.5)
all.results.1501.5

# Step 4
# Chisquare is significant, rmsea > .08 CFI < .95, so we reject configural invariance

# Comparison 6
# Grouping variable: committed strategies vs noncommitted strategies (targstrat)
# Scale variable: nonimpulsivity (IMP1 + IMP2 + IMP3 + IMP4 + IMP5 + IMP6 + IMP7)
model.1501.6 <- 'F =~ IMP1 + IMP2 + IMP3 + IMP4 + IMP5 + IMP6 + IMP7'
conf.fit.1501.6 <- cfa(model.1501.6, data = article1501.2, group = "targstrat")
load.fit.1501.6 <- cfa(model.1501.6, article1501.2, group = "targstrat", group.equal = "loadings")
int.fit.1501.6 <- cfa(model.1501.6, article1501.2, group = "targstrat", group.equal = c("loadings", "intercepts"))
all.results.1501.6 <- mi.results(conf.fit.1501.6,load.fit.1501.6,int.fit.1501.6)
all.results.1501.6

# Step 4
# Chisquare is significant, rmsea > .08 CFI < .95, so we reject configural invariance

# Comparison 7
# Grouping variable: religious target and nonreligious target (targrel)
# Scale variable: nonopportunistic behavior (OB1 + OB2 + OB3)
model.1501.7 <- 'F =~ OB1 + OB2 + OB3'
conf.fit.1501.7 <- cfa(model.1501.7, data = article1501.2, group = "targrel")
load.fit.1501.7 <- cfa(model.1501.7, article1501.2, group = "targrel", group.equal = "loadings")
int.fit.1501.7 <- cfa(model.1501.7, article1501.2, group = "targrel", group.equal = c("loadings", "intercepts"))
all.results.1501.7 <- mi.results(conf.fit.1501.7,load.fit.1501.7,int.fit.1501.7)
all.results.1501.7

# Step 4
# 3 items so we assume configural invariance
# metric: nonsig chisquare change, change RMSEA and CFI < .01; lower AIC and BIC; metric invariance holds
# scalar: sig chisquare change, change RMSEA > .01; larger AIC; scalar invariance rejected (CFI BIC ok)


# Comparison 8
# Grouping variable: committed strategies vs noncommitted strategies (targstrat)
# Scale variable: nonopportunistic behavior (OB1 + OB2 + OB3)
model.1501.8 <- 'F =~ OB1 + OB2 + OB3'
conf.fit.1501.8 <- cfa(model.1501.8, data = article1501.2, group = "targstrat")
load.fit.1501.8 <- cfa(model.1501.8, article1501.2, group = "targstrat", group.equal = "loadings")
int.fit.1501.8 <- cfa(model.1501.8, article1501.2, group = "targstrat", group.equal = c("loadings", "intercepts"))
all.results.1501.8 <- mi.results(conf.fit.1501.8,load.fit.1501.8,int.fit.1501.8)
all.results.1501.8

# Step 4
# 3 items so we assume configural invariance
# metric: sig chisquare change, change RMSEA > .01; larger AIC; metric invariance rejected (CFI BIC ok)


# Comparison 9
# Grouping variable: religious target and nonreligious target (targrel)
# Scale variable: hopeful ecology (ECOL1 + ECOL2 + ECOL3)
model.1501.9 <- 'F =~ ECOL1 + ECOL2 + ECOL3'
conf.fit.1501.9 <- cfa(model.1501.9, data = article1501.2, group = "targrel")
load.fit.1501.9 <- cfa(model.1501.9, article1501.2, group = "targrel", group.equal = "loadings")
int.fit.1501.9 <- cfa(model.1501.9, article1501.2, group = "targrel", group.equal = c("loadings", "intercepts"))
all.results.1501.9 <- mi.results(conf.fit.1501.9,load.fit.1501.9,int.fit.1501.9)
all.results.1501.9

# Step 4
# 3 items so we assume configural invariance
# metric: nonsig chisquare change, change CFI < .01; lower BIC;
# larger AIC, RMSEA change > .01; chisquare nonsignificant so we cannot reject metric invariance; metric invariance holds
# scalar: nonsig chisquare change, change CFI < .01; lower AIC and BIC; scalar invariance holds (change RMSEA > .01 but fit improves)


# Comparison 10
# Grouping variable: committed strategies vs noncommitted strategies (targstrat)
# Scale variable: hopeful ecology (ECOL1 + ECOL2 + ECOL3)
model.1501.10 <- 'F =~ ECOL1 + ECOL2 + ECOL3'
conf.fit.1501.10 <- cfa(model.1501.10, data = article1501.2, group = "targstrat")
load.fit.1501.10 <- cfa(model.1501.10, article1501.2, group = "targstrat", group.equal = "loadings")
int.fit.1501.10 <- cfa(model.1501.10, article1501.2, group = "targstrat", group.equal = c("loadings", "intercepts"))
all.results.1501.10 <- mi.results(conf.fit.1501.10,load.fit.1501.10,int.fit.1501.10)
all.results.1501.10

# Step 4
# 3 items so we assume configural invariance
# metric: nonsig chisquare change, change RMSEA and CFI < .01; lower AIC and BIC; metric invariance holds
# scalar: sig chisquare change, change RMSEA and CFI > .01; larger AIC and BIC; scalar invariance rejected


# Comparison 11
# Grouping variable: religious target and nonreligious target (targrel)
# Scale variable: trust (TR1 + TR2 + TR3 + TR4 + TR5 + TR6)
model.1501.11 <- 'F =~ TR1 + TR2 + TR3 + TR4 + TR5 + TR6'
conf.fit.1501.11 <- cfa(model.1501.11, data = article1501.2, group = "targrel")
load.fit.1501.11 <- cfa(model.1501.11, article1501.2, group = "targrel", group.equal = "loadings")
int.fit.1501.11 <- cfa(model.1501.11, article1501.2, group = "targrel", group.equal = c("loadings", "intercepts"))
all.results.1501.11 <- mi.results(conf.fit.1501.11,load.fit.1501.11,int.fit.1501.11)
all.results.1501.11

# Step 4
# chisquare non-significant, RMSEA < .08, CFI > .95 so we do not reject configural invariance
# metric: nonsig chisquare change, change in RMSEA < .01, change in CFI < .01, AIC and BIC are lower, we do not reject metric invariance
# scalar: nonsig chisquare change, change in RMSEA < .01, change in CFI < .01, AIC and BIC are lower, we do not reject scalar invariance

# Comparison 12
# Grouping variable: committed strategies vs noncommitted strategies (targstrat)
# Scale variable: trust (TR1 + TR2 + TR3 + TR4 + TR5 + TR6)
model.1501.12 <- 'F =~ TR1 + TR2 + TR3 + TR4 + TR5 + TR6'
conf.fit.1501.12 <- cfa(model.1501.12, data = article1501.2, group = "targstrat")
load.fit.1501.12 <- cfa(model.1501.12, article1501.2, group = "targstrat", group.equal = "loadings")
int.fit.1501.12 <- cfa(model.1501.12, article1501.2, group = "targstrat", group.equal = c("loadings", "intercepts"))
all.results.1501.12 <- mi.results(conf.fit.1501.12,load.fit.1501.12,int.fit.1501.12)
all.results.1501.12

# Step 4
# chisquare non-significant, RMSEA < .08, CFI > .95 so we do not reject configural invariance
# metric: nonsig chisquare change, change in RMSEA < .01, change in CFI < .01, AIC and BIC are lower, we do not reject metric invariance
# scalar: nonsig chisquare change, change in RMSEA < .01, change in CFI < .01, AIC and BIC are lower, we do not reject scalar invariance


# Study 3
# Comparisons 13-18
url <- 'https://osf.io/uh2ce//?action=download'
filename <- '../article1501.3.sav'
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
all.results.1501.13 <- mi.results(conf.fit.1501.13,load.fit.1501.13,int.fit.1501.13)
all.results.1501.13

# Step 4
# Chisquare significant, RMSEA > .08, CFI < .95, so we reject configural invariance

# Comparison 14
# Grouping variable: religious target and nonreligious target (targrel)
# Scale variable: Nonimpulsivity (Impulsive_1 + Impulsive_2 + Impulsive_3 + Impulsive_4 + Impulsive_5 + Impulsive_6 + Impulsive_7 + Impulsive_8)
# the syntax includes Impulsive_5 for this scale, but every participant answered 6 on that question because it was an attention check
# as such, we have no variance on Impulsive_5 and cannot include it. As it is not an item that actually adds information to the scale,
# we decided to estimate the model without that variable. 
model.1501.14 <- 'F =~ Impulsive_1 + Impulsive_2 + Impulsive_3 + Impulsive_4 + Impulsive_6 + Impulsive_7 + Impulsive_8'
conf.fit.1501.14 <- cfa(model.1501.14, data = article1501.3, group = "targrel")
load.fit.1501.14 <- cfa(model.1501.14, article1501.3, group = "targrel", group.equal = "loadings")
int.fit.1501.14 <- cfa(model.1501.14, article1501.3, group = "targrel", group.equal = c("loadings", "intercepts"))
all.results.1501.14 <- mi.results(conf.fit.1501.14,load.fit.1501.14,int.fit.1501.14)
all.results.1501.14

# Step 4
# Chisquare significant, RMSEA > .08, CFI < .95, so we reject configural invariance

# Comparison 15
# Grouping variable: religious target and nonreligious target (targrel)
# Scale variable: Nonopportunistic behaviour (Opportunistic_1 + Opportunistic_2 + Opportunistic_3)
model.1501.15 <- 'F =~ Opportunistic_1 + Opportunistic_2 + Opportunistic_3'
conf.fit.1501.15 <- cfa(model.1501.15, data = article1501.3, group = "targrel")
load.fit.1501.15 <- cfa(model.1501.15, article1501.3, group = "targrel", group.equal = "loadings")
int.fit.1501.15 <- cfa(model.1501.15, article1501.3, group = "targrel", group.equal = c("loadings", "intercepts"))
all.results.1501.15 <- mi.results(conf.fit.1501.15,load.fit.1501.15,int.fit.1501.15)
all.results.1501.15

# Step 4
# 3 items so we assume configural invariance
# metric: sig chisquare change, change RMSEA > .01; larger AIC; metric invariance rejected (CFI change and BIC ok)

# Comparison 16
# Grouping variable: religious target and nonreligious target (targrel)
# Scale variable: Hopeful ecology (Ecology_1 + Ecology_2 + Ecology_3)
model.1501.16 <- 'F =~ Ecology_1 + Ecology_2 + Ecology_3'
conf.fit.1501.16 <- cfa(model.1501.16, data = article1501.3, group = "targrel")
load.fit.1501.16 <- cfa(model.1501.16, article1501.3, group = "targrel", group.equal = "loadings")
int.fit.1501.16 <- cfa(model.1501.16, article1501.3, group = "targrel", group.equal = c("loadings", "intercepts"))
all.results.1501.16 <- mi.results(conf.fit.1501.16,load.fit.1501.16,int.fit.1501.16)
all.results.1501.16

# Step 4
# 3 items so we assume configural invariance
# metric: nonsig chisquare change, change RMSEA and CFI < .01; lower AIC and BIC; metric invariance holds
# scalar: nonsig chisquare change, change CFI < .01; lower AIC and BIC; scalar invariance holds (change RMSEA > .01)


# Comparison 17
# Grouping variable: religious target and nonreligious target (targrel)
# Scale variable: General Trust (Trust_1 + Trust_2 + Trust_3 + Trust_4 + Trust_5 + Trust_6 + Trust_7 + Trust_8 + Trust_9 + Trust_10)
model.1501.17 <- 'F =~ Trust_1 + Trust_2 + Trust_3 + Trust_4 + Trust_5 + Trust_6 + Trust_7 + Trust_8 + Trust_9 + Trust_10'
conf.fit.1501.17 <- cfa(model.1501.17, data = article1501.3, group = "targrel")
load.fit.1501.17 <- cfa(model.1501.17, article1501.3, group = "targrel", group.equal = "loadings")
int.fit.1501.17 <- cfa(model.1501.17, article1501.3, group = "targrel", group.equal = c("loadings", "intercepts"))
all.results.1501.17 <- mi.results(conf.fit.1501.17,load.fit.1501.17,int.fit.1501.17)
all.results.1501.17

# Step 4
# Chisquare significant, RMSEA > .08 so we reject configural invariance
# CFI ok (0.951)

# Comparison 18
# Grouping variable: religious target and nonreligious target (targrel)
# Scale variable: Accountancy Trust (AccTrust_11 + AccTrust_12 + AccTrust_13 + AccTrust_14 + AccTrust_15)
model.1501.18 <- 'F =~ AccTrust_11 + AccTrust_12 + AccTrust_13 + AccTrust_14 + AccTrust_15'
conf.fit.1501.18 <- cfa(model.1501.18, data = article1501.3, group = "targrel")
load.fit.1501.18 <- cfa(model.1501.18, article1501.3, group = "targrel", group.equal = "loadings")
int.fit.1501.18 <- cfa(model.1501.18, article1501.3, group = "targrel", group.equal = c("loadings", "intercepts"))
all.results.1501.18 <- mi.results(conf.fit.1501.18,load.fit.1501.18,int.fit.1501.18)
all.results.1501.18

# Step 4
# Chisquare significant, RMSEA > .08, CFI < .95, so we reject configural invariance


# Article 1681: Kouchaki --------------------------------------------------
# Study 1a
url <- 'https://osf.io/k948v//?action=download'
filename <- '../article1681.1.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article1681.1 <- read_sav(filename)
article1681.1$Condition <- as.numeric(article1681.1$Condition)

# Comparison 1
# Grouping variable: unethical (vs. ethical or neutral) behavior (Condition)
# Scale variable: Mind-Attribution scale - Self-dehumanization (DV_1 + DV_2 + DV_3 + DV_4 + DV_5 + DV_6 + DV_7 + DV_8 + DV_9 + DV_10)
model.1681.1 <- 'F =~ DV_1 + DV_2 + DV_3 + DV_4 + DV_5 + DV_6 + DV_7 + DV_8 + DV_9 + DV_10'
conf.fit.1681.1 <- cfa(model.1681.1, data = article1681.1, group = "Condition")
load.fit.1681.1 <- cfa(model.1681.1, article1681.1, group = "Condition", group.equal = "loadings")
int.fit.1681.1 <- cfa(model.1681.1, article1681.1, group = "Condition", group.equal = c("loadings", "intercepts"))
all.results.1681.1 <- mi.results(conf.fit.1681.1,load.fit.1681.1,int.fit.1681.1)
all.results.1681.1

# Step 4
# Chisquare significant, RMSEA > .08, CFI < .95, so we reject configural invariance

# Study 1b
url <- 'https://osf.io/9ejm6//?action=download'
filename <- '../article1681.2.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article1681.2 <- read_sav(filename)
article1681.2$Condition <- as.numeric(article1681.2$Condition)

# Comparison 2
# Grouping variable: unethical (vs. ethical or neutral) behavior (Condition)
# Scale variable: Mind-Attribution scale - Self-dehumanization (DV_1 + DV_2 + DV_3 + DV_4 + DV_5 + DV_6 + DV_7 + DV_8 + DV_9 + DV_10)
model.1681.2 <- 'F =~ DV_1 + DV_2 + DV_3 + DV_4 + DV_5 + DV_6 + DV_7 + DV_8 + DV_9 + DV_10'
conf.fit.1681.2 <- cfa(model.1681.2, data = article1681.2, group = "Condition")
load.fit.1681.2 <- cfa(model.1681.2, article1681.2, group = "Condition", group.equal = "loadings")
int.fit.1681.2 <- cfa(model.1681.2, article1681.2, group = "Condition", group.equal = c("loadings", "intercepts"))
all.results.1681.2 <- mi.results(conf.fit.1681.2,load.fit.1681.2,int.fit.1681.2)
all.results.1681.2

# Step 4
# Chisquare significant, RMSEA > .08, CFI < .95, so we reject configural invariance

# Study 1c
url <- 'https://osf.io/ys6h8//?action=download'
filename <- '../article1681.3.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article1681.3 <- read_sav(filename)
article1681.3$Cond <- as.numeric(article1681.3$Cond)

# Comparison 3
# Grouping variable: unethical (vs. ethical or neutral) behavior (Cond)
# Scale variable: Mind-Attribution scale - Self-dehumanization (DV_1 + DV_2 + DV_3 + DV_4 + DV_5 + DV_6 + DV_7 + DV_8 + DV_9 + DV_10)
model.1681.3 <- 'F =~ DV_1 + DV_2 + DV_3 + DV_4 + DV_5 + DV_6 + DV_7 + DV_8 + DV_9 + DV_10'
conf.fit.1681.3 <- cfa(model.1681.3, data = article1681.3, group = "Cond")
load.fit.1681.3 <- cfa(model.1681.3, article1681.3, group = "Cond", group.equal = "loadings")
int.fit.1681.3 <- cfa(model.1681.3, article1681.3, group = "Cond", group.equal = c("loadings", "intercepts"))
all.results.1681.3 <- mi.results(conf.fit.1681.3,load.fit.1681.3,int.fit.1681.3)
all.results.1681.3

# Step 4
# Chisquare significant, RMSEA > .08, CFI < .95, so we reject configural invariance

# Study 1d
url <- 'https://osf.io/tnjz8//?action=download'
filename <- '../article1681.4.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article1681.4 <- read_sav(filename)
article1681.4$Condition <- as.numeric(article1681.4$Condition)

# Comparison 4
# Grouping variable: unethical behavior (i.e.; cheating on an exam) vs a negative experience (i.e.; failing an exam) (Condition)
# Scale variable: Mind-Attribution scale - Self-dehumanization (DV_1 + DV_2 + DV_3 + DV_4 + DV_5 + DV_6 + DV_7 + DV_8 + DV_9 + DV_10)
model.1681.4 <- 'F =~ DV_1 + DV_2 + DV_3 + DV_4 + DV_5 + DV_6 + DV_7 + DV_8 + DV_9 + DV_10'
conf.fit.1681.4 <- cfa(model.1681.4, data = article1681.4, group = "Condition")
load.fit.1681.4 <- cfa(model.1681.4, article1681.4, group = "Condition", group.equal = "loadings")
int.fit.1681.4 <- cfa(model.1681.4, article1681.4, group = "Condition", group.equal = c("loadings", "intercepts"))
all.results.1681.4 <- mi.results(conf.fit.1681.4,load.fit.1681.4,int.fit.1681.4)
all.results.1681.4

# Step 4
# Chisquare significant, RMSEA > .08, CFI < .95, so we reject configural invariance

# Study 2b
url <- 'https://osf.io/j2uwq//?action=download'
filename <- '../article1681.5.sav'
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
all.results.1681.5 <- mi.results(conf.fit.1681.5,load.fit.1681.5,int.fit.1681.5)
all.results.1681.5

# Step 4
# Chisquare significant, RMSEA > .08, CFI < .95, so we reject configural invariance


# Article 1721: Gebauer ---------------------------------------------------
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
all.results.1721.1 <- mi.results(conf.fit.1721.1,load.fit.1721.1,int.fit.1721.1)
all.results.1721.1

# Step 4
# Chisquare significant, RMSEA> .08, so we reject configural invariance
# CFI ok (0.980)

# Comparison 2
# Grouping variable: yoga condition and control condition (cond)
# Scale variable: self-enhancement: better than average (bta01 + bta02 + bta03 + bta04)
model.1721.2 <- 'F =~ bta01 + bta02 + bta03 + bta04'
conf.fit.1721.2 <- cfa(model.1721.2, data = article1721.1, group = "cond")
load.fit.1721.2 <- cfa(model.1721.2, article1721.1, group = "cond", group.equal = "loadings")
int.fit.1721.2 <- cfa(model.1721.2, article1721.1, group = "cond", group.equal = c("loadings", "intercepts"))
all.results.1721.2 <- mi.results(conf.fit.1721.2,load.fit.1721.2,int.fit.1721.2)
all.results.1721.2

# Step 4
# Chisquare significant, RMSEA> .08, so we reject configural invariance
# CFI ok (0.988)

# Comparison 3
# Grouping variable: yoga condition and control condition (cond)
# Scale variable: self-enhancement: communal narcissism (cni01 + cni02 + cni03 + cni04)
model.1721.3 <- 'F =~ cni01 + cni02 + cni03 + cni04'
conf.fit.1721.3 <- cfa(model.1721.3, data = article1721.1, group = "cond")
load.fit.1721.3 <- cfa(model.1721.3, article1721.1, group = "cond", group.equal = "loadings")
int.fit.1721.3 <- cfa(model.1721.3, article1721.1, group = "cond", group.equal = c("loadings", "intercepts"))
all.results.1721.3 <- mi.results(conf.fit.1721.3,load.fit.1721.3,int.fit.1721.3)
all.results.1721.3

# Step 4
# Chisquare significant, RMSEA> .08, so we reject configural invariance
# CFI ok (0.959)

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
all.results.1721.4 <- mi.results(conf.fit.1721.4,load.fit.1721.4,int.fit.1721.4)
all.results.1721.4

# Step 4
# Chisquare significant, RMSEA> .08, CFI < .95 so we reject configural invariance

# Comparison 5
# Grouping variable: control condition and meditation condition (cond)
# Scale variable: self-enhancement: better than average (bta01 + bta02 + bta03 + bta04 + bta05 + bta06 + bta07 + bta08 + bta09 + bta10)
model.1721.5 <- 'F =~ bta01 + bta02 + bta03 + bta04 + bta05 + bta06 + bta07 + bta08 + bta09 + bta10'
conf.fit.1721.5 <- cfa(model.1721.5, data = article1721.2, group = "cond")
load.fit.1721.5 <- cfa(model.1721.5, article1721.2, group = "cond", group.equal = "loadings")
int.fit.1721.5 <- cfa(model.1721.5, article1721.2, group = "cond", group.equal = c("loadings", "intercepts"))
all.results.1721.5 <- mi.results(conf.fit.1721.5,load.fit.1721.5,int.fit.1721.5)
all.results.1721.5

# Step 4
# Chisquare significant, RMSEA> .08, CFI < .95 so we reject configural invariance

# Comparison 6
# Grouping variable: control condition and meditation condition (cond)
# Scale variable: self-enhancement: communal narcissism (cni01 + cni02 + cni03 + cni04 + cni05 + cni06 + cni07 + cni08 + cni09 + cni10 + cni11 + cni12 + cni13 + cni14 + cni15 + cni16)
model.1721.6 <- 'F =~ cni01 + cni02 + cni03 + cni04 + cni05 + cni06 + cni07 + cni08 + cni09 + cni10 + cni11 + cni12 + cni13 + cni14 + cni15 + cni16'
conf.fit.1721.6 <- cfa(model.1721.6, data = article1721.2, group = "cond")
load.fit.1721.6 <- cfa(model.1721.6, article1721.2, group = "cond", group.equal = "loadings")
int.fit.1721.6 <- cfa(model.1721.6, article1721.2, group = "cond", group.equal = c("loadings", "intercepts"))
all.results.1721.6 <- mi.results(conf.fit.1721.6,load.fit.1721.6,int.fit.1721.6)
all.results.1721.6

# Step 4
# Chisquare significant, RMSEA> .08, CFI < .95 so we reject configural invariance

# Comparison 7
# Grouping variable: control condition and meditation condition (cond)
# Scale variable: self-esteem (rse01 + rse02r + rse03 + rse04 + rse05r + rse06r + rse07 + rse08r + rse09r + rse10)
model.1721.7 <- 'F =~ rse01 + rse02r + rse03 + rse04 + rse05r + rse06r + rse07 + rse08r + rse09r + rse10'
conf.fit.1721.7 <- cfa(model.1721.7, data = article1721.2, group = "cond")
load.fit.1721.7 <- cfa(model.1721.7, article1721.2, group = "cond", group.equal = "loadings")
int.fit.1721.7 <- cfa(model.1721.7, article1721.2, group = "cond", group.equal = c("loadings", "intercepts"))
all.results.1721.7 <- mi.results(conf.fit.1721.7,load.fit.1721.7,int.fit.1721.7)
all.results.1721.7

# Step 4
# Chisquare significant, RMSEA> .08, CFI < .95 so we reject configural invariance

# Comparison 8
# Grouping variable: control condition and meditation condition (cond)
# Scale variable: well-being: hedonic (aff01 + aff02 + aff03 + aff04 + aff05r + aff06r + aff07r + aff08r + aff09r)
model.1721.8 <- 'F =~ aff01 + aff02 + aff03 + aff04 + aff05r + aff06r + aff07r + aff08r + aff09r'
conf.fit.1721.8 <- cfa(model.1721.8, data = article1721.2, group = "cond")
load.fit.1721.8 <- cfa(model.1721.8, article1721.2, group = "cond", group.equal = "loadings")
int.fit.1721.8 <- cfa(model.1721.8, article1721.2, group = "cond", group.equal = c("loadings", "intercepts"))
all.results.1721.8 <- mi.results(conf.fit.1721.8,load.fit.1721.8,int.fit.1721.8)
all.results.1721.8

# Step 4
# Chisquare significant, RMSEA> .08, CFI < .95 so we reject configural invariance

# Comparison 9
# Grouping variable: control condition and meditation condition (cond)
# Scale variable: well-being: eudemonic (eud01 + eud02r + eud03r + eud04 + eud05 + eud06r + eud07r + eud08 + eud09 + eud10r + eud011 + eud12r)
model.1721.9 <- 'F =~ eud01 + eud02r + eud03r + eud04 + eud05 + eud06r + eud07r + eud08 + eud09 + eud10r + eud011 + eud12r'
conf.fit.1721.9 <- cfa(model.1721.9, data = article1721.2, group = "cond")
load.fit.1721.9 <- cfa(model.1721.9, article1721.2, group = "cond", group.equal = "loadings")
int.fit.1721.9 <- cfa(model.1721.9, article1721.2, group = "cond", group.equal = c("loadings", "intercepts"))
all.results.1721.9 <- mi.results(conf.fit.1721.9,load.fit.1721.9,int.fit.1721.9)
all.results.1721.9

# Step 4
# Chisquare significant, RMSEA> .08, CFI < .95 so we reject configural invariance


# Article 1861: Block -----------------------------------------------------
# Study 1
url <- 'https://osf.io/tc7ys//?action=download'
filename <- '../article1861.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article1861 <- read_sav(filename)
# Syntax here: https://osf.io/khbvp/

# remove rows that do not have the grouping variable 
article1861$cgender <- as.numeric(article1861$cgender)
article1861_collapsed <- article1861[which(!is.na(article1861$cgender)),]

# Comparison 1
# Grouping variable: gender (boy and girl): cgender
# Scale variable: communal orientation (comm1 + comm2 + comm3 + comm4)

#Create model 
model1861.1 <- 'F =~ comm1 + comm2 + comm3 + comm4'

# Note that since in 1 group (cgender = 1) there is a 0 cell problem 
# non-observed score for the variables comm1 with value 5 --> we collapse value 5 into value 4 in both groups
article1861_collapsed["comm1"][article1861_collapsed["comm1"] == 5] <- 4
# non-observed score for the variables comm3 with value 4&5 --> we collapse value 4&5 into value 3 in both groups
article1861_collapsed["comm3"][article1861_collapsed["comm3"] == 5] <- 3
article1861_collapsed["comm3"][article1861_collapsed["comm3"] == 4] <- 3
# non-observed score for the variables comm4 with value 5 --> we collapse value 5 into value 4 in both groups
article1861_collapsed["comm4"][article1861_collapsed["comm4"] == 5] <- 4

# Here's the estimation for continuous MI, where we would conclude too large a change from config to metric
# for RMSEA, CFI, and AIC, meaning we would state that configural invariance holds
#conf.fit.1861.1 <- cfa(model1861.1, data = article1861_collapsed, group = "cgender")
#load.fit.1861.1 <- cfa(model1861.1, article1861_collapsed, group = "cgender", group.equal = "loadings")
#int.fit.1861.1 <- cfa(model1861.1, article1861_collapsed, group = "cgender", group.equal = c("loadings", "intercepts"))
#all.results.1861.1 <- mi.results(conf.fit.1861.1,load.fit.1861.1,int.fit.1861.1)
#all.results.1861.1

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

all.results.1861.1 <- mi.results.sc(conf.fit.1861.1,thres.fit.1861.1,load.fit.1861.1)
all.results.1861.1

# Step 4
# Non-significant chisquare, RMSEA < .08 and CFI > .95 so configural invariance holds
# Metric: RMSEA change is 0, CFI change is 0, AIC and BIC are not estimated due to not estimating a loglikelihood. Metric invariance holds
# Scalar: RMSEA change is 0, CFI change is 0, AIC and BIC are not estimated due to not estimating a loglikelihood. Scalar invariance holds

# Comparison 2
# Grouping variable: gender (boy and girl): cgender
# Scale variable: agentic value (agen1 + agen3 + agen4)
# the authors mention that they exclude agen2 from analyses

#Create model 
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

all.results.1861.2 <- mi.results.sc(conf.fit.1861.2,thres.fit.1861.2,load.fit.1861.2)
all.results.1861.2

# hi dami this is where I realized my mistake. so in this case I think we have to say that thresholds holds :) 

# Step 4
# 3 items so we assume configural invariance
# metric: nonsig chisquare change, change CFI < .01, change RMSEA > .01, AIC and BIC not estimated. Thresholds invariance holds
# scalar: nonsig chisquare change, change CFI < .01, change RMSEA > .01, AIC and BIC not estimated. Does loadings hold as well?


# Comparison 3
# Grouping variable: gender (boy and girl): cgender
# Scale variable: Explicit gender identification (ident1_bl + ident3_b + ident2_REV + Idnet4_REV)

#Create model 
model1861.3 <- 'F =~ ident1_bl + ident3_bl + ident2_REV + Idnet4_REV'

# Note that since in 1 group (cgender = 1) there is a 0 cell problem 
# non-observed score for the variables ident3_bl and Idnet4_REV with value 1 --> we collapse value 1 into value 2 in both groups
article1861_collapsed["ident3_bl"][article1861_collapsed["ident3_bl"] == 1] <- 2
article1861_collapsed["Idnet4_REV"][article1861_collapsed["Idnet4_REV"] == 1] <- 2

# Here's the estimation for continuous MI, where we would conclude that metric invariance held
#conf.fit.1861.3 <- cfa(model1861.3, data = article1861_collapsed, group = "cgender")
#load.fit.1861.3 <- cfa(model1861.3, article1861_collapsed, group = "cgender", group.equal = "loadings")
#int.fit.1861.3 <- cfa(model1861.3, article1861_collapsed, group = "cgender", group.equal = c("loadings", "intercepts"))
#all.results.1861.3 <- mi.results(conf.fit.1861.3,load.fit.1861.3,int.fit.1861.3)
#all.results.1861.3

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

all.results.1861.3 <- mi.results.sc(conf.fit.1861.3,thres.fit.1861.3,load.fit.1861.3)
all.results.1861.3

# Step 4
# Chisquare significant, RMSEA > .08, so we reject configural invariance
# CFI ok (0.984)

# Article 1931: Sawaoka ---------------------------------------------------
# Study 1
# Comparisons 1-2
url <- 'https://osf.io/sfq6a//?action=download'
filename <- '../article1931.1.sav'
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
all.results.1931.1 <- mi.results(conf.fit.1931.1,load.fit.1931.1,int.fit.1931.1)
all.results.1931.1

# Step 4
# Chisquare significant, RMSEA > .08, CFI < .95 so we reject configural invariance

# Comparison 2
# Grouping variable: target commenter: first or last (TargetCommenter)
# Scale variable: Evaluation of target / Impression of commenter (Neg1 + Neg2 + Pos1 + Pos2)
conf.fit.1931.2 <- cfa(model1931.1, data = article1931.1, group = "TargetCommenter")
load.fit.1931.2 <- cfa(model1931.1, article1931.1, group = "TargetCommenter", group.equal = "loadings")
int.fit.1931.2 <- cfa(model1931.1, article1931.1, group = "TargetCommenter", group.equal = c("loadings", "intercepts"))
all.results.1931.2 <- mi.results(conf.fit.1931.2,load.fit.1931.2,int.fit.1931.2)
all.results.1931.2

# Step 4
# Chisquare significant, RMSEA > .08, CFI < .95 so we reject configural invariance

# Study 2
# Comparisons 3-4
url <- 'https://osf.io/jbam4//?action=download'
filename <- '../article1931.2.sav'
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
all.results.1931.3 <- mi.results(conf.fit.1931.3,load.fit.1931.3,int.fit.1931.3)
all.results.1931.3

# Step 4
# Chisquare significant, RMSEA > .08, so we reject configural invariance
# CFI ok (0.961)

# Comparison 4
# Grouping variable: response type: control; anonymous; or upvoting (ResponseType)
# Scale variable: Evaluation of target / Impression of commenter (Neg1 + Neg2 + Pos1 + Pos2)
conf.fit.1931.4 <- cfa(model1931.1, data = article1931.2, group = "ResponseType")
load.fit.1931.4 <- cfa(model1931.1, article1931.2, group = "ResponseType", group.equal = "loadings")
int.fit.1931.4 <- cfa(model1931.1, article1931.2, group = "ResponseType", group.equal = c("loadings", "intercepts"))
all.results.1931.4 <- mi.results(conf.fit.1931.4,load.fit.1931.4,int.fit.1931.4)
all.results.1931.4

# Step 4
# Chisquare significant, RMSEA > .08 so we reject configural invariance
# CFI ok (0.961)

# Study 3
# Comparisons 5-6
url <- 'https://osf.io/pfzc6//?action=download'
filename <- '../article1931.3.sav'
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
all.results.1931.5 <- mi.results(conf.fit.1931.5,load.fit.1931.5,int.fit.1931.5)
all.results.1931.5

# Step 4
# Chisquare significant, RMSEA > .08, CFI < .95 so we reject configural invariance

# Comparison 6
# Grouping variable: surprise: control; no surprise; or surprise (Surprise)
# Scale variable: Evaluation of target / Impression of commenter (Neg1 + Neg2 + Pos1 + Pos2)
conf.fit.1931.6 <- cfa(model1931.1, data = article1931.3, group = "Surprise")
load.fit.1931.6 <- cfa(model1931.1, article1931.3, group = "Surprise", group.equal = "loadings")
int.fit.1931.6 <- cfa(model1931.1, article1931.3, group = "Surprise", group.equal = c("loadings", "intercepts"))
all.results.1931.6 <- mi.results(conf.fit.1931.6,load.fit.1931.6,int.fit.1931.6)
all.results.1931.6

# Step 4
# Chisquare significant, RMSEA > .08, CFI < .95 so we reject configural invariance

# Study 4a
# Comparisons 7-12
url <- 'https://osf.io/924gy//?action=download'
filename <- '../article1931.4.sav'
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
all.results.1931.7 <- mi.results(conf.fit.1931.7,load.fit.1931.7,int.fit.1931.7)
all.results.1931.7

# Step 4
# Chisquare significant, RMSEA > .08, CFI < .95 so we reject configural invariance

# Comparison 8
# Grouping variable: high-status-offender condition (Offender)
# Scale variable: Evaluation of target / Impression of commenter (Neg1 + Neg2 + Pos1 + Pos2)
conf.fit.1931.8 <- cfa(model1931.1, data = article1931.4, group = "Offender")
load.fit.1931.8 <- cfa(model1931.1, article1931.4, group = "Offender", group.equal = "loadings")
int.fit.1931.8 <- cfa(model1931.1, article1931.4, group = "Offender", group.equal = c("loadings", "intercepts"))
all.results.1931.8 <- mi.results(conf.fit.1931.8,load.fit.1931.8,int.fit.1931.8)
all.results.1931.8

# Step 4
# Chisquare significant, RMSEA > .08, CFI < .95 so we reject configural invariance

# Comparison 9
# Grouping variable: viral vs non-viral outrage (ViralOutrage)
# Scale variable: Perceived as harmful (Harm1 + Harm2 + Harm3 + Harm4)
model1931.2 <- 'F=~ Harm1 + Harm2 + Harm3 + Harm4' 

conf.fit.1931.9 <- cfa(model1931.2, data = article1931.4, group = "ViralOutrage")
load.fit.1931.9 <- cfa(model1931.2, article1931.4, group = "ViralOutrage", group.equal = "loadings")
int.fit.1931.9 <- cfa(model1931.2, article1931.4, group = "ViralOutrage", group.equal = c("loadings", "intercepts"))
all.results.1931.9 <- mi.results(conf.fit.1931.9,load.fit.1931.9,int.fit.1931.9)
all.results.1931.9

# Step 4
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI change < .01, AIC and BIC lower; metric invariance holds. (RMSEA > .01 but fit improves)
# Scalar invariance: nonsig chisq change + CFI change < .01, AIC and BIC lower; scalar invariance holds. (RMSEA > .01 but fit improves)

# Comparison 10
# Grouping variable: high-status-offender condition (Offender)
# Scale variable: Perceived as harmful (Harm1 + Harm2 + Harm3 + Harm4)
conf.fit.1931.10 <- cfa(model1931.2, data = article1931.4, group = "Offender")
load.fit.1931.10 <- cfa(model1931.2, article1931.4, group = "Offender", group.equal = "loadings")
int.fit.1931.10 <- cfa(model1931.2, article1931.4, group = "Offender", group.equal = c("loadings", "intercepts"))
all.results.1931.10 <- mi.results(conf.fit.1931.10,load.fit.1931.10,int.fit.1931.10)
all.results.1931.10

# Step 4
# Three items so we assume configural invariance
# Metric invariance: sig chisq change + RMSEA change > .01, AIC larger; metric invariance rejected (CFI change ok)


# Comparison 11
# Grouping variable: viral vs non-viral outrage (ViralOutrage)
# Scale variable: Perceived to be vulnerable to criticism (Vul1 + Vul2 + Vul3 + Vul4)
model1931.3 <- 'F=~ Vul1 + Vul2 + Vul3 + Vul4' 

conf.fit.1931.11 <- cfa(model1931.3, data = article1931.4, group = "ViralOutrage")
load.fit.1931.11 <- cfa(model1931.3, article1931.4, group = "ViralOutrage", group.equal = "loadings")
int.fit.1931.11 <- cfa(model1931.3, article1931.4, group = "ViralOutrage", group.equal = c("loadings", "intercepts"))
all.results.1931.11 <- mi.results(conf.fit.1931.11,load.fit.1931.11,int.fit.1931.11)
all.results.1931.11

# Step 4
# Chisquare significant, RMSEA > .08, CFI < .95 so we reject configural invariance


# Comparison 12
# Grouping variable: high-status-offender condition (Offender)
# Scale variable: Perceived to be vulnerable to criticism (Vul1 + Vul2 + Vul3 + Vul4)
conf.fit.1931.12 <- cfa(model1931.3, data = article1931.4, group = "Offender")
load.fit.1931.12 <- cfa(model1931.3, article1931.4, group = "Offender", group.equal = "loadings")
int.fit.1931.12 <- cfa(model1931.3, article1931.4, group = "Offender", group.equal = c("loadings", "intercepts"))
all.results.1931.12 <- mi.results(conf.fit.1931.12,load.fit.1931.12,int.fit.1931.12)
all.results.1931.12

# Step 4
# Chisquare significant, RMSEA > .08, CFI < .95 so we reject configural invariance

# Study 4b
# Comparisons 13-16
url <- 'https://osf.io/ndfpx//?action=download'
filename <- '../article1931.5.sav'
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
all.results.1931.13 <- mi.results(conf.fit.1931.13,load.fit.1931.13,int.fit.1931.13)
all.results.1931.13

# Step 4
# Chisquare significant, RMSEA > .08, CFI < .95 so we reject configural invariance

# Comparison 14
# Grouping variable: viral vs non-viral outrage (ViralOutrage)
# Scale variable: Perceived as harmful (Harm1 + Harm2 + Harm3 + Harm4)
conf.fit.1931.14 <- cfa(model1931.2, data = article1931.5, group = "ViralOutrage")
load.fit.1931.14 <- cfa(model1931.2, article1931.5, group = "ViralOutrage", group.equal = "loadings")
int.fit.1931.14 <- cfa(model1931.2, article1931.5, group = "ViralOutrage", group.equal = c("loadings", "intercepts"))
all.results.1931.14 <- mi.results(conf.fit.1931.14,load.fit.1931.14,int.fit.1931.14)
all.results.1931.14

# Step 4
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
all.results.1931.15 <- mi.results(conf.fit.1931.15,load.fit.1931.15,int.fit.1931.15)
all.results.1931.15

# Step 4
# Chisquare significant, RMSEA > .08, CFI < .95 so we reject configural invariance

# Comparison 16
# Grouping variable: offender: unsympathetic or control (Offender)
# Scale variable: Perceived as harmful (Harm1 + Harm2 + Harm3 + Harm4)
conf.fit.1931.16 <- cfa(model1931.2, data = article1931.5, group = "Offender")
load.fit.1931.16 <- cfa(model1931.2, article1931.5, group = "Offender", group.equal = "loadings")
int.fit.1931.16 <- cfa(model1931.2, article1931.5, group = "Offender", group.equal = c("loadings", "intercepts"))
all.results.1931.16 <- mi.results(conf.fit.1931.16,load.fit.1931.16,int.fit.1931.16)
all.results.1931.16

# Step 4
# Chisquare non-significant, RMSEA < .08, CFI > .95, so configural invariance holds
# Metric: change in CFI < .01, BIC is smaller.
# Metric: change in RMSEA is > .01 and increases (0 to 0.036), so it exceeds the cutoff. AIC is larger so it exceeds the cutoff
# Metric invariance is rejected; configural invariance holds.


# Article 2091: Sma -------------------------------------------------------
# Study 1b
url <- 'https://osf.io/jpyrd//?action=download'
filename <- '../data/data-main/article2091.xlsx'
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
all.results.2091 <- mi.results(conf.fit.2091,load.fit.2091,int.fit.2091)
all.results.2091

# Step 4
# Three items so we assume configural invariance
# Metric invariance: nonsig chisq change + CFI change < .01, BIC lower; 
# But: RMSEA change > .01 and AIC larger. Does metric invariance hold?

# Scalar invariance: nonsig chisq change + CFI change < .01, AIC and BIC lower; scalar invariance holds. (RMSEA > .01 but fit improves)

