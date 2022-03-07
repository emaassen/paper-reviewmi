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

### Esthers code, probably the wrong one

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

# Fit the thresholds invariance model
thres.mod.131.1 <- measEq.syntax(model.131.1,
                                      ID.fac = "std.lv",
                                      ID.cat = "Wu",
                                      ordered = c("Eq5Dmob", "Eq5Dselfc", "Eq5Duact", "Eq5Dpain", "Eq5Dmood"),
                                      group = "depression", 
                                      parameterization = "delta", 
                                      data = article131,
                                      group.equal = c("thresholds")) 


thres.fit.131.1 <- cfa(as.character(thres.mod.131.1), article131, group = "depression", estimator = "WLSMV")

# Fit the loadingsinvariance model
load.mod.131.1 <- measEq.syntax(model.131.1,
                                 ID.fac = "std.lv",
                                 ID.cat = "Wu",
                                 ordered = c("Eq5Dmob", "Eq5Dselfc", "Eq5Duact", "Eq5Dpain", "Eq5Dmood"),
                                 group = "depression", 
                                 parameterization = "delta", 
                                 data = article131,
                                 group.equal = c("thresholds", "loadings")) 


load.fit.131.1 <- cfa(as.character(load.mod.131.1), article131, group = "depression", estimator = "WLSMV")

all.results.131 <- mi.results.sc(conf.fit.131.1,thres.fit.131.1,load.fit.131.1)
all.results.131

### DAMI'S CODE, probably the good one. 

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

#Create model 
model.251.1 <- 'F1 =~ Lpls + Lcor + Lwon + Lgoo'

# Fit the configural invariance model
conf.fit.251.1 <- cfa(model.251.1, article251.1 , group = "man")

# Fit the loadings invariance model
load.fit.251.1 <- cfa(model.251.1, article251.1 , group = "man", group.equal = "loadings")

# Fit the intercepts invariance model
int.fit.251.1 <- cfa(model.251.1, article251.1 , group = "man", group.equal = c("loadings", "intercepts"))

all.results.251.1 <- mi.results(conf.fit.251.1,load.fit.251.1,int.fit.251.1)
all.results.251.1

# Study 2: three comparisons
url <- 'https://osf.io/vj8nz//?action=download'
filename <- '../data/data-main/article251.2.txt'
GET(url, write_disk(filename, overwrite = TRUE))
article251.2 <- read.table(filename, header=TRUE) 
Data <- article251.2

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

# These are the variables we can use if we want to do MI test, but want to ask you first. 

# comparison 3
# grouping variable is hypnosis vs relaxation condition at time 1 (variable name is hypnosis)
# scale is three self-report items probing likability; friendliness; and trustworthiness
# That would make the following variables:

## Compute Variables for the two targets
#Data$exp_Bobpre<-(Data$qBF_pre+Data$qBL_pre+Data$qBT_pre)/3
#Data$exp_Janpre<-(Data$qJF_pre+Data$qJL_pre+Data$qJT_pre)/3

# and then the final variable: 
# Data$exp_pre<-ifelse(Data$EC==1,Data$exp_Bobpre-Data$exp_Janpre,Data$exp_Janpre-Data$exp_Bobpre)

# comparison 4
# grouping variable is hypnosis vs relaxation condition at time 2 (variable name is hypnosis)
# scale is three self-report items probing likability; friendliness; and trustworthiness
# that would make the following variables:
## Compute Variables for the two targets
#Data$exp_Bobpost<-(Data$qBF_post+Data$qBL_post+Data$qBT_post)/3
#Data$exp_Janpost<-(Data$qJF_post+Data$qJL_post+Data$qJT_post)/3

# and then the final variable:
# Data$exp_post<-ifelse(Data$EC==1,Data$exp_Bobpost-Data$exp_Janpost,Data$exp_Janpost-Data$exp_Bobpost)


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
conf.fit.301.1 <- cfa(model.301.1, data = article301, group = "cond")

# Fit the loadings invariance model
load.fit.301.1 <- cfa(model.301.1, article301 , group = "cond", group.equal = "loadings")

# Fit the intercepts invariance model
int.fit.301.1 <- cfa(model.301.1, article301 , group = "cond", group.equal = c("loadings", "intercepts"))

all.results.301.1 <- mi.results(conf.fit.301.1,load.fit.301.1,int.fit.301.1)
all.results.301.1

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

# comparison 2: match - abortion - integrity
model.481.2 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.2 <- cfa(model.481.2, data = article481.abortion, group = "match")
load.fit.481.2 <- cfa(model.481.2, article481.abortion, group = "match", group.equal = "loadings")
int.fit.481.2 <- cfa(model.481.2, article481.abortion, group = "match", group.equal = c("loadings", "intercepts"))
all.results.481.2 <- mi.results(conf.fit.481.2,load.fit.481.2,int.fit.481.2)
all.results.481.2

# comparison 3: match - gun - integrity
model.481.3 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.3 <- cfa(model.481.3, data = article481.gun, group = "match")
load.fit.481.3 <- cfa(model.481.3, article481.gun, group = "match", group.equal = "loadings")
int.fit.481.3 <- cfa(model.481.3, article481.gun, group = "match", group.equal = c("loadings", "intercepts"))
all.results.481.3 <- mi.results(conf.fit.481.3,load.fit.481.3,int.fit.481.3)
all.results.481.3

# comparison 4: match - testing - integrity
model.481.4 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.4 <- cfa(model.481.4, data = article481.testing, group = "match")
load.fit.481.4 <- cfa(model.481.4, article481.testing, group = "match", group.equal = "loadings")
int.fit.481.4 <- cfa(model.481.4, article481.testing, group = "match", group.equal = c("loadings", "intercepts"))
all.results.481.4 <- mi.results(conf.fit.481.4,load.fit.481.4,int.fit.481.4)
all.results.481.4

# comparison 5: match - suicide - integrity
model.481.5 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.5 <- cfa(model.481.5, data = article481.suicide, group = "match")
load.fit.481.5 <- cfa(model.481.5, article481.suicide, group = "match", group.equal = "loadings")
int.fit.481.5 <- cfa(model.481.5, article481.suicide, group = "match", group.equal = c("loadings", "intercepts"))
all.results.481.5 <- mi.results(conf.fit.481.5,load.fit.481.5,int.fit.481.5)
all.results.481.5

# comparison 6: match - overall - integrity
model.481.6 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.6 <- cfa(model.481.6, data = article481.1, group = "match")
load.fit.481.6 <- cfa(model.481.6, article481.1, group = "match", group.equal = "loadings")
int.fit.481.6 <- cfa(model.481.6, article481.1, group = "match", group.equal = c("loadings", "intercepts"))
all.results.481.6 <- mi.results(conf.fit.481.6,load.fit.481.6,int.fit.481.6)
all.results.481.6

# comparison 7: match - capital - benevolence
model.481.7 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.7 <- cfa(model.481.7, data = article481.capital, group = "match")
load.fit.481.7 <- cfa(model.481.7, article481.capital, group = "match", group.equal = "loadings")
int.fit.481.7 <- cfa(model.481.7, article481.capital, group = "match", group.equal = c("loadings", "intercepts"))
all.results.481.7 <- mi.results(conf.fit.481.7,load.fit.481.7,int.fit.481.7)
all.results.481.7

# comparison 8: match - abortion - benevolence
model.481.8 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.8 <- cfa(model.481.8, data = article481.abortion, group = "match")
load.fit.481.8 <- cfa(model.481.8, article481.abortion, group = "match", group.equal = "loadings")
int.fit.481.8 <- cfa(model.481.8, article481.abortion, group = "match", group.equal = c("loadings", "intercepts"))
all.results.481.8 <- mi.results(conf.fit.481.8,load.fit.481.8,int.fit.481.8)
all.results.481.8

# comparison 9: match - gun - benevolence
model.481.9 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.9 <- cfa(model.481.9, data = article481.gun, group = "match")
load.fit.481.9 <- cfa(model.481.9, article481.gun, group = "match", group.equal = "loadings")
int.fit.481.9 <- cfa(model.481.9, article481.gun, group = "match", group.equal = c("loadings", "intercepts"))
all.results.481.9 <- mi.results(conf.fit.481.9,load.fit.481.9,int.fit.481.9)
all.results.481.9

# comparison 10: match - testing - benevolence
model.481.10 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.10 <- cfa(model.481.10, data = article481.testing, group = "match")
load.fit.481.10 <- cfa(model.481.10, article481.testing, group = "match", group.equal = "loadings")
int.fit.481.10 <- cfa(model.481.10, article481.testing, group = "match", group.equal = c("loadings", "intercepts"))
all.results.481.10 <- mi.results(conf.fit.481.10,load.fit.481.10,int.fit.481.10)
all.results.481.10

# comparison 11: match - suicide - benevolence
model.481.11 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.11 <- cfa(model.481.11, data = article481.suicide, group = "match")
load.fit.481.11 <- cfa(model.481.11, article481.suicide, group = "match", group.equal = "loadings")
int.fit.481.11 <- cfa(model.481.11, article481.suicide, group = "match", group.equal = c("loadings", "intercepts"))
all.results.481.11 <- mi.results(conf.fit.481.11,load.fit.481.11,int.fit.481.11)
all.results.481.11

# comparison 12: match - overall - benevolence
model.481.12 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.12 <- cfa(model.481.12, data = article481.1, group = "match")
load.fit.481.12 <- cfa(model.481.12, article481.1, group = "match", group.equal = "loadings")
int.fit.481.12 <- cfa(model.481.12, article481.1, group = "match", group.equal = c("loadings", "intercepts"))
all.results.481.12 <- mi.results(conf.fit.481.12,load.fit.481.12,int.fit.481.12)
all.results.481.12

# comparison 13: care - capital - integrity
model.481.13 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.13 <- cfa(model.481.13, data = article481.capital, group = "care")
load.fit.481.13 <- cfa(model.481.13, article481.capital, group = "care", group.equal = "loadings")
int.fit.481.13 <- cfa(model.481.13, article481.capital, group = "care", group.equal = c("loadings", "intercepts"))
all.results.481.13 <- mi.results(conf.fit.481.13,load.fit.481.13,int.fit.481.13)
all.results.481.13

# comparison 14: care - abortion - integrity
model.481.14 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.14 <- cfa(model.481.14, data = article481.abortion, group = "care")
load.fit.481.14 <- cfa(model.481.14, article481.abortion, group = "care", group.equal = "loadings")
int.fit.481.14 <- cfa(model.481.14, article481.abortion, group = "care", group.equal = c("loadings", "intercepts"))
all.results.481.14 <- mi.results(conf.fit.481.14,load.fit.481.14,int.fit.481.14)
all.results.481.14

# comparison 15: care - gun - integrity
model.481.15 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.15 <- cfa(model.481.15, data = article481.gun, group = "care")
load.fit.481.15 <- cfa(model.481.15, article481.gun, group = "care", group.equal = "loadings")
int.fit.481.15 <- cfa(model.481.15, article481.gun, group = "care", group.equal = c("loadings", "intercepts"))
all.results.481.15 <- mi.results(conf.fit.481.15,load.fit.481.15,int.fit.481.15)
all.results.481.15

# comparison 16: care - testing - integrity
model.481.16 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.16 <- cfa(model.481.16, data = article481.testing, group = "care")
load.fit.481.16 <- cfa(model.481.16, article481.testing, group = "care", group.equal = "loadings")
int.fit.481.16 <- cfa(model.481.16, article481.testing, group = "care", group.equal = c("loadings", "intercepts"))
all.results.481.16 <- mi.results(conf.fit.481.16,load.fit.481.16,int.fit.481.16)
all.results.481.16

# comparison 17: care - suicide - integrity
model.481.17 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.17 <- cfa(model.481.17, data = article481.suicide, group = "care")
load.fit.481.17 <- cfa(model.481.17, article481.suicide, group = "care", group.equal = "loadings")
int.fit.481.17 <- cfa(model.481.17, article481.suicide, group = "care", group.equal = c("loadings", "intercepts"))
all.results.481.17 <- mi.results(conf.fit.481.17,load.fit.481.17,int.fit.481.17)
all.results.481.17

# comparison 18: care - overall - integrity
model.481.18 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.18 <- cfa(model.481.18, data = article481.1, group = "care")
load.fit.481.18 <- cfa(model.481.18, article481.1, group = "care", group.equal = "loadings")
int.fit.481.18 <- cfa(model.481.18, article481.1, group = "care", group.equal = c("loadings", "intercepts"))
all.results.481.18 <- mi.results(conf.fit.481.18,load.fit.481.18,int.fit.481.18)
all.results.481.18

# comparison 19: care - capital - benevolence
model.481.19 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.19 <- cfa(model.481.19, data = article481.capital, group = "care")
load.fit.481.19 <- cfa(model.481.19, article481.capital, group = "care", group.equal = "loadings")
int.fit.481.19 <- cfa(model.481.19, article481.capital, group = "care", group.equal = c("loadings", "intercepts"))
all.results.481.19 <- mi.results(conf.fit.481.19,load.fit.481.19,int.fit.481.19)
all.results.481.19

# comparison 20: care - abortion - benevolence
model.481.20 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.20 <- cfa(model.481.20, data = article481.abortion, group = "care")
load.fit.481.20 <- cfa(model.481.20, article481.abortion, group = "care", group.equal = "loadings")
int.fit.481.20 <- cfa(model.481.20, article481.abortion, group = "care", group.equal = c("loadings", "intercepts"))
all.results.481.20 <- mi.results(conf.fit.481.20,load.fit.481.20,int.fit.481.20)
all.results.481.20

# comparison 21: care - gun - benevolence
model.481.21 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.21 <- cfa(model.481.21, data = article481.gun, group = "care")
load.fit.481.21 <- cfa(model.481.21, article481.gun, group = "care", group.equal = "loadings")
int.fit.481.21 <- cfa(model.481.21, article481.gun, group = "care", group.equal = c("loadings", "intercepts"))
all.results.481.21 <- mi.results(conf.fit.481.21,load.fit.481.21,int.fit.481.21)
all.results.481.21

# comparison 22: care - testing - benevolence
model.481.22 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.22 <- cfa(model.481.22, data = article481.testing, group = "care")
load.fit.481.22 <- cfa(model.481.22, article481.testing, group = "care", group.equal = "loadings")
int.fit.481.22 <- cfa(model.481.22, article481.testing, group = "care", group.equal = c("loadings", "intercepts"))
all.results.481.22 <- mi.results(conf.fit.481.22,load.fit.481.22,int.fit.481.22)
all.results.481.22

# comparison 23: care - suicide - benevolence
model.481.23 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.23 <- cfa(model.481.23, data = article481.suicide, group = "care")
load.fit.481.23 <- cfa(model.481.23, article481.suicide, group = "care", group.equal = "loadings")
int.fit.481.23 <- cfa(model.481.23, article481.suicide, group = "care", group.equal = c("loadings", "intercepts"))
all.results.481.23 <- mi.results(conf.fit.481.23,load.fit.481.23,int.fit.481.23)
all.results.481.23

# comparison 24: care - overall - benevolence
model.481.24 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.24 <- cfa(model.481.24, data = article481.1, group = "care")
load.fit.481.24 <- cfa(model.481.24, article481.1, group = "care", group.equal = "loadings")
int.fit.481.24 <- cfa(model.481.24, article481.1, group = "care", group.equal = c("loadings", "intercepts"))
all.results.481.24 <- mi.results(conf.fit.481.24,load.fit.481.24,int.fit.481.24)
all.results.481.24

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

# comparison 26: care - integrity
model.481.26 <- 'F =~ integ1 + integ2 + integ3'
conf.fit.481.26 <- cfa(model.481.26, data = article481.2, group = "care")
load.fit.481.26 <- cfa(model.481.26, article481.2, group = "care", group.equal = "loadings")
int.fit.481.26 <- cfa(model.481.26, article481.2, group = "care", group.equal = c("loadings", "intercepts"))
all.results.481.26 <- mi.results(conf.fit.481.26,load.fit.481.26,int.fit.481.26)
all.results.481.26

# comparison 27: match - benevolence
model.481.27 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.27 <- cfa(model.481.27, data = article481.2, group = "match")
load.fit.481.27 <- cfa(model.481.27, article481.2, group = "match", group.equal = "loadings")
int.fit.481.27 <- cfa(model.481.27, article481.2, group = "match", group.equal = c("loadings", "intercepts"))
all.results.481.27 <- mi.results(conf.fit.481.27,load.fit.481.27,int.fit.481.27)
all.results.481.27

# comparison 28: care - benevolence
model.481.28 <- 'F =~ benev1 + benev2 + benev3'
conf.fit.481.28 <- cfa(model.481.28, data = article481.2, group = "care")
load.fit.481.28 <- cfa(model.481.28, article481.2, group = "care", group.equal = "loadings")
int.fit.481.28 <- cfa(model.481.28, article481.2, group = "care", group.equal = c("loadings", "intercepts"))
all.results.481.28 <- mi.results(conf.fit.481.28,load.fit.481.28,int.fit.481.28)
all.results.481.28


