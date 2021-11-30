### CODE FOR MAIN STUDY SYSTEMATIC REVIEW MEASUREMENT INVARIANCE ##
### This is code to perform reproducibility checks in our main study

rm(list = ls()) # clear workspace
require("httr") # to load data from OSF into R
require("haven") # to load sav files into R
require("readxl") # to load xlsx files into R
require("psych")
require("lavaan")
require("semTools")
require("GPArotation")

# Article 22: Moreira -------------------------------------------------------------------------
# Raw data is shared via OSF
url <- 'https://osf.io/3qda9///?action=download'
filename <- '../article22.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article22 <- read_sav(filename)
# We can make a grouping variable (time) but we do not have item scores, only the sumscore for the Factor.
# As such, we cannot do a MI test 

# Article 77: Schulze -------------------------------------------------------------------------
# Data shared are covariance matrices for 5 groups in a word document, copied from here:
# https://doi.org/10.1371/journal.pone.0207331.s003

us1 <- matrix(c(1.60,0.94,1.36,0.84,0.80,0.84,0.74,0.54,0.45,0.49,0.43,0.52,0.48,0.29,0.39,0.94,1.62,0.99,1.07,0.81,
                0.91,0.77,0.70,0.64,0.63,0.67,0.64,0.55,0.42,0.30,1.36,0.99,1.71,0.91,0.87,0.87,0.72,0.59,0.50,0.64,
                0.55,0.53,0.56,0.35,0.39,0.84,1.07,0.91,1.43,0.89,0.89,0.70,0.59,0.60,0.60,0.57,0.58,0.73,0.49,0.46,
                0.80,0.81,0.87,0.89,1.59,0.95,0.78,0.60,0.60,0.55,0.53,0.55,0.77,0.38,0.29,0.84,0.91,0.87,0.89,0.95,
                1.56,0.85,0.69,0.43,0.60,0.60,0.60,0.73,0.42,0.48,0.74,0.77,0.72,0.70,0.78,0.85,1.65,0.81,0.36,0.68,
                0.71,0.68,0.70,0.63,0.50,0.54,0.70,0.59,0.59,0.60,0.69,0.81,1.40,0.69,0.68,0.77,0.79,0.60,0.53,0.55,
                0.45,0.64,0.50,0.60,0.60,0.43,0.36,0.69,1.73,1.10,0.99,0.51,0.63,0.45,0.44,0.49,0.63,0.64,0.60,0.55,
                0.60,0.68,0.68,1.10,1.56,1.10,0.55,0.74,0.57,0.58,0.43,0.67,0.55,0.57,0.53,0.60,0.71,0.77,0.99,1.10,
                1.47,0.61,0.71,0.61,0.53,0.52,0.64,0.53,0.58,0.55,0.60,0.68,0.79,0.51,0.55,0.61,1.19,0.61,0.56,0.56,
                0.48,0.55,0.56,0.73,0.77,0.73,0.70,0.60,0.63,0.74,0.71,0.61,1.46,0.77,0.60,0.29,0.42,0.35,0.49,0.38,
                0.42,0.63,0.53,0.45,0.57,0.61,0.56,0.77,1.12,0.66,0.39,0.30,0.39,0.46,0.29,0.48,0.50,0.55,0.44,0.58,
                0.53,0.56,0.60,0.66,1.18), nrow=15,ncol=15)

us2 <- matrix(c(1.50,1.05,1.10,0.94,0.64,0.83,0.70,0.39,0.68,0.58,0.50,0.62,0.62,0.35,0.31,1.05,1.61,1.07,0.97,0.65,
                0.85,0.76,0.59,0.69,0.53,0.53,0.84,0.70,0.34,0.30,1.10,1.07,1.62,1.05,0.79,0.95,0.71,0.54,0.62,0.64,
                0.47,0.58,0.56,0.45,0.26,0.94,0.97,1.05,1.48,0.63,0.70,0.62,0.60,0.71,0.56,0.49,0.50,0.61,0.39,0.26,
                0.64,0.65,0.79,0.63,1.40,0.96,0.57,0.41,0.48,0.40,0.33,0.45,0.51,0.26,0.22,0.83,0.85,0.95,0.70,0.96,
                1.51,0.82,0.51,0.54,0.57,0.45,0.72,0.60,0.42,0.32,0.70,0.76,0.71,0.62,0.57,0.82,1.55,0.55,0.49,0.53,
                0.47,0.67,0.55,0.42,0.35,0.39,0.59,0.54,0.60,0.41,0.51,0.55,1.22,0.64,0.74,0.60,0.78,0.61,0.45,0.28,
                0.68,0.69,0.62,0.71,0.48,0.54,0.49,0.64,1.44,0.84,0.78,0.66,0.73,0.31,0.28,0.58,0.53,0.64,0.56,0.40,
                0.57,0.53,0.74,0.84,1.31,0.85,0.73,0.59,0.47,0.45,0.50,0.53,0.47,0.49,0.33,0.45,0.47,0.60,0.78,0.85,
                1.17,0.61,0.51,0.41,0.41,0.62,0.84,0.58,0.50,0.45,0.72,0.67,0.78,0.66,0.73,0.61,1.47,0.58,0.48,0.34,
                0.62,0.70,0.56,0.61,0.51,0.60,0.55,0.61,0.73,0.59,0.51,0.58,1.26,0.32,0.31,0.35,0.34,0.45,0.39,0.26,
                0.42,0.42,0.45,0.31,0.47,0.41,0.48,0.32,0.89,0.47,0.31,0.30,0.26,0.26,0.22,0.32,0.35,0.28,0.28,0.45,
                0.41,0.34,0.31,0.47,0.67), nrow=15, ncol=15)

ger1 <- matrix(c(1.88,1.38,1.60,1.31,1.00,1.09,1.06,0.72,0.52,0.58,0.52,0.57,0.85,0.64,0.40,1.38,1.86,1.22,1.38,0.97,
                 1.22,1.17,0.88,0.77,0.70,0.66,0.69,0.85,0.59,0.26,1.60,1.22,1.95,1.39,0.91,1.09,0.98,0.69,0.60,0.65,
                 0.58,0.63,0.97,0.69,0.58,1.31,1.38,1.39,1.75,0.93,1.04,1.06,0.67,0.66,0.64,0.61,0.61,1.00,0.69,0.47,
                 1.00,0.97,0.91,0.93,1.87,1.24,0.91,0.63,0.58,0.78,0.65,0.47,0.72,0.53,0.35,1.09,1.22,1.09,1.04,1.24,
                 2.18,1.27,0.84,0.69,0.81,0.56,0.72,0.81,0.53,0.47,1.06,1.17,0.98,1.06,0.91,1.27,1.98,0.94,0.94,0.86,
                 0.69,0.81,0.82,0.49,0.40,0.72,0.88,0.69,0.67,0.63,0.84,0.94,1.67,1.31,0.89,0.92,1.04,0.59,0.65,0.52,
                 0.52,0.77,0.60,0.66,0.58,0.69,0.94,1.31,1.70,1.03,1.14,0.96,0.60,0.67,0.52,0.58,0.70,0.65,0.64,0.78,
                 0.81,0.86,0.89,1.03,1.98,0.95,0.96,0.62,0.62,0.46,0.52,0.66,0.58,0.61,0.65,0.56,0.69,0.92,1.14,0.95,
                 1.38,0.75,0.50,0.66,0.53,0.57,0.69,0.63,0.61,0.47,0.72,0.81,1.04,0.96,0.96,0.75,1.36,0.51,0.60,0.56,
                 0.85,0.85,0.97,1.00,0.72,0.81,0.82,0.59,0.60,0.62,0.50,0.51,1.31,0.72,0.56,0.64,0.59,0.69,0.69,0.53,
                 0.53,0.49,0.65,0.67,0.62,0.66,0.60,0.72,1.41,0.82,0.40,0.26,0.58,0.47,0.35,0.47,0.40,0.52,0.52,0.46,
                 0.53,0.56,0.56,0.82,1.25), nrow=15, ncol=15)

ger2 <- matrix(c(1.75,1.27,1.34,1.14,0.56,0.78,0.68,0.48,0.37,0.61,0.43,0.52,0.60,0.50,0.25,1.27,1.85,1.17,1.27,0.60,
                 1.08,0.86,0.64,0.53,0.69,0.46,0.56,0.59,0.51,0.26,1.34,1.17,1.67,1.21,0.59,0.72,0.59,0.53,0.34,0.56,
                 0.40,0.51,0.63,0.53,0.34,1.14,1.27,1.21,1.64,0.61,0.88,0.69,0.58,0.48,0.66,0.43,0.59,0.79,0.59,0.30,
                 0.56,0.60,0.59,0.61,1.45,0.76,0.74,0.41,0.49,0.43,0.35,0.36,0.43,0.19,0.06,0.78,1.08,0.72,0.88,0.76,
                 1.73,0.98,0.61,0.57,0.70,0.38,0.55,0.58,0.39,0.15,0.68,0.86,0.59,0.69,0.74,0.98,1.84,0.57,0.47,0.45,
                 0.35,0.62,0.57,0.22,0.15,0.48,0.64,0.53,0.58,0.41,0.61,0.57,1.24,0.68,0.58,0.56,0.70,0.46,0.35,0.10,
                 0.37,0.53,0.34,0.48,0.49,0.57,0.47,0.68,1.24,0.81,0.72,0.57,0.41,0.37,0.37,0.61,0.69,0.56,0.66,0.43,
                 0.70,0.45,0.58,0.81,1.65,0.62,0.68,0.57,0.49,0.34,0.43,0.46,0.40,0.43,0.35,0.38,0.35,0.56,0.72,0.62,
                 1.06,0.58,0.44,0.37,0.27,0.52,0.56,0.51,0.59,0.36,0.55,0.62,0.70,0.57,0.68,0.58,1.26,0.47,0.31,0.23,
                 0.60,0.59,0.63,0.79,0.43,0.58,0.57,0.46,0.41,0.57,0.44,0.47,1.04,0.50,0.34,0.50,0.51,0.53,0.59,0.19,
                 0.39,0.22,0.35,0.37,0.49,0.37,0.31,0.50,1.28,0.66,0.25,0.26,0.34,0.30,0.06,0.15,0.15,0.10,0.37,0.34,
                 0.27,0.23,0.34,0.66,1.10), nrow=15, ncol=15)

india <- matrix(c(1.17,0.76,0.73,0.82,0.50,0.57,0.44,0.36,0.54,0.28,0.06,0.62,0.38,0.19,0.22,0.76,1.17,0.63,0.87,0.48,
                  0.52,0.31,0.30,0.48,0.27,0.09,0.58,0.37,0.19,0.23,0.73,0.63,1.06,0.63,0.41,0.51,0.33,0.26,0.53,0.34,
                  0.05,0.49,0.35,0.11,0.15,0.82,0.87,0.63,1.14,0.48,0.52,0.41,0.35,0.56,0.34,0.15,0.65,0.43,0.18,0.22,
                  0.50,0.48,0.41,0.48,1.03,0.54,0.33,0.27,0.33,0.18,0.07,0.50,0.34,0.09,0.14,0.57,0.52,0.51,0.52,0.54,
                  1.03,0.49,0.38,0.51,0.26,0.07,0.54,0.34,0.15,0.17,0.44,0.31,0.33,0.41,0.33,0.49,0.94,0.52,0.48,0.40,
                  0.28,0.44,0.31,0.20,0.17,0.36,0.30,0.26,0.35,0.27,0.38,0.52,0.82,0.54,0.36,0.31,0.46,0.29,0.25,0.25,
                  0.54,0.48,0.53,0.56,0.33,0.51,0.48,0.54,1.28,0.61,0.37,0.70,0.48,0.25,0.27,0.28,0.27,0.34,0.34,0.18,
                  0.26,0.40,0.36,0.61,0.94,0.46,0.40,0.25,0.21,0.16,0.06,0.09,0.05,0.15,0.07,0.07,0.28,0.31,0.37,0.46,
                  0.74,0.23,0.16,0.23,0.17,0.62,0.58,0.49,0.65,0.50,0.54,0.44,0.46,0.70,0.40,0.23,1.07,0.52,0.19,0.22,
                  0.38,0.37,0.35,0.43,0.34,0.34,0.31,0.29,0.48,0.25,0.16,0.52,0.82,0.19,0.20,0.19,0.19,0.11,0.18,0.09,
                  0.15,0.20,0.25,0.25,0.21,0.23,0.19,0.19,0.51,0.28,0.22,0.23,0.15,0.22,0.14,0.17,0.17,0.25,0.27,0.16,
                  0.17,0.22,0.20,0.28,0.46), nrow=15, ncol=15)

# variables for factor: item1 until item15
# grouping variables are the countries (us1, us2, ger1, ger2, india)

# Create list of correlation matrices to fit to lavaan
sigma.pop.Schulze <- list(ger1, ger2, us1, us2)

# Provide column names for covariance matrices
for(i in 1:length(sigma.pop.Schulze)){
  colnames(sigma.pop.Schulze[[i]]) <- paste0("V", 1:15)
}

# Create model for dataset based on article

Mod.Schulze_4F <- "
F1 =~ V1 + V2 + V4 
F2 =~ V5 + V6 + V7 + V13
F3 =~ V8 + V9 + V10 + V11 
F4 =~ V13 + V14 + V15

F1 ~~ F2
F1 ~~ F3
F1 ~~ F4
F2 ~~ F3
F2 ~~ F4
F3 ~~ F4
"

#Mod.schulze <- NA
#for(l in 1:ncol(sigma.pop.Schulze[[1]])){
#  Mod.schulze[l] <- c(paste0("F1 =~", 'V', l))
#}

# Create vector of sample size based on information provided by authors
sample.schulze <- c(146, 194, 155, 193, 432)

# Specify measurement model based on the type of data. In this case, the data are ordinal, and therefore
# the model specifications and constraints proposed by Wu and Estabrook (2016; https://link.springer.com/article/10.1007%2Fs11336-016-9506-0) should be used )
# However, since DWLS requires full data, we ran the analysis using a classic (i.e., linear) CFA with ML 
# Note that, for 5 categories items (such as the ones used here), 
# Rehmtulla et al. (2012; link) indicated that CFA may be appropriate. In this case, we a


# Fit the configural model
Conf.fit.schulze <- cfa(model = Mod.Schulze_4F, 
                        sample.cov = sigma.pop.Schulze, 
                        sample.nobs = c(146, 194, 193, 432),
                        estimator = "ML"
                        )

# Create results table and store configural goodness-of-fit measures
all.results.schulze <- matrix(NA, ncol = 11, nrow = 3)
colnames(all.results.schulze) <- c("chisq","df","pvalue", 
                                   "rmsea", "cfi", "srmr", "diffRMSEA", "diffCFI", "lavtestLRT", "AIC", "BIC")

all.results.schulze[1,1:6]<-round(data.matrix(fitmeasures(Conf.fit.schulze,
                                               fit.measures = c("chisq","df","pvalue", 
                                                                "rmsea", "cfi", "srmr"))), digits=3)


# Fit the loadings invariant model
Load.fit.schulze <- cfa(model = Mod.Schulze_4F, 
                        sample.cov = sigma.pop.Schulze, 
                        sample.nobs = c(146, 194,193, 432),
                        estimator = "ML",
                        group.equal = "loadings"
)

# Store loadings invariance model goodness-of-fit measures results
all.results.schulze[2,1:6]<-round(data.matrix(fitmeasures(Load.fit.schulze,
                                                       fit.measures = c("chisq","df","pvalue", 
                                                                        "rmsea", "cfi", "srmr"))), digits=3)

# Fit the intercepts invariance model
Int.fit.schulze <- cfa(model = Mod.Schulze_4F, 
                        sample.cov = sigma.pop.Schulze, 
                        sample.nobs = c(146, 194, 193, 432),
                        estimator = "ML",
                        group.equal = c("loadings", "intercepts")
)

# Store the intercepts invariance model goodness-of-fit measures results
all.results.schulze[3,1:6]<-round(data.matrix(fitmeasures(Int.fit.schulze,
                                                       fit.measures = c("chisq","df","pvalue", 
                                                                        "rmsea", "cfi", "srmr"))), digits=3)


# Compute the difference in goodness-of-fit measures
all.results.schulze[2,7:8] <- all.results.schulze[2,4:5] - all.results.schulze[1,4:5]
all.results.schulze[3,7:8] <- all.results.schulze[3,4:5] - all.results.schulze[2,4:5]
  
# Compute the LRT between the different models and store the p-value result
all.results.schulze[2,9] <- lavTestLRT(Conf.fit.schulze, Load.fit.schulze)$`Pr(>Chisq)`[2]
all.results.schulze[3,9] <- lavTestLRT(Load.fit.schulze, Int.fit.schulze)$`Pr(>Chisq)`[2]

all.results.schulze[1,10] <- AIC(Conf.fit.schulze)
all.results.schulze[2,10] <- AIC(Load.fit.schulze)
all.results.schulze[3,10] <- AIC(Int.fit.schulze)

all.results.schulze[1,11] <- BIC(Conf.fit.schulze)
all.results.schulze[2,11] <- BIC(Load.fit.schulze)
all.results.schulze[3,11] <- BIC(Int.fit.schulze)

# Results do not match those presented by the authors (up to model 3). However, the authors reported 
# that the Satorra-Bentler values were adjusted and that BSEM was used. This, unfortunately, is not directly reproducible in R,
# and thus we stopped with the analyses.
# Note that, we still tried to reproduce the analyses using lavaan, but our results differ
# from those in the article. Such differences, are due to the different estimators/models (classical CFA vs BSEM).

# Article 133: Abdin --------------------------------------------------------------------------
# The article does not share raw data or correlations between items.  Statement Data Availability: "The data underlying the 
# results of this study are available upon request due to ethical restrictions imposed by the SingHealth Centralised Institutional Review 
# Board, in Singapore. All study subjects were informed that only non-identifiable and aggregate data will be used in subsequent reports 
# and scientific publications, and communications to companies and stakeholders. Hence, individual data is not available for public online access. R
# Readers may write to the Workplace Safety and Health Institute, Ministry of Manpower at [address]"
# The data is available upon request, so we cannot do a MI test because we have no data.


# Article 207: Ortuno-Sierra ------------------------------------------------------------------
# Data shared is .sav file as supporting information to the article
# https://dx.plos.org/10.1371/journal.pone.0221696.s001

url <- 'https://dx.plos.org/10.1371/journal.pone.0221696.s001'
filename <- '../article207.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article207 <- read_sav(filename)

# comparison 1:
# variables for scale: PAN01 until PAN20
# grouping variable for id 868: education (variable name: curso: primero ESO & segundo ESO)
# labels: primary -10 to 12 years old- versus secondary students -13 to 15 years old

# The authors reported using an ESEM two factors model to test for MI. However, it is not clear how the ESEM model is obtained 
# nor constructed. Here is our attempt at reproducing their analyses

library(psych)

#1) Estimate EFA on full sample 

polycor207<- polychoric(article207[,5:24])

Ortuno_Sierra_efa <- fa(polycor207$rho, nfact = 2, rotate = "geominQ", cor= "poly")

#Ortuno_Sierra_efa <- fa(article207[,5:24], nfact = 2, rotate = "varimax")

Ortuno_Sierra_load <- zapsmall(matrix(round(Ortuno_Sierra_efa$loadings,3),
                                      nrow = 20, ncol = 2))

rownames(Ortuno_Sierra_load) <- colnames(article207[,5:24])

#Create model
Model_Ortuno_Sierra <- NA


for (l in 1:ncol(Ortuno_Sierra_load)) {
  Model_Ortuno_Sierra[l] <- paste0("F", l, "=~", paste0(c(Ortuno_Sierra_load[,l]), "*",
                                              rownames(Ortuno_Sierra_load), collapse =  "+" ))
}


#Create results matrix
all.results.Ortuno_Sierra <- matrix(NA, ncol = 11, nrow = 3)
colnames(all.results.Ortuno_Sierra) <- c("chisq","df","pvalue", 
                                         "rmsea", "cfi", "srmr", "diffRMSEA", "diffCFI", "lavtestLRT", "AIC", "BIC")

# Since DWLS was used to etimate the data (i.e., acccounting for their ordinality), we used the guidelines proposed by
# Wu and Estabrook (2016) for MI testing with ordinal data. 


#1 Create configural MI model following Wu and Estabrook (2016)

Configural_Ortuno_Sierra <- measEq.syntax(Model_Ortuno_Sierra, 
                                          ID.fac = "std.lv", 
                                          ID.cat = "Wu",
                                          ordered = colnames(article207[,5:24]),
                                          parameterization = "delta",
                                          data = article207,
                                          group = "CURSO_RECO", 
                                          group.equal = "configural", 
                                          orthogonal = T)

# Fit the configural model
Conf.fit.Ortuno_Sierra <- cfa(as.character(Configural_Ortuno_Sierra),
                              article207, 
                              ordered = colnames(article207[,5:24]),
                              group = "CURSO_RECO", 
                              estimator = "WLSMV")

# Store configural invariance model goodness-of-fit measures results
all.results.Ortuno_Sierra[1,1:6]<-round(data.matrix(fitmeasures(Conf.fit.Ortuno_Sierra,
                                                       fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", 
                                                                        "rmsea.scaled", "cfi.scaled", "srmr"))), digits=3)

# Create the thresholds invariance model following Wu and Estabrook (2016)
Thr_Ortuno_Sierra <- measEq.syntax(Model_Ortuno_Sierra, 
                                          ID.fac = "std.lv", 
                                          ID.cat = "Wu",
                                          ordered = colnames(article207[,5:24]),
                                          parameterization = "delta",
                                          data = article207,
                                          group = "CURSO_RECO", 
                                          group.equal = "thresholds", 
                                          orthogonal = T)
# Fit the thresholds invariance model
Thr.fit.Ortuno_Sierra <- cfa(as.character(Thr_Ortuno_Sierra),
                              article207, 
                              ordered = colnames(article207[,5:24]),
                              group = "CURSO_RECO", 
                              estimator = "WLSMV")

# Store thresholds invariance model goodness-of-fit measures results
all.results.Ortuno_Sierra[2,1:6] <-round(data.matrix(fitmeasures(Thr.fit.Ortuno_Sierra,
                                                             fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", 
                                                                              "rmsea.scaled", "cfi.scaled", "srmr"))), digits=3)

# Create the thresholds invariance model following Wu and Estabrook (2016)
Load_Ortuno_Sierra <- measEq.syntax(Model_Ortuno_Sierra, 
                                   ID.fac = "std.lv", 
                                   ID.cat = "Wu",
                                   ordered = colnames(article207[,5:24]),
                                   parameterization = "delta",
                                   data = article207,
                                   group = "CURSO_RECO", 
                                   group.equal = c("thresholds","loadings"), 
                                   orthogonal = T)

# Fit the loadings invariance model
Load.fit.Ortuno_Sierra <- cfa(as.character(Load_Ortuno_Sierra),
                             article207, 
                             ordered = colnames(article207[,5:24]),
                             group = "CURSO_RECO", 
                             estimator = "WLSMV")

# Store loadings invariance model goodness-of-fit measures results
all.results.Ortuno_Sierra[3,1:6] <-round(data.matrix(fitmeasures(Load.fit.Ortuno_Sierra,
                                                              fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", 
                                                                               "rmsea.scaled", "cfi.scaled", "srmr"))), digits=3)

# Compute the difference in goodness-of-fit measures
all.results.Ortuno_Sierra[2,7:8] <- all.results.Ortuno_Sierra[2,][4:5] - all.results.Ortuno_Sierra[1,][4:5]
all.results.Ortuno_Sierra[3,7:8] <- all.results.Ortuno_Sierra[3,][4:5] - all.results.Ortuno_Sierra[2,][4:5]

# Compute the LRT between the different models and store the p-value result
all.results.Ortuno_Sierra[2,9] <- lavTestLRT(Conf.fit.Ortuno_Sierra, Thr.fit.Ortuno_Sierra)$`Pr(>Chisq)`[2]
all.results.Ortuno_Sierra[3,9] <- lavTestLRT(Thr.fit.Ortuno_Sierra, Load.fit.Ortuno_Sierra)$`Pr(>Chisq)`[2]

#Not available since AIC-BIC only work with ML estimation
#all.results.Ortuno_Sierra[1,10] <- AIC(Conf.fit.Ortuno_Sierra)
#all.results.Ortuno_Sierra[2,10] <- AIC(Thr.fit.Ortuno_Sierra)
#all.results.Ortuno_Sierra[3,10] <- AIC(Load.fit.Ortuno_Sierra)

#all.results.Ortuno_Sierra[1,11] <- BIC(Conf.fit.Ortuno_Sierra)
#all.results.Ortuno_Sierra[2,11] <- BIC(Thr.fit.Ortuno_Sierra)
#all.results.Ortuno_Sierra[3,11] <- BIC(Load.fit.Ortuno_Sierra)

# Results could not be fully reproduced due to lack of documenting how the ESEM two factor model that was used for 
# invariance testing is specified. We tried to reproduce the results following the standard approach
# for ESEM MI testing (Fischer and Karl, 2019; https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6657455/pdf/fpsyg-10-01507.pdf)
# and MI holds both for loadings and intercepts (as reported by the authors) but chi-squared statistics and df differ.


#------

# comparison 2:
# variables for scale: PAN01 until PAN20
# grouping variable for id 869: gender (variable name: Género)
# labels: males versus females

#Add variable to dataset based on Género to make sure that is numeric and can be recognized by lavaan

article207$gender <- as.numeric(article207$Género)
#Create results matrix
all.results.Ortuno_Sierra_2 <- matrix(NA, ncol = 11, nrow = 3)
colnames(all.results.Ortuno_Sierra_2) <- c("chisq","df","pvalue", 
                                           "rmsea", "cfi", "srmr", "diffRMSEA", "diffCFI", "lavtestLRT", "AIC", "BIC")


# Since models were generated for the earlier comparison we can skip the syntax generation step here and skip to model estimation
# and storing the results.

Conf.fit.Ortuno_Sierra_2 <- cfa(as.character(Configural_Ortuno_Sierra),
                              article207, 
                              ordered = colnames(article207[,5:24]),
                              group = "gender", 
                              estimator = "WLSMV")

all.results.Ortuno_Sierra_2[1,1:6]<-round(data.matrix(fitmeasures(Conf.fit.Ortuno_Sierra_2,
                                                                fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", 
                                                                                 "rmsea.scaled", "cfi.scaled", "srmr"))), digits=3)

# Fit the thresholds invariance model
Thr.fit.Ortuno_Sierra_2 <- cfa(as.character(Thr_Ortuno_Sierra),
                             article207, 
                             ordered = colnames(article207[,5:24]),
                             group = "gender", 
                             estimator = "WLSMV")

# Store thresholds invariance model goodness-of-fit measures results
all.results.Ortuno_Sierra_2[2,1:6] <-round(data.matrix(fitmeasures(Thr.fit.Ortuno_Sierra_2,
                                                                 fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", 
                                                                                  "rmsea.scaled", "cfi.scaled", "srmr"))), digits=3)

# Fit the loadings invariance model
Load.fit.Ortuno_Sierra_2 <- cfa(as.character(Load_Ortuno_Sierra),
                              article207, 
                              ordered = colnames(article207[,5:24]),
                              group = "gender", 
                              estimator = "WLSMV")

# Store loadings invariance model goodness-of-fit measures results
all.results.Ortuno_Sierra_2[3,1:6] <-round(data.matrix(fitmeasures(Load.fit.Ortuno_Sierra_2,
                                                                 fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", 
                                                                                  "rmsea.scaled", "cfi.scaled", "srmr"))), digits=3)

# Compute the difference in goodness-of-fit measures
all.results.Ortuno_Sierra_2[2,7:8] <- all.results.Ortuno_Sierra_2[2,][4:5] - all.results.Ortuno_Sierra_2[1,][4:5]
all.results.Ortuno_Sierra_2[3,7:8] <- all.results.Ortuno_Sierra_2[3,][4:5] - all.results.Ortuno_Sierra_2[2,][4:5]

# Compute the LRT between the different models and store the p-value result
all.results.Ortuno_Sierra_2[2,9] <- lavTestLRT(Conf.fit.Ortuno_Sierra_2, Thr.fit.Ortuno_Sierra_2)$`Pr(>Chisq)`[2]
all.results.Ortuno_Sierra_2[3,9] <- lavTestLRT(Thr.fit.Ortuno_Sierra_2, Load.fit.Ortuno_Sierra_2)$`Pr(>Chisq)`[2]

#Not available since AIC-BIC only work with ML estimation
#all.results.Ortuno_Sierra_2[1,10] <- AIC(Conf.fit.Ortuno_Sierra_2)
#all.results.Ortuno_Sierra_2[2,10] <- AIC(Thr.fit.Ortuno_Sierra_2)
#all.results.Ortuno_Sierra_2[3,10] <- AIC(Load.fit.Ortuno_Sierra_2)

#all.results.Ortuno_Sierra_2[1,11] <- BIC(Conf.fit.Ortuno_Sierra_2)
#all.results.Ortuno_Sierra_2[2,11] <- BIC(Thr.fit.Ortuno_Sierra_2)
#all.results.Ortuno_Sierra_2[3,11] <- BIC(Load.fit.Ortuno_Sierra_2)

# Results could not be fully reproduced due to lack of documenting how the ESEM two factor model that was used for 
# invariance testing is specified. We tried to reproduce the results following the standard approach
# for ESEM MI testing (Fischer and Karl, 2019; https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6657455/pdf/fpsyg-10-01507.pdf)
# and MI holds both for loadings and intercepts (as reported by the authors) but chi-squared statistics and df differ.


# Article 351: Kievit -------------------------------------------------------------------------
# Data shared is raw data .csv file and R code. The code below is copied from this R file: https://osf.io/8g96x/
# https://osf.io/pz8es/ 

url <- 'https://osf.io/pz8es//?action=download'
filename <- '../article351.csv'
GET(url, write_disk(filename, overwrite = TRUE))
article351 <- read.csv2(filename, header=TRUE, na.strings="NA", sep=',')
cogdat <- article351 # cogdat is the name in the original code

# variables for scale: t1ssmatrix, t1ssvocab, t2ssmatrix, t2ssvocab, t3ssmatrix, t3ssvocab
# grouping variable: gender

# Code copied from from https://osf.io/8g96x/:
# Multigroup model for boys and girls: equality constraints don't adversely affect fit
# Equality constraints are hand coded by appending 'a' and 'b' for the freely 
# estimated model.

mutualism_constrained_lv_mg <- '

t1ssvoclv=~1*t1ssvocab
t2ssvoclv=~1*t2ssvocab
t3ssvoclv=~1*t3ssvocab
t1ssmatlv=~1*t1ssmatrix
t2ssmatlv=~1*t2ssmatrix
t3ssmatlv=~1*t3ssmatrix

t2ssvoclv ~ 1 * t1ssvoclv
t3ssvoclv ~ 1 * t2ssvoclv

etavoc1 =~ 1 * t2ssvoclv
etavoc2 =~ 1 * t3ssvoclv

etavoc1 ~ c(vocint1,vocint1) *  1   
etavoc2 ~ c(vocint2,vocint2) * 1 # only exception to cross-wave equality constraints 

t1ssvoclv ~ c(mvoc,mvoc) *1
t2ssvoclv ~ 0 * 1
t3ssvoclv ~ 0 * 1

t2ssmatlv ~ 1 * t1ssmatlv
t3ssmatlv ~ 1 * t2ssmatlv

etamat1 =~ 1 * t2ssmatlv
etamat2 =~ 1 * t3ssmatlv

etamat1 ~ c(matint1,matint1)  * 1   
etamat2 ~ c(matint1,matint1)  * 1   

t1ssmatlv ~c(mmat,mmat) * 1
t2ssmatlv ~ 0 * 1
t3ssmatlv ~ 0 * 1

##### Variances

etamat1 ~~ c(varetamat1,varetamat1) * etamat1
etamat2 ~~ c(varetamat1,varetamat1) * etamat2
etavoc1 ~~ c(varetavoc1,varetavoc1) * etavoc1
etavoc2 ~~ c(varetavoc1,varetavoc1) * etavoc2

t1ssvoclv~~c(varvoc,varvoc) * t1ssvoclv
t1ssmatlv~~c(varmat,varmat) * t1ssmatlv


##### Covariances
etamat1~~c(a,a) * etavoc1
etamat2~~c(a,a) * etavoc2
t1ssmatrix~~c(cov,cov) * t1ssvocab


##### Structural model
etamat1~c(negmat,negmat) * t1ssmatrix+c(couplinga,couplinga) * t1ssvocab
etamat2~c(negmat,negmat) * t2ssmatrix+c(couplinga,couplinga) * t2ssvocab

etavoc1~c(negvoc,negvoc) * t1ssvocab+c(couplingb,couplingb) * t1ssmatrix
etavoc2~c(negvoc,negvoc) * t2ssvocab+c(couplingb,couplingb) * t2ssmatrix


#Residual variances
t1ssvocab~~ 4.32957*t1ssvocab
t2ssvocab~~ 6.644618*t2ssvocab
t3ssvocab~~ 6.759454*t3ssvocab
t1ssmatrix~~1.678911*t1ssmatrix
t2ssmatrix~~3.14567*t2ssmatrix
t3ssmatrix~~3.257446*t3ssmatrix
'

est_mutualism_constrained_lv_mg <- growth(mutualism_constrained_lv_mg, data=cogdat, estimator='mlr',fixed.x=FALSE,group='Gender',missing='fiml')
summary(est_mutualism_constrained_lv_mg, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE,ci=T)

mutualism_free_lv_mg <- '

t1ssvoclv=~1*t1ssvocab
t2ssvoclv=~1*t2ssvocab
t3ssvoclv=~1*t3ssvocab
t1ssmatlv=~1*t1ssmatrix
t2ssmatlv=~1*t2ssmatrix
t3ssmatlv=~1*t3ssmatrix

t2ssvoclv ~ 1 * t1ssvoclv
t3ssvoclv ~ 1 * t2ssvoclv

etavoc1 =~ 1 * t2ssvoclv
etavoc2 =~ 1 * t3ssvoclv

etavoc1 ~ c(vocint1a,vocint1b) *  1   
etavoc2 ~ c(vocint2a,vocint2b) * 1 # only exception to cross-wave equality constraints 

t1ssvoclv ~ c(mvoca,mvocb) *1
t2ssvoclv ~ 0 * 1
t3ssvoclv ~ 0 * 1

t2ssmatlv ~ 1 * t1ssmatlv
t3ssmatlv ~ 1 * t2ssmatlv

etamat1 =~ 1 * t2ssmatlv
etamat2 =~ 1 * t3ssmatlv

etamat1 ~ c(matint1a,matint1b)  * 1   
etamat2 ~ c(matint1a,matint1b)  * 1   

t1ssmatlv ~c(mmata,mmatb) * 1
t2ssmatlv ~ 0 * 1
t3ssmatlv ~ 0 * 1

##### Variances

etamat1 ~~ c(varetamat1a,varetamat1b) * etamat1
etamat2 ~~ c(varetamat1a,varetamat1b) * etamat2
etavoc1 ~~ c(varetavoc1a,varetavoc1b) * etavoc1
etavoc2 ~~ c(varetavoc1a,varetavoc1b) * etavoc2

t1ssvoclv~~c(varvoca,varvocb) * t1ssvoclv
t1ssmatlv~~c(varmata,varmatb) * t1ssmatlv


##### Covariances
etamat1~~c(a,a) * etavoc1
etamat2~~c(a,a) * etavoc2
t1ssmatrix~~c(cov,cov) * t1ssvocab


##### Structural model
etamat1~c(negmata,negmatb) * t1ssmatrix+c(couplinga_a,couplinga_b) * t1ssvocab
etamat2~c(negmata,negmatb) * t2ssmatrix+c(couplinga_a,couplinga_b) * t2ssvocab

etavoc1~c(negvoca,negvocb) * t1ssvocab+c(couplingb_a,couplingb_b) * t1ssmatrix
etavoc2~c(negvoca,negvocb) * t2ssvocab+c(couplingb_a,couplingb_b) * t2ssmatrix


#Residual variances
t1ssvocab~~ 4.32957*t1ssvocab
t2ssvocab~~ 6.644618*t2ssvocab
t3ssvocab~~ 6.759454*t3ssvocab
t1ssmatrix~~1.678911*t1ssmatrix
t2ssmatrix~~3.14567*t2ssmatrix
t3ssmatrix~~3.257446*t3ssmatrix
'

est_mutualism_free_lv_mg <- growth(mutualism_free_lv_mg, data=cogdat, estimator='mlr',fixed.x=FALSE,group='Gender',missing='fiml')
summary(est_mutualism_free_lv_mg, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE,ci=T)

#No significant improvement upon freely estimating key parameters
anova(est_mutualism_constrained_lv_mg,est_mutualism_free_lv_mg)


#Results could be fully reproduced using the data and the code provided by the authors.

# Article 710: Protzko ------------------------------------------------------------------------
# Data shared is raw data in .sav file: https://osf.io/x7c8w/ 
url <- 'https://osf.io/x7c8w//?action=download'
filename <- '../article710.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article710 <- read_sav(filename)

# variables for scale: Psd1, sd2R, sd3, sd4R, sd5, sd6R, s7R, sd8R, sd9, sd10 
# grouping variable: fast 

#Create model
Model_Protzko <- 'F1 =~ sd1 +  sd2R + sd3 + sd4R + sd5 + sd6R + s7R +  sd8R + sd9 + sd10'

#Create results matrix
all.results.Protzko <- matrix(NA, ncol = 11, nrow = 3)
colnames(all.results.Protzko) <- c("chisq","df","pvalue", 
                                   "rmsea", "cfi", "srmr", "diffRMSEA", "diffCFI", "lavtestLRT", "AIC", "BIC")

#change type of variables to numeric for estimating the model
article710$sd1 <- as.numeric(article710$sd1)
article710$sd2R <- as.numeric(article710$sd2R)
article710$sd3 <- as.numeric(article710$sd3)
article710$sd4R <- as.numeric(article710$sd4R)
article710$sd5 <- as.numeric(article710$sd5)
article710$sd6R <- as.numeric(article710$sd6R)
article710$s7R <- as.numeric(article710$sd7R)
article710$sd8R <- as.numeric(article710$sd8R)
article710$sd9 <- as.numeric(article710$sd9)
article710$sd10 <- as.numeric(article710$sd10)
article710$fast <- as.numeric(article710$fast)

# Fit the configural invariance model
Conf.fit.Protzko <- cfa(Model_Protzko,
                        article710, 
                        group = "fast")

# Store configural invariance model goodness-of-fit measures results
all.results.Protzko[1,1:6] <-round(data.matrix(fitmeasures(Conf.fit.Protzko,
                                                                 fit.measures = c("chisq","df","pvalue", 
                                                                                  "rmsea", "cfi", "srmr"))), digits=3)

# Fit the loadings invariance model
Load.fit.Protzko <- cfa(Model_Protzko,
                        article710, 
                        group = "fast",
                        group.equal = "loadings")

# Store loadings invariance model goodness-of-fit measures results
all.results.Protzko[2,1:6] <-round(data.matrix(fitmeasures(Load.fit.Protzko,
                                                           fit.measures = c("chisq","df","pvalue", 
                                                                            "rmsea", "cfi", "srmr"))), digits=3)

# Fit the intercepts invariance model
Int.fit.Protzko <- cfa(Model_Protzko,
                        article710, 
                        estimator = "ML", 
                        group = "fast",
                       group.equal =  c("loadings", "intercepts"))

# Store intercepts invariance model goodness-of-fit measures results
all.results.Protzko[3,1:6] <-round(data.matrix(fitmeasures(Int.fit.Protzko,
                                                           fit.measures = c("chisq","df","pvalue", 
                                                                            "rmsea", "cfi", "srmr"))), digits=3)


# Compute the difference in goodness-of-fit measures
all.results.Protzko[2,7:8] <- all.results.Protzko[2,][4:5] - all.results.Protzko[1,][4:5]
all.results.Protzko[3,7:8] <- all.results.Protzko[3,][4:5] - all.results.Protzko[2,][4:5]

# Compute the LRT between the different models and store the p-value result
all.results.Protzko[2,9] <- lavTestLRT(Conf.fit.Protzko, Load.fit.Protzko)$`Pr(>Chisq)`[2]
all.results.Protzko[3,9] <- lavTestLRT(Load.fit.Protzko, Int.fit.Protzko)$`Pr(>Chisq)`[2]

all.results.Protzko[1,10] <- AIC(Conf.fit.Protzko)
all.results.Protzko[2,10] <- AIC(Load.fit.Protzko)
all.results.Protzko[3,10] <- AIC(Int.fit.Protzko)

all.results.Protzko[1,11] <- BIC(Conf.fit.Protzko)
all.results.Protzko[2,11] <- BIC(Load.fit.Protzko)
all.results.Protzko[3,11] <- BIC(Int.fit.Protzko)

# Results do not entirely match those reported by the authors. Specifically, the cfi highly deviates from that reported
# by the authors in all steps (authors: confcfi = 0.905, loadcfi = 0.905, intcfi = 0.901; reproduced 
# confcfi = 840, loadcfi = 0.838, intcfi = 0.827). The rmsea only slightly deviates.


# Article 711: Obaidi -------------------------------------------------------------------------
# The supplemental material (.pdf) reports measurement invariance checks in test:
# https://journals.sagepub.com/doi/suppl/10.1177/0956797619834879/suppl_file/ObaidiSupplementalMaterial.pdf
# Correlations are reported between factors but not between individual items
# We cannot do a MI test because we have no data


# Article 9: Leitner --------------------------------------------------------------------------
# Data shared is raw data in .sav file, including .sps code file: https://osf.io/yxvuk/files/
# study 1; 3 comparisons
url <- 'https://osf.io/mzyh5//?action=download'
filename <- '../article7_1.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article7_1 <- read_sav(filename)

# comparison 1: this is probably a different dataset!
# variables for scale: 
# grouping: confederate (black / white)

#variable for scale could not be found, and thus we could not reproduce this comparison. 

# comparison 2: 
# variables for scale: post negative affect (DV_sad, DV_hopeless, DV_Discouraged, DV_angry, DV_resentful, DV_annoyed, DV_fatigued, DV_wornout, DV_exhausted, DV_vigor, DV_lively, DV_cheer)
# grouping: C_CnCI (low and high self-disclosure)

#Create model
Model_Leitner <- 'F1 =~ DV_sad +  DV_hopeless + DV_Discouraged + DV_angry + DV_resentful + DV_annoyed + DV_fatigued +
                  DV_wornout +  DV_exchausted + DV_vigor + DV_lively + DV_cheer'

#Create results matrix
all.results.Leitner <- matrix(NA, ncol = 11, nrow = 3)
colnames(all.results.Leitner) <- c("chisq","df","pvalue", 
                                   "rmsea", "cfi", "srmr", "diffRMSEA", "diffCFI", "lavtestLRT", "AIC", "BIC")

#change type of variables to numeric for estimating the model
article7_1$group <- as.numeric(article7_1$C_CnCls)

# Fit the configural invariance model
Conf.fit.Leitner<- cfa(Model_Leitner,
                        article7_1, 
                        group = "group")

# Store configural invariance model goodness-of-fit measures results
all.results.Leitner[1,1:6] <-round(data.matrix(fitmeasures(Conf.fit.Leitner,
                                                           fit.measures = c("chisq","df","pvalue", 
                                                                            "rmsea", "cfi", "srmr"))), digits=3)

# Fit the loadings invariance model
Load.fit.Leitner <- cfa(Model_Leitner,
                        article7_1, 
                        group = "group",
                        group.equal = "loadings")

# Store loadings invariance model goodness-of-fit measures results
all.results.Leitner[2,1:6] <-round(data.matrix(fitmeasures(Load.fit.Leitner,
                                                           fit.measures = c("chisq","df","pvalue", 
                                                                            "rmsea", "cfi", "srmr"))), digits=3)

# Fit the intercepts invariance model
Int.fit.Leitner <- cfa(Model_Leitner,
                        article7_1, 
                        group = "group",
                        group.equal = c("loadings", "intercepts"))

# Store loadings invariance model goodness-of-fit measures results
all.results.Leitner[3,1:6] <-round(data.matrix(fitmeasures(Int.fit.Leitner,
                                                           fit.measures = c("chisq","df","pvalue", 
                                                                            "rmsea", "cfi", "srmr"))), digits=3)


# Compute the difference in goodness-of-fit measures
all.results.Leitner[2,7:8] <- all.results.Leitner[2,][4:5] - all.results.Leitner[1,][4:5]
all.results.Leitner[3,7:8] <- all.results.Leitner[3,][4:5] - all.results.Leitner[2,][4:5]

# Compute the LRT between the different models and store the p-value result
all.results.Leitner[2,9] <- lavTestLRT(Conf.fit.Leitner, Load.fit.Leitner)$`Pr(>Chisq)`[2]
all.results.Leitner[3,9] <- lavTestLRT(Load.fit.Leitner, Int.fit.Leitner)$`Pr(>Chisq)`[2]

all.results.Leitner[1,10] <- AIC(Conf.fit.Leitner)
all.results.Leitner[2,10] <- AIC(Load.fit.Leitner)
all.results.Leitner[3,10] <- AIC(Int.fit.Leitner)

all.results.Leitner[1,11] <- BIC(Conf.fit.Leitner)
all.results.Leitner[2,11] <- BIC(Load.fit.Leitner)
all.results.Leitner[3,11] <- BIC(Int.fit.Leitner)

#MI test rejected at the configural invariance level.

# comparison 3: 
# variables for scale: post negative affect (DV_sad, DV_hopeless, DV_Discouraged, DV_angry, DV_resentful, DV_annoyed, DV_fatigued, DV_wornout, DV_exhausted, DV_vigor, DV_lively, DV_cheer)
# grouping: confederate (black / white)
url <- 'https://osf.io/mzyh5//?action=download'
filename <- '../article7_1.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article7_2 <- read_sav(filename)

# study 2; 2 comparisons

#Create model
Model_Leitner_2 <- 'F1 =~ DV_sad +  DV_hopeless + DV_Discouraged + DV_angry + DV_resentful + DV_annoyed + DV_fatigued +
                  DV_wornout +  DV_exchausted + DV_vigor + DV_lively + DV_cheer'

#Create results matrix
all.results.Leitner_2 <- matrix(NA, ncol = 11, nrow = 3)
colnames(all.results.Leitner_2) <- c("chisq","df","pvalue", 
                                     "rmsea", "cfi", "srmr", "diffRMSEA", "diffCFI","lavtestLRT", "AIC", "BIC")


# Fit the configural invariance model
Conf.fit.Leitner_2<- cfa(Model_Leitner_2,
                         article7_2, 
                         group = "ConfederateRace")

# Store configural invariance model goodness-of-fit measures results
all.results.Leitner_2[1,1:6] <-round(data.matrix(fitmeasures(Conf.fit.Leitner_2,
                                                             fit.measures = c("chisq","df","pvalue", 
                                                                              "rmsea", "cfi", "srmr"))), digits=3)

# Fit the loadings invariance model
Load.fit.Leitner_2 <- cfa(Model_Leitner_2,
                          article7_2, 
                          group = "ConfederateRace",
                          group.equal = "loadings")

# Store loadings invariance model goodness-of-fit measures results
all.results.Leitner_2[2,1:6] <-round(data.matrix(fitmeasures(Load.fit.Leitner_2,
                                                             fit.measures = c("chisq","df","pvalue", 
                                                                              "rmsea", "cfi", "srmr"))), digits=3)

# Fit the intercepts invariance model
Int.fit.Leitner_2 <- cfa(Model_Leitner_2,
                         article7_2, 
                         group = "ConfederateRace",
                         group.equal = c("loadings", "intercepts"))

# Store loadings invariance model goodness-of-fit measures results
all.results.Leitner_2[3,1:6] <-round(data.matrix(fitmeasures(Int.fit.Leitner_2,
                                                             fit.measures = c("chisq","df","pvalue", 
                                                                              "rmsea", "cfi", "srmr"))), digits=3)


# Compute the difference in goodness-of-fit measures
all.results.Leitner_2[2,7:8] <- all.results.Leitner_2[2,][4:5] - all.results.Leitner_2[1,][4:5]
all.results.Leitner_2[3,7:8] <- all.results.Leitner_2[3,][4:5] - all.results.Leitner_2[2,][4:5]

# Compute the LRT between the different models and store the p-value result
all.results.Leitner_2[2,9] <- lavTestLRT(Conf.fit.Leitner_2, Load.fit.Leitner_2)$`Pr(>Chisq)`[2]
all.results.Leitner_2[3,9] <- lavTestLRT(Load.fit.Leitner_2, Int.fit.Leitner_2)$`Pr(>Chisq)`[2]

all.results.Leitner_2[1,10] <- AIC(Conf.fit.Leitner_2)
all.results.Leitner_2[2,10] <- AIC(Load.fit.Leitner_2)
all.results.Leitner_2[3,10] <- AIC(Int.fit.Leitner_2)

all.results.Leitner_2[1,11] <- BIC(Conf.fit.Leitner_2)
all.results.Leitner_2[2,11] <- BIC(Load.fit.Leitner_2)
all.results.Leitner_2[3,11] <- BIC(Int.fit.Leitner_2)


#MI test rejected at the configural invariance level.

# comparison 4: 
# variables for scale: negative affect after speech but before feedback 
# POMS_prefdbk_hopeless,POMS_prefdbk_discouraged, POMS_prefdbk_angry, POMS_prefdbk_resent, POMS_prefdbk_annyd, POMS_prefdbk_fatigued, POMS_prefdbk_wornout, POMS_prefdbk_exhauste
# grouping: confederate C_CnCI (low and high self-disclosure)
url <- 'https://osf.io/g85jp//?action=download'
filename <- '../article7_2.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article7_3 <- read_sav(filename)

#change grouping variable class
article7_3$group <- as.numeric(article7_3$C_CnCl)


#Create model using the variables colnames for the POMS scale prefdbk
Model_Leitner_3 <- NA
for (i in 1:12){
  Model_Leitner_3[i] <- paste0("F1 =~" , colnames(article7_3[,i+19]))
}

#Create results matrix
all.results.Leitner_3 <- matrix(NA, ncol = 11, nrow = 3)
colnames(all.results.Leitner_3) <- c("chisq","df","pvalue", 
                                     "rmsea", "cfi", "srmr", "diffRMSEA", "diffCFI","lavtestLRT", "AIC", "BIC")


# Fit the configural invariance model
Conf.fit.Leitner_3<- cfa(Model_Leitner_3,
                         article7_3, 
                         group = "group")

# Store configural invariance model goodness-of-fit measures results
all.results.Leitner_3[1,1:6] <-round(data.matrix(fitmeasures(Conf.fit.Leitner_3,
                                                             fit.measures = c("chisq","df","pvalue", 
                                                                              "rmsea", "cfi", "srmr"))), digits=3)

# Fit the loadings invariance model
Load.fit.Leitner_3 <- cfa(Model_Leitner_3,
                          article7_3, 
                          group = "group",
                          group.equal = "loadings")

# Store loadings invariance model goodness-of-fit measures results
all.results.Leitner_3[2,1:6] <-round(data.matrix(fitmeasures(Load.fit.Leitner_3,
                                                             fit.measures = c("chisq","df","pvalue", 
                                                                              "rmsea", "cfi", "srmr"))), digits=3)

# Fit the intercepts invariance model
Int.fit.Leitner_3 <- cfa(Model_Leitner_3,
                         article7_3, 
                         group = "group",
                         group.equal = c("loadings", "intercepts"))

# Store loadings invariance model goodness-of-fit measures results
all.results.Leitner_3[3,1:6] <-round(data.matrix(fitmeasures(Int.fit.Leitner_3,
                                                             fit.measures = c("chisq","df","pvalue", 
                                                                              "rmsea", "cfi", "srmr"))), digits=3)


# Compute the difference in goodness-of-fit measures
all.results.Leitner_3[2,7:8] <- all.results.Leitner_3[2,][4:5] - all.results.Leitner_3[1,][4:5]
all.results.Leitner_3[3,7:8] <- all.results.Leitner_3[3,][4:5] - all.results.Leitner_3[2,][4:5]

# Compute the LRT between the different models and store the p-value result
all.results.Leitner_3[2,9] <- lavTestLRT(Conf.fit.Leitner_3, Load.fit.Leitner_3)$`Pr(>Chisq)`[2]
all.results.Leitner_3[3,9] <- lavTestLRT(Load.fit.Leitner_3, Int.fit.Leitner_3)$`Pr(>Chisq)`[2]

all.results.Leitner_3[1,10] <- AIC(Conf.fit.Leitner_3)
all.results.Leitner_3[2,10] <- AIC(Load.fit.Leitner_3)
all.results.Leitner_3[3,10] <- AIC(Int.fit.Leitner_3)

all.results.Leitner_3[1,11] <- BIC(Conf.fit.Leitner_3)
all.results.Leitner_3[2,11] <- BIC(Load.fit.Leitner_3)
all.results.Leitner_3[3,11] <- BIC(Int.fit.Leitner_3)

#MI test rejected at the configural invariance level.

# comparison 5: 
# variables for scale: negative affect after speech but before feedback 
# POMS_prefdbk_hopeless,POMS_prefdbk_discouraged, POMS_prefdbk_angry, POMS_prefdbk_resent, POMS_prefdbk_annyd, POMS_prefdbk_fatigued, POMS_prefdbk_wornout, POMS_prefdbk_exhauste, POMS_vigor_prefdbk_R, POMS_lively_prefdbk_R, POMS_cheer_prefdbk_R
# grouping: confederate confederate (black / white)

#Create results matrix
all.results.Leitner_4 <- matrix(NA, ncol = 11, nrow = 3)
colnames(all.results.Leitner_4) <- c("chisq","df","pvalue", 
                                     "rmsea", "cfi", "srmr", "diffRMSEA", "diffCFI","lavtestLRT", "AIC", "BIC")


# Fit the configural invariance model
Conf.fit.Leitner_4<- cfa(Model_Leitner_3,
                         article7_3, 
                         group = "RaceCondition")

# Store configural invariance model goodness-of-fit measures results
all.results.Leitner_4[1,1:6] <-round(data.matrix(fitmeasures(Conf.fit.Leitner_4,
                                                             fit.measures = c("chisq","df","pvalue", 
                                                                              "rmsea", "cfi", "srmr"))), digits=3)

# Fit the loadings invariance model
Load.fit.Leitner_4 <- cfa(Model_Leitner_3,
                          article7_3, 
                          group = "RaceCondition",
                          group.equal = "loadings")

# Store loadings invariance model goodness-of-fit measures results
all.results.Leitner_4[2,1:6] <-round(data.matrix(fitmeasures(Load.fit.Leitner_4,
                                                             fit.measures = c("chisq","df","pvalue", 
                                                                              "rmsea", "cfi", "srmr"))), digits=3)

# Fit the intercepts invariance model
Int.fit.Leitner_4 <- cfa(Model_Leitner_3,
                         article7_3, 
                         group = "RaceCondition",
                         group.equal = c("loadings", "intercepts"))

# Store loadings invariance model goodness-of-fit measures results
all.results.Leitner_4[3,1:6] <-round(data.matrix(fitmeasures(Int.fit.Leitner_4,
                                                             fit.measures = c("chisq","df","pvalue", 
                                                                              "rmsea", "cfi", "srmr"))), digits=3)


# Compute the difference in goodness-of-fit measures
all.results.Leitner_4[2,7:8] <- all.results.Leitner_4[2,][4:5] - all.results.Leitner_4[1,][4:5]
all.results.Leitner_4[3,7:8] <- all.results.Leitner_4[3,][4:5] - all.results.Leitner_4[2,][4:5]

# Compute the LRT between the different models and store the p-value result
all.results.Leitner_4[2,9] <- lavTestLRT(Conf.fit.Leitner_4, Load.fit.Leitner_4)$`Pr(>Chisq)`[2]
all.results.Leitner_4[3,9] <- lavTestLRT(Load.fit.Leitner_4, Int.fit.Leitner_4)$`Pr(>Chisq)`[2]

all.results.Leitner_4[1,10] <- AIC(Conf.fit.Leitner_4)
all.results.Leitner_4[2,10] <- AIC(Load.fit.Leitner_4)
all.results.Leitner_4[3,10] <- AIC(Int.fit.Leitner_4)

all.results.Leitner_4[1,11] <- BIC(Conf.fit.Leitner_4)
all.results.Leitner_4[2,11] <- BIC(Load.fit.Leitner_4)
all.results.Leitner_4[3,11] <- BIC(Int.fit.Leitner_4)


#MI test rejected at the configural invariance level.


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
# We cannot do a MI test


# Article 56: AlMahmoud -----------------------------------------------------------------------
url <- 'https://doi.org/10.1371/journal.pone.0202466.s002'
filename <- '../article56.xlxs'
GET(url, write_disk(filename, overwrite = TRUE))
article56 <- read_excel(filename)

# comparison 1:
# variables for scale: EIC1, EIC2, EIC3, EIC4, EIC5, EIC6, EIC7, EIC8, EIC9
# grouping: Gender

#change colnames for first variables 

article56_1 <- article56[,c(2:10,47) ]

colnames(article56_1) <- c("EIC1", "EIC2", "EIC3", "EIC4", "EIC5", "EIC6", "EIC7", "EIC8", "EIC9", "Gender")

#Create model using the variables colnames for the POMS scale prefdbk
Model_AlMahmoud_1 <- NA
for (i in 1:9){
  Model_AlMahmoud_1[i] <- paste0("F1 =~" , "EIC", i)
}

#Create results matrix
all.results.AlMahmoud_1 <- matrix(NA, ncol = 11, nrow = 3)
colnames(all.results.AlMahmoud_1) <- c("chisq","df","pvalue", 
                                     "rmsea", "cfi", "srmr", "diffRMSEA", "diffCFI","lavtestLRT", "AIC", "BIC")


# Fit the configural invariance model
Conf.fit.AlMahmoud_1<- cfa(Model_AlMahmoud_1,
                         article56_1, 
                         group = "Gender")

# Store configural invariance model goodness-of-fit measures results
all.results.AlMahmoud_1[1,1:6] <-round(data.matrix(fitmeasures(Conf.fit.AlMahmoud_1,
                                                             fit.measures = c("chisq","df","pvalue", 
                                                                              "rmsea", "cfi", "srmr"))), digits=3)

# Fit the loadings invariance model
Load.fit.AlMahmoud_1 <- cfa(Model_AlMahmoud_1,
                          article56_1, 
                          group = "Gender",
                          group.equal = "loadings")

# Store loadings invariance model goodness-of-fit measures results
all.results.AlMahmoud_1[2,1:6] <-round(data.matrix(fitmeasures(Load.fit.AlMahmoud_1,
                                                             fit.measures = c("chisq","df","pvalue", 
                                                                              "rmsea", "cfi", "srmr"))), digits=3)

# Fit the intercepts invariance model
Int.fit.AlMahmoud_1 <- cfa(Model_AlMahmoud_1,
                         article56_1, 
                         group = "Gender",
                         group.equal = c("loadings", "intercepts"))

# Store loadings invariance model goodness-of-fit measures results
all.results.AlMahmoud_1[3,1:6] <-round(data.matrix(fitmeasures(Int.fit.AlMahmoud_1,
                                                             fit.measures = c("chisq","df","pvalue", 
                                                                              "rmsea", "cfi", "srmr"))), digits=3)


# Compute the difference in goodness-of-fit measures
all.results.AlMahmoud_1[2,7:8] <- all.results.AlMahmoud_1[2,][4:5] - all.results.AlMahmoud_1[1,][4:5]
all.results.AlMahmoud_1[3,7:8] <- all.results.AlMahmoud_1[3,][4:5] - all.results.AlMahmoud_1[2,][4:5]

# Compute the LRT between the different models and store the p-value result
all.results.AlMahmoud_1[2,9] <- lavTestLRT(Conf.fit.AlMahmoud_1, Load.fit.AlMahmoud_1)$`Pr(>Chisq)`[2]
all.results.AlMahmoud_1[3,9] <- lavTestLRT(Load.fit.AlMahmoud_1, Int.fit.AlMahmoud_1)$`Pr(>Chisq)`[2]

all.results.AlMahmoud_1[1,10] <- AIC(Conf.fit.AlMahmoud_1)
all.results.AlMahmoud_1[2,10] <- AIC(Load.fit.AlMahmoud_1)
all.results.AlMahmoud_1[3,10] <- AIC(Int.fit.AlMahmoud_1)

all.results.AlMahmoud_1[1,11] <- BIC(Conf.fit.AlMahmoud_1)
all.results.AlMahmoud_1[2,11] <- BIC(Load.fit.AlMahmoud_1)
all.results.AlMahmoud_1[3,11] <- BIC(Int.fit.AlMahmoud_1)



# comparison 2:
# variables for scale: EBEI1, EBEI2, EBEI3, EBEI4, EBEI5, EBEI6, EBEI7, EBEI8, EBEI9, EBEI10
# grouping: Gender

article56_2 <- article56[,c(11:20,47) ]

colnames(article56_2) <- c("EBEI1", "EBEI2", "EBEI3", "EBEI4", "EBEI5", "EBEI6", "EBEI7", "EBEI8", "EBEI9", "EBEI10", "Gender")

#Create model using the variables colnames for the POMS scale prefdbk
Model_AlMahmoud_2 <- NA
for (i in 1:9){
  Model_AlMahmoud_2[i] <- paste0("F1 =~" , "EBEI", i)
}

#Create results matrix
all.results.AlMahmoud_2 <- matrix(NA, ncol = 11, nrow = 3)
colnames(all.results.AlMahmoud_2) <- c("chisq","df","pvalue", 
                                       "rmsea", "cfi", "srmr", "diffRMSEA", "diffCFI","lavtestLRT", "AIC", "BIC")


# Fit the configural invariance model
Conf.fit.AlMahmoud_2<- cfa(Model_AlMahmoud_2,
                           article56_2, 
                           group = "Gender")

# Store configural invariance model goodness-of-fit measures results
all.results.AlMahmoud_2[1,1:6] <-round(data.matrix(fitmeasures(Conf.fit.AlMahmoud_2,
                                                               fit.measures = c("chisq","df","pvalue", 
                                                                                "rmsea", "cfi", "srmr"))), digits=3)

# Fit the loadings invariance model
Load.fit.AlMahmoud_2 <- cfa(Model_AlMahmoud_2,
                            article56_2, 
                            group = "Gender",
                            group.equal = "loadings")

# Store loadings invariance model goodness-of-fit measures results
all.results.AlMahmoud_2[2,1:6] <-round(data.matrix(fitmeasures(Load.fit.AlMahmoud_2,
                                                               fit.measures = c("chisq","df","pvalue", 
                                                                                "rmsea", "cfi", "srmr"))), digits=3)

# Fit the intercepts invariance model
Int.fit.AlMahmoud_2 <- cfa(Model_AlMahmoud_2,
                           article56_2, 
                           group = "Gender",
                           group.equal = c("loadings", "intercepts"))

# Store loadings invariance model goodness-of-fit measures results
all.results.AlMahmoud_2[3,1:6] <-round(data.matrix(fitmeasures(Int.fit.AlMahmoud_2,
                                                               fit.measures = c("chisq","df","pvalue", 
                                                                                "rmsea", "cfi", "srmr"))), digits=3)


# Compute the difference in goodness-of-fit measures
all.results.AlMahmoud_2[2,7:8] <- all.results.AlMahmoud_2[2,][4:5] - all.results.AlMahmoud_2[1,][4:5]
all.results.AlMahmoud_2[3,7:8] <- all.results.AlMahmoud_2[3,][4:5] - all.results.AlMahmoud_2[2,][4:5]

# Compute the LRT between the different models and store the p-value result
all.results.AlMahmoud_2[2,9] <- lavTestLRT(Conf.fit.AlMahmoud_2, Load.fit.AlMahmoud_2)$`Pr(>Chisq)`[2]
all.results.AlMahmoud_2[3,9] <- lavTestLRT(Load.fit.AlMahmoud_2, Int.fit.AlMahmoud_2)$`Pr(>Chisq)`[2]

all.results.AlMahmoud_2[1,10] <- AIC(Conf.fit.AlMahmoud_2)
all.results.AlMahmoud_2[2,10] <- AIC(Load.fit.AlMahmoud_2)
all.results.AlMahmoud_2[3,10] <- AIC(Int.fit.AlMahmoud_2)

all.results.AlMahmoud_2[1,11] <- BIC(Conf.fit.AlMahmoud_2)
all.results.AlMahmoud_2[2,11] <- BIC(Load.fit.AlMahmoud_2)
all.results.AlMahmoud_2[3,11] <- BIC(Int.fit.AlMahmoud_2)

# comparison 3:s
# variables for scale: ECOVP1, ECOVP2, ECOVP3, ECOVP4, ECOVP5, ECOVP6, ECOVP7, ECOVP8, ECOVP9, ECOVP10,
# ECOVP11, ECOVP12, ECOVP13, ECOVP14, ECOVP15, ECOVP16, ECOVP17, ECOVP18, ECOVP19, ECOVP20, ECOVP21, 
# ECOVP22, ECOVP23, ECOVP24, ECOVP25
# grouping: Gender

article56_3 <- article56[,c(21:45,47) ]

colnames(article56_3) <- c("ECOVP1", "ECOVP2", "ECOVP3", "ECOVP4", "ECOVP5", "ECOVP6", "ECOVP7", "ECOVP8", "ECOVP9", "ECOVP10",
                           "ECOVP11", "ECOVP12", "ECOVP13", "ECOVP14", "ECOVP15", "ECOVP16", "ECOVP17", "ECOVP18" , 
                           "ECOVP19", "ECOVP20", "ECOVP21", "ECOVP22", "ECOVP23", "ECOVP24", "ECOVP25", "Gender")

#Create model using the variables colnames for the POMS scale prefdbk
Model_AlMahmoud_3 <- NA
for (i in 1:9){
  Model_AlMahmoud_3[i] <- paste0("F1 =~" , "ECOVP", i)
}

#Create results matrix
all.results.AlMahmoud_3 <- matrix(NA, ncol = 11, nrow = 3)
colnames(all.results.AlMahmoud_3) <- c("chisq","df","pvalue", 
                                       "rmsea", "cfi", "srmr", "diffRMSEA", "diffCFI","lavtestLRT", "AIC", "BIC")


# Fit the configural invariance model
Conf.fit.AlMahmoud_3<- cfa(Model_AlMahmoud_3,
                           article56_3, 
                           group = "Gender")

# Store configural invariance model goodness-of-fit measures results
all.results.AlMahmoud_3[1,1:6] <-round(data.matrix(fitmeasures(Conf.fit.AlMahmoud_3,
                                                               fit.measures = c("chisq","df","pvalue", 
                                                                                "rmsea", "cfi", "srmr"))), digits=3)

# Fit the loadings invariance model
Load.fit.AlMahmoud_3 <- cfa(Model_AlMahmoud_3,
                            article56_3, 
                            group = "Gender",
                            group.equal = "loadings")

# Store loadings invariance model goodness-of-fit measures results
all.results.AlMahmoud_3[2,1:6] <-round(data.matrix(fitmeasures(Load.fit.AlMahmoud_3,
                                                               fit.measures = c("chisq","df","pvalue", 
                                                                                "rmsea", "cfi", "srmr"))), digits=3)

# Fit the intercepts invariance model
Int.fit.AlMahmoud_3 <- cfa(Model_AlMahmoud_3,
                           article56_3, 
                           group = "Gender",
                           group.equal = c("loadings", "intercepts"))

# Store loadings invariance model goodness-of-fit measures results
all.results.AlMahmoud_3[3,1:6] <-round(data.matrix(fitmeasures(Int.fit.AlMahmoud_3,
                                                               fit.measures = c("chisq","df","pvalue", 
                                                                                "rmsea", "cfi", "srmr"))), digits=3)


# Compute the difference in goodness-of-fit measures
all.results.AlMahmoud_3[2,7:8] <- all.results.AlMahmoud_3[2,][4:5] - all.results.AlMahmoud_3[1,][4:5]
all.results.AlMahmoud_3[3,7:8] <- all.results.AlMahmoud_3[3,][4:5] - all.results.AlMahmoud_3[2,][4:5]

# Compute the LRT between the different models and store the p-value result
all.results.AlMahmoud_3[2,9] <- lavTestLRT(Conf.fit.AlMahmoud_3, Load.fit.AlMahmoud_3)$`Pr(>Chisq)`[2]
all.results.AlMahmoud_3[3,9] <- lavTestLRT(Load.fit.AlMahmoud_3, Int.fit.AlMahmoud_3)$`Pr(>Chisq)`[2]

all.results.AlMahmoud_3[1,10] <- AIC(Conf.fit.AlMahmoud_3)
all.results.AlMahmoud_3[2,10] <- AIC(Load.fit.AlMahmoud_3)
all.results.AlMahmoud_3[3,10] <- AIC(Int.fit.AlMahmoud_3)

all.results.AlMahmoud_3[1,11] <- BIC(Conf.fit.AlMahmoud_3)
all.results.AlMahmoud_3[2,11] <- BIC(Load.fit.AlMahmoud_3)
all.results.AlMahmoud_3[3,11] <- BIC(Int.fit.AlMahmoud_3)

# Article 74: Cardoso -------------------------------------------------------------------------
url <- 'https://doi.org/10.1371/journal.pone.0205352.s002'
filename <- '../article74.csv'
GET(url, write_disk(filename, overwrite = TRUE))
article74 <- read.csv2(filename, header=F, na.strings="NA", sep=",")

# variables for scale: V1 V2 V3 (do not forget to delete the first row)
# grouping: we cannot find the grouping variable.
# (A: cow housing pasture and presence of shade; B: cow housing pasture and absence of shade; C: cow housing indoors and presence of shade; D: cow housing indoors and absence of shade)


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
Model_Senanayake_1 <- 'F1 =~ Eq5Dmob +  Eq5Dselfc + Eq5Duact +  Eq5Dpain + Eq5Dmood'
  

#Create results matrix
all.results.Senanayake_1 <- matrix(NA, ncol = 11, nrow = 2)
colnames(all.results.Senanayake_1) <- c("chisq","df","pvalue", 
                                       "rmsea", "cfi", "srmr", "diffRMSEA", "diffCFI","lavtestLRT", "AIC", "BIC")


# Fit the configural invariance model
Conf.Mod.Senanayake_1<- measEq.syntax(Model_Senanayake_1,
                                      ID.fac = "std.lv",
                                      ID.cat = "Wu",
                                      ordered = c("Eq5Dmob", "Eq5Dselfc", "Eq5Duact", "Eq5Dpain", "Eq5Dmood"),
                                      group = "depression", 
                                      parameterization = "delta", 
                                      data = article131,
                                      group.equal = "configural") 

Conf.fit.Senanayake_1<- cfa(as.character(Conf.Mod.Senanayake_1),
                           article131, 
                           group = "depression", 
                           estimator = "WLSMV")

# Store configural invariance model goodness-of-fit measures results
all.results.Senanayake_1[1,1:6] <-round(data.matrix(fitmeasures(Conf.fit.Senanayake_1,
                                                               fit.measures = c("chisq.scaled","df.scaled","pvalue", 
                                                                                "rmsea.scaled", "cfi.scaled", "srmr"))), digits=3)


# Fit the intercepts invariance model
Load.Mod.Senanayake_1<- measEq.syntax(Model_Senanayake_1,
                                     ID.fac = "std.lv",
                                     ID.cat = "Wu",
                                     ordered = c("Eq5Dmob", "Eq5Dselfc", "Eq5Duact", "Eq5Dpain", "Eq5Dmood"),
                                     group = "depression", 
                                     parameterization = "delta", 
                                     data = article131,
                                     group.equal = c("thresholds", "loadings", "intercepts")) 


Load.fit.Senanayake_1 <- cfa(as.character(Load.Mod.Senanayake_1),
                            article131, 
                            group = "depression", 
                            estimator = "WLSMV")

# Store loadings invariance model goodness-of-fit measures results
all.results.Senanayake_1[2,1:6] <-round(data.matrix(fitmeasures(Load.fit.Senanayake_1,
                                                                fit.measures = c("chisq.scaled","df.scaled","pvalue", 
                                                                                 "rmsea.scaled", "cfi.scaled", "srmr"))), digits=3)


# Compute the difference in goodness-of-fit measures
all.results.Senanayake_1[2,7:8] <- all.results.Senanayake_1[2,][4:5] - all.results.Senanayake_1[1,][4:5]

# Compute the LRT between the different models and store the p-value result
all.results.Senanayake_1[2,9] <- lavTestLRT(Conf.fit.Senanayake_1, Load.fit.Senanayake_1)$`Pr(>Chisq)`[2]

#Not available for non-ML estimation
#all.results.Senanayake_1[1,10] <- AIC(Conf.fit.Senanayake_1)
#all.results.Senanayake_1[3,10] <- AIC(Load.fit.Senanayake_1)

#all.results.Senanayake_1[1,11] <- BIC(Conf.fit.Senanayake_1)
#all.results.Senanayake_1[3,11] <- BIC(Load.fit.Senanayake_1)


# comparison 2:
# variables for scale: Eq5Dmob, Eq5Dselfc, Eq5Duact, Eq5Dpain, Eq5Dmood
# grouping: distress

#Create model 
Model_Senanayake_2 <- 'F1 =~ Eq5Dmob +  Eq5Dselfc + Eq5Duact +  Eq5Dpain + Eq5Dmood'

# Note that since in 1 group there is a 0 cell problem (not observed score for the variables Eq5Dmob, selfc, Duact, mood)
# we decided to collapse categories 3 scores in 2

article131_collapsed <- article131
article131_collapsed$Eq5Dmob <- ifelse(article131_collapsed$Eq5Dmob >=2, 2, 1)
article131_collapsed$Eq5Dselfc <- ifelse(article131_collapsed$Eq5Dselfc >=2, 2, 1)
article131_collapsed$Eq5Duact <- ifelse(article131_collapsed$Eq5Duact >=2, 2, 1)
article131_collapsed$Eq5Dmood <- ifelse(article131_collapsed$Eq5Dmood >=2, 2, 1)


#Create results matrix
# Fit the configural invariance model
Conf.Mod.Senanayake_2<- measEq.syntax(Model_Senanayake_2,
                                      ID.fac = "std.lv",
                                      ID.cat = "Wu",
                                      ordered = c("Eq5Dmob", "Eq5Dselfc", "Eq5Duact", "Eq5Dpain", "Eq5Dmood"),
                                      group = "distress", 
                                      parameterization = "delta", 
                                      data = article131_collapsed,
                                      group.equal = "configural") 

all.results.Senanayake_2 <- matrix(NA, ncol = 11, nrow = 2)
colnames(all.results.Senanayake_2) <- c("chisq","df","pvalue", 
                                        "rmsea", "cfi", "srmr", "diffRMSEA", "diffCFI","lavtestLRT", "AIC", "BIC")

Conf.fit.Senanayake_2<- cfa(as.character(Conf.Mod.Senanayake_2),
                            article131_collapsed, 
                            group = "distress", 
                            estimator = "WLSMV")

# Store configural invariance model goodness-of-fit measures results
all.results.Senanayake_2[1,1:6] <-round(data.matrix(fitmeasures(Conf.fit.Senanayake_2,
                                                                fit.measures = c("chisq.scaled","df.scaled","pvalue", 
                                                                                 "rmsea.scaled", "cfi.scaled", "srmr"))), digits=3)

# Fit the intercepts invariance model
Load.Mod.Senanayake_2<- measEq.syntax(Model_Senanayake_2,
                                      ID.fac = "std.lv",
                                      ID.cat = "Wu",
                                      ordered = c("Eq5Dmob", "Eq5Dselfc", "Eq5Duact", "Eq5Dpain", "Eq5Dmood"),
                                      group = "distress", 
                                      parameterization = "delta", 
                                      data = article131_collapsed,
                                      group.equal = c("thresholds", "loadings", "intercepts")) 

Load.fit.Senanayake_2<- cfa(as.character(Load.Mod.Senanayake_2),
                            article131_collapsed, 
                            group = "distress", 
                            estimator = "WLSMV")

# Store loadings invariance model goodness-of-fit measures results
all.results.Senanayake_2[2,1:6] <-round(data.matrix(fitmeasures(Load.fit.Senanayake_2,
                                                                fit.measures = c("chisq.scaled","df.scaled","pvalue", 
                                                                                 "rmsea.scaled", "cfi.scaled", "srmr"))), digits=3)


# Compute the difference in goodness-of-fit measures
all.results.Senanayake_2[2,7:8] <- all.results.Senanayake_2[2,][4:5] - all.results.Senanayake_2[1,][4:5]

# Compute the LRT between the different models and store the p-value result
all.results.Senanayake_2[2,9] <- lavTestLRT(Conf.fit.Senanayake_2, Load.fit.Senanayake_2)$`Pr(>Chisq)`[2]

#Not available for non-ML estimation
#all.results.Senanayake_2[1,10] <- AIC(Conf.fit.Senanayake_2)
#all.results.Senanayake_2[3,10] <- AIC(Load.fit.Senanayake_2)

#all.results.Senanayake_2[1,11] <- BIC(Conf.fit.Senanayake_2)
#all.results.Senanayake_2[3,11] <- BIC(Load.fit.Senanayake_2)



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
# Sum scores are used for the constructs, so we cannot do a MI test


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

# Grouping variable is found. All factors are sum scores so we cannot do a MI test.


# Article 199: Goswami ------------------------------------------------------------------------
# Data Availability: All relevant data are within the manuscript, Supporting Information files 
# and via the DRYAD repository: https://datadryad.org/review?doi=doi:10.5061/dryad.m2q4c83.
# The data from DRYAD is not available. 
# There is no supplemental material added to the article
# The article does not contain correlations for the items of the WHO-DAS
# So we cannot do a MI test because we have no data


#------------------------------------------------------------------------------------------------------#
#--------------------------------- Descriptives reproducibility part-----------------------------------#
#------------------------------------------------------------------------------------------------------#

rm(list = ls()) # clear workspace
options(scipen=999) # no scientific notation
library(readxl)

reproducibility_codebook_Sheet1 <- read_csv("reproducibility-codebook - Sheet1.csv")
# df <- read_excel("../data/codebook-main-reporting.xlsx") # load data 

df <- reproducibility_codebook_Sheet1
colnames(df)

#Toal study that were eligible to be reproduced
nrow(df)

# Divide datasets that were usable between those that did and did not conduct a measurement invariance test
df_MI_yes <- subset(df, df$mitest_rep == 1)
round(nrow(df_MI_yes)/nrow(df),3); nrow(df_MI_yes) #41 (22.5)

df_MI_no <- subset(df, df$mitest_rep == 0)
round(nrow(df_MI_no)/nrow(df),3); nrow(df_MI_no) #141

# How many studies actually had available data?      

#For those that did conduct a MI test
round(sum(df_MI_yes$`data availability`, na.rm = T)/nrow(df_MI_yes),3); sum(df_MI_yes$`data availability`, na.rm = T) #6 (0.14)
Nav_yes <- sum(df_MI_yes$`data availability`, na.rm = T) #6

round(sum(df_MI_no$`data availability`, na.rm = T)/nrow(df_MI_no),3); sum(df_MI_no$`data availability`, na.rm = T) #72 (0.51)
Nav_no <- sum(df_MI_no$`data availability`, na.rm = T)

# Of those that had available data (78), for how many could we construct a grouping variable?
round(sum(df_MI_yes$open_group, na.rm = T)/Nav_yes,3); sum(df_MI_yes$open_group, na.rm=T) #6 (1.0)

round(sum(df_MI_no$open_group, na.rm = T)/Nav_no,3); sum(df_MI_no$open_group, na.rm=T) #64 (0.89)

# Of those that had available data (78), for how many could we construct a scale?
round(sum(df_MI_yes$open_scale, na.rm = T)/Nav_yes,3); sum(df_MI_yes$open_scale, na.rm=T) #5 (0.83)

round(sum(df_MI_no$open_scale, na.rm = T)/Nav_no,3); sum(df$open_scale, na.rm=T) #15 (0.139)

# Of those that had available data (78), for how many could we construct both a scale and a grouping variable?
length(which(df_MI_yes$open_scale == 1 & df_MI_yes$open_group == 1))/Nav_yes; length(which(df_MI_yes$open_scale == 1 & df_MI_yes$open_group == 1)) 
N_usable_yes <- length(which(df_MI_yes$open_scale == 1 & df_MI_yes$open_group == 1)) #5 (0.83)

length(which(df_MI_no$open_scale == 1 & df_MI_no$open_group == 1))/Nav_no; length(which(df_MI_no$open_scale == 1 & df_MI_no$open_group == 1)) 
N_usable_no <- length(which(df_MI_no$open_scale == 1 & df_MI_no$open_group == 1)) #9 (0.125)

# Of those studies where we tried to reproduce MI testing, could MI be tested?
round(sum(df_MI_yes$mitest, na.rm = T)/N_usable_yes,3); sum(df_MI_yes$mitest, na.rm = T) #5 (1.0)
N_Mi_yes_test <- sum(df_MI_yes$mitest_rep, na.rm = T)

# Of those studies where we did test for MI, could MI be tested?
round(sum(df_MI_no$mitest, na.rm = T)/N_usable_no,3); sum(df_MI_no$mitest, na.rm = T) #9 (1.0)
N_Mi_no_test <- sum(df_MI_no$mitest_rep, na.rm = T)

# Of those studies where we tried to reproduce MI testing and could test for MI, did MI hold?
round(sum(df_MI_yes$miresult, na.rm = T)/N_usable_yes,3); sum(df_MI_yes$miresult, na.rm = T) #4 (0.8)
N_Mi_yes_test_hold <- sum(df_MI_yes$miresult, na.rm = T) 

# Of those studies where we did test for MI and could test for MI, did MI hold?       
round(sum(df_MI_no$miresult, na.rm = T)/N_usable_no,3); sum(df_MI_no$miresult, na.rm = T) #0 (0.0)
N_Mi_no_test_hold <- sum(df_MI_no$miresult, na.rm = T) 

# Of those studies where we tried to reproduce MI testing, could test for MI and MI hold, at what level did MI hold?
df_MI_yes_hold <- subset(df_MI_yes, df_MI_yes$miresult == 1) #1 (configural), 1 (metric), 2 (scalar)
mi_level_yes <- table(df_MI_yes_hold$milevel)
mi_level_yes

#does MI level obtain match that reported? 
df_MI_yes_hold$milevel == df_MI_yes_hold$milevel_rep #Only for 2 (0.5)

