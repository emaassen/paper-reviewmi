### CODE FOR MAIN STUDY SYSTEMATIC REVIEW MEASUREMENT INVARIANCE ##
### This is code to perform step 2 and step 3 of our main study; reproducing results for studies that reported on MI

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

# Article 22: Moreira -------------------------------------------------------------------------
# Raw data is shared via OSF
url <- 'https://osf.io/3qda9///?action=download'
filename <- '../data/data-main/article22.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article22 <- read_sav(filename)
# We can make a grouping variable (time) but we do not have item scores, only the sumscore for the Factor.
# We cannot reproduce the MI test (step 2) and cannot perform one ourselves (step 3)


# Article 77: Schulze -------------------------------------------------------------------------
# Data shared are covariance matrices for 5 groups in a word document, copied from here:
# https://doi.org/10.1371/journal.pone.0207331.s003

# The article mentions the following on measurement invariance:

# Following Chen’s [45] recommendations for unequal sample sizes, we retained the hypothesis of loading invariance when the chi-square 
# change was insignificant (α > .05), or a decrease in CFI < .010 was accompanied by an increase in RMSEA < .010 or an increase in SRMR 
# < .030. When concerning intercept and residual invariance the same rules applied except an increase in SRMR < .005 [45].

# Measurement invariance was thus only evaluated using maximum-likelihood CFA.

# In this manner, we derived a model with a fourth factor (called philanthropy) and excluded items 3 and 12 in the process due to 
# high residual correlations with other items indicating redundancy. 

# Following the results from the CFA and BSEM estimations, the 4F model displayed satisfying fit in all four Western samples, but not 
# in the Indian sample (see Table 2). The two descendants of the stakeholder factor, environmental CSR and philanthropy, were rather 
# strongly correlated in all samples (.77 < r < .88). In order to avoid an overfitted model, we compared model 4F with a simplified variant, 
# where these two factors were melted into one and all other model properties were left unchanged. Likelihood ratio tests of for these two 
# competing models revealed significant better fit of model 4F for all five samples (ps < .017). We thus concluded to keep four factors 
# despite high factor correlations.

# The configural model showed sufficient model fit when estimated simultaneously across the four groups (see Table 3). When the 
# loadings were constrained to equality, statistically insignificant change in chi-square was observed. Weak invariance was therefore 
# accepted for the four Western groups.

# In contrast, strong invariance could not be obtained (see Table 3). Restraining item intercepts to equality led to a statistically 
# significant chi-square difference as well as a sizeable change in the fit indices. When examining the source of invariance regarding 
# the intercepts, we found that items 6 and 9 were the most important causes, reducing noninvariance considerably with differences in 
# the fit indices being close to their cut-offs.

# As the stage of strong invariance could not be verified, strict invariance could not be obtained, too, although the change in fit indices 
# was marginal and non-substantial when the item residuals were set equal. 

# The authors used BSEM for their analyses.

# load the data
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
sigma.pop.77 <- list(ger1, ger2, us1, us2)

# Provide column names for covariance matrices
for(i in 1:length(sigma.pop.77)){
  colnames(sigma.pop.77[[i]]) <- paste0("V", 1:15)
}

# Create model for dataset based on article
mod.77_4F <- "
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

# Create vector of sample size based on information provided by authors
sample.77 <- c(146, 194, 155, 193, 432)

# The authors use Satorra-Bentler adjusted chisquares, meaning we should use the MLM estimator.
# However, the MLM estimator requires full data, which we don't have, therefore we will use the ML estimator.

# Fit the configural model
conf.fit.77 <- cfa(model = mod.77_4F, 
                        sample.cov = sigma.pop.77, 
                        sample.nobs = c(146, 194, 193, 432),
                        estimator = "ML",
                        meanstructure = TRUE)

# Fit the loadings invariant model
load.fit.77 <- cfa(model = mod.77_4F, 
                        sample.cov = sigma.pop.77, 
                        sample.nobs = c(146, 194,193, 432),
                        estimator = "ML",
                        group.equal = "loadings",
                        meanstructure = TRUE)

# Fit the intercepts invariance model
int.fit.77 <- cfa(model = mod.77_4F, 
                       sample.cov = sigma.pop.77, 
                       sample.nobs = c(146, 194, 193, 432),
                       estimator = "ML",
                       group.equal = c("loadings", "intercepts"),
                       meanstructure = TRUE)

# View results
all.results.77 <- mi.results(conf.fit.77,load.fit.77,int.fit.77)
all.results.77

# Step 2:
# We were able to reproduce the same dfs in all three models
# The chisquare (all 3 models) was satorra bentler adjusted, meaning we should use estimator MLM. 
# However, MLM requires full data which we do not have, so we used the ML estimator. 
# This means all chisquare values were irreproducible
# the fit measures: CFI (all 3 models), SRMR (model 1 and 3), and RMSEA (model 1 and 2) were irreproducible.
# The authors conclude loading invariance because the change in chisquare was non-significant from config-loading.
# We find a significant p-value, meaning the result for step 2 is irreproducible.
# The authors do not state what their cutoffs are to conclude configural invariance. As we find different estimates
# for the configural invariance model than the authors, we conclude that no level of invariance held according to our estimates.

# Step 3:
# The data are ordinal, meaning we will use specifications and constraints proposed by Wu and Estabrook (2016; https://link.springer.com/article/10.1007%2Fs11336-016-9506-0)
# For ordinal data we should use the DWLS estimator. However, since DWLS requires full data which we do not have,
# we ran the analysis using a classic (i.e., linear) CFA with ML.
# Because the items have 5 answering categories, CFA is also appropriate (Rhemtulla et al. (2012)).
# This means that model specifications stay the same as in step 2, but we use our own cutoffs
all.results.77

# Configural model is statistically significant & has rmsea > .08 & CFI < .95, so level of invariance = 0.



# Article 133: Abdin --------------------------------------------------------------------------
# The article does not share raw data or correlations between items.Statement Data Availability: "The data underlying the 
# results of this study are available upon request due to ethical restrictions imposed by the SingHealth Centralised Institutional Review 
# Board, in Singapore. All study subjects were informed that only non-identifiable and aggregate data will be used in subsequent reports 
# and scientific publications, and communications to companies and stakeholders. Hence, individual data is not available for public online access. R
# Readers may write to the Workplace Safety and Health Institute, Ministry of Manpower at [address]"

# Step 2 and step 3:
# The data is available upon request, so we cannot do a MI test because we have no data.


# Article 207: Ortuno-Sierra ------------------------------------------------------------------
# Data shared is .sav file as supporting information to the article
# https://dx.plos.org/10.1371/journal.pone.0221696.s001

# The authors state the following on MI: 

# Attending to the limitations of the delta chisquare regarding its sensitivity to sample size, we followed the change in CFI (delta CFI), 
# proposed by Cheung and Rensvold [57] as a more suitable criterion, to determine whether nested models are practically equivalent. 
# Latent mean differences across gender and educational level groups were estimated, fixing the latent mean values to zero in the 
# male and in the Primary students groups. For comparisons among groups in the latent means, statistical significance was based 
# on the z statistic. The group in which the latent mean was fixed to zero was considered as the reference group 

# The comparison between the configural and scalar invariance models showed differences in CFI (delta CFI) below .01 across gender and educational 
# level. Thus, the hypothesis of MI was confirmed by these two variables

# The authors state the following on fitting models in groups separately, it is unclear whether this information also holds for the MI testing.
# They do use the TLI, RMSEA in their presentation of results of MI testing, so we assumed that this information also pertained to MI testing.

# Different analysis were carried out in the study. With the intention to analyze the internal structure of the PANAS, 
# we conducted several CFAs to at the item level. The weighted least squares means and variance adjusted (WLSMV) estimator 
# and the polychoric correlation matrix were used, attending to the categorical nature of the variables. We used the oblique 
# Geomin rotation with epsilon value = 0.5 [28,52]. The chi-square, the Comparative Fit Index (CFI), the Tucker-Lewis Index (TLI),
# and the root mean square error of approximation (RMSEA), and its 90% confidence interval and the Weighted Root Mean Square Residual (WRMR) 
# were considered as goodness-of-fit indices. In order to achieve a good fit of the data to the model, the values of CFI and TLI over 0.95 
# are considered adequate and over .90 acceptable, and the RMSEA values should be under 0.08 for a reasonable fit and under 0.05 for a good fit 
# [53–55]. For the WRMR, lower values than 1.0 have been reported as adequate [55].

# load the data
url <- 'https://dx.plos.org/10.1371/journal.pone.0221696.s001'
filename <- '../data/data-main/article207.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article207 <- read_sav(filename)
article207 <- as.data.frame(article207)

# comparison 1:
# variables for scale: PAN01 until PAN20
# grouping variable for id 868: education (variable name: curso: primero ESO & segundo ESO)
# labels: primary -10 to 12 years old- versus secondary students -13 to 15 years old

# Step 2:
# The authors reported using an ESEM two factors model to test for MI, with the items for each factor being displayed in Table 2.
# construct model
mod.207.1 <- "
PAf =~ PAN1 + PAN3 + PAN5 + PAN9 + PAN10 + PAN12 + PAN14 + PAN16 + PAN17 + PAN19
NAf =~ PAN2 + PAN4 + PAN6 + PAN7 + PAN8 + PAN11 + PAN13 + PAN15 + PAN18 + PAN20
PAf ~~ NAf"

# fit the configural model
conf.fit.207.1 <- cfa(mod.207.1,
                              article207, 
                              ordered = colnames(article207[,5:24]),
                              group = "CURSO_RECO", 
                              estimator = "WLSMV",
                              meanstructure = T)

# fit the scalar invariance model 
# note that we do not test the thresholds but the intercepts for invariance,
# even though the authors state that they take care of the ordered nature of the data
# this is because the degrees of freedom for the intercept invariance model coincide with the reported values, whereas the degrees 
# of freedom for the threshold invariance do not coincide. We therefore inferred that they tested intercepts instead of thresholds. 

int.fit.207.1 <- cfa(mod.207.1,
                             article207, 
                             ordered = colnames(article207[,5:24]),
                             group = "CURSO_RECO", 
                             estimator = "WLSMV",
                             group.equal = "intercepts",
                             meanstructure = T)

# Store model goodness-of-fit measures results
# the WRMR is not estimated in lavaan, so we cannot reproduce it. 
all.results.207.1 <- matrix(NA, ncol = 9, nrow = 2)
colnames(all.results.207.1) <- c("chisq.sc","df.sc","pvalue.sc", "rmsea.sc", "cfi.sc", "TLI.sc",
                                 "diffRMSEA", "diffCFI", "lavtestLRT-pvalue")

all.results.207.1[1,1:6]<-round(data.matrix(fitmeasures(conf.fit.207.1,
                                                                fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", 
                                                                                 "rmsea.scaled", "cfi.scaled", "TLI.scaled"))), digits=3)
all.results.207.1[2,1:6] <-round(data.matrix(fitmeasures(int.fit.207.1,
                                                                 fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", 
                                                                                  "rmsea.scaled", "cfi.scaled", "TLI.scaled"))), digits=3)
# Compute the difference in goodness-of-fit measures
all.results.207.1[2,7:8] <- all.results.207.1[2,][4:5] - all.results.207.1[1,][4:5]

# Compute the LRT between the different models and store the p-value result
all.results.207.1[2,9] <- lavTestLRT(conf.fit.207.1, int.fit.207.1)$`Pr(>Chisq)`[2]

# check results
all.results.207.1

# Step 2:
# We were able to reproduce the dfs for one model (model 2), and the RMSEA for one model (model 1)
# we could not reproduce the chisquare values, CFI values, TLI values.
# the authors state a change in CFI of < .01 means measurement invariance holds. We also find this.
# the authors conclude scalar invariance, but we cannot, hence irreproducible. 

# Step 3: 
# The data is ordered, meaning we used guidelines proposed by Wu and Estabrook (2016) for MI testing with ordinal data. 
# This also means we need to construct syntax to estimate threshold invariance.

# Create and fit configural model
syntax.207.1.config <- measEq.syntax(mod.207.1, 
                              ID.fac = "std.lv", 
                              ID.cat = "Wu",
                              ordered = colnames(article207[,5:24]),
                              parameterization = "delta",
                              data = article207,
                              group = "CURSO_RECO", 
                              group.equal = "configural", 
                              orthogonal = T)

# Fit the configural model
config.207.1 <- cfa(as.character(syntax.207.1.config),
                    article207, 
                    ordered = colnames(article207[,5:24]),
                    group = "CURSO_RECO", 
                    estimator = "WLSMV")

# Create the thresholds invariance model following Wu and Estabrook (2016)
syntax.207.1.tresholds <- measEq.syntax(mod.207.1,
                                        ID.fac = "std.lv", 
                                        ID.cat = "Wu",
                                        ordered = colnames(article207[,5:24]),
                                        parameterization = "delta",
                                        data = article207,
                                        group = "CURSO_RECO", 
                                        group.equal = "thresholds", 
                                        orthogonal = T)

# Fit the thresholds invariance model
tresh.207.1 <- cfa(as.character(syntax.207.1.tresholds),
                   article207, 
                   ordered = colnames(article207[,5:24]),
                   group = "CURSO_RECO", 
                   estimator = "WLSMV")

# Create the loadings invariance model following Wu and Estabrook (2016)
syntax.207.1.loadings <- measEq.syntax(mod.207.1, 
                                    ID.fac = "std.lv", 
                                    ID.cat = "Wu",
                                    ordered = colnames(article207[,5:24]),
                                    parameterization = "delta",
                                    data = article207,
                                    group = "CURSO_RECO", 
                                    group.equal = c("thresholds","loadings"), 
                                    orthogonal = T)

# Fit the loadings invariance model
load.207.1 <- cfa(as.character(syntax.207.1.loadings),
                              article207, 
                              ordered = colnames(article207[,5:24]),
                              group = "CURSO_RECO", 
                              estimator = "WLSMV")

# Store goodness-of-fit measures results
# We cannot compute AIC and BIC because log-likelihood is not calculated with estimator WLSMV
all.results.207.1 <- mi.results.sc(config.207.1,tresh.207.1,load.207.1)
all.results.207.1

# Step 3
# Configural: significant p-value and CFI is below 0.95. As such, configural invariance does not hold. 

#------

# comparison 2:
# variables for scale: PAN01 until PAN20
# grouping variable for id 869: gender (variable name: G?nero)
# labels: males versus females

# recode gender variable because it has a sign that may not be readable on all machines
article207$gender <- as.numeric(article207[,4])

# Step 2:
# The authors reported using an ESEM two factors model to test for MI, with the items being displayed in Table 2.
# construct model
mod.207.2 <- "
PAf =~ PAN1 + PAN3 + PAN5 + PAN9 + PAN10 + PAN12 + PAN14 + PAN16 + PAN17 + PAN19
NAf =~ PAN2 + PAN4 + PAN6 + PAN7 + PAN8 + PAN11 + PAN13 + PAN15 + PAN18 + PAN20
PAf ~~ NAf"

# fit the configural model
conf.fit.207.2 <- cfa(mod.207.2,
                      article207, 
                      ordered = colnames(article207[,5:24]),
                      group = "gender", 
                      estimator = "WLSMV",
                      meanstructure = T)

# fit the strong factorial invariance model 
# note that we do not estimate a model with tresholds even though the authors state that they take care of the ordered nature of the data
# this is because the degrees of freedom for the intercept invariance model coincide with the reported values, whereas the degrees 
# of freedom for the treshold invariance do not coincide.

int.fit.207.2 <- cfa(mod.207.2,
                     article207, 
                     ordered = colnames(article207[,5:24]),
                     group = "gender", 
                     estimator = "WLSMV",
                     group.equal = "intercepts",
                     meanstructure = T)

# Store model goodness-of-fit measures results
all.results.207.2 <- matrix(NA, ncol = 9, nrow = 2)

colnames(all.results.207.2) <- c("chisq.sc","df.sc","pvalue.sc", "rmsea.sc", "cfi.sc", "TLI.sc",
                                 "diffRMSEA", "diffCFI", "lavtestLRT-pvalue")

all.results.207.2[1,1:6]<-round(data.matrix(fitmeasures(conf.fit.207.2,
                                                        fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", 
                                                                         "rmsea.scaled", "cfi.scaled", "TLI.scaled"))), digits=3)
all.results.207.2[2,1:6] <-round(data.matrix(fitmeasures(int.fit.207.2,
                                                         fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", 
                                                                          "rmsea.scaled", "cfi.scaled", "TLI.scaled"))), digits=3)
# Compute the difference in goodness-of-fit measures
all.results.207.2[2,7:8] <- all.results.207.2[2,][4:5] - all.results.207.2[1,][4:5]

# Compute the LRT between the different models and store the p-value result
all.results.207.2[2,9] <- lavTestLRT(Conf.fit.207.2, Int.fit.207.2)$`Pr(>Chisq)`[2]

# check results
all.results.207.2

# Step 2:
# We were able to reproduce the dfs for one model (model 2), and the RMSEA for one model (model 1)
# we could not reproduce the chisquare values, CFI values, TLI values.
# the authors state a change in CFI of < .01 means measurement invariance holds. We find a change of -0.026
# the authors conclude strong factorial invariance, but we cannot based on their cutoff and based on their estimated 
# chisquare, CFI, TLI values, hence irreproducible. 

# Step 3: 
# The data is ordered, meaning we used guidelines proposed by Wu and Estabrook (2016) for MI testing with ordinal data. 
# This also means we need to construct syntax to estimate treshold invariance.

# Create and fit configural model
syntax.207.2.config <- measEq.syntax(mod.207.2, 
                                     ID.fac = "std.lv", 
                                     ID.cat = "Wu",
                                     ordered = colnames(article207[,5:24]),
                                     parameterization = "delta",
                                     data = article207,
                                     group = "gender", 
                                     group.equal = "configural", 
                                     orthogonal = T)

# Fit the configural model
config.207.2 <- cfa(as.character(syntax.207.2.config),
                    article207, 
                    ordered = colnames(article207[,5:24]),
                    group = "gender", 
                    estimator = "WLSMV")

# Create the thresholds invariance model following Wu and Estabrook (2016)
syntax.207.2.tresholds <- measEq.syntax(mod.207.2,
                                        ID.fac = "std.lv", 
                                        ID.cat = "Wu",
                                        ordered = colnames(article207[,5:24]),
                                        parameterization = "delta",
                                        data = article207,
                                        group = "gender", 
                                        group.equal = "thresholds", 
                                        orthogonal = T)

# Fit the thresholds invariance model
tresh.207.2 <- cfa(as.character(syntax.207.2.tresholds),
                   article207, 
                   ordered = colnames(article207[,5:24]),
                   group = "gender", 
                   estimator = "WLSMV")

# Create the loadings invariance model following Wu and Estabrook (2016)
syntax.207.2.loadings <- measEq.syntax(mod.207.2, 
                                       ID.fac = "std.lv", 
                                       ID.cat = "Wu",
                                       ordered = colnames(article207[,5:24]),
                                       parameterization = "delta",
                                       data = article207,
                                       group = "gender", 
                                       group.equal = c("thresholds","loadings"), 
                                       orthogonal = T)

# Fit the loadings invariance model
load.207.2 <- cfa(as.character(syntax.207.2.loadings),
                  article207, 
                  ordered = colnames(article207[,5:24]),
                  group = "gender", 
                  estimator = "WLSMV")

# Store goodness-of-fit measures results
# Note that AIC and BIC are not computed because our estimator is not ML
all.results.207.2 <- mi.results.sc(config.207.2,tresh.207.2,load.207.2)
all.results.207.2 

# Step 3
# Configural: significant p-value and CFI is below 0.95. As such, configural invariance does not hold. 
# Results could not be fully reproduced due to lack of documenting how the ESEM two factor model that was used for 
# invariance testing is specified.


# Article 351: Kievit -------------------------------------------------------------------------
# Data shared is raw data .csv file and R code. The code below is copied from this R file: https://osf.io/8g96x/
# https://osf.io/pz8es/ 

url <- 'https://osf.io/pz8es//?action=download'
filename <- '../data/data-main/article351.csv'
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

# Step 2
# The only thing the authors mention on MI is:
# "As before; a multigroup model with gender as the grouping factor provided no evidence for differences". 
# The code above runs, and the authors run an anova and find a non-significant chisquare difference across groups. 
# We assume that this makes them conclude that there is measurement invariance. We can run this code as well.
# Meaning we conclude this study is reproducible. 
# Note that the level at which MI hold mostly refers to the structural part of the model (no measurement part is included), 
# and thus we coded it as partial measurement invariance.

# Step 3
# We do not conduct step 3 for this study as we were able to reproduce the result. 


# Article 710: Protzko ------------------------------------------------------------------------
# Data shared is raw data in .sav file: https://osf.io/x7c8w/ 
url <- 'https://osf.io/x7c8w//?action=download'
filename <- '../data/data-main/article710.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article710 <- read_sav(filename)

# The authors say this about measurement invariance: To investigate this possibility, we conducted a series of tests of measurement 
# invariance to determine whether the speeded manipulation affected the latent social-desirability factor or some other aspect of the 
# measurement process (e.g., on only some items). This procedure tested whether, given the same latent score on the measure, there were
# equal factor loadings or (more germanely) difference intercepts on the individual items. Violations of intercept invariance would suggest 
# that some items were being affected by the manipulation over others. 

# To test this, we fitted a series of models decreasing in equivalence parameters. We started with a strict invariance model, testing the
# two groups in a multigroup confirmatory factor analysis because the groups were created through randomization and did not naturally occur.
# We fitted the same single-factor model to the data and reduced the equivalence parameters, testing for decrements in model fit. 
# We followed the convention that a change in comparative fit index (delta CFI) greater than .01 or a change in the root-mean-square error of
# approximation (delta RMSEA) greater than 0.015 (Cheung & Rensvold, 2002) indicates that model fit significantly
# worsened after equal factor loadings and intercept constraints were imposed. 

# the results showed that constraining factor loadings to be equal among the groups did not cause a significant change in model fit (baseline
# CFI = .905, configural CFI = .905; baseline RMSEA = 0.067, configural RMSEA = 0.067). Further restricting the loading and intercepts to be 
# invariant between the groups also did not show a significant reduction in model fit (strict CFI = .901, strict RMSEA = 0.065).

# The authors use the term strict invariance to constrict loadings and intercepts; we use the term strong. 
# The authors state that they start with loadings+intercepts equal, but then also state that they first constrain factor loadings,
# and then "further restricting the loading and intercepts", indicating that the steps they take do seem to be config-load-intercept.
# it is unclear to us what the "baseline" model is and how it differs from the configural model. 

# variables for scale: Psd1, sd2R, sd3, sd4R, sd5, sd6R, s7R, sd8R, sd9, sd10 
# grouping variable: fast 

#Create model
mod.710 <- 'F1 =~ sd1 +  sd2R + sd3 + sd4R + sd5 + sd6R + s7R +  sd8R + sd9 + sd10'

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
conf.fit.710<- cfa(mod.710,
                        article710, 
                        group = "fast")

load.fit.710 <- cfa(mod.710,
                        article710, 
                        group = "fast",
                        group.equal = "loadings")

int.fit.710 <- cfa(mod.710,
                       article710, 
                       estimator = "ML", 
                       group = "fast",
                       group.equal =  c("loadings", "intercepts"))


# store goodness of fit measures
all.results.710 <- mi.results(conf.fit.710,load.fit.710,int.fit.710)
all.results.710

# Step 2:
# the authors only mention the CFI and RMSEA values, so this is what we look at.
# the CFI and the RMSEA difference is below the cutoff but our CFI and RMSEA estimates are different than reported.
# their CFI vales are: baseline (.905), configural (.905), strict (.901)
# their RMSEA values are: baseline (0.067), configural (0.067), strict (0.065).
# None of the CFI and RMSEA values coincide for none of the models. 
# The authors state that a change in RMSEA < .015 is ok; we find this for all models.
# The authors state that a change in CFI < .01 is ok; we find this only from config to loadings.
# because the estimates are irreproducible, we conclude irreproducibility of results. 

# Step 3
# the model and estimation stay the same, only our cutoffs are used.
# we find significant chisquare value for configural invariance & CFI < .95, so configural invariance is rejected.


# Article 711: Obaidi -------------------------------------------------------------------------
# The supplemental material (.pdf) reports measurement invariance checks in text:
# https://journals.sagepub.com/doi/suppl/10.1177/0956797619834879/suppl_file/ObaidiSupplementalMaterial.pdf
# Correlations are reported between factors but not between individual items
# We cannot do a MI test because we have no data
