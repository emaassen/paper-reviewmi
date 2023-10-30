### THE DIRE DISREGARD OF MEASUREMENT INVARIANCE TESTING ###
### This is code to simulate data for a one-factor model and 
### to run a measurement invariance test via MGCFA or MGCCFA

# Install and load packages -----------------------------------------------
# Clear workspace
#rm(list=ls())

# `faux` to use rnorm_multi function to simulate data
#install.packages("faux")
library(faux)

# `lavaan` to perform measurement invariance tests
#install.packages("lavaan")
library(lavaan)

# `semTools` to construct syntax with thresholds for items for CCFA
#install.packages("semTools")
library(semTools)

# MGCFA example -----------------------------------------------------------

# Sample size
n <- 1000                                                                              

# Variable names
varnames <- c(paste0("item", 1:7))                                                     
varnames

# Make grouping variable                
group <- c(rep(0,n/2),rep(1,n/2))                                                       
group

# Matrix to store data
scores <- matrix(NA, nrow = n, ncol = length(varnames))                                 

# Set seed and generate data with correlation of .25
set.seed(1709)
scores <- round(rnorm_multi(n = n, mu = 2.5, sd = 1, r = .25, varnames = varnames),0)   

# Column names for matrix
colnames(scores) <- varnames                                                           

# Make dataframe out of matrix
scores <- data.frame(scores)                                                           

# Combine scores and grouping variable into dataframe
scores <- cbind(scores,group)   
scores

# Construct measurement model (one factor with seven items)
model <- 'F =~ item1 + item2 + item3 + item4 + item5 + item6 + item7'

# Fit configural invariance model
conf.fit <- cfa(model = model, data = scores, group = "group")

# Fit metric (loading) invariance model
metric.fit <- cfa(model = model, data = scores, group = "group", group.equal = "loadings")

# Fit scalar (intercept) invariance model
scalar.fit <- cfa(model = model, data = scores, group = "group", group.equal = c("loadings", "intercepts"))

# View all fit measures
fitmeasures(conf.fit)
fitmeasures(metric.fit)
fitmeasures(scalar.fit)

# Save some selected fit measures in matrix
all.results <- matrix(NA, ncol = 12, nrow = 3)

colnames(all.results) <- c("chisq", "df", "pvalue", "rmsea", "cfi", "srmr",
                           "lavtestLRT-pvalue", "diffRMSEA", "diffCFI", "diffSRMR",
                           "AIC", "BIC")

# save fit measures from models
sel <- c("chisq","df","pvalue","rmsea","cfi","srmr") 
all.results[1,sel] <- round(data.matrix(fitmeasures(conf.fit, fit.measures = sel)), digits=3)
all.results[2,sel] <- round(data.matrix(fitmeasures(metric.fit, fit.measures = sel)), digits=3)
all.results[3,sel] <- round(data.matrix(fitmeasures(scalar.fit, fit.measures = sel)), digits=3)

# compute the difference in goodness-of-fit measures
# difference config-metric
all.results[2,c("diffRMSEA", "diffCFI", "diffSRMR")] <- all.results[2,c("rmsea","cfi","srmr")] - all.results[1,c("rmsea","cfi","srmr")]
# difference metric-scalar
all.results[3,c("diffRMSEA", "diffCFI", "diffSRMR")] <- all.results[3,c("rmsea","cfi","srmr")] - all.results[2,c("rmsea","cfi","srmr")]
  
# Compute the likelihood ratio test between the different models and store the p-values
# LRT between config and metric
all.results[2,"lavtestLRT-pvalue"] <- lavTestLRT(conf.fit,metric.fit)$`Pr(>Chisq)`[2] 
# LRT between metric and scalar
all.results[3,"lavtestLRT-pvalue"] <- lavTestLRT(metric.fit,scalar.fit)$`Pr(>Chisq)`[2] 

# Save the AIC estimate for all models
all.results[1,"AIC"] <- AIC(conf.fit)
all.results[2,"AIC"] <- AIC(metric.fit)
all.results[3,"AIC"] <- AIC(scalar.fit)

# Save the BIC estimate for all models
all.results[1,"BIC"] <- BIC(conf.fit)
all.results[2,"BIC"] <- BIC(metric.fit)
all.results[3,"BIC"] <- BIC(scalar.fit)

# View model fit results
all.results


# MGCCFA example ----------------------------------------------------------

# We perform MG-CCFA (multigroup categorical confirmatory factor analysis) for ordinal data.
# The syntax for the measurement model is different because thresholds are used for each item.
# We used the constraints reported by Wu & Estabrook (https://doi.org/10.1007/s11336-016-9506-0) 
# and the WLSMV estimator. The steps tested here are configural - thresholds - loadings. 

# Simulate dataset
# Sample size
n <- 1000                                                                              

# Variable names
varnames <- c(paste0("item", 1:4))                                                     
varnames

# Make grouping variable                
group <- c(rep(0,n/2),rep(1,n/2))                                                       
group

# Matrix to store data
scores <- matrix(NA, nrow = n, ncol = length(varnames))                                 

# Set seed and generate data with correlation of .25
set.seed(1709)
scores <- round(rnorm_multi(n = n, mu = 2.5, sd = 0.5, r = .25, varnames = varnames),0)   

# Column names for matrix
colnames(scores) <- varnames                                                           

# Make dataframe out of matrix
scores <- data.frame(scores)                                                           

# Combine scores and grouping variable into dataframe
scores <- cbind(scores,group)   
scores

# Construct measurement model (one factor with four items)
model <- 'F =~ item1 + item2 + item3 + item4'

# Syntax configural model
syntax.config <- measEq.syntax(model, 
                               ID.fac = "std.lv", 
                               ID.cat = "Wu",
                               ordered = colnames(scores[,1:4]),
                               parameterization = "delta",
                               data = scores,
                               group = "group", 
                               group.equal = "configural", 
                               orthogonal = T)

# Fit configural invariance model
conf.fit <- cfa(as.character(syntax.config),
                data = scores, 
                ordered = colnames(scores[,1:4]),
                group = "group", 
                estimator = "WLSMV")

# Syntax threshold model
syntax.thres <- measEq.syntax(model, 
                              ID.fac = "std.lv", 
                              ID.cat = "Wu",
                              ordered = colnames(scores[,1:4]),
                              parameterization = "delta",
                              data = scores,
                              group = "group", 
                              group.equal = "thresholds", 
                              orthogonal = T)

# Fit threshold invariance model
thres.fit <- cfa(as.character(syntax.thres),
                 data = scores, 
                 ordered = colnames(scores[,1:4]),
                 group = "group", 
                 estimator = "WLSMV")

# Syntax loading invariance model
syntax.load <- measEq.syntax(model, 
                             ID.fac = "std.lv", 
                             ID.cat = "Wu",
                             ordered = colnames(scores[,1:4]),
                             parameterization = "delta",
                             data = scores,
                             group = "group", 
                             group.equal = c("thresholds","loadings"), 
                             orthogonal = T)

# Fit loading invariance model
load.fit <- cfa(as.character(syntax.load),
                data= scores, 
                ordered = colnames(scores[,1:4]),
                group = "group", 
                estimator = "WLSMV")

# View all fit measures
fitmeasures(conf.fit)
fitmeasures(thres.fit)
fitmeasures(load.fit)

# Save some selected fit measures in matrix
all.results <- matrix(NA, ncol = 12, nrow = 3)
colnames(all.results) <- c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.scaled", "cfi.scaled","srmr", "lavtestLRT-pvalue", "diffRMSEA", "diffCFI","diffSRMR", "AIC", "BIC")

# Save fit measures from models
sel <- c("chisq.scaled","df.scaled","pvalue.scaled","rmsea.scaled", "cfi.scaled", "srmr")

# Using "sel" for selected measures for each model
all.results[1,sel] <- round(data.matrix(fitmeasures(conf.fit, fit.measures = sel)), digits=3)
all.results[2,sel] <- round(data.matrix(fitmeasures(thres.fit, fit.measures = sel)), digits=3)
all.results[3,sel] <- round(data.matrix(fitmeasures(load.fit, fit.measures = sel)), digits=3)

# Compute the difference in goodness-of-fit measures
# Difference config-threshold
all.results[2,c("diffRMSEA", "diffCFI", "diffSRMR")] <- all.results[2,c("rmsea.scaled","cfi.scaled","srmr")] - all.results[1,c("rmsea.scaled","cfi.scaled","srmr")]
# Difference threshold-loading
all.results[3,c("diffRMSEA", "diffCFI", "diffSRMR")] <- all.results[3,c("rmsea.scaled","cfi.scaled","srmr")] - all.results[2,c("rmsea.scaled","cfi.scaled","srmr")]

# Compute the likelihood ratio test between the different models and store the p-values
# LRT between config and threshold
all.results[2,"lavtestLRT-pvalue"] <- lavTestLRT(conf.fit,thres.fit)$`Pr(>Chisq)`[2]
# LRT between threshold and loading
all.results[3,"lavtestLRT-pvalue"] <- lavTestLRT(thres.fit,load.fit)$`Pr(>Chisq)`[2] 

# Save the AIC estimate for all models
# Since we do not use the ML estimator, the AIC and BIC are unavailable
all.results[1,"AIC"] <- AIC(conf.fit)
all.results[2,"AIC"] <- AIC(thres.fit)
all.results[3,"AIC"] <- AIC(load.fit)

# Save the BIC estimate for all models
# Since we do not use the ML estimator, the AIC and BIC are unavailable
all.results[1,"BIC"] <- BIC(conf.fit)
all.results[2,"BIC"] <- BIC(thres.fit)
all.results[3,"BIC"] <- BIC(load.fit)

# View model fit results
all.results
