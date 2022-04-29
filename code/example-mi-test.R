### CODE FOR MAIN STUDY SYSTEMATIC REVIEW MEASUREMENT INVARIANCE ##
## This is code to simulate data for a one-factor model and to 
## run a measurement invariance test via MGCFA or MGCCFA

## Please note that the code we used to run MI tests on the data in our sample can be found at 

library(faux)     # to use rnorm_multi function to simulate data
library(lavaan)   # to perform measurement invariance tests
library(semTools) # to construct syntax with thresholds for items for CCFA

# MGCFA example -----------------------------------------------------------

# simulate dataset
n <- 1000                                                                               # sample size
varnames <- c(paste0("item", 1:7))                                                      # variable names
group <- c(rep(0,n/2),rep(1,n/2))                                                       # make grouping variable                
scores <- matrix(NA, nrow = n, ncol = length(varnames))                                 # matrix to store data
set.seed(017889)                                                                        # seed 
scores <- round(rnorm_multi(n = n, mu = 2.5, sd = 1, r = .25, varnames = varnames),0)   # generate data with correlation of .25
colnames(scores) <- varnames                                                            # column names for matrix
scores <- data.frame(scores)                                                            # make dataframe out of matrix
scores <- cbind(scores,group)                                                           # combine scores and grouping variable into dataframe

# construct measurement model (one factor with seven items)
model <- 'F =~ item1 + item2 + item3 + item4 + item5 + item6 + item7'

# fit configural invariance model
conf.fit <- cfa(model = model, data = scores, group = "group")

# fit metric (loading) invariance model
metric.fit <- cfa(model = model, data = scores, group = "group", group.equal = "loadings")

# fit scalar (intercept) invariance model
scalar.fit <- cfa(model = model, data = scores, group = "group", group.equal = c("loadings", "intercepts"))

# view all fit measures from the models:
fitmeasures(conf.fit)
fitmeasures(metric.fit)
fitmeasures(scalar.fit)

# save some selected fit measures in matrix
all.results <- matrix(NA, ncol = 11, nrow = 3)
colnames(all.results) <- c("chisq","df","pvalue", "rmsea", "cfi", "srmr","diffRMSEA", "diffCFI", "lavtestLRT-pvalue", "AIC", "BIC")

# save fit measures from models
all.results[1,1:6] <- round(data.matrix(fitmeasures(conf.fit, fit.measures = c("chisq","df","pvalue","rmsea", "cfi", "srmr"))), digits=3)
all.results[2,1:6] <- round(data.matrix(fitmeasures(metric.fit, fit.measures = c("chisq","df","pvalue","rmsea", "cfi", "srmr"))), digits=3)
all.results[3,1:6] <- round(data.matrix(fitmeasures(scalar.fit, fit.measures = c("chisq","df","pvalue","rmsea", "cfi", "srmr"))), digits=3)

# compute the difference in goodness-of-fit measures
all.results[2,7:8] <- all.results[2,4:5] - all.results[1,4:5] # difference config-metric
all.results[3,7:8] <- all.results[3,4:5] - all.results[2,4:5] # difference metric-scalar
  
# Compute the likelihood ratio test between the different models and store the p-values
all.results[2,9] <- lavTestLRT(conf.fit,metric.fit)$`Pr(>Chisq)`[2] # LRT between config and metric
all.results[3,9] <- lavTestLRT(metric.fit,scalar.fit)$`Pr(>Chisq)`[2] # LRT between metric and scalar

# Save the AIC estimate for all models
all.results[1,10] <- AIC(conf.fit)
all.results[2,10] <- AIC(metric.fit)
all.results[3,10] <- AIC(scalar.fit)

# Save the BIC estimate for all models
all.results[1,11] <- BIC(conf.fit)
all.results[2,11] <- BIC(metric.fit)
all.results[3,11] <- BIC(scalar.fit)

# view model fit results
all.results


# MGCCFA example ----------------------------------------------------------

# we performed MG-CCFA (multigroup categorical confirmatory factor analysis) for ordinal data
# in these cases, the syntax for the measurement model is different because thresholds are used for each item
# we used the constraints reported by Wu & Estabrook (https://doi.org/10.1007/s11336-016-9506-0) and the WLSMV estimator
# the steps tested here are configural - thresholds - loadings. 

# simulate dataset
n <- 1000                                                                               # sample size
varnames <- c(paste0("item", 1:4))                                                      # variable names
group <- c(rep(0,n/2),rep(1,n/2))                                                       # make grouping variable                
scores <- matrix(NA, nrow = n, ncol = length(varnames))                                 # matrix to store data
set.seed(017889)                                                                        # seed 
scores <- round(rnorm_multi(n = n, mu = 2.5, sd = 0.5, r = .25, varnames = varnames),0) # generate data with correlation of .25
colnames(scores) <- varnames                                                            # column names for matrix
scores <- data.frame(scores)                                                            # make dataframe out of matrix
scores <- cbind(scores,group)                                                           # combine scores and grouping variable into dataframe

# construct measurement model (one factor with four items)
model <- 'F =~ item1 + item2 + item3 + item4'

# syntax configural model
syntax.config <- measEq.syntax(model, 
                               ID.fac = "std.lv", 
                               ID.cat = "Wu",
                               ordered = colnames(scores[,1:4]),
                               parameterization = "delta",
                               data = scores,
                               group = "group", 
                               group.equal = "configural", 
                               orthogonal = T)

# fit configural invariance model
conf.fit <- cfa(as.character(syntax.config),
                data = scores, 
                ordered = colnames(scores[,1:4]),
                group = "group", 
                estimator = "WLSMV")

# syntax threshold model
syntax.thres <- measEq.syntax(model, 
                              ID.fac = "std.lv", 
                              ID.cat = "Wu",
                              ordered = colnames(scores[,1:4]),
                              parameterization = "delta",
                              data = scores,
                              group = "group", 
                              group.equal = "thresholds", 
                              orthogonal = T)

# fit threshold invariance model
thres.fit <- cfa(as.character(syntax.thres),
                 data = scores, 
                 ordered = colnames(scores[,1:4]),
                 group = "group", 
                 estimator = "WLSMV")

# syntax loading invariance model
syntax.load <- measEq.syntax(model, 
                             ID.fac = "std.lv", 
                             ID.cat = "Wu",
                             ordered = colnames(scores[,1:4]),
                             parameterization = "delta",
                             data = scores,
                             group = "group", 
                             group.equal = c("thresholds","loadings"), 
                             orthogonal = T)

# fit loading invariance model
load.fit <- cfa(as.character(syntax.load),
                data= scores, 
                ordered = colnames(scores[,1:4]),
                group = "group", 
                estimator = "WLSMV")

# view all fit measures from the models:
fitmeasures(conf.fit)
fitmeasures(thres.fit)
fitmeasures(load.fit)

# save some selected fit measures in matrix
all.results <- matrix(NA, ncol = 11, nrow = 3)
colnames(all.results) <- c("chisq.sc","df.sc","pvalue.sc", "rmsea.sc", "cfi.sc", "srmr.sc", "diffRMSEA", "diffCFI", "lavtestLRT-pvalue", "AIC", "BIC")

# save fit measures from models
# note that because we deal with ordinal data, we save the scaled fit measures instead of the "original" ones
all.results[1,1:6] <- round(data.matrix(fitmeasures(conf.fit, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled","rmsea.scaled", "cfi.scaled", "srmr"))), digits=3)
all.results[2,1:6] <- round(data.matrix(fitmeasures(thres.fit, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled","rmsea.scaled", "cfi.scaled", "srmr"))), digits=3)
all.results[3,1:6] <- round(data.matrix(fitmeasures(load.fit, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled","rmsea.scaled", "cfi.scaled", "srmr"))), digits=3)

# compute the difference in goodness-of-fit measures
all.results[2,7:8] <- all.results[2,4:5] - all.results[1,4:5] # difference config-threshold
all.results[3,7:8] <- all.results[3,4:5] - all.results[2,4:5] # difference threshold-loading

# Compute the likelihood ratio test between the different models and store the p-values
all.results[2,9] <- lavTestLRT(conf.fit,thres.fit)$`Pr(>Chisq)`[2] # LRT between config and threshold
all.results[3,9] <- lavTestLRT(thres.fit,load.fit)$`Pr(>Chisq)`[2] # LRT between theshold and loading

# View model fit results
# Since we do not use the ML estimator, the AIC and BIC are unavailable
all.results