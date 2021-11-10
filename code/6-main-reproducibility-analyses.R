### CODE FOR MAIN STUDY SYSTEMATIC REVIEW MEASUREMENT INVARIANCE ##
### This is code to perform reproducibility checks in our main study

rm(list = ls()) # clear workspace
require("httr") # to load data from OSF into R
require("haven") # to load sav files into R
require("readxl") # to load xlsx files into R

#require("httr")
#url <- 'https://osf.io/gdr4q//?action=download'
#filename <- 'osf_dataframe.csv'
#GET(url, write_disk(filename, overwrite = TRUE))
#data <- read.csv2(filename, header=TRUE, na.strings="NA")

#url <- 'https://osf.io/3qda9/?action=download'
#filename <- '../data/data-main/article22.sav'
#GET(url, write_disk(filename, overwrite = TRUE))
#article22 <- read_sav(filename)


# Article 22: Moreira -------------------------------------------------------------------------
# The data shared are results from MI checks, but no raw data: https://doi.org/10.1371/journal.pone.0204012.s001
# Correlations between composite variables (i.e. sum scores) are given but no correlations between separate items
# We cannot do a MI test with this data, we cannot make grouping variables or constructs
url <- 'https://doi.org/10.1371/journal.pone.0204012.s001'
filename <- '../data/data-main/article22.xlsx'
GET(url, write_disk(filename, overwrite = TRUE))
article22 <- read_excel(filename)


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
filename <- '../data/data-main/article207.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article207 <- read_sav(filename)

# comparison 1:
# variables for scale: PAN01 until PAN20
# grouping variable for id 868: education (variable name: curso: primero ESO & segundo ESO)
# labels: primary -10 to 12 years old- versus secondary students -13 to 15 years old


# comparison 2:
# variables for scale: PAN01 until PAN20
# grouping variable for id 869: gender (variable name: Género)
# labels: males versus females


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


# Article 710: Protzko ------------------------------------------------------------------------
# Data shared is raw data in .sav file: https://osf.io/x7c8w/ 
url <- 'https://osf.io/x7c8w//?action=download'
filename <- '../data/data-main/article710.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article710 <- read_sav(filename)

# variables for scale: Psd1, sd2R, sd3, sd4R, sd5, sd6R, s7R, sd8R, sd9, sd10 
# grouping variable: fast 


# Article 711: Obaidi -------------------------------------------------------------------------
# The supplemental material (.pdf) reports measurement invariance checks in test:
# https://journals.sagepub.com/doi/suppl/10.1177/0956797619834879/suppl_file/ObaidiSupplementalMaterial.pdf
# Correlations are reported between factors but not between individual items
# We cannot do a MI test because we have no data


# Article 9: Leitner --------------------------------------------------------------------------
# Data shared is raw data in .sav file, including .sps code file: https://osf.io/yxvuk/files/
# study 1; 3 comparisons
url <- 'https://osf.io/mzyh5//?action=download'
filename <- '../data/data-main/article7_1.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article7_1 <- read_sav(filename)

# comparison 1: this is probably a different dataset!
# variables for scale: 
# grouping: confederate (black / white)

# comparison 2: 
# variables for scale: post negative affect (DV_sad, DV_hopeless, DV_Discouraged, DV_angry, DV_resentful, DV_annoyed, DV_fatigued, DV_wornout, DV_exhausted, DV_vigor, DV_lively, DV_cheer)
# grouping: C_CnCI (low and high self-disclosure)

# comparison 3: 
# variables for scale: post negative affect (DV_sad, DV_hopeless, DV_Discouraged, DV_angry, DV_resentful, DV_annoyed, DV_fatigued, DV_wornout, DV_exhausted, DV_vigor, DV_lively, DV_cheer)
# grouping: confederate (black / white)

# study 2; 2 comparisons
url <- 'https://osf.io/g85jp//?action=download'
filename <- '../data/data-main/article7_2.sav'
GET(url, write_disk(filename, overwrite = TRUE))
article7_2 <- read_sav(filename)

# comparison 4: 
# variables for scale: negative affect after speech but before feedback 
# POMS_prefdbk_hopeless,POMS_prefdbk_discouraged, POMS_prefdbk_angry, POMS_prefdbk_resent, POMS_prefdbk_annyd, POMS_prefdbk_fatigued, POMS_prefdbk_wornout, POMS_prefdbk_exhauste, POMS_vigor_prefdbk_R, POMS_lively_prefdbk_R, POMS_cheer_prefdbk_R
# grouping: confederate C_CnCI (low and high self-disclosure)

# comparison 5: 
# variables for scale: negative affect after speech but before feedback 
# POMS_prefdbk_hopeless,POMS_prefdbk_discouraged, POMS_prefdbk_angry, POMS_prefdbk_resent, POMS_prefdbk_annyd, POMS_prefdbk_fatigued, POMS_prefdbk_wornout, POMS_prefdbk_exhauste, POMS_vigor_prefdbk_R, POMS_lively_prefdbk_R, POMS_cheer_prefdbk_R
# grouping: confederate confederate (black / white)


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
filename <- '../data/data-main/article56.xlxs'
GET(url, write_disk(filename, overwrite = TRUE))
article56 <- read_excel(filename)

# comparison 1:
# variables for scale: EIC1, EIC2, EIC3, EIC4, EIC5, EIC6, EIC7, EIC8, EIC9
# grouping: Gender

# comparison 2:
# variables for scale: EBEI1, EBEI2, EBEI3, EBEI4, EBEI5, EBEI6, EBEI7, EBEI8, EBEI9, EBEI10
# grouping: Gender

# comparison 3:s
# variables for scale: ECOVP1, ECOVP2, ECOVP3, ECOVP4, ECOVP5, ECOVP6, ECOVP7, ECOVP8, ECOVP9, ECOVP10,
# ECOVP11, ECOVP12, ECOVP13, ECOVP14, ECOVP15, ECOVP16, ECOVP17, ECOVP18, ECOVP19, ECOVP20, ECOVP21, 
# ECOVP22, ECOVP23, ECOVP24, ECOVP25
# grouping: Gender



# Article 74: Cardoso -------------------------------------------------------------------------
url <- 'https://doi.org/10.1371/journal.pone.0205352.s002'
filename <- '../data/data-main/article74.csv'
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
filename <- '../data/data-main/article131.xlsx'
GET(url, write_disk(filename, overwrite = TRUE))
article131 <- read_excel(filename)

# comparison 1:
# variables for scale: Eq5Dmob, Eq5Dselfc, Eq5Duact, Eq5Dpain, Eq5Dmood
# grouping: depression

# comparison 2:
# variables for scale: Eq5Dmob, Eq5Dselfc, Eq5Duact, Eq5Dpain, Eq5Dmood
# grouping: distress


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



