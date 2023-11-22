### THE DIRE DISREGARD OF MEASUREMENT INVARIANCE TESTING ###
### This is code to sample articles for the pilot study

# Setup -------------------------------------------------------------------
# Clear workspace
#rm(list=ls())

# No scientific notation
options(scipen=999)

# Sampling studies --------------------------------------------------------
# We will take a random sample of 20 articles from the JDM article set (total k = 223) for the pilot study.
# We will take a random sample of 20 articles from the PS article set (total k = 144) for the pilot study.
# We will take a random sample of 20 articles from the PlosONE article set (total k = 252) for the pilot study.
k.jdm <- 1:223
k.ps <- 1:144
k.plos <- 1:252

set.seed(0904)
pilot.jdm <- sample(k.jdm,20)

set.seed(1708)
pilot.ps <- sample(k.ps,20)

set.seed(09041708)
pilot.plos <- sample(k.plos,20)

pilot.jdm <- sort(pilot.jdm)
pilot.ps <- sort(pilot.ps)
pilot.plos <- sort(pilot.plos)

# Selected articles for pilot study
pilot.jdm
pilot.ps
pilot.plos

# These numbers represent the articles we sampled from our existing index and included in our pilot data codebook.

# Reproducibility code ----------------------------------------------------
# This is example code we used to read in datasets, construct measurement models, and perform measurement invariance checks

# make model + perform measurement invariance tests
library(lavaan)
# load spss data
library(foreign)
# load spss data 
library(haven)
# measurement invariance tests   
library(semTools) 

# Load data
df <- read.csv(file = '18-1.csv', sep = ";")
df <- read.table("29-1.xlsx", sep = "", header=TRUE)
df <- read_excel("29-1.xlsx")

# foreign package
df <- read.spss("13-1.sav", to.data.frame=TRUE) 

# haven package
df <- read_spss("23-1.sav") 
df <- read_sav("57-1.SAV")

# Recode data
df <- data.frame(lapply(df, function(x) { gsub("Strongly disagree", 1, x) }))

df[,8] <- as.numeric(df[,8])

df$years_old[df$years_old==10] <- 2

df[114:157] <- sapply(df[114:157],as.numeric) 

df1 <- subset(df, warning == 0)

# Make a model
model <- ' f1 =~ PassiveRis_1 + PassiveRis_2 + PassiveRis_3 + PassiveRis_4 + 
                  PassiveRis_5 + PassiveRis_6 + PassiveRis_7 + PassiveRis_8 +
                  PassiveRis_9 + PassiveRis_10 + PassiveRis_11 + PassiveRis_12
                  
           f2 =~ PassiveRis_13 + PassiveRis_14 + PassiveRis_15 + PassiveRis_16 +
                 PassiveRis_17 + PassiveRis_18 + PassiveRis_19 
                 
           f3 =~ PassiveRis_20 + PassiveRis_21 + PassiveRis_22 + PassiveRis_23 +
                  PassiveRis_24 + PassiveRis_25

           f1 ~~ 0*f2
           f1 ~~ 0*f3
           f2 ~~ 0*f3'

# Measurement invariance check with function in semTools
measurementInvariance(model = model, data = df, group = "Mturk", missing = "ML")

# Measurement invariance check by restricting step by step
config <- cfa(model, data=df, group="Mturk") 
summary(config, fit.measures=T)

weak <- cfa(model, data=df, group="Mturk", group.equal="loadings") 
summary(weak, fit.measures=T)

strong<- cfa(model, data=df, group="Mturk", group.equal = c("loadings", "intercepts")) 
summary(strong, fit.measures=T)

strict<- cfa(model, data=df, group="Mturk", group.equal = c("loadings", "intercepts", "residuals")) 
summary(strict, fit.measures=T)

anova(config,weak,strong,strict)

# Fit measures
fitmeasures(config,c("rmsea","cfi","tli"))
fitmeasures(weak,c("rmsea","cfi","tli"))
fitmeasures(strong,c("rmsea","cfi","tli"))
fitmeasures(strict,c("rmsea","cfi","tli"))
