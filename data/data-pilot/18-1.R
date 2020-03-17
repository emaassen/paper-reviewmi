rm(list=ls())
library(lavaan)
library(foreign)
library(semTools)
library(readxl)
library(dplyr)

# Study 1

# load data
df <- read.csv(file = '18-1.csv', sep = ";")

# recode age into groups
df$years_old[df$years_old==7] <- 1 # this one is not categorized as 1 in the paper, but we decided to.
df$years_old[df$years_old==8] <- 1
df$years_old[df$years_old==9] <- 1
df$years_old[df$years_old==10] <- 2
df$years_old[df$years_old==11] <- 2
df$years_old[df$years_old==12] <- 3
df$years_old[df$years_old==13] <- 3
df$years_old[df$years_old==14] <- 4
df$years_old[df$years_old==15] <- 4
df$years_old[df$years_old==16] <- 5
df$years_old[df$years_old==17] <- 5

unique(df$years_old)

colnames(df[114]);colnames(df[157]) # P variables
df[114:157] <- sapply(df[114:157],as.numeric) # Recode P variables to numeric
colnames(df[219]);colnames(df[233]) # P recoded variables
df[219:233] <- sapply(df[219:233],as.numeric) # Recode P recoded variables to numeric

model1 <- 'Control =~ P1 + P19R + P22R + P25 + P38 + P40R + P4R + P10R + P16 + P23 + P32R + P6R + P7 + P14R + P27 + P42'


model2 <- 'Surgency =~ P2R + P11R + P15R + P26 + P31 + P34 + P5 + P8 + P28 + P35R + P17 + P20 + P24 + P29 + P33 + P37'


# measurement invariance check with function in semTools
measurementInvariance(model = model1, data = df, group = "years_old", missing = "ML")
measurementInvariance(model = model2, data = df, group = "years_old", missing = "ML")

#config <- cfa(model1, data=df, group="years_old", ordered=T) 
#summary(config, fit.measures=T)

# group sizes
sum(df$years_old == 1)
sum(df$years_old == 2)
sum(df$years_old == 3)
sum(df$years_old == 4)
sum(df$years_old == 5)