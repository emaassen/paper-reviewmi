rm(list=ls())
library(lavaan)
library(foreign)
library(semTools)
library(readxl)

# Study 1

# load data
df <- read_excel("44-1.xlsx", 1)

t.test(E1Gaze ~ Condition, data = df)
# this means that the final columns in the dataset are the "Attention scale" they measure

# model - higher order factor because sum scores used
model <- ' attention =~ E1Gaze + E2Gaze + ToyGaze + OtherGaze '

# measurement invariance check with function in semTools
measurementInvariance(model = model, data = df, group = "Condition", missing = "ML")

# configural model did not converge



# Study 2
df <- read_excel("44-1.xlsx", 2)

# model - higher order factor because sum scores used
model <- ' attention =~ EGE1 + EGE2 + Egtoy + Other '

# measurement invariance check with function in semTools
measurementInvariance(model = model, data = df, group = "Condition", missing = "ML")

# configural model did not converge
