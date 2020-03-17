rm(list=ls())
library(lavaan)
library(foreign)
library(semTools)
library(readxl)

# load data - we needed to import the dataset via the menu because the code below does not work.
#df = read.table("Table_S1_edit.xlsx", sep = "", header=TRUE)

# model - higher order factor because sum scores used
model <- ' NEO =~ neo-neuro + neo-open + neo-extra + neo-agere + neo-conscient '

# measurement invariance check with function in semTools
measurementInvariance(model = model, data = Table_S1_edit, group = "gender", missing = "ML")
measurementInvariance(model = model, data = Table_S1_edit, group = "group", missing = "ML")

# both configural models did not converge
