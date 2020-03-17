rm(list=ls())
library(lavaan)
library(foreign)
library(semTools)
library(readxl)

# load data
# we edited the original data (named 42-1.xlsx); we removed the top row, assigned variable names
# in all columns and deleted the text below in the original xlsx file.
df <- read_excel("42-1-edit.xlsx")

# model - higher order factor because sum scores used
model <- ' NEO =~ neoneuro + neoopen + neoextra + neoagere + neoconscient '

# measurement invariance check with function in semTools
measurementInvariance(model = model, data = df, group = "gender", missing = "ML")
measurementInvariance(model = model, data = df, group = "group", missing = "ML")

# final model does not converge.