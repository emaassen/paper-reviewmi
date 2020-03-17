rm(list=ls())
library(lavaan)
library(foreign)
library(semTools)
library(readxl)

# Study 1

# load data 
df <- read_excel("48-1.xlsx", 1)

# the paper uses 4 tests (trier, CP...) and 4 timepoints for each (.1, .2 etc.)
# we do not combine the 4 tests (e.g. trier1 with cp1), because the authors don't and it wouldn't make sense
# and making a factor for all timepoints together (e.g. trier = trier.1, trier.2 etc.) also isn't done in the paper.

model <- 'trier1 =~ Tension.Trier.1 + Depression.Trier.1 + Anger.Trier.1 + Vigor.Trier.1 + Fatigue.Trier.1 + Confusion.Trier.1' 

# there is no logical grouping variable, and all participants answer all 4 questionnaires / are in all 4 conditions (Trier, CP, Math and Control)
# so there is no logical way to compare groups.
# also, the scores seem to be centered, instead of item scores?

# measurement invariance check with function in semTools
measurementInvariance(model = model, data = df, group = "Condition", missing = "ML")
# no way to group groups so no MI test.
