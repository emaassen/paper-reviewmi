rm(list=ls())
library(lavaan)
library(foreign)
library(semTools)
library(readxl)
library(haven)

# load data - this is data for study 2, there is no data available for study 1.
X46_1 <- read_sav("46-1.SAV")

# only factor scores used, not able to construct a measurement model based on groups (they removed
# the grouping variable gender, and do not indicate a cutoff for age (e.g. old and young).