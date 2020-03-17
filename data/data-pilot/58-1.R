rm(list=ls())
library(lavaan)
library(foreign)
library(semTools)
library(readxl)
library(haven)

# load data 
df <- as.data.frame(read.spss("58-1.SAV"))

# impossible to compare groups as reported in paper.

