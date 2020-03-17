rm(list=ls())
library(lavaan)
library(foreign)
library(semTools)
library(readxl)

# Study 1

# load data
df1 <- read_excel("59-1.xlsx", 1)
df2 <- read_excel("59-1.xlsx", 2)
df <- rbind(df1,df2)
# remove empty rows
df <- df[1:35,]
df <- df[-c(16:20),]
group <- c(rep(1,15),rep(2,15))
df[,1] <- group
colnames(df)[1] <- "group"

colnames(df)[2] <- "mind"
colnames(df)[3] <- "att"
colnames(df)[4] <- "sleep"
colnames(df)[5] <- "fat"
 
model <- 'model =~ mind + att + sleep + fat' 

# measurement invariance check with function in semTools
measurementInvariance(model = model, data = df, group = "group")

# scalar invariance

