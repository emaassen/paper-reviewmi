rm(list=ls())
library(lavaan)
library(foreign)
library(semTools)
library(readxl)

# Study 1
# load data - we needed to import the dataset via the menu because the code below does not work.
df <- read.csv(file = '14-1.csv', sep = ";")

df <- data.frame(lapply(df, function(x) { gsub("Strongly disagree", 1, x) }))
df <- data.frame(lapply(df, function(x) { gsub("Disagree", 2, x) }))
df <- data.frame(lapply(df, function(x) { gsub("Neutral", 3, x) }))
df <- data.frame(lapply(df, function(x) { gsub("Agree", 4, x) }))
df <- data.frame(lapply(df, function(x) { gsub("Strongly agree", 5, x) }))

df[,8] <- as.numeric(df[,8])
df[,9] <- as.numeric(df[,9])
df[,10] <- as.numeric(df[,10])
df[,11] <- as.numeric(df[,11])
df[,12] <- as.numeric(df[,12])
df[,13] <- as.numeric(df[,13])
df[,14] <- as.numeric(df[,14])
df[,15] <- as.numeric(df[,15])
df[,16] <- as.numeric(df[,16])
df[,17] <- as.numeric(df[,17])
df[,18] <- as.numeric(df[,18])
df[,19] <- as.numeric(df[,19])
df[,20] <- as.numeric(df[,20])
df[,21] <- as.numeric(df[,21])
df[,22] <- as.numeric(df[,22])
df[,23] <- as.numeric(df[,23])
df[,24] <- as.numeric(df[,24])
df[,25] <- as.numeric(df[,25])
df[,26] <- as.numeric(df[,26])
df[,27] <- as.numeric(df[,27])
df[,28] <- as.numeric(df[,28])
df[,29] <- as.numeric(df[,29])


model1 <- 'IOC =~ DBC_IOC_HotMachine + DBC_IOC_Familiar + DBC_IOC_DiceWin + DBC_IOC_GamblingSystem + DBC_IOC_HeadsTails + DBC_IOC_Secrets + DBC_IOC_LotteryNos + DBC_IOC_Sportsperson + DBC_IOC_BornLucky + DBC_IOC_LongLosing + DBC_IOC_MachineSystem '               

model2 <- 'DBC =~ DBC_Sup_MagicNos + DBC_Sup_Ritual + DBC_Sup_Horoscope + DBC_Sup_PlayerVsMach + DBC_Sup_Fate + DBC_Sup_ChanceGame + DBC_Sup_LuckyPen + DBC_Sup_LuckyClothes + DBC_Sup_Superstitious + DBC_Sup_LuckyCharm + DBC_Sup_LotterySystem '                        


# measurement invariance check with function in semTools
measurementInvariance(model = model1, data = df, group = "SuccessSlopeCond", missing = "ML")

measurementInvariance(model = model2, data = df, group = "SuccessSlopeCond", missing = "ML")


config <- cfa(model1, data=df, group="SuccessSlopeCond") 
summary(config, fit.measures=T)

