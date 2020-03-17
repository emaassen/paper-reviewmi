rm(list=ls())
library(lavaan)
library(foreign)
library(semTools)
library(readxl)
library(haven)

# load data - this is data for study 2, there is no data available for study 1.
df <- read_sav("57-1.SAV")

emque <- 'emp =~ emp1 + emp2 + emp3 + emp4 + emp5 + emp6 + emp7 + emp8 + emp9 + emp10 + emp11 + emp12 + emp13 + emp14 + emp15 + emp16 + emp17 + emp18 + emp19 + emp20 + emp21 + emp22 + emp23 + emp24 + emp25 + emp26 + emp27 + emp28 + emp29 + emp30 + emp31 + emp32 + emp33 + emp34 + emp35 + emp36'
eaq <- 'eaq =~ eaq1 + eaq2 + eaq3 + eaq4 + eaq5 + eaq6 + eaq7 + eaq8 + eaq9 + eaq10 + eaq11 + eaq12 + eaq13 + eaq14 + eaq15 + eaq16 + eaq17 + eaq18 + eaq19 + eaq20 + eaq21 + eaq22 + eaq23 + eaq24 + eaq25 + eaq26 + eaq27 + eaq28 + eaq29 + eaq30'
eeq <- 'eeq =~ eeq1 + eeq2 + eeq3 + eeq4 + eeq5 + eeq6 + eeq7 + eeq8 + eeq9 + eeq10 + eeq11 + eeq12 + eeq13 + eeq14 + eeq15 + eeq16 + eeq17 + eeq18 + eeq19 + eeq20 + eeq21 + eeq22 + eeq23 + eeq24 + eeq25 + eeq26 + eeq27 + eeq28 + eeq29 + eeq30 + eeq31 + eeq32 + eeq33 + eeq34 + eeq35'

diag <- c(rep(1,122),rep(2,162))
df$diag <- diag

# measurement invariance check with function in semTools
measurementInvariance(model = emque, data = df, group = "diag")
measurementInvariance(model = eaq, data = df, group = "diag")
measurementInvariance(model = eeq, data = df, group = "diag")
