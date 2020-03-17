rm(list=ls())
library(lavaan)
library(psych)
library(semTools)
library(semPlot)

# load data
# experiment 2 timepoint 1
df1 <- read.csv("exp2part1.csv", sep = ";")

# experiment 2 timepoint 2
df2 <- read.csv("exp2part2.csv", sep = ";")

# delete variables that do not occur in both datasets
df1 <- subset(df1[,1:26])
  
# create group variable in both datasets
df1[,27] <- 1
df2[,27] <- 2
df <- rbind(df1,df2);df

# make a model
model <- ' f1 =~ PassiveRis_1 + PassiveRis_2 + PassiveRis_3 + PassiveRis_4 + 
                  PassiveRis_5 + PassiveRis_6 + PassiveRis_7 + PassiveRis_8 +
                  PassiveRis_9 + PassiveRis_10 + PassiveRis_11 + PassiveRis_12
                  
           f2 =~ PassiveRis_13 + PassiveRis_14 + PassiveRis_15 + PassiveRis_16 +
                 PassiveRis_17 + PassiveRis_18 + PassiveRis_19 
                 
           f3 =~ PassiveRis_20 + PassiveRis_21 + PassiveRis_22 + PassiveRis_23 +
                  PassiveRis_24 + PassiveRis_25

           f1 ~~ 0*f2
           f1 ~~ 0*f3
           f2 ~~ 0*f3'

# measurement invariance check with function in semTools
measurementInvariance(model = model, data = df, group = "V27")

# measurement invariance check by restricting step by step
config <- cfa(model, data=df, group="V27") 
summary(config, fit.measures=T)

weak <- cfa(model, data=df, group="V27", group.equal="loadings") 
summary(weak, fit.measures=T)

strong<- cfa(model, data=df, group="V27", group.equal = c("loadings", "intercepts")) 
summary(strong, fit.measures=T)

strict<- cfa(model, data=df, group="V27", group.equal = c("loadings", "intercepts", "residuals")) 
summary(strict, fit.measures=T)

anova(config,weak,strong,strict)

fitmeasures(config,c("rmsea","cfi","tli"))
fitmeasures(weak,c("rmsea","cfi","tli"))
fitmeasures(strong,c("rmsea","cfi","tli"))
fitmeasures(strict,c("rmsea","cfi","tli"))

layout(t(1:2))
semPaths(config)
semPaths(weak, "mod", "est")
semPaths(strong)
semPaths(strict)
