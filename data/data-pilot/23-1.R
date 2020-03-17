rm(list=ls())
library(lavaan)
library(foreign)
library(semTools)
library(readxl)
library(haven)

# load data
df = read_spss("23-1.sav")

model <- ' SS =~ isel12tot '

# measurement invariance check with function in semTools
measurementInvariance(model = model, data = df, group = "sex", missing = "ML")

# measurement invariance check by restricting step by step
config <- cfa(model, data=df, group="warning") 
summary(config, fit.measures=T)

weak <- cfa(model, data=df, group="warning", group.equal="loadings") 
summary(weak, fit.measures=T)

strong<- cfa(model, data=df, group="warning", group.equal = c("loadings", "intercepts")) 
summary(strong, fit.measures=T)

strict<- cfa(model, data=df, group="warning", group.equal = c("loadings", "intercepts", "residuals")) 
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
