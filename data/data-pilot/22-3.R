rm(list=ls())
library(lavaan)
library(foreign)
library(semTools)

# load data
df = read.spss("Study3_warning.sav", to.data.frame=TRUE)

df1 <- subset(df, warning == 0)
df2 <- subset(df, warning == 1)
mean(df1$FAhistory);mean(df2$FAhistory)

t.test(df$FA~df$warning)

model <- ' FAA =~ FAhistory + FAphilosophy + FAbiology '

# measurement invariance check with function in semTools
MI <- measurementInvariance(model = model, data = df, group = "warning", missing = "ML")
summary(MI)


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
