rm(list=ls())
library(lavaan)
library(foreign)
library(semTools)

# load data
df = read.spss("13-1.sav", to.data.frame=TRUE)

model <- ' sad =~ sadnovels + sadpaint + sadmovies + crysadmov + sadmusic 
           burn =~ burnmouth + spicyfood + eyetear + tacoshot + sweatburn
           disgust =~ disgjoke + disgexp + pinchpimp + picknose 
           fear =~ poundhrt + frightmov + thrillrides 
           pain =~ massagpain + coldpain + hotpain 
           alc =~ beer + scotch
           exhst =~ exhaust + feelactiv
           bitter =~ bitterfoods + unswtoff '

           
# measurement invariance check with function in semTools
measurementInvariance(model = model, data = df, group = "Mturk", missing = "ML")

# measurement invariance check by restricting step by step
config <- cfa(model, data=df, group="Mturk") 
summary(config, fit.measures=T)

weak <- cfa(model, data=df, group="Mturk", group.equal="loadings") 
summary(weak, fit.measures=T)

strong<- cfa(model, data=df, group="Mturk", group.equal = c("loadings", "intercepts")) 
summary(strong, fit.measures=T)

strict<- cfa(model, data=df, group="Mturk", group.equal = c("loadings", "intercepts", "residuals")) 
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
