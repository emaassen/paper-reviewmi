### CODE FOR MAIN STUDY SYSTEMATIC REVIEW MEASUREMENT INVARIANCE ##
### This is code to sample articles for which we will attempt to perform measurement invariance checks

rm(list=ls())

# packages
library("readxl")
library("lavaan")
library("writexl")

# function to check how many groups are compared for each study
assign.groups <- function(x) {
  if(is.na(x['n3_rep'])) {
    no_group <- 2
  } else if(is.na(x['n4_rep'])) {
    no_group <- 3
  } else if(is.na(x['n5_rep'])) {
    no_group <- 4 
  } else {
    no_group <- 5
  }
  return(no_group)
}

# load data from main study
df <- read_excel('../data/codebook-main-reporting.xlsx')

# give all data points a unique id so we can delete doubles later
df$id <- 1:nrow(df)

# filter out studies that reported on doing a measurement invariance check
df$mitest_rep <- as.numeric(df$mitest_rep)
df.checked <- subset(df,mitest_rep == 1)

# check distribution of reliabilities of all studies
df$reltot <- as.numeric(df$reltot)
hist(as.numeric(df$reltot))
mean(df$reltot, na.rm=T)

# check distribution of number of items of all studies
df$no_items <- as.numeric(df$no_items)
hist(df$no_items)

# select only studies that report reliability and number of items
dfs <- df[!is.na(df$reltot) & df$no_items != "NA" & !is.na(df$no_items),]

# some studies have sample sizes for groups but not the total one. 
# we will assign n_rep = sum of all the group sample sizes

# calculate inter item correlation for all studies
dfs$iic <- -(dfs$reltot / (dfs$no_items * dfs$reltot - dfs$reltot - dfs$no_items))
dfs <- as.data.frame(dfs)

# how many groups are compared for each row?
dfs$no_group <- apply(dfs,1,assign.groups)

# simulate factor model and check power to detect measurement non-invariance in the intercepts
for (i in 1:nrow(dfs)) {
  
  # simulate one-factor model with specific parameter values
  k <- dfs$no_group[i]
  
  # extract sample size for each level of k groups
  if (k == 2) {
    
    nobs <- as.numeric(c(dfs$n1_rep[i],dfs$n2_rep[i]))
    
  } else if (k == 3) {
    
    nobs <- as.numeric(c(dfs$n1_rep[i],dfs$n2_rep[i],dfs$n3_rep[i]))
    
  } else if (k == 4) {
    
    nobs <- as.numeric(c(dfs$n1_rep[i],dfs$n2_rep[i],dfs$n3_rep[i],dfs$n4_rep[i]))
    
  } else if (k == 5) {
    
    nobs <- as.numeric(c(dfs$n1_rep[i],dfs$n2_rep[i],dfs$n3_rep[i],dfs$n4_rep[i],dfs$n5_rep[i]))
    
  }
  
  # if no group sample sizes reported, take the total sample size and divide it by the number of groups
  if (is.na(nobs[1])) {
    
    nobs <- round(rep(as.numeric(dfs$n_rep[i])/k,k))

  }
  
  # number of items
  q <- dfs$no_items[i]
  
  # factor loadings (square root of the inter-item correlation)
  lambdas <- rep(sqrt(dfs$iic[i]),q)
  lambda <- matrix(c(lambdas), nrow = q, ncol = 1) 
  
  # error variances (1 - lambda^2)
  errorvar <- 1 - lambdas[1:q] * lambdas[1:q]  
  theta <- matrix(0,nrow=q,ncol=q)
  diag(theta) <- errorvar
  
  # latent variance
  phi <- 1 
  
  # assign values to item intercepts 
  # (50% of groups gets intercept difference of 0.5 in all items, see line 182 for more info)
  if (k == 2) {
    
    nu <- c(0,0.5)
    
  } else if (k == 3) {
    
    nu <- c(0,0,0.5)
    
  } else if (k == 4) {
    
    nu <- c(0,0,0.5,0.5)
    
  } else if (k == 5) {
    
    nu <- c(0,0,0,0.5,0.5)
    
  }
  
  # latent mean estimate
  alpha <- rep(0,k)
  
  # column names for covariance matrices and mean vectors (needed to estimate in lavaan)
  colnms <- NA
  for(z in 1:q){
    colnms[z] <- paste0('V', z)
  }
  
  # construct covariance matrices and mean vectors for each level of k
  if (k == 2) {
    
    mu.pop.1 <- nu[1] + lambda %*% alpha[1]									                   
    mu.pop.2 <- nu[2] + lambda %*% alpha[2]
    mu.pop <- list(mu.pop.1,mu.pop.2)
    
    sigma.pop.1 <- lambda %*% phi %*% t(lambda) + theta
    colnames(sigma.pop.1) <- colnms
    sigma.pop <- list(sigma.pop.1,sigma.pop.1)
    
    
  } else if (k == 3) {
    
    mu.pop.1 <- nu[1] + lambda %*% alpha[1]									                   									                   
    mu.pop.2 <- nu[2] + lambda %*% alpha[2]
    mu.pop.3 <- nu[3] + lambda %*% alpha[3]
    mu.pop <- list(mu.pop.1,mu.pop.2,mu.pop.3)
    
    sigma.pop.1 <- lambda %*% phi %*% t(lambda) + theta
    colnames(sigma.pop.1) <- colnms
    sigma.pop <- list(sigma.pop.1,sigma.pop.1,sigma.pop.1)
    
  } else if (k == 4) {
    
    mu.pop.1 <- nu[1] + lambda %*% alpha[1]									                   
    mu.pop.2 <- nu[2] + lambda %*% alpha[2]
    mu.pop.3 <- nu[3] + lambda %*% alpha[3]
    mu.pop.4 <- nu[4] + lambda %*% alpha[4]
    mu.pop <- list(mu.pop.1,mu.pop.2,mu.pop.3,mu.pop.4)
    
    sigma.pop.1 <- lambda %*% phi %*% t(lambda) + theta
    colnames(sigma.pop.1) <- colnms
    sigma.pop <- list(sigma.pop.1,sigma.pop.1,sigma.pop.1,sigma.pop.1)
    
  } else if (k == 5) {
    
    mu.pop.1 <- nu[1] + lambda %*% alpha[1]									                   
    mu.pop.2 <- nu[2] + lambda %*% alpha[2]
    mu.pop.3 <- nu[3] + lambda %*% alpha[3]
    mu.pop.4 <- nu[4] + lambda %*% alpha[4]
    mu.pop.5 <- nu[5] + lambda %*% alpha[5]
    mu.pop <- list(mu.pop.1,mu.pop.2,mu.pop.3,mu.pop.4,mu.pop.5)
    
    sigma.pop.1 <- lambda %*% phi %*% t(lambda) + theta
    colnames(sigma.pop.1) <- colnms
    sigma.pop <- list(sigma.pop.1,sigma.pop.1,sigma.pop.1,sigma.pop.1,sigma.pop.1)
    
  }
  
  # we assigned bias in all indicators for 50% of the groups, but we only need bias in 1/3 of those indicators
  # here we assign an intercept of 0 (no bias) to 2/3 of the indicators that were 0.50. 
  for(x in 2:k){
    mu.pop[[x]][((round(q/3))+1):q] <- 0
  }
  
  # construct model syntax for lavaan to analyze
  mods.metric <- mods.scalar <- NA

  for(l in 1:q){
    mods.metric[l] <- c(paste0("F1 =~", 'V', l))
    mods.scalar[l] <- c(paste0("F1 =~", 'V', l))
    
  }

  mods.metric[q+1] <- paste0("F1~ c(", paste0(rep(0,k), sep= "",collapse = ","), ")*1")
  mods.scalar[q+1] <- paste0("F1~ c(0,", paste0(rep(NA,k-1), sep= "",collapse = ","), ")*1")
  
  # Fit metric (loadings equal across groups) and scalar invariance (loadings and intercepts equal across groups) models
  mod.metric <- cfa(mods.metric, sample.cov = sigma.pop, sample.mean = mu.pop, sample.nobs = nobs, std.lv=T, group.equal = c("loadings"))
  mod.scalar <- cfa(mods.scalar, sample.cov = sigma.pop, sample.mean = mu.pop, sample.nobs = nobs, std.lv=T, group.equal = c("loadings","intercepts"))
  
  # likelihood ratio test between the models
  mod <- anova(mod.metric,mod.scalar)

  # extract chisquare difference between the two models as NCP + extract degrees of freedom
  ncp <- mod$`Chisq diff`[2]
  dof <- mod$`Df`[2] - mod$`Df`[1]
  
  # power estimate given degrees of freedom and NCP
  # indicates power to detect intercept non-invariance in 1/3 of the indicators for 1/2 of the groups
  power.cfa.th <- 1 - pchisq(qchisq(.95, df = dof), df = dof, ncp = ncp) 
  dfs$powerncp[i] <- as.numeric(power.cfa.th)
}

# select only those studies that have a power to detect non-invariance of > 0.80
df.select <- dfs[dfs$powerncp > 0.80,1:36]

# merge the dataset of selected studies with the dataset of studies that reported on doing measurement invariance checks
df.rep <- rbind(df.checked,df.select)

# IDs 178, 868 and 869 are duplicated, remove them
df.rep <- df.rep[!duplicated(df.rep$id), ] 

# save final dataset of studies that we will attempt to reproduce
write_xlsx(df.rep, "../data/codebook-main-reproducibility.xlsx")
