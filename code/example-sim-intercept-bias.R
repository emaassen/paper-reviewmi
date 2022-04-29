### CODE FOR MAIN STUDY SYSTEMATIC REVIEW MEASUREMENT INVARIANCE ##
## This is code to simulate intercept bias of 0.5 in 1/3 of items and 1/2 of groups,
## based on a reliability estimate, number of items, number of groups, and sample size

## Please note that the code we used on our real dataset can be found at https://osf.io/fju7n/

require("lavaan")    # to simulate factor model and run MI tests
require("truncnorm") # to simulate data via rnorm that is truncated between values (for no_group)

# simulate 50 scale mean comparisons that each have: 
# a reliability estimate for the scale, a number of items in the scale, and number of groups that are compared
# we limit the number of groups from 2 to 5
# we assume the sample size of all groups within one comparison is the same

n <- 50                                                                 # number of comparisons
set.seed(017889)                                                        # seed
reltot <- rnorm(n = n, mean = 0.7, sd = 0.1)                            # reliability estimates for scale
no_items <- round(rnorm(n = n, mean = 5, sd = 1))                       # number of items in the scale
no_group <- round(rtruncnorm(n = n, mean = 2, sd = 2, a = 2, b = 5))    # number of groups that are compared
nobs <- round(rnorm(n = n, mean = 150, sd = 50))
df <- cbind(reltot,no_items,no_group,nobs)                              # combine all variables in dataframe
df <- as.data.frame(df)

# calculate inter item correlation for all comparisons
df$iic <- -(df$reltot / (df$no_items * df$reltot - df$reltot - df$no_items))

# simulate factor model and check power to detect measurement non-invariance in the intercepts
for (i in 1:nrow(df)) {
  
  # simulate one-factor model with specific parameter values
  k <- df$no_group[i]
  
  # take the total sample size and divide it by the number of groups
  nobs <- round(rep(as.numeric(df$nobs[i])/k,k))
  
  # number of items
  q <- df$no_items[i]
  
  # factor loadings (square root of the inter-item correlation)
  lambdas <- rep(sqrt(df$iic[i]),q)
  lambda <- matrix(c(lambdas), nrow = q, ncol = 1) 
  
  # error variances (1 - lambda^2)
  errorvar <- 1 - lambdas[1:q] * lambdas[1:q]  
  theta <- matrix(0,nrow=q,ncol=q)
  diag(theta) <- errorvar
  
  # latent variance
  phi <- 1 
  
  # assign values to item intercepts 
  # 50% of groups gets intercept difference of 0.5 in all items
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
  
  # we assigned intercept bias in all items for 50% of the groups, 
  # but we only need bias in 1/3 of those items
  # here we assign an intercept of 0 (no bias) to 2/3 of the indicators that were 0.50
  
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
  df$powerncp[i] <- as.numeric(power.cfa.th)
}

# select only those studies that have a power to detect non-invariance of > 0.80
df.select <- df[df$powerncp > 0.80,]
df.select
