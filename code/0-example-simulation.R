### THE DIRE DISREGARD OF MEASUREMENT INVARIANCE TESTING ###
### This is code to simulate an intercept bias of 0.5 in 1/3 
### of the items and 1/2 of the groups in a one-factor model,
### given certain reliability estimates, number of items, 
### number of groups, and sample sizes.

# Install and load packages -----------------------------------------------
# Clear workspace
#rm(list=ls())

# `truncnorm` to simulate data via rnorm that is truncated between values (for no_group)
#install.packages("truncnorm")
library(truncnorm)

# Simulation --------------------------------------------------------------
# Generate 50 studies (i.e., scale mean comparisons), each having:
# - a reliability estimate for the scale
# - a number of items in the scale (minimum is 3)
# - number of groups that are compared (range 2 to 5)

# Specify number of studies / comparisons
n <- 50

# Set seed
set.seed(0804)

# Generate reliability estimates for scale with mean 0.7 and SD 0.1
reltot <- rnorm(n = n, mean = 0.7, sd = 0.1)

# Generate number of items in the scale with mean 5 and SD 2, bounded by min = 3
no_items <- round(rtruncnorm(n = n, mean = 6, sd = 2, a = 3))

# Generate number of compared groups with mean 2 and SD 2, bounded by 2 and 5
no_group <- round(rtruncnorm(n = n, mean = 2, sd = 2, a = 2, b = 5))

# Generate sample sizes per comparison with mean 100 and SD 25
nobs <- round(abs(rnorm(n = n, mean = 100, sd = 50)))

# Combine generated variables into a dataframe
df <- cbind(reltot, no_items, no_group, nobs)
df <- as.data.frame(df)

# Calculate inter-item correlation for all comparisons
df$iic <- -(df$reltot / (df$no_items * df$reltot - df$reltot - df$no_items))

# Choose intercept bias
int_bias <- 0.5

# Create lists to store all covariance matrices and mean vectors
mu_pops <- list()
sigma_pops <- list()

# Simulate factor model with intercept bias
for (i in 1:nrow(df)) {

  # Simulate one-factor model with specific parameter values
  # Extract the number of groups (k) for the current comparison
  k <- df$no_group[i]
  
  # Determine the sample size for each group
  nobs <- round(rep(as.numeric(df$nobs[i])/k, k))
  
  # Extract the number of items (q)
  q <- df$no_items[i]
  
  # Assign factor loadings (square root of the inter-item correlation)
  lambdas <- rep(sqrt(df$iic[i]), q)
  lambda <- matrix(c(lambdas), nrow = q, ncol = 1)

  # Compute error variances (1 - lambda^2)
  errorvar <- 1 - lambdas[1:q] * lambdas[1:q]  
  theta <- matrix(0, nrow = q, ncol = q)
  diag(theta) <- errorvar
  
  # Specify latent variance
  phi <- 1 
  
  # Assign values to item intercepts
  # 50% of groups get an intercept difference of 0.5 in all items
  # Choose intercept values based on the number of groups
  if (k == 2) { nu <- c(0, int_bias) }
  else if (k == 3) { nu <- c(0, 0, int_bias) }
  else if (k == 4) { nu <- c(0, 0, int_bias, int_bias) }
  else if (k == 5) { nu <- c(0, 0, 0, int_bias, int_bias) }
  
  # Specify latent mean estimate
  alpha <- rep(0, k)
  
  # Generate column names for covariance matrices and mean vectors
  colnms <- NA
  for(z in 1:q){ colnms[z] <- paste0('V', z) }
  
  # Construct covariance matrices and mean vectors for each level of k
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
  
  # We assigned intercept bias in all items for 50% of the groups 
  # but we only need bias in 1/3 of those items
  # We assign an intercept of 0 (no bias) to 2/3 of the indicators
  for(x in 2:k){
    mu.pop[[x]][((round(q/3))+1):q] <- 0
  }

  # Store mu.pop and sigma.pop
  mu_pops[[i]] <- mu.pop
  sigma_pops[[i]] <- sigma.pop 

}

# Check results for first comparison
sigma_pops[[1]]
mu_pops[[1]]

# Estimate models and perform measurement invariance test
for (j in 1:nrow(df)) {
  
  # Extract mu.pop and sigma.pop
  mu.pop <- mu_pops[[j]]
  sigma.pop <- sigma_pops[[j]]
  
  # Extract the number of items (q) and groups (k)
  q <- df$no_items[j]
  k <- df$no_group[j]
  
  # Construct model syntax for lavaan to analyze
  mods.metric <- mods.scalar <- character(q + 1)
  
  for(l in 1:q){
    mods.metric[l] <- paste0("F1 =~ V", l)
    mods.scalar[l] <- paste0("F1 =~ V", l)
  }
  
  mods.metric[q+1] <- paste0("F1~ c(", paste(rep(0, k), collapse = ","), ")*1")
  mods.scalar[q+1] <- paste0("F1~ c(0,", paste(rep(NA, k-1), collapse = ","), ")*1")


  # Fit metric (loadings equal across groups) and scalar invariance (loadings and intercepts equal across groups) models
  mod.metric <- cfa(mods.metric, sample.cov = sigma.pop, sample.mean = mu.pop, sample.nobs = rep(df$nobs[j],k), std.lv=T, group.equal = c("loadings"))

  mod.scalar <- cfa(mods.scalar, sample.cov = sigma.pop, sample.mean = mu.pop, sample.nobs = rep(df$nobs[j],k), std.lv=T, group.equal = c("loadings","intercepts"))
  
  # Likelihood ratio test metric-scalar models
  mod <- anova(mod.metric,mod.scalar)
  
  # Extract chi-square and dof difference
  ncp <- mod$`Chisq diff`[2]
  dof <- mod$`Df`[2] - mod$`Df`[1]
  
  # Power estimate given degrees of freedom and NCP
  # indicates power to detect intercept non-invariance
  power.cfa.th <- 1 - pchisq(qchisq(.95, df = dof), df = dof, ncp = ncp)
  df$ncp[j] <- ncp
  df$dof[j] <- dof
  df$powerncp[j] <- as.numeric(power.cfa.th)
}

# Check dataframe
head(df)

# Select only those studies with power > 0.80 to detect non-invariance
df.select <- df[df$powerncp > 0.80,]
df.select
