### THE DIRE DISREGARD OF MEASUREMENT INVARIANCE TESTING ###
### This is code to select comparisons for which we will 
### attempt to perform measurement invariance checks in step 4

# Install and load packages/data ------------------------------------------
# Clear workspace
#rm(list=ls())

# `lavaan` to simulate factor model and run MI tests
#install.packages("lavaan")
library(lavaan)

# `readxl` to load the codebook
#install.packages("readxl")
library(readxl)

# `writexl` to write away final codebooks
#install.packages("writexl")
library(writexl)

# `dplyr` to use pipe operator in functions
#install.packages("dplyr")
library(dplyr)

# Load data from main study
# Load data step 1
df <- read_excel("../data/codebook-main-step1.xlsx")

# Load data step2+3 to check overlap in IDs
df.checked <- read_excel("../data/codebook-main-step2step3-sample-without-results.xlsx")

# Functions ---------------------------------------------------------------
# Function to check how many groups are compared for each study
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

# Count total number of articles per journal
count.articles <- function(df) {
  a <- length(unique(df$article_id_recoded))
  b <- length(unique(df[df$journal_id == 0,]$article_id_recoded))
  c <- length(unique(df[df$journal_id == 1,]$article_id_recoded))
  return(c("total"=a, "plos"=b, "ps"=c))
}

# Count total number of studies per journal
count.studies <- function(df) {
  
  df.temp <- df %>%
    distinct(article_id, study_recoded, journal_id) %>%
    group_by(article_id, journal_id) %>%
    summarise("studies" = n(), .groups = "keep")
  
  a <- sum(df.temp$studies)
  b <- sum(df.temp[df.temp$journal_id == 0,]$studies)
  c <- sum(df.temp[df.temp$journal_id == 1,]$studies)
  return(c("total"=a, "plos"=b, "ps"=c))
}

# Simulation --------------------------------------------------------------
# Check distribution of reliabilities of all studies: 0.83
df$rel_rep <- as.numeric(df$rel_rep)
hist(as.numeric(df$rel_rep))
mean(df$rel_rep, na.rm=T) 

# Check distribution of number of items of all studies in codebook-main-step1.xlsx
df$no_items <- as.numeric(df$no_items)
hist(df$no_items)

# Remove studies that checked for MI
df$mitest_rep <- as.numeric(df$mitest_rep)
df.notchecked <- subset(df,mitest_rep == 0)

# Check if all rows are comparisons on reflective scale
nrow(df.notchecked) + nrow(df.checked) == sum(df$reflective == 1, na.rm = TRUE)

# Count articles and comparisons per journal
# Total: 91 articles
# PLOS: 46 articles
# PS: 45 articles
count.articles(df.notchecked) 
nrow(df.notchecked)
# Comparisons: 890
# 608 comparisons in PLOS
# 282 comparisons in PS
nrow(df.notchecked[df.notchecked$journal_id == 0,]) 
nrow(df.notchecked[df.notchecked$journal_id == 1,]) 

# Select only studies that report reliability and number of items and sample size
dfs <- df.notchecked[!is.na(df.notchecked$rel_rep) & df.notchecked$no_items != "NA" & !is.na(df.notchecked$no_items),]
# select only studies that report either total or group sample size
dfs <- dfs[dfs$n_rep != "NA" | dfs$n1_rep != "NA" | dfs$n2_rep != "NA" | dfs$n3_rep != "NA" | dfs$n4_rep != "NA" | dfs$n5_rep != "NA",]
# Remove empty rows from df
dfs <- dfs[!apply(is.na(dfs) | dfs == "", 1, all),]
# Total: 37 articles
# PLOS: 11 articles
# PS: 26 articles
count.articles(dfs)
# Total: 69 studies
# PLOS: 12 studiess
# PS: 57 studies
count.studies(dfs)  
# 335 comparisons total
# 158 comparisons PLOS
# 177 comparisons PS
nrow(dfs) 
nrow(dfs[dfs$journal_id==0,]) 
nrow(dfs[dfs$journal_id==1,]) 

# Number of articles, studies, comparisons that did not report number of items, reliability, or sample size
# Select only studies that report reliability and number of items and sample size
dfs.n <- df.notchecked[is.na(df.notchecked$rel_rep) | df.notchecked$no_items == "NA" | is.na(df.notchecked$no_items) | df.notchecked$n_rep == "NA" | is.na(df.notchecked$n_rep),]
# Total: 54 articles
# 35 articles PLOS
# 19 articles PS
count.articles(df.notchecked) - count.articles(dfs)
# 555 comparisons total
# 450 comparisons PLOS
# 105 comparisons PS
nrow(dfs.n) 
nrow(dfs.n[dfs.n$journal_id==0,]) 
nrow(dfs.n[dfs.n$journal_id==1,]) 

# If some studies have sample sizes for groups but not the total one, we will assign n_rep = sum of all the group sample sizes

# Calculate inter item correlation for all studies
#dfs$iic <- -(dfs$rel_rep / (dfs$no_items * dfs$rel_rep - dfs$rel_rep - dfs$no_items)) # alternative formulation
dfs$iic <- dfs$rel_rep / (dfs$no_items - dfs$rel_rep * (dfs$no_items - 1))
dfs <- as.data.frame(dfs)

# How many groups are compared for each row?
dfs$no_group <- apply(dfs,1,assign.groups)

# One study (row_id 1452 and 1454) has 10 groups, which our codebook doesn't accommodate:
dfs[dfs$row_id == 1452,]$no_group <- 10
dfs[dfs$row_id == 1454,]$no_group <- 10

# Choose intercept bias
int_bias <- 0.5

# Simulate factor model and check power to detect measurement non-invariance in the intercepts
for (i in 1:nrow(dfs)) {
  
  # Simulate one-factor model with specific parameter values
  k <- dfs$no_group[i]
  
  # Extract sample size for each level of k groups
  if (k == 2) {
    
    nobs <- as.numeric(c(dfs$n1_rep[i],dfs$n2_rep[i]))
    
  } else if (k == 3) {
    
    nobs <- as.numeric(c(dfs$n1_rep[i],dfs$n2_rep[i],dfs$n3_rep[i]))
    
  } else if (k == 4) {
    
    nobs <- as.numeric(c(dfs$n1_rep[i],dfs$n2_rep[i],dfs$n3_rep[i],dfs$n4_rep[i]))
    
  } else if (k == 5) {
    
    nobs <- as.numeric(c(dfs$n1_rep[i],dfs$n2_rep[i],dfs$n3_rep[i],dfs$n4_rep[i],dfs$n5_rep[i]))
    
  } else {
    
    # The comparisons with 10 groups do not have group sample sizes, so we make an empty variable "nobs"
    nobs <- NA
    
  }
  
  # If no group sample sizes reported, take the total sample size and divide it by the number of groups
  if (is.na(nobs[1])) {
    
    nobs <- round(rep(as.numeric(dfs$n_rep[i])/k,k))
    
    }
  
  # Number of items
  q <- dfs$no_items[i]
  
  # Factor loadings (square root of the inter-item correlation)
  lambdas <- rep(sqrt(dfs$iic[i]),q)
  lambda <- matrix(c(lambdas), nrow = q, ncol = 1) 
  
  # Error variances (1 - lambda^2)
  errorvar <- 1 - lambdas[1:q] * lambdas[1:q]  
  theta <- matrix(0,nrow=q,ncol=q)
  diag(theta) <- errorvar
  
  # Latent variance
  phi <- 1 
  
  # Assign values to item intercepts 
  # (50% of groups gets intercept difference of 0.5 in all items for now)
  if (k == 2) { nu <- c(0, int_bias) }
    else if (k == 3) { nu <- c(0, 0, int_bias) }
    else if (k == 4) { nu <- c(0, 0, int_bias, int_bias) }
    else if (k == 5) { nu <- c(0, 0, 0, int_bias, int_bias) } 
    else if (k == 10) { nu <- c(0,0,0,0,0,int_bias,int_bias,int_bias,int_bias,int_bias) }
  
  # Latent mean estimate
  alpha <- rep(0,k)
  
  # Column names for covariance matrices and mean vectors (needed to estimate in lavaan)
  colnms <- NA
  for(z in 1:q){
    colnms[z] <- paste0('V', z)
  }
  
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
    
  } else if (k == 10) {
    
    mu.pop.1 <- nu[1] + lambda %*% alpha[1]									                   
    mu.pop.2 <- nu[2] + lambda %*% alpha[2]
    mu.pop.3 <- nu[3] + lambda %*% alpha[3]
    mu.pop.4 <- nu[4] + lambda %*% alpha[4]
    mu.pop.5 <- nu[5] + lambda %*% alpha[5]
    mu.pop.6 <- nu[6] + lambda %*% alpha[6]
    mu.pop.7 <- nu[7] + lambda %*% alpha[7]
    mu.pop.8 <- nu[8] + lambda %*% alpha[8]
    mu.pop.9 <- nu[9] + lambda %*% alpha[9]
    mu.pop.10 <- nu[10] + lambda %*% alpha[10]
    mu.pop <- list(mu.pop.1,mu.pop.2,mu.pop.3,mu.pop.4,mu.pop.5,mu.pop.6,mu.pop.7,mu.pop.8,mu.pop.9,mu.pop.10)
    
    sigma.pop.1 <- lambda %*% phi %*% t(lambda) + theta
    colnames(sigma.pop.1) <- colnms
    sigma.pop <- list(sigma.pop.1,sigma.pop.1,sigma.pop.1,sigma.pop.1,sigma.pop.1,sigma.pop.1,sigma.pop.1,sigma.pop.1,sigma.pop.1,sigma.pop.1)
    
  }
  
  # We assigned bias in all indicators for 50% of the groups, but we only need bias in 1/3 of those indicators
  # Here we assign an intercept of 0 (no bias) to 2/3 of the indicators that were 0.50. 
  for(x in 2:k){
    mu.pop[[x]][((round(q/3))+1):q] <- 0
  }
  
  # Construct model syntax for lavaan to analyze
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
  
  # Likelihood ratio test of metric and scalar models
  mod <- anova(mod.metric,mod.scalar)

  # Extract chisquare difference between the two models as NCP + extract degrees of freedom
  ncp <- mod$`Chisq diff`[2]
  dof <- mod$`Df`[2] - mod$`Df`[1]
  
  # Power estimate given degrees of freedom and NCP
  # indicates power to detect intercept non-invariance in 1/3 of the indicators for 1/2 of the groups
  power.cfa.th <- 1 - pchisq(qchisq(.95, df = dof), df = dof, ncp = ncp) 
  dfs$powerncp[i] <- as.numeric(power.cfa.th)

  # Print progress
  print(i)
}

# Select only those studies that have a power to detect non-invariance of > 0.80
df.select <- dfs[dfs$powerncp >= 0.80,1:43]
nrow(df.select)

# Number of comparisons dropout
# 2 articles (1 PLOS, 1 PS)
# 34 studies (2 PLOS, 32 PS)
df.dropout <- dfs[!dfs$powerncp >= 0.80,1:43] 
count.articles(dfs) - count.articles(df.select)
count.studies(dfs) - count.articles(df.select)
# 13 comparisons (7 for PLOS, 6 for PS)
nrow(df.dropout)
nrow(df.dropout[df.dropout$journal_id==0,]) 
nrow(df.dropout[df.dropout$journal_id==1,]) 

### Number of comparisons to include in step 4
# Total: 35 articles, 66 studies
# PLOS: 10 articles, 11 studies
# PS: 25 articles, 55 studies
count.articles(df.select)
count.studies(df.select)
# Total comparisons
# 322
nrow(df.select)
# PS comparisons (PLOS = 0, PS = 1)
# 171 
sum(df.select$journal_id)
# PLOS comparisons (PLOS = 0, PS = 1)
# 151 
nrow(df.select) - sum(df.select$journal_id)

# Are the colnames for the selected studies and the ones reporting on MI the same?
df.select <- df.select[,1:length(colnames(df.select))-1]
colnames(df.select) == colnames(df.checked)

# Multiple ids in both dataframes?
sum(unique(df.checked$row_id) %in% unique(df.select$row_id))
sum(unique(df.select$row_id) %in% unique(df.checked$row_id))

# Save final dataset of studies that we will attempt to reproduce
write_xlsx(df.select, "data/codebook-main-step4-sample-without-results.xlsx")
