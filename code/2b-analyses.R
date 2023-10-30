### THE DIRE DISREGARD OF MEASUREMENT INVARIANCE TESTING ###
### This is code to analyze data from step 2+3

# Install and load packages/data ------------------------------------------
# Clear workspace
#rm(list=ls())

# `readxl` to load coding sheet step2-3 xlsx
#install.packages("readxl")
library(readxl)   

# `dplyr` for function to count the total number of studies
#install.packages("dplyr")
library(dplyr)

# Functions ---------------------------------------------------------------
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

# Count frequency of variable per journal
count.var <- function(df, var, val, dichotomous=TRUE) { 
  if (dichotomous == FALSE) {
    
    a <- sum(df[var] != "NA", na.rm=TRUE)
    b <- sum(df[df$journal_id==0,][var] != "NA", na.rm=TRUE) 
    c <- sum(df[df$journal_id==1,][var] != "NA", na.rm=TRUE) 
    res <- c("total"=a, "total %"=a/nrow(df)*100, "plos"=b, "plos %"=b/nrow(df[df$journal_id==0,])*100, "ps"=c, "ps %"=c/nrow(df[df$journal_id==1,])*100)
    return(round(res,1))

  } else {
    
    a <- sum(df[var] == val, na.rm=TRUE)
    b <- sum(df[df$journal_id==0,][var] == val, na.rm=TRUE)
    c <- sum(df[df$journal_id==1,][var] == val, na.rm=TRUE)
    res <- c("total"= a, "total %"= a/nrow(df)*100, "plos"= b, "plos %"= b/nrow(df[df$journal_id==0,])*100, "ps"=c, "ps %"= c/nrow(df[df$journal_id==1,])*100)
    return(round(res,1))   
  } 
}

# Analyses ----------------------------------------------------------------
# Load in data file
df <- read_excel("data/codebook-main-step2step3.xlsx")

# Make columns numeric
df$measure_scale <- as.numeric(df$measure_scale)
df$width_scale_recoded <- as.numeric(df$width_scale_recoded)
df$no_items <- as.numeric(df$no_items)
df$n_rep <- as.numeric(df$n_rep)
df$open_group <- as.numeric(df$open_group)
df$open_scale <- as.numeric(df$open_scale)
df$n_res <- as.numeric(df$n_res)
df$miresult_step2 <- as.numeric(df$miresult_step2)
df$milevel_step2 <- as.numeric(df$milevel_step2)

### Count articles, studies, comparisons
# 39 comparions
# 6 articles, 6 studies
# PLOS: 4 articles, 4 studies
# PS: 2 articles, 2 studies
nrow(df)                             
count.articles(df)                
count.studies(df)   

### Count articles / studies / variables with open data
df.data <- filter(df, open_data == 1)
# 4 articles open data
# PLOS: 3
# PS: 1
count.articles(df.data)              
# 4 studies open data
# PLOS: 3 studies
# PS: 1 study
count.studies(df.data)                                 
# 5 comparisons open data (13%)
# PLOS: 4 comparisons (12 %)
# PS: 1 comparison (20 %)
count.var(df, "open_data", 1)

### Count articles / studies / variables without open data
df.ndata <- filter(df, open_data != 1)
# 2 articles have no open data
# PLOS: 1
# PS: 1
count.articles(df.ndata)              
# 2 studies have no open data
# PLOS: 1 studies
# PS: 1 study
count.studies(df.ndata)                                 
# 34 comparisons have no open data (87 %)
# PLOS: 30 comparisons (88 %)
# PS: 4 comparison (80 %)
count.var(df, "open_data", 0)

### Count articles / studies / variables for 
### which we could construct a grouping variable
df.group <- filter(df, open_group == 1)
# For 4 articles we can construct a grouping variable
# PLOS: 3
# PS: 1
count.articles(df.group)              
# For 4 studies we can construct a grouping variable
# PLOS: 3 studies
# PS: 1 study
count.studies(df.group)                                 
# For 5 comparisons we can construct a grouping variable (13%)
# PLOS: 4 comparisons (12 %)
# PS: 1 comparison (20 %)
count.var(df, "open_group", 1)

### Count articles / studies / variables for 
### which we could construct a scale
df.scale <- filter(df, open_scale == 1)
# For 3 articles we can construct a scale
# PLOS: 2
# PS: 1
count.articles(df.scale)              
# For 3 studies we can construct a scale
# PLOS: 2 studies
# PS: 1 study
count.studies(df.scale)                                 
# For 4 comparisons we can construct a scale (10%)
# PLOS: 3 comparisons (9 %)
# PS: 1 comparison (20 %)
count.var(df, "open_scale", 1)

### Count articles / studies / variables for 
### which we could construct a group + scale variable
df.gs <- filter(df, open_scale == 1  & open_group == 1) 
# For 3 articles we can construct a group and scale
# PLOS: 2
# PS: 1
count.articles(df.gs)              
# For 3 studies we can construct a group and scale
# PLOS: 2 studies
# PS: 1 study
count.studies(df.gs)                                 
# For 4 comparisons we can construct a group and scale
# PLOS: 3 comparisons
# PS: 1 comparison
nrow(df.gs)
sum(df.gs$journal_id == 0)
sum(df.gs$journal_id == 1)

### Count articles / studies / variables for MI test
df.mi <- filter(df, mitest_step2 == 1)
# For 3 articles we can test MI
# PLOS: 2
# PS: 1
count.articles(df.mi)              
# For 3 studies we can test MI
# PLOS: 2 studies
# PS: 1 study
count.studies(df.mi)                                 
# For 4 comparisons we can test MI
# PLOS: 3 comparisons
# PS: 1 comparison
nrow(df.mi)
sum(df.mi$journal_id == 1)

### Count articles / studies / variables for MI result
df.mires <- filter(df, miresult_step2 == 1)
# For 0 articles we find a level of MI holds
count.articles(df.mires)              
# For 0 studies MI we find a level of MI holds
count.studies(df.mires)                                 
# For 0 comparisons we find a level of MI holds

# Comparisons that were reproducible
# 0 comparisons could be reproduced in step 2
sum(df$reproduced_step2)                                 

### STEP 3

### Count articles / studies / comparisons for which we could test MI
df.mi3 <- filter(df, mitest_step3 == 1)
# For 3 articles we perform a MI test
# PLOS: 2
# PS: 1
count.articles(df.mi3)              
# For 3 studies we perform a MI test
# PLOS: 2 studies
# PS: 1 study
count.studies(df.mi3)                                 
# For 4 comparisons we perform a MI test
# PLOS: 3 comparisons
# PS: 1 comparison
nrow(df.mi3)
sum(df.mi3$journal_id == 1)

### Count articles / studies / variables for which we found MI held
df.mires <- filter(df, miresult_step3 == 1)
# For 0 articles we find a level of MI holds
count.articles(df.mires)              
# For 0 studies MI we find a level of MI holds
count.studies(df.mires)                                 
# For 0 comparisons we find a level of MI holds

