### THE DIRE DISREGARD OF MEASUREMENT INVARIANCE TESTING ###
### This is code to analyze step 4 of the main study; 
### for studies that did not report on MI

# Install and load packages/data ------------------------------------------

# Clear workspace
#rm(list=ls())

# `readxl` to load in the coding sheet of step 4
library(readxl) 

# `dplyr` for function to count the total number of studies
library(dplyr)

# Load data
df <- read_excel("../data/codebook-main-step4.xlsx")

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

# Convert variables to numeric
df$open_group <- as.numeric(df$open_group)
df$open_scale <- as.numeric(df$open_scale)
df$miresult_step4 <- as.numeric(df$miresult_step4)
df$milevel_step4 <- as.numeric(df$milevel_step4)
df$config_assumed <- as.numeric(df$config_assumed)


# Analyses ----------------------------------------------------------------
### Count articles, studies, comparisons we attempt to test MI
# Articles: 35
# PLOS: 10
# PS: 25
count.articles(df)  
# Studies: 66
# PLOS: 11
# PS: 54
count.studies(df)       
# Comparisons: 322
# PLOS: 151
# PS: 171
nrow(df) 
sum(df$journal_id==1)
nrow(df) - sum(df$journal_id==1)        

### Count articles / studies / variables with open data
df.data <- filter(df, open_data == 1)
# 32 articles open data
# PLOS: 7
# PS: 25
count.articles(df.data)              
# 62 studies open data
# PLOS: 8 studies
# PS: 54 studies
count.studies(df.data)                                 
# 249 comparisons open data (77 %)
# PLOS: 79 comparisons (52 %)
# PS: 170 comparison (99 %)
count.var(df, "open_data", 1)

### Count articles / studies / variables without open data
df.ndata <- filter(df, open_data != 1)
# 4 articles no open data
# PLOS: 3
# PS: 1
count.articles(df.ndata)              
# 4 studies no open data
# PLOS: 3 studies
# PS: 1 study
count.studies(df.ndata)                                 
# 73 comparisons no open data (23 %)
# PLOS: 72 comparisons (48 %)
# PS: 1 comparison (1 %)
count.var(df, "open_data", 0)

### Count articles / studies / variables for 
### which we could construct a grouping variable
df.group <- filter(df, open_group == 1)
# 30 articles open group
# PLOS: 5
# PS: 25
count.articles(df.group)              
# 60 studies open group
# PLOS: 6 studies
# PS: 54 studies
count.studies(df.group)                                 
# 239 comparisons open group (74 %)
# PLOS: 69 comparisons (46 %)
# PS: 170 comparison (99 %)
count.var(df, "open_group", 1)

### Count articles / studies / variables for 
### which we could construct a scale
df.scale <- filter(df, open_scale == 1)
# 27 articles open scale
# PLOS: 4
# PS: 23
count.articles(df.scale)              
# 57 studies open scale
# PLOS: 5 studies
# PS: 52 studies
count.studies(df.scale)                                 
# 175 comparisons open scale (54 %)
# PLOS: 14 comparisons (9 %)
# PS: 161 comparison (94 %)
count.var(df, "open_scale", 1)

### Count articles / studies / variables for 
### which we could construct a group + scale variable
df.gs <- filter(df, open_scale == 1  & open_group == 1) 
# 26 articles open group + open scale
# PLOS: 3
# PS: 23
count.articles(df.gs)              
# 56 studies open group + open scale
# PLOS: 4 studies
# PS: 52 studies
count.studies(df.gs)                                 
# 174 comparisons open group + open scale
# PLOS: 13 comparisons
# PS: 161 comparison
nrow(df.gs)
nrow(df.gs) - sum(df.gs$journal_id == 1)
sum(df.gs$journal_id == 1)

### Count articles / studies / variables for MI test
df.mi <- filter(df, mitest_step4 == 1)
# 26 articles MI test
# PLOS: 3
# PS: 23
count.articles(df.mi)              
# 56 studies MI test
# PLOS: 4 studies
# PS: 52 study
count.studies(df.mi)                                 
# 174 comparisons MI test (54 %)
# PLOS: 13 comparisons (9 %)
# PS: 161 comparison (94 %)
count.var(df, "mitest_step4", 1)

### Count articles / studies / variables that cannot test for MI
# 6 articles
# PLOS: 4
# PS: 2
count.articles(df.data) - count.articles(df.gs)
# 75 comparisons
# PLOS: 66
# PS: 9
nrow(df.data) - nrow(df.gs)
sum(df.data$journal_id == 0) - sum(df.gs$journal_id == 0)
sum(df.data$journal_id == 1) - sum(df.gs$journal_id == 1)

### Count articles / studies / variables for which MI held at some level
df.mires.1 <- filter(df, miresult_step4 == 1)
# 16 articles MI held at some level
# PLOS: 0
# PS: 16
count.articles(df.mires.1)              
# 27 studies MI held at some level
# PLOS: 0
# PS: 27
count.studies(df.mires.1)                                 
# 73 comparisons MI held at some level
nrow(df.mires.1)

### Count articles / studies / variables for which MI was rejected (i.e., configural invariance rejected)
df.mires.0 <- filter(df, miresult_step4 == 0)
# 19 articles MI held at some level
# PLOS: 3
# PS: 16
count.articles(df.mires.0)              
# 41 studies MI held at some level
# PLOS: 4
# PS: 37
count.studies(df.mires.0)                                 
# 101 comparisons MI held at some level
# PLOS: 13 (2 %)
# PS: 88 (31 %)
nrow(df.mires.0)
sum(df.mires.0$journal_id == 0); sum(df.mires.0$journal_id == 0)/642*100
sum(df.mires.0$journal_id == 1); sum(df.mires.0$journal_id == 1)/287*100

### MI level reported (0 = none, 1 = config, 2 = metric, 3 = scalar, 4 = residual, 5 = partial, 99 = other, NA = not applicable)
# No invariance (configural rejected): 101 comparisons (58 %)
# Configural invariance: 13 (8 %)
# Metric invariance: 15 (9 %)
# Scalar invariance: 45 (26 %)
Hmisc::describe(df$milevel_step4)
# PLOS: 13 comparisons
# No invariance (configural rejected): 13
Hmisc::describe(df[df$journal_id == 0,]$milevel_step4)
# PS: 161 comparisons
# No invariance (configural rejected): 88 
# Configural invariance: 13
# Metric invariance: 15
# Scalar invariance: 45
Hmisc::describe(df[df$journal_id == 1,]$milevel_step4)

### Subset by level of invariance to count articles and studies
nconfig <- df[df$milevel_step4 == 0,]
config <- df[df$milevel_step4 == 1,]
metric <- df[df$milevel_step4 == 2,]
scalar <- df[df$milevel_step4 == 3,]
# remove empty rows from df
nconfig <- nconfig[!apply(is.na(nconfig) | nconfig == "", 1, all),]
config <- config[!apply(is.na(config) | config == "", 1, all),]
metric <- metric[!apply(is.na(metric) | metric == "", 1, all),]
scalar <- scalar[!apply(is.na(scalar) | scalar == "", 1, all),]

# No invariance (configural rejected): 
# Articles: 19
# PLOS: 3
# PS: 16
count.articles(nconfig)
# Studies: 41 
# PLOS: 4
# PS: 37
count.studies(nconfig)

# Configural invariance:
# Articles: 7
# PLOS: 0
# PS: 7
count.articles(config)
# Studies: 10
# PLOS: 0
# PS: 10
count.studies(config)

# Metric invariance:
# Articles: 8
# PLOS: 0
# PS: 8
count.articles(metric)
# Studies: 10
# PLOS: 0
# PS: 10
count.studies(metric)

# Scalar invariance:
# Articles: 14
# PLOS: 0
# PS: 14
count.articles(scalar)
# Studies: 20
# PLOS: 0
# PS: 20
count.studies(scalar)

# Analyses split by number of items (3 and >3) ----------------------------
### 3 items
# Subset of comparisons for which configural invariance was assumed because we had 3 items.
df.conf <- subset(df, df$config_assumed == 1)
# 61 comparisons in total
# 13 articles
# 22 studies
nrow(df.conf)
count.articles(df.conf)
count.studies(df.conf)
# 35 % of all scales
round(nrow(df.conf) / nrow(df.mi) * 100,1)

### MI level reported (0 = none, 1 = config, 2 = metric, 3 = scalar, 4 = residual, 5 = partial, 99 = other, NA = not applicable)
# Configural invariance: 11 (18 %)
# Metric invariance: 13 (21 %)
# Scalar invariance: 37 (61 %)
Hmisc::describe(df.conf$milevel_step4)
# PLOS: 0 comparisons
# No invariance (configural rejected): 0
Hmisc::describe(df.conf[df.conf$journal_id == 0,]$milevel_step4)
# PS: 61 comparisons
# Configural invariance: 11
# Metric invariance: 13
# Scalar invariance: 37
Hmisc::describe(df.conf[df.conf$journal_id == 1,]$milevel_step4)

### Subset by level of invariance to count articles and studies
config <- df.conf[df.conf$milevel_step4 == 1,]
metric <- df.conf[df.conf$milevel_step4 == 2,]
scalar <- df.conf[df.conf$milevel_step4 == 3,]
# Remove empty rows from df
config <- config[!apply(is.na(config) | config == "", 1, all),]
metric <- metric[!apply(is.na(metric) | metric == "", 1, all),]
scalar <- scalar[!apply(is.na(scalar) | scalar == "", 1, all),]

# Configural invariance:
# Articles: 5
# PLOS: 0
# PS: 5
count.articles(config)
# Studies: 8
# PLOS: 0
# PS: 8
count.studies(config)

# Metric invariance:
# Articles: 6
# PLOS: 0
# PS: 6
count.articles(metric)
# Studies: 8 
# PLOS: 0
# PS: 8
count.studies(metric)

# Scalar invariance:
# Articles: 12
# PLOS: 0
# PS: 12
count.articles(scalar)
# Studies: 17
# PLOS: 0 
# PS: 17
count.studies(scalar)

### More than 3 items
# Subset of comparisons for which configural invariance was assumed because we had 3 items.
df.nconf <- subset(df, df$config_assumed != 1)
# 113 comparisons in total
# 20 articles
# 44 studies
nrow(df.nconf)
count.articles(df.nconf)
count.studies(df.nconf)
# 65 % of all scales
round(nrow(df.nconf) / nrow(df.mi) * 100,1)

### MI level reported (0 = none, 1 = config, 2 = metric, 3 = scalar, 4 = residual, 5 = partial, 99 = other, NA = not applicable)
# No invariance (configural rejected): 101 comparisons (89 %)
# Configural invariance: 2 (2 %)
# Metric invariance: 2 (2 %)
# Scalar invariance: 8 (7 %)
Hmisc::describe(df.nconf$milevel_step4)
# PLOS: 13 comparisons
# No invariance (configural rejected): 13
Hmisc::describe(df.nconf[df.nconf$journal_id == 0,]$milevel_step4)
# PS: 100 comparisons
# No invariance (configural rejected): 88
# Configural invariance: 2
# Metric invariance: 2
# Scalar invariance: 8
Hmisc::describe(df.nconf[df.nconf$journal_id == 1,]$milevel_step4)

### Subset by level of invariance to count articles and studies
nconfig <- df.nconf[df.nconf$milevel_step4 == 0,]
config <- df.nconf[df.nconf$milevel_step4 == 1,]
metric <- df.nconf[df.nconf$milevel_step4 == 2,]
scalar <- df.nconf[df.nconf$milevel_step4 == 3,]
# remove empty rows from df
nconfig <- nconfig[!apply(is.na(nconfig) | nconfig == "", 1, all),]
config <- config[!apply(is.na(config) | config == "", 1, all),]
metric <- metric[!apply(is.na(metric) | metric == "", 1, all),]
scalar <- scalar[!apply(is.na(scalar) | scalar == "", 1, all),]

# No invariance (configural rejected): 
# Articles: 19
# PLOS: 3
# PS: 16
count.articles(nconfig)

# Studies: 41 
# PLOS: 4
# PS: 37
count.studies(nconfig)

# Configural invariance:
# Articles: 2
# PLOS: 0
# PS: 2
count.articles(config)

# Studies: 2
# PLOS: 0
# PS: 2
count.studies(config)

# Metric invariance:
# Articles: 2
# PLOS: 0
# PS: 2
count.articles(metric)

# Studies: 2
# PLOS: 0
# PS: 2 
count.studies(metric)

# Scalar invariance:
# Articles: 6
# PLOS: 0
# PS: 6
count.articles(scalar)

# Studies: 7
# PLOS: 0
# PS: 7
count.studies(scalar)

# Extra analyses based on reviewer comments -------------------------
# Check factor structure for comparisons that rejected configural invariance
df.fs <- subset(df, df$milevel_step4 == 0)

# Make number of factors variable numeric
df.mi$f_total <- as.numeric(df.mi$f_total)
df.fs$f_total
Hmisc::describe(df.fs$f_total)

# Check results by group
table(df.fs$f_group)

# Check how many scales with scalar invariance are unique
df_scalar <- subset(df, as.numeric(df$milevel_step4) == 3)
length(df_scalar)
df_scalar$name_scale
unique(df_scalar$name_scale)
length(unique(df_scalar$name_scale))

# Check levels of invariance based on type of scale (existing / ad hoc)
table(df$type_scale, df$milevel_step4)

# Crosstabs type scale with assumed configural invariance (i.e., 3-item scales)
# Of 20 existing scales, config assumed in 2 (10%)
# Of 154 ad hoc scales, config assumed in 59 (38%)
table(df$type_scale, df$config_assumed)
