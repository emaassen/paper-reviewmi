### THE DIRE DISREGARD OF MEASUREMENT INVARIANCE TESTING ###
### This is code to analyze data from step 1, the reporting results

# Install and load packages/data ------------------------------------------
# Clear workspace
#rm(list=ls())

# No scientific notation
options(scipen=999) 

# `dplyr` for function to count the total number of studies
#install.packages("dplyr")
library(dplyr)

# `psych` to calculate interrater reliability
#install.packages("psych")
library(psych)

# `Hmisc` to utilize the describe function
#install.packages("Hmisc")
library(Hmisc)

# `readxl` to load codebook step 1
#install.packages("readxl")
library(readxl)

# `writexl` to write codebook steps 2 and 3
#install.packages("writexl")
library(writexl)

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

# Selected studies---------------------------------------------------------
# Load the main dataset (codebook) from step 1
df.raw <- read_excel("data/codebook-main-step1.xlsx")

# Load data to calculate interrater reliability
irr <- read_excel("data/interraterreliability.xlsx") 

# Check column names
colnames(df.raw)                                            

### Count number of journals, articles, and studies
# 2 journals (PLOS = 0, PS = 1)
unique(df.raw$journal_id)                             
# 426 articles (PLOS = 213, PS = 213)
n.art <- count.articles(df.raw); n.art                
# 841 studies (PLOS = 237, PS = 604) 
n.stu <- count.studies(df.raw); n.stu                 

### Count articles with empirical data 
df.emp <- filter(df.raw, empirical == 1)
# 342 empirical articles (80 %);
# 141 PLOS (66 %)
# 201 PS (94 %)
n.art.emp <- count.articles(df.emp); n.art.emp                                    
round(n.art.emp / n.art * 100)               
# 748 empirical studies (89 %);
# 160 PLOS (68 %)
# 588 PS (97 %)
n.stu.emp <- count.studies(df.emp); n.stu.emp                                     
round(n.stu.emp / n.stu * 100)    

### Articles can contain multiple studies and comparisons, so 
### it's possible that one study of an article drops out 
### because there is no empirical data, whereas the other study 
### remains. We want to count the dropout of articles and studies 
### in each step, so we subtract the remaining number of 
### articles/studies from the original number of articles/studies.

### Dropout empirical
# 84 articles;
# 72 PLOS (34%)
# 12 PS (6%)
n.art - n.art.emp
round((n.art - n.art.emp) / n.art * 100)
# 93 studies; 
# 77 PLOS (32%)
# 16 PS (16%)
n.stu - n.stu.emp
round((n.stu - n.stu.emp) / n.stu * 100)

### Count articles with group comparisons
df.group <- filter(df.emp, compare_group == 1)
# 274 empirical articles (64 %);
# 107 PLOS (50 %)
# 167 PS (78 %)
n.art.group <- count.articles(df.group); n.art.group                                    
round(n.art.group / n.art * 100)               
# 615 empirical studies (73 %);
# 124 PLOS (52 %)
# 491 PS (81 %)
n.stu.group <- count.studies(df.group); n.stu.group                                     
round(n.stu.group / n.stu * 100)  

### Dropout group comparison
# 68 articles;
# 34 PLOS (16 %)
# 34 PS (16 %)
n.art.emp - n.art.group
round((n.art.emp - n.art.group) / n.art * 100)
# 133 studies; 
# 36 PLOS (15%)
# 97 PS (16%)
n.stu.emp - n.stu.group
round((n.stu.emp - n.stu.group) / n.stu * 100)

### Count comparisons made on scales
df.scale <- filter(df.group, scale == 1)
# 191 articles with scales (45 %);
# 67 PLOS (31 %)
# 124 PS (58 %)
n.art.scale <- count.articles(df.scale); n.art.scale                                    
round(n.art.scale / n.art * 100)               
# 397 studies with scales (47 %);
# 81 PLOS (34 %)
# 316 PS (52 %)
n.stu.scale <- count.studies(df.scale); n.stu.scale                                    
round(n.stu.scale / n.stu * 100)  

### Dropout scale comparison
# 83 articles;
# 40 PLOS (19%)
# 43 PS (20%)
n.art.group - n.art.scale
round((n.art.group - n.art.scale) / n.art * 100)
# 218 studies; 
# 43 PLOS (18%)
# 175 PS (29%)
n.stu.group - n.stu.scale
round((n.stu.group - n.stu.scale) / n.stu * 100)

### Count reflective scales
df.ref <- filter(df.scale, reflective == 1)
# 96 articles with reflective scales (23 %);
# 50 PLOS (23 %)
# 46 PS (22 %)
n.art.ref <- count.articles(df.ref); n.art.ref                                   
round(n.art.ref / n.art * 100)               
# 149 studies with reflective scales (18 %);
# 53 PLOS (22 %)
# 96 PS (16 %)
n.stu.ref <- count.studies(df.ref); n.stu.ref                                    
round(n.stu.ref / n.stu * 100)  

### Dropout reflective scale comparison
# 95 articles;
# 17 PLOS (8%)
# 78 PS (37%)
n.art.scale - n.art.ref
round((n.art.scale - n.art.ref) / n.art * 100)
# 248 studies; 
# 28 PLOS (12%)
# 220 PS (36%)
n.stu.scale - n.stu.ref
round((n.stu.scale - n.stu.ref) / n.stu * 100)

# Analyses ----------------------------------------------------------------
# Main data to work with are now the 913 comparisons on a reflective scale
df <- df.ref
# 929 comparisons
# 642 comparisons PLOS
# 287 comparisons PS
count.var(df, "reflective", 1)

### Type of group (0 = existing group; 1 = newly constructed group)
# 334 existing groups (36 %)
# 309 PLOS (48 %)
# 25 PS (9 %)
count.var(df, "type_group", 0)
# 595 constructed groups (64%)
# 333 PLOS (52 %)
# 262 PS (91 %)
count.var(df, "type_group", 1)

### Type of scale (0 = existing scale, 1 = modified or new scale)
# 437 existing scale (47 %)
# 383 PLOS (60 %)
# 54 PS (19 %)
count.var(df, "type_scale", 0)
# 492 modified or newly constructed scale (53 %)
# 259 PLOS (40 %)
# 233 PS (81 %)
count.var(df, "type_scale", 1)

### Measure of scale (0 = dichotomous, 1 = ordinal (up to 5 categories), 2 = continuous (5 or more categories))
# 617 comparisons indicated the measure of the scale (66 %)
# 391 PLOS (61 %)
# 226 PS (79 %)
count.var(df,"measure_scale", dichotomous = FALSE)
# Dichotomous 29 (3 %)
# Ordinal 310 (33 %)
# Continuous 278 (30 %)
# NA 312 (34 %)
Hmisc::describe(df$measure_scale)
# PLOS Dichotomous 17 (3 %)
# PLOS Ordinal 297 (46 %)
# PLOS Continuous 77 (12 %)
# PLOS NA 251 (39 %)
Hmisc::describe(df[df$journal_id == 0,]$measure_scale)
# PS Dichotomous 12 (4 %)
# PS Ordinal 13 (5 %)
# PS Continuous 201 (70 %)
# PS NA 61 (21 %)
Hmisc::describe(df[df$journal_id == 1,]$measure_scale)

### Width of the scale 
# 614 comparisons indicated the width of the scale (66 %)
# 389 PLOS (61 %)
# 225 PS (78 %)
count.var(df,"width_scale_recoded", dichotomous = FALSE)
# NA 315 
# Range: 2 to 81
# Median: 7 (24.3%)
Hmisc::describe(df$width_scale_recoded)
# PLOS range 2 - 11
# PLOS NA 253 
Hmisc::describe(df[df$journal_id == 0,]$width_scale_recoded)
# PS range 2 - 81
# PS NA 62 
Hmisc::describe(df[df$journal_id == 1,]$width_scale_recoded)

### Number of items
# make numeric
df$no_items <- as.numeric(df$no_items)               
# 642 comparisons indicates number of items (69 %)
# 396 PLOS (62 %)
# 246 PS (86 %)
count.var(df,"no_items", dichotomous = FALSE)
# NA: 287 comparisons 
# Mean number of items: 12.8
# Median number of items: 7
# Range number of items: 3 to 135
Hmisc::describe(df$no_items)

# PLOS: 396 comparisons 
# PLOS NA: 246 comparisons
# PLOS Mean number of items: 14.5
# PLOS Median number of items: 10
# PLOS Range number of items: 3 to 90
Hmisc::describe(df[df$journal_id == 0,]$no_items)
# PS: 246 comparisons 
# PS NA: 41 comparisons
# PS Mean number of items: 9.9
# PS Median number of items: 5
# PS Range number of items: 3 to 135
Hmisc::describe(df[df$journal_id == 1,]$no_items)

### Both Width Scale and Number of Items
# make numeric
df$width_scale_recoded <- as.numeric(df$width_scale_recoded)
# Missings on width scale and number of items: 206 (22% of comparisons)
sum(is.na(df$width_scale_recoded) & is.na(df$no_items))
round(sum(is.na(df$width_scale_recoded) & is.na(df$no_items)) / nrow(df) * 100,1)

# PLOS Missings on width scale and number of items: 176 (27% of comparisons)
sum(is.na(df[df$journal_id==0,]$width_scale_recoded) & is.na(df[df$journal_id==0,]$no_items))
round(sum(is.na(df[df$journal_id==0,]$width_scale_recoded) & is.na(df[df$journal_id==0,]$no_items)) / nrow(df[df$journal_id==0,]) * 100,1)

# PS Missings on width scale and number of items: 30 (11% of comparisons)
sum(is.na(df[df$journal_id==1,]$width_scale_recoded) & is.na(df[df$journal_id==1,]$no_items))
round(sum(is.na(df[df$journal_id==1,]$width_scale_recoded) & is.na(df[df$journal_id==1,]$no_items)) / nrow(df[df$journal_id==1,]) * 100,1)

### Sample size (total sample)
# Make numeric
df$n_rep <- as.numeric(df$n_rep)
# 867 comparisons indicate sample size (94 %)
# PLOS 590 (92 %)
# PS 286 (100 %)
count.var(df,"n_rep", dichotomous = FALSE)
# NA 53 (6 %)
# Median: 418
# Range: 15 to 4393362
Hmisc::describe(df$n_rep)
# PLOS 590 comparisons indicate sample size
# PLOS NA 52 
# PLOS Median: 659
# PLOS Range: 15 to 168104
Hmisc::describe(df[df$journal_id==0,]$n_rep)
# PS 286 comparisons indicate sample size
# PS NA 1 
# PS Median: 322
# PS Range: 36 to 4393362
Hmisc::describe(df[df$journal_id==1,]$n_rep)

### Sample size per group
# Make numeric
df$n1_rep <- as.numeric(df$n1_rep)
# 644 comparisons indicate sample size per group (69.3 %)
# PLOS 515 (80.2 %)
# PS 129 (44.9 %)
# NA 285 

### Reliability (total sample)
# Make numeric
df$rel_rep <- as.numeric(df$rel_rep)
# 415 comparisons indicate reliability (45 %)
# PLOS 220 (34 %)
# PS 195 (68 %)
count.var(df,"rel_rep", dichotomous = FALSE)
# NA 514 
# Range: 0.46 - 0.97
# Mean: 0.83
# Median: 0.84
Hmisc::describe(df$rel_rep)
# PLOS 220 comparisons 
# PLOS NA: 422
# Range: 0.46 - 0.97
# Mean: 0.81
# Median: 0.83
Hmisc::describe(df[df$journal_id==0,]$rel_rep)
# PS 195 comparisons 
# PS NA 92
# Range: 0.53 - 0.97
# Mean: 0.85
# Median: 0.86
Hmisc::describe(df[df$journal_id==1,]$rel_rep)

### Reliability estimate per group
# Make numeric
df$rel1_rep <- as.numeric(df$rel1_rep)
# 25 comparisons indicate sample size per group (2.7%)
# PLOS 11 (1.7%)
# PS 14 (4.9%)
count.var(df,"rel1_rep", dichotomous = FALSE)
# NA 904
Hmisc::describe(df$rel1_rep)

### Correlation reliability and sample size
# r = 0.35
cor(df$no_items, df$rel_rep, use = "complete.obs")

### Power
# Check how often power is missing
df.power <- df[which(df$power == "NA"),]
# 625 comparisons do not mention power (67 %)
nrow(df.power); round(nrow(df.power) / nrow(df) * 100,1)
# 59 articles do not mention power (62 %)
# PLOS: 37 articles (74 %)
# PS: 22 articles (48 %)
count.articles(df.power)
round(count.articles(df.power) / count.articles(df) * 100,1)
# 76 studies do not mention power (51 %)
# PLOS: 40 studies (76 %)
# PS: 36 studies (38 %)
count.studies(df.power)
round(count.studies(df.power) / count.studies(df) * 100,1)

# Measurement Invariance --------------------------------------------------
### MI test not reported
# 890 comparisons do not report MI test (96 %)
# PLOS: 608 (95 %)
# PS: 282 (98 %)
count.var(df,"mitest_rep",0)
# Subset of studies that do not report on MI
df.nmi <- df[df$mitest_rep == 0,]
# 91 articles
# PLOS: 46 articles
# PS: 45 articles
count.articles(df.nmi)

### MI test reported
# 39 comparisons report MI test (4 %)
# PLOS: 34 (5 %)
# PS: 5 (2 %)
count.var(df,"mitest_rep",1)

# Subset of studies that report on MI
df.mi <- df[df$mitest_rep == 1,]
# 39 comparisons reported MI in 6 articles
# PLOS: 4 articles
# PS: 2 articles
count.articles(df.mi)
# 39 comparisons reported MI in 6 studies
# PLOS: 4 studies
# PS: 2 studies
count.studies(df.mi)


### MI method reported (1 = scale-based; 2 = item-based)
# Make numeric
df$mimethod_rep <- as.numeric(df$mimethod_rep)
# 39 comparisons report MI method (4 %)
# PLOS: 34 (5 %)
# PS: 45 (2 %)
count.var(df,"mimethod_rep", dichotomous = FALSE)
# NA: 890 (96 %)
# Scale-based: 9 (23%)
# Item-based: 30 (77%)
Hmisc::describe(df$mimethod_rep)
# PLOS: 34 comparisons
# NA: 608 comparisons
# Scale-based: 4 (12%)
# Item-based: 30 (88%)
Hmisc::describe(df[df$journal_id == 0,]$mimethod_rep)
# PS: 5 comparisons
# NA: 282 comparisons
# Scale-based: 5
# Item-based: 0 
Hmisc::describe(df[df$journal_id == 1,]$mimethod_rep)

### MI result reported
# 39 comparisons report MI result
# MI holds: 30 comparisons 
# MI does not hold: 9 comparisons
count.var(df,"miresult_rep", dichotomous = FALSE)
sum(df$miresult_rep, na.rm =TRUE)
# PLOS: 34 comparisons report MI result
# MI holds: 25 comparisons 
# MI does not hold: 9 comparisons
sum(df[df$journal_id==0,]$miresult_rep, na.rm =TRUE)
# PS: 5 comparisons report MI result
# MI holds: 5 comparisons 
# MI does not hold: 0 comparisons
sum(df[df$journal_id==1,]$miresult_rep, na.rm =TRUE)

### MI level reported (0 = none, 1 = config, 2 = metric, 3 = scalar, 4 = residual, 5 = partial, 99 = other, NA = not applicable)
# Make numeric
df$milevel_rep <- as.numeric(df$milevel_rep)
# Metric invariance (score = 2): 5 comparisons
# Scalar invariance (score = 3): 3 comparisons
# Partial invariance (score = 5): 1 comparison
# Other (score = 99): 30 comparisons
Hmisc::describe(df$milevel_rep)
# PLOS: 34 comparisons
# Metric invariance (score = 2): 1 comparisons
# Scalar invariance (score = 3): 2 comparisons
# Partial invariance (score = 5): 1 comparison
# Other (score = 99: 30 comparisons
Hmisc::describe(df[df$journal_id == 0,]$milevel_rep)
# PS: 5 comparisons
# Metric invariance (score = 2): 4 comparisons
# Scalar invariance (score = 3): 1 comparisons
Hmisc::describe(df[df$journal_id == 1,]$milevel_rep)

# Investigate the partial invariance study by id
unique(df[df$milevel_rep == 5,]$row_id)
# 80: partial invariance here is partial strong invariance 
# (i.e., all but one item is scalar invariant). The authors state: 
# "Nested models of longitudinal invariance demonstrated the existence 
# of partial strong invariance over time. In other words, this indicates
# that there is an equivalence of the factorial structure and factor 
# loadings for all items; this was also observed for the item intercepts 
# for all the items, except for one of the items from the EXEC dimension."

# Investigate the Other studies by id
unique(df[df$milevel_rep == 99,]$row_id)
# There is DIF reported for 8 comparisons:
# 429: MIMIC; DIF at item level for 10 items in 2 subscales; so no measurement invariance
# 431: MIMIC; DIF at item level for 10 items in 2 subscales; so no measurement invariance
# 432: MIMIC; DIF at item level for 10 items in 2 subscales; so no measurement invariance
# 433: MIMIC; DIF at item level for 10 items in 2 subscales; so no measurement invariance
# 434: MIMIC; DIF at item level for 10 items in 2 subscales; so no measurement invariance
# 435: MIMIC; DIF at item level for 10 items in 2 subscales; so no measurement invariance
# 439: MIMIC; DIF at item level for 10 items in 2 subscales; so no measurement invariance
# 440: MIMIC; DIF at item level for 10 items in 2 subscales; so no measurement invariance

# Interrater reliability --------------------------------------------------
# Empirical study; 0.77
table(irr$empirical_d,irr$empirical_e)
empirical.irr <- cbind(irr$empirical_d,irr$empirical_e)
cohen.kappa(empirical.irr) 

# Compare group; 0.32
table(irr$comparegroup_d,irr$comparegroup_e)
comparegroup.irr <- cbind(irr$comparegroup_d,irr$comparegroup_e)
cohen.kappa(comparegroup.irr)

# Reflective/SCM; 0.95
table(irr$reflective_d,irr$reflective_e)
reflective.irr <- cbind(irr$reflective_d,irr$reflective_e)
cohen.kappa(reflective.irr)

# MI test; 1
table(irr$mirep_d,irr$mirep_e)
mitest.irr <- cbind(irr$mirep_d,irr$mirep_e)
cohen.kappa(mitest.irr)

# MI result; 1
table(irr$mires_d,irr$mires_e)
mires.irr <- cbind(irr$mires_d,irr$mires_e)
cohen.kappa(mires.irr)

# As the categories in the MI level variable are ordered but the distance between the levels
# is not the same, we chose not to calculate the IRR for this variable. 

# Results for table -------------------------------------------------------
### Validity evidence
### Citation to previous study reported
# Comparisons: 743 (80%)
# PLOS: 589 (92 %)
# PS: 154 (54 %)
count.var(df,"reference",1)
### Psychometric properties reported
# Comparisons: 118 (13 %)
# PLOS: 106 (17 %)
# PS: 12 (4 %)
count.var(df,"psychometric",1)
### Internal consistency reported
# Comparisons: 435 (47 %)
# PLOS: 233 (36 %)
# PS: 202 (70 %)
count.var(df,"consistency",1)

# Select studies for step 2 -----------------------------------------------

# Filter out studies that reported on doing a measurement invariance check
df.checked <- subset(df,mitest_rep == 1) 

# Save dataset of studies that investigated MI (step 2 and step 3)
write_xlsx(df.checked, "data/codebook-main-step2step3-sample-without-results.xlsx")
