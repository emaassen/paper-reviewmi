### THE DIRE DISREGARD OF MEASUREMENT INVARIANCE TESTING ###
### This is code to analyze pilot study data

# Install and load packages -----------------------------------------------
# Clear workspace
#rm(list=ls())

# No scientific notation
options(scipen=999) 

# Load excel files
library(readxl)     

# Analysis of Pilot Data --------------------------------------------------
# Load data
df <- read_excel("Projects/paper-reviewmi/data/codebook-pilot.xlsx") 

# How many unique articles and journals were studied?
# 60 articles, 3 journals
unique(df$paper_id)
unique(df$journal_id)

# Total number of entries in the df: 117
nrow(df)
N <- nrow(df)

# Count and percentage of empirical studies
# 112 empirical studies, making up 96% of all studies
sum(df$empirical)
sum(df$empirical)/N 
Ne <- sum(df$empirical)

# Among empirical studies, how many contain group comparisons?
# 85 comparisons, 76% of all entries in the dataframe.
sum(df$between_groups, na.rm = T)
sum(df$between_groups, na.rm = T)/Ne
Ng <- sum(df$between_groups, na.rm = T)

# How many constructed / existing groups across comparisons?
table(df$type_group)
# 71 comparisons across existing, 14 across constructed groups
# 84% of comparisons done across existing groups
sum(df$type_group, na.rm = T)/Ng

# Among group comparisons and all studies, how many use scales?
# 37 comparisons, 44% of group comparisons, 32% of all studies
sum(df$scale, na.rm = T)
sum(df$scale, na.rm = T)/Ng
sum(df$scale, na.rm = T)/N

# Among group comparisons and all studies, how many use reflective scales?
# 36 comparisons, 42% of group comparisons, 31% of all studies
sum(df$reflective, na.rm = T)
sum(df$reflective, na.rm = T)/Ng
sum(df$reflective, na.rm=T)/N
Ns <- sum(df$reflective, na.rm = T)

# How many and what percentage of scales are existing (not ad hoc)?
# 28 scales, 75% of all reflective scales
sum(df$type_scale == "existing", na.rm=T)
sum(df$type_scale == "existing", na.rm=T)/Ns

# Among comparisons with reflective scales, how many share their data?
# 30 comparisons, 83% of reflective scale comparisons
sum(df$open_data, na.rm=T)
sum(df$open_data, na.rm=T)/Ns
Nd <- sum(df$open_data, na.rm=T)

# For how many comparisons can we construct scales from the shared data?
# 15 comparisons, 50% of comparisons with shared data
sum(df$open_scale, na.rm=T)
sum(df$open_scale, na.rm=T)/Nd

# For how many comparisons can we construct groups from the shared data?
# 22 comparisons, 73% of comparisons with shared data
sum(df$open_group, na.rm=T)
sum(df$open_group, na.rm=T)/Nd

# For how many comparisons can we construct both scales and groups from the shared data?
# 15 comparisons, 50% of comparisons with shared data
sum(df$open_scale == 1 & df$open_group == 1, na.rm=T)
sum(df$open_scale == 1 & df$open_group == 1, na.rm=T)/Nd

# Among comparisons with reflective scales, how many report MI?
# 4 comparisons, 11% of comparisons with reflective scale 
sum(df$mi_rep, na.rm=T)
sum(df$mi_rep, na.rm=T)/Ns

# Among comparisons with reflective scales, how many test for MI?
# 1 study, 2% of comparisons with reflective scale 
sum(df$mitest_rep, na.rm=T)
sum(df$mitest_rep, na.rm=T)/Ns

# Among comparisons that test for MI, how many find support for MI?
# 1 study, 100% of comparisons testing for MI
sum(df$mitest_rep == 1 & df$miresult_rep == 1, na.rm=T)

# Of the comparisons with reflective scales and shared data, 
# how many have interpretable data?
# 27 comparisons, 90% of comparisons with reflective scales and shared data
sum(df$data_usability, na.rm=T)
sum(df$data_usability, na.rm=T)/Nd

# Among comparisons with reflective scales, how many can we run MI analysis on?
# 15 comparisons, 50% of comparisons with reflective scales and shared data
sum(df$mitest, na.rm=T)
sum(df$mitest, na.rm=T)/Nd

# What levels of MI are present?
# 11 comparisons no configural invariance, 4 comparisons not converged.
table(df$milevel)

# Among comparisons with reflective scales and data sharing, for how many do we find a MI result?
# 11 comparisons, 37% of comparisons with reflective scales and shared data
# All 11 are no configural invariance found. 
sum(df$miresult, na.rm=T)
sum(df$miresult, na.rm=T)/Nd

# How many comparisons have reproducible MI results?
# 0 studies
sum(df$reproduced, na.rm=T)

# How many comparisons that compare reflective scales across groups can be found within each journal?
# JDM: 10, PLOS: 19, PS: 7.
table(df$journal_id, df$reflective)
