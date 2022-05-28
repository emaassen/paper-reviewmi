### CODE FOR PILOT STUDY SYSTEMATIC REVIEW MEASUREMENT INVARIANCE ##
### This is code to analyze the reporting results of our pilot study

#rm(list = ls()) # clear workspace
options(scipen=999) # no scientific notation
library(readxl)

# Analysis of Pilot Data --------------------------------------------------
df <- read_excel("../data/codebook-pilot.xlsx") # load data 

# How many articles and journals studied?
unique(df$paper_id) # 60
unique(df$journal_id) # 3
nrow(df) #117 studies done in these 60 articles
N <- nrow(df)

# Empirical studies and % / 112, 96%
sum(df$empirical);sum(df$empirical)/N 
Ne <- sum(df$empirical)

# Of those empirical studies, how many do groups comparison? / 85, 76%
sum(df$between_groups, na.rm = T);sum(df$between_groups, na.rm = T)/Ne
Ng <- sum(df$between_groups, na.rm = T)

# Of those empirical studies that do group comparison, how many use existing groups? / 71, 84%
sum(df$type_group, na.rm = T);sum(df$type_group, na.rm = T)/Ng

# Of those empirical studies that compare groups (85), how many use scales/constructs? / 37, 44%
sum(df$scale, na.rm = T); sum(df$scale, na.rm = T)/Ng

# Of those empirical studies that compare groups (85), how many use reflective scales / 36, 42%
sum(df$reflective, na.rm = T); sum(df$reflective, na.rm = T)/Ng

# How many of those scales (37) were reflective? / 36, 97%
sum(df$reflective, na.rm=T); sum(df$reflective, na.rm=T)/Ng
Nfinal <- sum(df$reflective, na.rm=T)

# Of all studies we sampled, how many use scales? / 37, 32% of all 117 studies
sum(df$scale, na.rm = T); sum(df$scale, na.rm = T)/N
Ns <- sum(df$scale, na.rm = T)

# Of all studies we sampled, how many use reflective scales? / 36, 31% of all studies
sum(df$reflective, na.rm=T); sum(df$reflective, na.rm=T)/N

# How many of the scales (37) were existing (not ad hoc made scales)? / 28, 76%
sum(df$type_scale == "existing", na.rm=T); sum(df$type_scale == "existing", na.rm=T)/Ns

# How many of the studies that have reflective scales (36) share their data? / 30, 83%
sum(df$open_data, na.rm=T);sum(df$open_data,na.rm=T)/Nfinal
Nd <- sum(df$open_data,na.rm=T)

# From those that share their data, can we construct scales based on the data? / 16, 53%
sum(df$open_scale, na.rm=T);sum(df$open_scale, na.rm=T)/Nd

# From those that share their data, can we construct groups based on the data? / 22, 73%
sum(df$open_group, na.rm=T);sum(df$open_group, na.rm=T)/Nd

# From those that share their data, can we construct both scales and groups / 16, 53%
sum(df$open_scale == 1 & df$open_group == 1, na.rm=T)
sum(df$open_scale == 1 & df$open_group == 1, na.rm=T)/Nd

# From all studies that have a reflective scale (36), how many report MI? / 4, 11%
sum(df$mi_rep, na.rm=T); sum(df$mi_rep, na.rm=T)/Nfinal

# From all studies that have a reflective scale (36), how many test for MI? / 1, 2%
sum(df$mitest_rep, na.rm=T); sum(df$mitest_rep, na.rm=T)/Nfinal

# From all studies that test for MI, how many find MI? / 1, 100%
sum(df$mitest_rep == 1 & df$miresult_rep == 1, na.rm=T)

# From all studies that have reflective scales and share their data (30), 
# How many have interpretable data / 25, 83%
sum(df$data_usability, na.rm=T);sum(df$data_usability, na.rm=T)/Nd

# From all studies that have reflective scales share their data (30), for how many can we run MI analysis? / 16, 53%
sum(df$mitest, na.rm=T);sum(df$mitest, na.rm=T)/Nd

# What kind of MI holds?
table(df$milevel)

# From all studies that have reflective scales share their data, for how many do we find MI? / 11, 37%
sum(df$miresult, na.rm=T);sum(df$miresult, na.rm=T)/Nd

# From all studies that we checked MI (16), for how many do we find MI? / 11, 85%
sum(df$miresult, na.rm=T);sum(df$miresult, na.rm=T)/sum(df$mitest, na.rm=T)

# For how many studies do we reproduce their MI result? # 1
sum(df$reproduced, na.rm=T)

# Within Journals, how many studies found that compare reflective scales across groups?
table(df$journal_id,df$reflective)

