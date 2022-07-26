### CODE FOR MAIN STUDY SYSTEMATIC REVIEW MEASUREMENT INVARIANCE ##
### This is code to analyze step 1, the reporting results of the main study

#rm(list=ls())       # clear workspace
options(scipen=999)  # no scientific notation
require("dplyr")     # for function to count the total number of studies
require("readxl")    # to load codebook
require("writexl")   # to write away final codebooks
require("psych")     # interrater reliability

# function to count the total number of articles and studies and SCMs in a dataframe
count_articles <- function(x) {
  a <- length(unique(x$article_id_recoded))
  b <- length(unique(x[x$journal_id==0,]$article_id_recoded))
  c <- length(unique(x[x$journal_id==1,]$article_id_recoded))
  return(c("total"=a, "plos"=b, "ps"=c))
}

count_studies <- function(x) {
  
  df.temp <- x %>%
    distinct(article_id, study_recoded, journal_id) %>%
    group_by(article_id, journal_id) %>%
    summarize("studies" = n())
  
  a <- sum(df.temp$studies)
  b <- sum(df.temp[df.temp$journal_id == 0,]$studies)
  c <- sum(df.temp[df.temp$journal_id == 1,]$studies)
  return(c("total"=a, "plos"=b, "ps"=c))
}

count_scm <- function(x) {
  
  a <- nrow(x)
  b <- nrow(x[x$journal_id==0,])
  c <- nrow(x[x$journal_id==1,])
  return(c("total"=a, "plos"=b, "ps"=c))
  
}

df <- read_excel("../data/codebook-main-step1.xlsx")    # main codebook to analyze for step 1
irr <- read_excel("../data/interraterreliability.xlsx") # to calculate interrater reliability
df.raw <- df                                            # original dataset saved as a backup
colnames(df)                                            # check column names

# How many journals, articles, studies in total?
unique(df$journal_id)                             # 2 journals
n_art <- count_articles(df); n_art                # 426 articles, 213 plos, 213 ps
n_stu <- count_studies(df); n_stu                 # 841 studies, 237 plos, 604 ps
nrow(df)                                          # 1843 rows in total

# Quantitative data
df1 <- filter(df, empirical == 1)
count_articles(df1)                                    # 342 articles, 141 plos, 201 ps
round(count_articles(df1) / n_art * 100)               # 80% of articles, 66% plos, 94% ps
count_studies(df1)                                     # 748 studies, 160 plos, 588 ps
round(count_studies(df1) / n_stu * 100)                # 89% of studies, 68% plos, 97% ps

# Articles can contain multiple studies and multiple comparisons, of which some can be quantitative and some not.
# Because we want to count the articles and studies that dropped out because there weren't ANY quantitative studies,
# we count the remaining number of articles and studies and subtract that from the original number or articles and studies
# to get an estimate of number of articles and studies that did not contain any quantitative data.

# Number of articles and studies that dropped out due to non-quantitative data
n_art - count_articles(df1)                          # 84 articles, 72 plos, 12 ps
round((n_art - count_articles(df1)) / n_art * 100)   # 20% of articles, 34% plos, 6% ps
n_stu - count_studies(df1)                           # 93 studies, 77 plos, 16 ps
round((n_stu - count_studies(df1)) / n_stu * 100)    # 11% of studies, 32% plos, 3% ps
  
# Group comparison 
df2 <- filter(df1, compare_group == 1)
count_articles(df2)                                    # 274 articles, 107 plos, 167 ps
round(count_articles(df2) / n_art * 100)               # 64% of articles, 50% plos, 78% ps
count_studies(df2)                                     # 615 studies, 124 plos, 491 ps
round(count_studies(df2) / n_stu * 100)                # 73% of studies, 52% plos, 81% ps

# Number of articles and studies that dropped out due to no group comparisons
count_articles(df1) - count_articles(df2)                        # 68 articles, 34 plos, 34 ps
round((count_articles(df1) - count_articles(df2)) / n_art * 100) # 16% of articles, 16% plos, 16% ps
count_studies(df1) - count_studies(df2)                          # 133 studies, 36 plos, 97 ps
round((count_studies(df1) - count_studies(df2)) / n_stu * 100)   # 16% of studies, 15% plos, 16% ps

# Comparison made on scale 
df3 <- filter(df2, scale == 1)
count_articles(df3)                              # 194 articles, 70 plos, 124 ps
round(count_articles(df3) / n_art * 100)         # 46% of articles, 33% plos, 58% ps
count_studies(df3)                               # 400 studies, 84 plos, 316 ps
round(count_studies(df3) / n_stu * 100)          # 48% of studies, 35% plos, 52% ps

# Number of articles and studies that dropped out due to not containing scales
count_articles(df2) - count_articles(df3)                         # 80 articles, 37 plos, 43 ps
round((count_articles(df2) - count_articles(df3)) / n_art * 100)  # 19% of articles, 17% plos, 20% ps
count_studies(df2) - count_studies(df3)                           # 215 studies, 40 plos, 175 ps
round((count_studies(df2) - count_studies(df3)) / n_stu * 100)    # 26% of studies, 17% plos, 29% ps

# Comparison made on scale assuming a reflective measurement model
df4 <- filter(df3, reflective == 1)
count_articles(df4)                              # 97 articles, 51 plos, 46 ps
round(count_articles(df4) / n_art * 100)         # 23% of articles, 24% plos, 22% ps
count_studies(df4)                               # 150 studies, 54 plos, 96 ps
round(count_studies(df4) / n_stu * 100)          # 18% of studies, 23% plos, 16% ps

# Number of articles and studies that dropped out due to scales that couldn't be assumed to have an underlying reflective measurement model
count_articles(df3) - count_articles(df4)                        # 97 articles, 19 plos, 78 ps
round((count_articles(df3) - count_articles(df4)) / n_art * 100) # 23% of articles, 9% plos, 37% ps
count_studies(df3) - count_studies(df4)                          # 250 studies, 30 plos, 220 ps
round((count_studies(df3) - count_studies(df4)) / n_stu * 100)   # 30% of studies, 13% plos, 36% ps

# Delete rows from dataframe that do not have a comparison on a reflective scale
# The remaining data set will be our main unit of analysis
df.final <- filter(df4, reflective == 1)
count_articles(df.final)                             # 97 articles left, 51 plos, 46 ps
count_studies(df.final)                              # 150 studies left, 54 plos, 96 ps
n_scm <- count_scm(df.final); n_scm                  # 918 comparisons, 642 plos, 276 ps

# Type of group comparison (only for total sample, not per journal)
sum(df$type_group == 0)                                   # 334 comparisons across existing groups
round(sum(df$type_group == 0) / nrow(df.final) * 100)     # 36% of comparisons are made across existing groups
sum(df$type_group == 1)                                   # 584 comparisons across new groups
round(sum(df$type_group == 1) / nrow(df.final) * 100)     # 64% of comparisons are made across new groups

# Type of scale (only for total sample, not per journal)
sum(df$type_scale == 0)                            # 447 ad hoc scales
round(sum(df$type_scale == 0) / nrow(df) * 100)    # 49% of comparisons have ad hoc scales
sum(df$type_scale == 1)                            # 471 existing scales
round(sum(df$type_scale == 1) / nrow(df) * 100)    # 51% of comparisons have existing scales

# Measure scale (only for total sample, not per journal)
df$measure_scale <- as.numeric(df$measure_scale)               # make numeric
table(df$measure_scale)                                        # dichotomous = 29 / ordinal = 309 / continuous = 265
sum(is.na(df$measure_scale))                                   # no reporting on measure of scale = 315
round(sum(df$measure_scale==0, na.rm=T) / nrow(df) * 100)      # 3% of scales have dichotomous items (2 categories)
round(sum(df$measure_scale==1, na.rm=T) / nrow(df) * 100)      # 34% of scales have ordinal items (3 to 5 categories)
round(sum(df$measure_scale==2, na.rm=T) / nrow(df) * 100)      # 29% of scales have continuous items (more than 5 categories)
round(sum(is.na(df$measure_scale)) / nrow(df) * 100)           # 34% of scales did not indicate the measure of the scale

# Width scale (only for total sample, not per journal)
table(df$width_scale_recoded)                                 # note "mix" and "NA" categories will drop off when making this variable numeric:
df$width_scale_recoded <- as.numeric(df$width_scale_recoded)  # make numeric
sum(is.na(df$width_scale_recoded))                            # 320 scales did not indicate the width of the scale
round(sum(is.na(df$width_scale_recoded)) / nrow(df) * 100)    # 35% of scales did not indicate the width of the scale
summary(as.numeric(df$width_scale_recoded))                   # NAs are 320 here because two scales had "mix" instead of a numeric value. median = 5

# Number of items 
table(df$no_items)                                     # note "mix" and "NA" categories will drop off when making this variable numeric:
df$no_items <- as.numeric(df$no_items)                 # make numeric
table(df$no_items) 
sum(is.na(df$no_items))                                # 299 scales did not indicate the number of items
round(sum(is.na(df$no_items)) / nrow(df) * 100)        # 33% of scales did not indicate the number of items
summary(as.numeric(df$no_items))                       # median = 7

# Number of items per journal
table(df[df$journal_id==0,]$no_items) 
sum(is.na(df[df$journal_id==0,]$no_items))                                                   # 253 scales in PO did not indicate the number of items
round(sum(is.na(df[df$journal_id==0,]$no_items)) / nrow(df[df$journal_id==0,]) * 100)        # 39% of scales in PO did not indicate the number of items
summary(as.numeric(df[df$journal_id==0,]$no_items))

table(df[df$journal_id==1,]$no_items) 
sum(is.na(df[df$journal_id==1,]$no_items))                                                   # 46 scales in PO did not indicate the number of items
round(sum(is.na(df[df$journal_id==1,]$no_items)) / nrow(df[df$journal_id==1,]) * 100)        # 17% of scales in PO did not indicate the number of items
summary(as.numeric(df[df$journal_id==1,]$no_items))

# No number of items & no item response categories reported 
sum(is.na(as.numeric(df$width_scale_recoded)) & is.na(as.numeric(df$no_items)))                          # 209 comparisons did not report number of items and number of item response categories
round(sum(is.na(as.numeric(df$width_scale_recoded)) & is.na(as.numeric(df$no_items))) / nrow(df) * 100)  # 23%

# No number of items & no item response categories reported per journal
sum(is.na(as.numeric(df[df$journal_id==0,]$width_scale_recoded)) & is.na(as.numeric(df[df$journal_id==0,]$no_items)))                            
# 179 comparisons in PO did not report number of items and number of item response categories 
round(sum(is.na(as.numeric(df[df$journal_id==0,]$width_scale_recoded)) & is.na(as.numeric(df[df$journal_id==0,]$no_items))) / nrow(df[df$journal_id==0,])  * 100)  
# 28%

sum(is.na(as.numeric(df[df$journal_id==1,]$width_scale_recoded)) & is.na(as.numeric(df[df$journal_id==1,]$no_items)))                            
# 30 comparisons in PS did not report number of items and number of item response categories 
round(sum(is.na(as.numeric(df[df$journal_id==1,]$width_scale_recoded)) & is.na(as.numeric(df[df$journal_id==1,]$no_items))) / nrow(df[df$journal_id==1,])  * 100)  
# 11%

# Power not mentioned (only for total sample, not per journal)
length(which(is.na(df$power))) + length(which(df$power == "NA"))                        # 629 comparisons do not mention power
round((length(which(is.na(df$power))) + length(which(df$power == "NA")))/nrow(df)*100)  # 69% of comparisons do not mention power

# Keep comparisons that do mention power
temp <- df[which(!df$power == "NA"),]                                         
count_articles(df) - count_articles(temp)                                           # 56 articles do not mention power, 38 articles plos, 18 articles ps
round((count_articles(df) - count_articles(temp)) / count_articles(df.raw) * 100)   # 58% of remaining articles do not mention power, 75% plos, 39% ps
count_studies(df) - count_studies(temp)                                             # 77 studies do not mention power, 41 studies plos, 36 studies ps
round((count_studies(df) - count_studies(temp)) / count_studies(df) * 100)          # 51% of remaining studies do not mention power, 76% plos, 38% ps

# Power mentioned
count_articles(temp)                                                        # 41 articles mention power, 13 plos, 28 ps
count_studies(temp)                                                         # 73 studies mention power, 13 plos, 60 ps

# Sample size (only for total sample, not per journal)
sum(is.na(as.numeric(df$n_rep)) & is.na(as.numeric(df$n1_rep)) & is.na(as.numeric(df$n2_rep)) & is.na(as.numeric(df$n3_rep)) & is.na(as.numeric(df$n4_rep)) & is.na(as.numeric(df$n5_rep)))
# 45 did not report any sample size at all
round(sum(is.na(as.numeric(df$n_rep)) & is.na(as.numeric(df$n1_rep)) & is.na(as.numeric(df$n2_rep)) & is.na(as.numeric(df$n3_rep)) & is.na(as.numeric(df$n4_rep)) & is.na(as.numeric(df$n5_rep)))/nrow(df)*100)
# 5% did not report any sample size at all

# Range of sample size (only for total sample, not per journal)
df$n_rep <- as.numeric(df$n_rep)
range(df$n_rep, na.rm=T) # 15 - 4393362
summary(df$n_rep) # mean = 29791, median= 400

# Sample size not reported (only for total sample, not per journal)
sum(is.na(df$n_rep))                          # 81 did not report total sample size
round(sum(is.na(df$n_rep)) / nrow(df) * 100)  # 9% did not report total sample size

# Sample size not reported per group (only for total sample, not per journal)
# temporarily delete the scales that do report total sample size
temp <- subset(df,!is.na(as.numeric(df$n_rep)))
# If the sample size for group 1 is not reported, we know there are no group sample sizes reported overall
sum(is.na(as.numeric(temp$n1_rep)))                          # 238 comparisons did not report subsample sizes
round(sum(is.na(as.numeric(temp$n1_rep))) / nrow(df) * 100)  # 26% did not report subsample sizes

# Reliability (only for total sample, not per journal)
sum(is.na(as.numeric(df$reltot)) & is.na(as.numeric(df$rel1)) & is.na(as.numeric(df$rel2)) & is.na(as.numeric(df$rel3)) & is.na(as.numeric(df$rel4)) & is.na(as.numeric(df$rel5)))
# 539 did not report any reliability estimate at all
round(sum(is.na(as.numeric(df$reltot)) & is.na(as.numeric(df$rel1)) & is.na(as.numeric(df$rel2)) & is.na(as.numeric(df$rel3)) & is.na(as.numeric(df$rel4)) & is.na(as.numeric(df$rel5)))/nrow(df)*100)
# 59% did not report any reliability estimate at all

# Reliability not reported (only for total sample, not per journal)
sum(is.na(as.numeric(df$reltot)))                   # 555 comparisons did not report total reliability
sum(is.na(as.numeric(df$reltot))) / nrow(df) * 100  # 60% did not report total reliability
summary(as.numeric(df$reltot))                      # range 0.46 - 0.97, mean = 0.8285, median = 0.8400

# Reliability not reported per group (only for total sample, not per journal)
# temporarily delete the scales that do report total reliability
temp <- subset(df,!is.na(as.numeric(df$reltot)))
# If the reliability for group 1 is not reported, we know there are no subsample reliabilities reported
sum(is.na(as.numeric(temp$rel1)))                         # 353 did not report subsample reliabilities
round(sum(is.na(as.numeric(temp$rel1))) / nrow(df) * 100) # 38% did not report subsample reliabilities

round(cor(df$reltot,df$no_items, use = "complete.obs"),2) # correlation number of items and reliability estimate is r = 0.36

# MI tested (only for total sample, not per journal)
sum(df$mitest_rep)                         # 40 scales reported MI testing
table(df$mitest_rep)                       # 0 = 878, 1 = 40
round(sum(df$mitest_rep) / nrow(df) * 100) # 4% did report on MI

# Subset comparisons that report on MI
dfmi <- subset(df,mitest_rep == 1)
N <- nrow(dfmi)
count_articles(dfmi)                         # 6 articles report on mi, 4 plos, 2 ps
round(count_articles(dfmi) / n_art * 100)    # 1% of total, 2% plos, 1% ps
count_studies(dfmi)                          # 6 studies, 4 plos, 2 ps
round(count_studies(dfmi) / n_stu * 100)     # 1% of total, 2% plos, 0% ps
count_scm(dfmi)                              # 40 comparisons, 34 plos, 6 ps
round(count_scm(dfmi) / count_scm(df) * 100) # 4% of total SCMs, 5% plos, 2% ps

# MI method (only for total sample, not per journal)
df$mimethod_rep <- as.numeric(df$mimethod_rep)          # make numeric
table(df$mimethod_rep, useNA="always")
sum(df$mimethod_rep==1, na.rm=T)/sum(df$mitest_rep)*100 # 25%, 10 of 40 use a scale-based method
sum(df$mimethod_rep==2, na.rm=T)/sum(df$mitest_rep)*100 # 75%, 30 of 40 use an item-based method

# MI result (only for total sample, not per journal)
df$miresult_rep <- as.numeric(df$miresult_rep)                                                   # make numeric
table(df$miresult_rep, useNA="always")
sum(df$miresult_rep==0, na.rm=T) / sum(df$miresult_rep == 0 | df$miresult_rep == 1, na.rm=T)*100 # 22.5%; 9 of 40 do not find MI
sum(df$miresult_rep==1, na.rm=T) / sum(df$miresult_rep == 0 | df$miresult_rep == 1, na.rm=T)*100 # 77.5%; 31 of 40 do find MI

# Subset those that did not find any MI and those that did find MI
mi_yes <- subset(df,miresult_rep == 1)     
table(mi_yes$milevel_rep)                      # 6 metric invariance, 3 scalar invariance, 22 MIMIC method
count_articles(mi_yes)                         # 5 articles, 3 plos, 2 ps
count_studies(mi_yes)                          # 5 studies, 3 plos, 2 ps
count_scm(mi_yes)                              # 31 comparisons, 25 plos, 6 ps

mi_no <- subset(df,miresult_rep == 0)     
table(mi_no$milevel_rep)                      # 5 partial invariance, 8 MIMIC method
count_articles(mi_no)                         # 2 articles, 2 plos, 0 ps
count_studies(mi_no)                          # 2 studies, 2 plos, 0 ps
count_scm(mi_no)                              # 9 comparisons, 9 plos, 0 ps

# MI level
table(mi_yes$milevel_rep, useNA="always")     # 6 metric invariance, 3 scalar invariance, 22 MIMIC
table(mi_no$milevel_rep, useNA="always")      # 1 partial invariance, 8 MIMIC 

# Investigate the partial invariance and MIMIC studies by id 
unique(mi_no[mi_no$milevel_rep == "5.0",]$row_id)
# 80: partial invariance is partial strong invariance (all but one item is scalar invariant)
# 80: the authors state "Nested models of longitudinal invariance demonstrated the existence of partial strong invariance over time. 
# 80: In other words, this indicates that there is an equivalence of the factorial structure and factor loadings for all items; 
# 80: this was also observed for the item intercepts for all the items, except for one of the items from the EXEC dimension."
unique(mi_no[mi_no$milevel_rep == "99.0",]$row_id)
# 429: MIMIC; DIF at item level for 10 items in 2 subscales; so no measurement invariance
# 431: MIMIC; DIF at item level for 10 items in 2 subscales; so no measurement invariance
# 432: MIMIC; DIF at item level for 10 items in 2 subscales; so no measurement invariance
# 433: MIMIC; DIF at item level for 10 items in 2 subscales; so no measurement invariance
# 434: MIMIC; DIF at item level for 10 items in 2 subscales; so no measurement invariance
# 435: MIMIC; DIF at item level for 10 items in 2 subscales; so no measurement invariance
# 439: MIMIC; DIF at item level for 10 items in 2 subscales; so no measurement invariance
# 440: MIMIC; DIF at item level for 10 items in 2 subscales; so no measurement invariance


# Interrater reliability
# Empirical study
table(irr$empirical_d,irr$empirical_e)
empirical.irr <- cbind(irr$empirical_d,irr$empirical_e)
cohen.kappa(empirical.irr) # 0.77

# Compare group
table(irr$comparegroup_d,irr$comparegroup_e)
comparegroup.irr <- cbind(irr$comparegroup_d,irr$comparegroup_e)
cohen.kappa(comparegroup.irr) # 0.33

# Reflective/SCM
table(irr$reflective_d,irr$reflective_e)
reflective.irr <- cbind(irr$reflective_d,irr$reflective_e)
cohen.kappa(reflective.irr) # 0.95

# MI test
table(irr$mirep_d,irr$mirep_e)
mitest.irr <- cbind(irr$mirep_d,irr$mirep_e)
cohen.kappa(mitest.irr) # 1

# MI result
table(irr$mires_d,irr$mires_e)
# the NA values should not be included
irr$mires_d[irr$mires_d == "NA"] <- NA
irr$mires_e[irr$mires_e == "NA"] <- NA
table(dfd$miresult_rep,dfe$miresult_rep)
miresult.irr <- cbind(dfd$miresult_rep,dfe$miresult_rep)
cohen.kappa(mitest.irr) # 1

# As the categories in the MI level variable are ordered but the distance between the levels
# is not the same, we decided not to calculate the IRR for this variable. 

