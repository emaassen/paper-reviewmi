### CODE FOR MAIN STUDY SYSTEMATIC REVIEW MEASUREMENT INVARIANCE ##
### This is code to analyze the reporting results of our main study
rm(list=ls())        # clear workspace
options(scipen=999)  # no scientific notation
require("dplyr")     # for function to count the total number of studies
require("readxl")    # to load codebook
require("writexl")   # to write away final codebooks

# function to count the total number of articles and studies in a dataframe
count_articles <- function(x) {
  length(unique(x$article_id_recoded))
}

count_studies <- function(x) {
  df.temp <- x %>%
    distinct(article_id, study_recoded) %>%
    group_by(article_id) %>%
    summarize("studies" = n())
  no_studies <- sum(df.temp$studies) 
  return(no_studies)
}

df <- read_excel("../data/codebook-main-step1.xlsx")
df.final <- df # dataset to save
colnames(df) # check if column names are correct

# How many journals, articles, studies in total?
unique(df$journal_id)                             # 2 journals
n_art <- count_articles(df); n_art                # 426 articles
n_stu <- count_studies(df); n_stu                 # 841 studies
nrow(df)                                          # 1843 rows

# Quantitative data
df1 <- filter(df, empirical == 1)
count_articles(df1)                                    # 342 articles
count_articles(df1) / count_articles(df) * 100         # 80% of articles contain quantitative data
count_studies(df1)                                     # 748 studies
count_studies(df1) / count_studies(df) * 100           # 89% of studies contain quantitative data
nrow(df1)                                              # 1750 rows

# Articles can contain multiple studies and multiple comparisons, of which some can be quantitative and some not.
# Because we want to count the articles and studies that dropped out because there weren't ANY quantitative studies,
# we count the remaining number of articles and studies and subtract that from the original number or articles and studies
# to get an estimate of number of articles and studies that did not contain any quantitative data.

# Number of articles and studies that dropped out due to non-quantitative data
n_art - count_articles(df1)                  # 84 articles
(n_art - count_articles(df1)) / n_art * 100  # 20% of articles
n_stu - count_studies(df1)                   # 93 studies
(n_stu - count_studies(df1)) / n_stu * 100   # 11% of articles
  
# Group comparison 
df2 <- filter(df1, compare_group == 1)
count_articles(df2)                                                # 274 articles
count_articles(df2) / count_articles(df) * 100                     # 64% of articles contain group comparisons
count_studies(df2)                                                 # 615 studies
count_studies(df2) / count_studies(df) * 100                       # 73% of studies contain group comparisons
nrow(df2)                                                          # 1614 rows

# Number of articles and studies that dropped out due to non-group comparisons
count_articles(df1) - count_articles(df2)                 # 68 articles
(count_articles(df1) - count_articles(df2)) / n_art * 100 # 16% of articles
count_studies(df1) - count_studies(df2)                   # 133 studies
(count_studies(df1) - count_studies(df2)) / n_stu * 100   # 16% of studies

# Comparison made on scale 
df3 <- filter(df2, scale == 1)
count_articles(df3)                                    # 194 articles
count_articles(df3) / count_articles(df) * 100         # 46% of articles contain scales
count_studies(df3)                                     # 400 studies
count_studies(df3) / count_studies(df) * 100           # 48% of studies contain scales
nrow(df3)                                              # 1288 rows

# Number of articles and studies that dropped out due to non-scales
count_articles(df2) - count_articles(df3)                 # 80 articles
(count_articles(df2) - count_articles(df3)) / n_art * 100 # 19% of articles
count_studies(df2) - count_studies(df3)                   # 215 studies
(count_studies(df2) - count_studies(df3)) / n_stu * 100   # 26% of studies

# Comparison made on reflective scale 
df4 <- filter(df3, reflective == 1)
count_articles(df4)                                    # 97 articles
count_articles(df4) / count_articles(df) * 100         # 23% of articles contain scales
count_studies(df4)                                     # 150 studies
count_studies(df4) / count_studies(df) * 100           # 18% of studies contain scales
nrow(df4)                                              # 918 rows

# Number of articles and studies that dropped out due to non reflective scales
count_articles(df3) - count_articles(df4)                 # 97 articles
(count_articles(df3) - count_articles(df4)) / n_art * 100 # 23% of articles
count_studies(df3) - count_studies(df4)                   # 250 studies
(count_studies(df3) - count_studies(df4)) / n_stu * 100   # 30% of studies

# Delete rows from dataframe that do not have a comparison on a reflective scale
# This is our main unit of analysis
df <- filter(df4, reflective == 1)
count_articles(df)                                               # 97 articles left
count_studies(df)                                                # 150 studies left
nrow(df)                                                         # 918 comparisons

# Type of group comparison
sum(df$type_group == 0)                     # 334 comparisons across existing groups
sum(df$type_group == 0) / nrow(df) * 100    # 36% of comparisons are made across existing groups
sum(df$type_group == 1)                     # 584 comparisons across new groups
sum(df$type_group == 1) / nrow(df) * 100    # 64% of comparisons are made across new groups

# Type of scale
sum(df$type_scale == 0)                     # 447 ad hoc scales
sum(df$type_scale == 0) / nrow(df) * 100    # 49% of comparisons have ad hoc scales
sum(df$type_scale == 1)                     # 471 existing scales
sum(df$type_scale == 1) / nrow(df) * 100    # 51% of comparisons have existing scales

# Measure scale 
df$measure_scale <- as.numeric(df$measure_scale)       # make numeric
table(df$measure_scale)                                # dichotomous = 29 / ordinal = 309 / continuous = 265
sum(is.na(df$measure_scale))                           # no reporting on measure of scale = 315
sum(df$measure_scale==0, na.rm=T) / nrow(df) * 100     # 3% of scales have dichotomous items (2 categories)
sum(df$measure_scale==1, na.rm=T) / nrow(df) * 100     # 34% of scales have ordinal items (3 to 5 categories)
sum(df$measure_scale==2, na.rm=T) / nrow(df) * 100     # 29% of scales have continuous items (more than 5 categories)
sum(is.na(df$measure_scale)) / nrow(df) * 100          # 34% of scales did not indicate the measure of the scale

# Width scale
table(df$width_scale_recoded)                                 # note "mix" and "NA" categories will drop off when making this variable numeric:
df$width_scale_recoded <- as.numeric(df$width_scale_recoded)  # make numeric
sum(is.na(df$width_scale_recoded))                            # 320 scales did not indicate the width of the scale
sum(is.na(df$width_scale_recoded)) / nrow(df) * 100           # 35% of scales did not indicate the width of the scale
summary(as.numeric(df$width_scale_recoded))                   # NAs are 320 here because two scales had "mix" instead of a numeric value

# Number of items
table(df$no_items)                                     # note "mix" and "NA" categories will drop off when making this variable numeric:
df$no_items <- as.numeric(df$no_items)                 # make numeric
table(df$no_items) 
sum(is.na(df$no_items))                                # 299 scales did not indicate the number of items
sum(is.na(df$no_items)) / nrow(df) * 100               # 33% of scales did not indicate the number of items
summary(as.numeric(df$no_items))

# No number of items & no item response categories reported
sum(is.na(as.numeric(df$width_scale_recoded)) & is.na(as.numeric(df$no_items)))                   # 209 comparisons did not report number of items and number of item response categories
sum(is.na(as.numeric(df$width_scale_recoded)) & is.na(as.numeric(df$no_items))) / nrow(df) * 100  # 23%

# Power not mentioned
length(which(is.na(df$power))) + length(which(df$power == "NA"))                 # 629 comparisons do not mention power
(length(which(is.na(df$power))) + length(which(df$power == "NA")))/nrow(df)*100  # 69% of comparisons do not mention power

# Keep comparisons that do mention power
temp <- df[which(!df$power == "NA"),]                                         
count_articles(df) - count_articles(temp)                                 # 56 articles do not mention power
(count_articles(df) - count_articles(temp)) / count_articles(df) * 100    # 58% of remaining articles do not mention power
count_studies(df) - count_studies(temp)                                   # 77 studies do not mention power
(count_studies(df) - count_studies(temp)) / count_studies(df) * 100       # 51% of remaining studies do not mention power

# Power mentioned
count_articles(temp)                                                        # 41 articles mention power
count_studies(temp)                                                         # 73 studies mention power

# Sample size
sum(is.na(as.numeric(df$n_rep)) & is.na(as.numeric(df$n1_rep)) & is.na(as.numeric(df$n2_rep)) & is.na(as.numeric(df$n3_rep)) & is.na(as.numeric(df$n4_rep)) & is.na(as.numeric(df$n5_rep)))
# 45 did not report any sample size at all
sum(is.na(as.numeric(df$n_rep)) & is.na(as.numeric(df$n1_rep)) & is.na(as.numeric(df$n2_rep)) & is.na(as.numeric(df$n3_rep)) & is.na(as.numeric(df$n4_rep)) & is.na(as.numeric(df$n5_rep)))/nrow(df)*100
# 5% did not report any sample size at all

# Range of sample size
df$n_rep <- as.numeric(df$n_rep)
range(df$n_rep, na.rm=T) # 15 - 4393362
summary(df$n_rep) # mean = 29791, median= 400

# Sample size total
sum(is.na(df$n_rep))                   # 81 did not report total sample size
sum(is.na(df$n_rep)) / nrow(df) * 100  # 81 (9%) did not report total sample size

# Sample size per group
# temporarily delete the scales that do report total sample size
temp <- subset(df,!is.na(as.numeric(df$n_rep)))
# If the sample size for group 1 is not reported, we know there are no group sample sizes reported overall
sum(is.na(as.numeric(temp$n1_rep)))                   # 238 comparisons did not report subsample sizes
sum(is.na(as.numeric(temp$n1_rep))) / nrow(df) * 100  # 26% did not report subsample sizes

# Reliability
sum(is.na(as.numeric(df$reltot)) & is.na(as.numeric(df$rel1)) & is.na(as.numeric(df$rel2)) & is.na(as.numeric(df$rel3)) & is.na(as.numeric(df$rel4)) & is.na(as.numeric(df$rel5)))
# 539 did not report any reliability estimate at all
sum(is.na(as.numeric(df$reltot)) & is.na(as.numeric(df$rel1)) & is.na(as.numeric(df$rel2)) & is.na(as.numeric(df$rel3)) & is.na(as.numeric(df$rel4)) & is.na(as.numeric(df$rel5)))/nrow(df)*100
# 59% did not report any reliability estimate at all

# Reliability total
sum(is.na(as.numeric(df$reltot)))                   # 555 comparisons did not report total reliability
sum(is.na(as.numeric(df$reltot))) / nrow(df) * 100  # 60% did not report total reliability
summary(as.numeric(df$reltot))                      # mean = 0.8285, median = 0.8400

# Reliability per group
# temporarily delete the scales that do report total reliability
temp <- subset(df,!is.na(as.numeric(df$reltot)))
# If the reliability for group 1 is not reported, we know there are no subsample reliabilities reported
sum(is.na(as.numeric(temp$rel1)))                  # 353 did not report subsample reliabilities
sum(is.na(as.numeric(temp$rel1))) / nrow(df) * 100 # 38% did not report subsample reliabilities

# Range of reliability
df$reltot <- as.numeric(df$reltot)
range(df$reltot, na.rm=T) # 0.46 - 0.97
summary(df$reltot)

# MI tested
sum(df$mitest_rep)                  # 40 scales reported MI testing
table(df$mitest_rep)                # 0 = 878, 1 = 40
sum(df$mitest_rep) / nrow(df) * 100 # 40 (4%) did report on MI

# Only select rows that report on MI
dfmi <- subset(df,mitest_rep == 1)
N <- nrow(dfmi)
count_articles(dfmi) # 6
count_studies(dfmi)  # 6

# articles / studies / comparisons that drop out because MI not reported
count_articles(df4) - count_articles(df4[df4$mitest_rep == 1,])    # 91 articles
count_studies(df4) - count_studies(df4[df4$mitest_rep == 1,])      # 144 studies

# MI method
df$mimethod_rep <- as.numeric(df$mimethod_rep)          # make numeric
table(df$mimethod_rep, useNA="always")
sum(df$mimethod_rep==1, na.rm=T)/sum(df$mitest_rep)*100 # 25% of 40 use a scale-based method
sum(df$mimethod_rep==2, na.rm=T)/sum(df$mitest_rep)*100 # 75% of 40 use an item-based method

# MI result
df$miresult_rep <- as.numeric(df$miresult_rep)                                                   # make numeric
table(df$miresult_rep, useNA="always")
sum(df$miresult_rep==0, na.rm=T) / sum(df$miresult_rep == 0 | df$miresult_rep == 1, na.rm=T)*100 # 25%; 10 of 40 do not find MI
sum(df$miresult_rep==1, na.rm=T) / sum(df$miresult_rep == 0 | df$miresult_rep == 1, na.rm=T)*100 # 75%; 30 of 40 do find MI

# Subset those that tested for MI 
dftest <- subset(df,mitest_rep == 1)
table(dftest$milevel_rep)
count_articles(dftest); count_studies(dftest) # 6 articles and 6 studies, 40 comparisons

# Subset those that did not find any MI and those that did find MI
dftest3 <- subset(dftest,miresult_rep == 1)     
table(dftest3$milevel_rep)                      # 6 metric invariance, 3 scalar invariance, 22 MIMIC method
count_articles(dftest3); count_studies(dftest3) # 4 articles and 4 studies, 30 comparisons

dftest2 <- subset(dftest,miresult_rep == 0)       # 10 comparisons
table(dftest2$milevel_rep)                        # 1 metric, 1 partial invariance, 8 MIMIC models
count_articles(dftest2)                           # 3 articles 
count_studies(dftest2)                            # 3 studies

# MI level
dftest$milevel_rep <- as.numeric(dftest$milevel_rep)             # make numeric
table(dftest$milevel_rep, useNA="always")                        # 6 metric invariance, 3 scalar invariance, 1 partial invariance, 30 MIMIC

# of the MIMIC models, how many found DIF and how many did not?
sum(dftest[dftest$milevel_rep == 99,]$miresult_rep == 0)   # 8 found DIF
sum(dftest[dftest$milevel_rep == 99,]$miresult_rep == 1)   # 22 did not find DIF

# Investigate the partial invariance and MIMIC studies by id 
unique(dftest[dftest$milevel_rep == 5,]$row_id)
# 80: partial strong invariance (all but one item)
# 80: the authors state "Nested models of longitudinal invariance demonstrated the existence of partial strong invariance over time. 
# 80: In other words, this indicates that there is an equivalence of the factorial structure and factor loadings for all items; 
# 80: this was also observed for the item intercepts for all the items, except for one of the items from the EXEC dimension."
unique(dftest[dftest$milevel_rep == 99,]$row_id)
# 429: MIMIC; DIF at item level for 10 items in 2 subscales
# 431: MIMIC; DIF at item level for 10 items in 2 subscales
# 432: MIMIC; DIF at item level for 10 items in 2 subscales
# 433: MIMIC; DIF at item level for 10 items in 2 subscales
# 434: MIMIC; DIF at item level for 10 items in 2 subscales
# 435: MIMIC; DIF at item level for 10 items in 2 subscales
# 439: MIMIC; DIF at item level for 10 items in 2 subscales
# 440: MIMIC; DIF at item level for 10 items in 2 subscales


# Only select rows that have scalar invariance
#dfmi.scalar <- subset(df,milevel_rep == 3)
#N <- nrow(dfmi.scalar)

# How many articles studied?
#count_articles(dfmi.scalar) # 2
#count_studies(dfmi.scalar) # 2 

