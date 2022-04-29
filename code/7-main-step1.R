### CODE FOR MAIN STUDY SYSTEMATIC REVIEW MEASUREMENT INVARIANCE ##
### This is code to analyze the reporting results of our main study
rm(list = ls()) # clear workspace
options(scipen=999) # no scientific notation
library(dplyr) # for function to count the total number of studies

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

df <- read.csv("../data/codebook-main-step1.csv") # load data
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
(count_articles(df1) - count_articles(df2)) / n_art * 100 # 15% of articles
count_studies(df1) - count_studies(df2)                   # 133 studies
(count_studies(df1) - count_studies(df2)) / n_stu * 100   # 16% of studies

# Comparison made on scale 
df3 <- filter(df2, scale == 1)
count_articles(df3)                                    # 195 articles
count_articles(df3) / count_articles(df) * 100         # 46% of articles contain scales
count_studies(df3)                                     # 401 studies
count_studies(df3) / count_studies(df) * 100           # 48% of studies contain scales
nrow(df3)                                              # 1289 rows

# Number of articles and studies that dropped out due to non-scales
count_articles(df2) - count_articles(df3)                 # 79 articles
(count_articles(df2) - count_articles(df3)) / n_art * 100 # 19% of articles
count_studies(df2) - count_studies(df3)                   # 214 studies
(count_studies(df2) - count_studies(df3)) / n_stu * 100   # 25% of studies

# Comparison made on reflective scale 
df4 <- filter(df3, reflective == 1)
count_articles(df4)                                    # 96 articles
count_articles(df4) / count_articles(df) * 100         # 23% of articles contain scales
count_studies(df4)                                     # 149 studies
count_studies(df4) / count_studies(df) * 100           # 17% of studies contain scales
nrow(df4)                                              # 915 rows

# Number of articles and studies that dropped out due to non-scales
count_articles(df3) - count_articles(df4)                 # 99 articles
(count_articles(df3) - count_articles(df4)) / n_art * 100 # 23% of articles
count_studies(df3) - count_studies(df4)                   # 252 studies
(count_studies(df3) - count_studies(df4)) / n_stu * 100   # 30% of studies

# Delete rows from dataframe that do not have a comparison on a reflective scale
# This is our main unit of analysis
df <- filter(df4, reflective == 1)
count_articles(df)                                               # 96 articles left
count_studies(df)                                                # 149 studies left
count_studies(df) / count_studies(df) * 100                      # 18% of studies left 
nrow(df)                                                         # 915 comparisons

# Type of group comparison
sum(df$type_group == 0)                     # 348 comparisons across existing groups
sum(df$type_group == 0) / nrow(df) * 100    # 38% of comparisons are made across existing groups
sum(df$type_group == 1)                     # 567 comparisons across new groups
sum(df$type_group == 1) / nrow(df) * 100    # 62% of comparisons are made across new groups

# Type of scale
sum(df$type_scale == 0)                     # 447 ad hoc scales
sum(df$type_scale == 0) / nrow(df) * 100    # 49% of comparisons have ad hoc scales
sum(df$type_scale == 1)                     # 468 existing scales
sum(df$type_scale == 1) / nrow(df) * 100    # 51% of comparisons have existing scales

# Measure scale 
table(df$measure_scale)                                # dichotomous = 27 / ordinal = 309 / continuous = 265
sum(is.na(df$measure_scale))                           # no reporting on measure of scale = 314
sum(df$measure_scale==0, na.rm=T) / nrow(df) * 100     # 3% of scales have dichotomous items (2 categories)
sum(df$measure_scale==1, na.rm=T) / nrow(df) * 100     # 34% of scales have ordinal items (3 to 5 categories)
sum(df$measure_scale==2, na.rm=T) / nrow(df) * 100     # 29% of scales have continuous items (more than 5 categories)
sum(is.na(df$measure_scale)) / nrow(df) * 100          # 34% of scales did not indicate the measure of the scale

# Width scale
table(df$width_scale_recoded)
sum(is.na(df$width_scale_recoded))                     # 317 scales did not indicate the width of the scale
sum(is.na(df$width_scale_recoded)) / nrow(df) * 100    # 34% of scales did not indicate the width of the scale
summary(as.numeric(df$width_scale_recoded))            # NAs are 319 here because two scales had "mix" instead of a numeric value

# Number of items
table(df$no_items) 
sum(is.na(df$no_items))                                # 297 scales did not indicate the number of items
sum(is.na(df$no_items)) / nrow(df) * 100               # 32% of scales did not indicate the number of items
summary(as.numeric(df$no_items))

# Power not mentioned
length(which(is.na(df$power))) + length(which(df$power == "NA"))            # 627 comparisons do not mention power
(length(which(is.na(df$power))) + length(which(df$power == "NA")))/nrow(df) # 69% of comparisons do not mention power

# Keep comparisons that do mention power
temp <- df[which(!is.na(df$power)),]                                         
count_articles(df) - count_articles(temp)                                 # 55 articles do not mention power
(count_articles(df) - count_articles(temp)) / count_articles(df) * 100    # 57% of remaining 97 articles do not mention power
count_studies(df) - count_studies(temp)                                   # 76 studies do not mention power
(count_studies(df) - count_studies(temp)) / count_studies(df) * 100       # 51% of remaining 149 studies do not mention power

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
range(df$n_rep, na.rm=T)
summary(df$n_rep)

# Sample size total
sum(is.na(df$n_rep))                   # 81 did not report total sample size
sum(is.na(df$n_rep)) / nrow(df) * 100  # 81 (9%) did not report total sample size

# Sample size per group
# temporarily delete the scales that do report total sample size
temp <- subset(df,!is.na(as.numeric(df$n_rep)))
# If the sample size for group 1 is not reported, we know there are no group sample sizes reported overall
sum(is.na(as.numeric(temp$n1_rep)))                   # 237 comparisons did not report subsample sizes
sum(is.na(as.numeric(temp$n1_rep))) / nrow(df) * 100  # 26% did not report subsample sizes

# Reliability
sum(is.na(as.numeric(df$reltot)) & is.na(as.numeric(df$rel1)) & is.na(as.numeric(df$rel2)) & is.na(as.numeric(df$rel3)) & is.na(as.numeric(df$rel4)) & is.na(as.numeric(df$rel5)))
# 538 did not report any reliability estimate at all
sum(is.na(as.numeric(df$reltot)) & is.na(as.numeric(df$rel1)) & is.na(as.numeric(df$rel2)) & is.na(as.numeric(df$rel3)) & is.na(as.numeric(df$rel4)) & is.na(as.numeric(df$rel5)))/nrow(df)*100
# 59% did not report any reliability estimate at all

# Reliability total
sum(is.na(as.numeric(df$reltot)))                   # 552 comparisons did not report total reliability
sum(is.na(as.numeric(df$reltot))) / nrow(df) * 100  # 60% did not report total reliability
summary(as.numeric(df$reltot))

# Reliability per group
# temporarily delete the scales that do report total reliability
temp <- subset(df,!is.na(as.numeric(df$reltot)))
# If the reliability for group 1 is not reported, we know there are no subsample reliabilities reported
sum(is.na(as.numeric(temp$rel1)))                  # 353 did not report subsample reliabilities
sum(is.na(as.numeric(temp$rel1))) / nrow(df) * 100 # 39% did not report subsample reliabilities

# Range of reliability
df$reltot <- as.numeric(df$reltot)
range(df$reltot, na.rm=T)
summary(df$reltot)

# No number of items & no item response categories reported
sum(is.na(df$width_scale_recoded) & is.na(df$no_items))                   # 208 comparisons did not report number of items and number of item response categories
sum(is.na(df$width_scale_recoded) & is.na(df$no_items)) / nrow(df) * 100  # 23%

# MI tested
sum(df$mitest_rep)                  # 41 scales reported MI testing
table(df$mitest_rep)                # 0 = 874, 1 = 41
sum(df$mitest_rep) / nrow(df) * 100 # 41 (4.5%) did report on MI

# Only select rows that report on MI
dfmi <- subset(df,mitest_rep == 1)
N <- nrow(dfmi)
count_articles(dfmi) # 7
count_studies(dfmi)  # 7

# MI method
table(df$mimethod_rep, useNA="always")
sum(df$mimethod_rep==1, na.rm=T)/sum(df$mitest_rep)*100 # 24% of 40 use a scale-based method
sum(df$mimethod_rep==2, na.rm=T)/sum(df$mitest_rep)*100 # 73% of 40 use an item-based method

# MI result
table(df$miresult_rep, useNA="always")
sum(df$miresult_rep==0, na.rm=T) / sum(df$miresult_rep == 0 | df$miresult_rep == 1, na.rm=T)*100 # 24% of 41 do not find MI
sum(df$miresult_rep==1, na.rm=T) / sum(df$miresult_rep == 0 | df$miresult_rep == 1, na.rm=T)*100 # 76% of 41 do find MI

# MI level
df.mi <- subset(df, df$miresult_rep == 1)
table(df.mi$milevel_rep, useNA="always") # 2 = metric invariance, 3 = scalar invariance
sum(df.mi$milevel_rep==2, na.rm=T)/nrow(df.mi)*100 # 16% of 31 find metric invariance
sum(df.mi$milevel_rep==3, na.rm=T)/nrow(df.mi)*100 # 10% of 31 find scalar invariance
sum(is.na(df.mi$milevel_rep), na.rm=T)/nrow(df.mi)*100 # 74% of 31 do not mention the level of MI.

# Only select rows that have scalar invariance
dfmi.scalar <- subset(df,milevel_rep == 3)
N <- nrow(dfmi.scalar)

# How many articles studied?
count_articles(dfmi.scalar) # 2
count_studies(dfmi.scalar) # 2 
