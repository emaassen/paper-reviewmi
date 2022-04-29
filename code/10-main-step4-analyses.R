### CODE FOR MAIN STUDY SYSTEMATIC REVIEW MEASUREMENT INVARIANCE ##
### This is code to analyze step 4 of our main study, in which we 
### conducted MI tests for comparisons that did not report on MI

rm(list = ls())     # clear workspace
options(scipen=999) # no scientific notation
library(readxl)     # load in the coding sheet of step 4
library(dplyr)      # for function to count the total number of studies

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

# load in data file
df <- read_excel("../data/codebook-main-step4.xlsx")
nrow(df)                                # we attempt to reproduce 294 comparisons
count_articles(df)                      # across 35 articles
count_studies(df)                       # across 65 studies

# Make certain columns numeric so we can use them in subsequent steps
df$open_group <- as.numeric(df$open_group)
df$open_scale <- as.numeric(df$open_scale)
df$miresult_step4 <- as.numeric(df$miresult_step4)
df$config_assumed <- as.numeric(df$config_assumed)

# Comparisons for which we could access the data
df1 <- filter(df, open_data == 1)       # filter out comparisons that had open data
nrow(df1)                               # 225 comparisons
nrow(df1) / nrow(df) * 100              # 77 % of comparisons had open data
count_articles(df1)                     # 32 articles had open data
count_studies(df1)                      # 61 studies had open data

# Comparisons for which we could make a grouping variable
df2 <- filter(df, open_group == 1)       # filter out comparisons that had open group
nrow(df2)                                # 217 comparisons
nrow(df2) / nrow(df) * 100               # 74 % of comparisons had open group
count_articles(df2)                      # 30 articles had open group
count_studies(df2)                       # 59 studies had open group

# Comparisons for which we could make a scaling variable
df3 <- filter(df, open_scale == 1)        # filter out comparisons that had open scale
nrow(df3)                                 # 158 comparisons
nrow(df3) / nrow(df) * 100                # 54% of comparisons had open scale
count_articles(df3)                       # 27 articles had open scale
count_studies(df3)                        # 56 studies had open scale

# Comparisons for which we could make a group & scaling variable
df4 <- filter(df, open_scale == 1  & open_group == 1)     # filter out comparisons that had open scale & open group
nrow(df4)                                                 # 157 comparisons
nrow(df4) / nrow(df) * 100                                # 53% of comparisons had open scale
count_articles(df4)                                       # 26 articles had open scale
count_studies(df4)                                        # 55 studies had open scale

# Comparisons for which we could do a MI test
df5 <- filter(df, mitest_step4 == 1)                      # filter out comparisons that could do a MI test
nrow(df5)                                                 # 157 comparisons
nrow(df5) / nrow(df) * 100                                # 53% of comparisons could to MI test
count_articles(df5)                                       # 26 articles could to MI test
count_studies(df5)                                        # 55 studies could to MI test

# Comparisons for which MI held
df6 <- filter(df, miresult_step4 == 1)                    # filter out comparisons that found MI
nrow(df6)                                                 # 72 comparisons
nrow(df6) / nrow(df) * 100                                # 24% of comparisons MI held
count_articles(df6)                                       # 15 articles MI held
count_studies(df6)                                        # 26 studies MI held

# Comparisons for which MI did not hold
df7 <- filter(df, miresult_step4 != 1)                    # filter out comparisons that did not find MI
nrow(df7)                                                 # 85 comparisons did not find MI
nrow(df7) / nrow(df) * 100                                # 28% of comparisons did not find MI
count_articles(df7)                                       # 19 articles did not find MI
count_studies(df7)                                        # 41 studies did not find MI

# Comparisons for which MI hold + level 
mi_level_yes <- table(df6$milevel_step4)                  # configural = 1, metric = 2, scalar = 3
mi_level_yes                                              # 13 configural, 13 metric, 46 scalar
mi_level_yes/nrow(df6)*100                                # 18% configural, 18% metric, 64% scalar

# How many of the comparisons for which a level of MI held had more than 3 items?
sum(df6$config_assumed == 0, na.rm=T)                     # 11 comparisons had more than 3 items

# Subset of comparisons for which configural invariance was assumed because we had 3 items.
df_conf_assumed <- subset(df, df$config_assumed == 1)
nrow(df_conf_assumed)                                           # 61 comparisons had 3 items
table(df_conf_assumed$milevel_step4)                            # configural = 11, metric = 12, scalar = 38
table(df_conf_assumed$milevel_step4)/nrow(df_conf_assumed)*100  # 18% configural, 20% metric, 62% scalar
count_articles(df_conf_assumed)                                 # 13 articles
count_studies(df_conf_assumed)                                  # 22 studies

# Subset: how many comparisons found a level of measurement invariance?
sum(df_conf_assumed$miresult_step4)                                   # 61 comparisons
sum(df_conf_assumed$miresult_step4) / nrow(df_conf_assumed) * 100     # 100% found a level of MI. 

table(df_conf_assumed$milevel_step4)                            # 11 configural, 12 metric, 38 scalar
table(df_conf_assumed$milevel_step4)/nrow(df_conf_assumed)*100  # 18% configural, 20% metric, 64% scalar

# Check how many scales with scalar invariance are unique
df_scalar <- subset(df, as.numeric(df$milevel_step4) == 3)
length(df_scalar)
df_scalar$name_scale
unique(df_scalar$name_scale)
length(unique(df_scalar$name_scale))

# How many scalar comparisons belonged to PLOS and PS
length(which(df_scalar$journal_id == 0)) # 0 to PLOS
length(which(df_scalar$journal_id == 1)) # 46 to PS

# how many articles and studies found scalar invariance?
count_articles(df_scalar) # 14 articles
count_studies(df_scalar) # 21 studies

#------------------------------------------------------------------------------------------------------#
#Exclusion of articles prior to final analyses per journal
dfPLOS <- subset(df, df$journal_id == 0)
dfPS <- subset(df, df$journal_id == 1)

#How many comparisons had enough power?
nrow(dfPLOS) #141
nrow(dfPS) #153

#How many comparisons had data availbale?
sum(as.numeric(dfPLOS$open_data), na.rm = T) #73
sum(as.numeric(dfPS$open_data), na.rm = T) #152

#For how many, of those that were available, could we construct a grouping variable?
sum(as.numeric(dfPLOS$open_group), na.rm = T) #65
sum(as.numeric(dfPS$open_group), na.rm = T) #152

#For how many, of those that were available, could we construct a scale?
sum(as.numeric(dfPLOS$open_scale), na.rm = T) #10
sum(as.numeric(dfPS$open_scale), na.rm = T) #148

#For how many, of those that were available, could we construct a grouping variable and a scale?
length(which(as.numeric(dfPLOS$open_scale) == 1 & as.numeric(dfPLOS$open_group) == 1)) #9
length(which(as.numeric(dfPS$open_scale) == 1 & as.numeric(dfPS$open_group) == 1)) #148

