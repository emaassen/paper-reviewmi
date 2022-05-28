### CODE FOR MAIN STUDY SYSTEMATIC REVIEW MEASUREMENT INVARIANCE ##
### This is code to analyze step 2+3 of our main study
rm(list = ls())       # clear workspace
options(scipen=999)   # no scientific notation
require("readxl")     # load in the coding sheet of step 4
require("dplyr")      # for function to count the total number of studies

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
df <- read_excel("../data/codebook-main-step2step3.xlsx")

# Make certain columns numeric so we can use them in subsequent steps
df$measure_scale <- as.numeric(df$measure_scale)
df$width_scale_recoded <- as.numeric(df$width_scale_recoded)
df$no_items <- as.numeric(df$no_items)
df$n_rep <- as.numeric(df$n_rep)
df$mimethod_rep <- as.numeric(df$mimethod_rep)
df$miresult_step2 <- as.numeric(df$miresult_step2)
df$milevel_rep <- as.numeric(df$milevel_rep)
df$open_group <- as.numeric(df$open_group)
df$open_scale <- as.numeric(df$open_scale)
df$n_res <-as.numeric(df$n_res)

# count comparisons, articles, studies
nrow(df)                                # we attempt to reproduce 41 comparisons
count_articles(df)                      # across 7 articles
count_studies(df)                       # across 7 studies

# Comparisons for which we could access the data
df1 <- filter(df, open_data == 1)       # filter out comparisons that had open data
nrow(df1)                               # 6 comparisons
nrow(df1) / nrow(df) * 100              # 15 % of comparisons had open data
count_articles(df1)                     # 5 articles had open data
count_studies(df1)                      # 5 studies had open data

# Comparisons for which we could make a grouping variable
df2 <- filter(df, open_group == 1)       # filter out comparisons that had open group
nrow(df2)                                # 6 comparisons
nrow(df2) / nrow(df) * 100               # 15 % of comparisons had open group
count_articles(df2)                      # 5 articles had open group
count_studies(df2)                       # 5 studies had open group

# Comparisons for which we could make a scaling variable
df3 <- filter(df, open_scale == 1)        # filter out comparisons that had open scale
nrow(df3)                                 # 5 comparisons
nrow(df3) / nrow(df) * 100                # 12% of comparisons had open scale
count_articles(df3)                       # 4 articles had open scale
count_studies(df3)                        # 4 studies had open scale

# Comparisons for which we could make a group & scaling variable
df4 <- filter(df, open_scale == 1  & open_group == 1)     # filter out comparisons that had open scale & open group
nrow(df4)                                                 # 5 comparisons
nrow(df4) / nrow(df) * 100                                # 12% of comparisons had open scale
count_articles(df4)                                       # 4 articles had open scale
count_studies(df4)                                        # 4 studies had open scale

# Comparisons for which we could do a MI test
df5 <- filter(df, mitest_step2 == 1)                      # filter out comparisons that could do a MI test
nrow(df5)                                                 # 5 comparisons
nrow(df5) / nrow(df) * 100                                # 12% of comparisons could to MI test
count_articles(df5)                                       # 4 articles could to MI test
count_studies(df5)                                        # 4 studies could to MI test

# Comparisons for which MI held
df6 <- filter(df, miresult_step2 == 1)                    # filter out comparisons that found MI
nrow(df6)                                                 # 1 comparison
nrow(df6) / nrow(df) * 100                                # 2% of comparisons MI held
count_articles(df6)                                       # 1 article MI held
count_studies(df6)                                        # 1 study MI held

# Comparisons for which MI did not hold
df7 <- filter(df, miresult_step2 != 1)                    # filter out comparisons that did not find MI
nrow(df7)                                                 # 4 comparisons did not find MI
nrow(df7) / nrow(df) * 100                                # 10% of comparisons did not find MI
count_articles(df7)                                       # 3 articles did not find MI
count_studies(df7)                                        # 3 studies did not find MI

# Comparisons for which MI hold + level 
mi_level_yes <- table(df6$milevel_step2)                  
mi_level_yes                                              # partial = 1
mi_level_yes/nrow(df6)*100                                # 100% partial

# Comparisons that were reproducible
sum(df$reproduced_step2)                                    # 1 comparison could be reproduced in step 2
sum(df$reproduced_step2) / sum(df$mitest_step2 == 1) * 100  # 20% of comparisons could be reproduced in step 2
