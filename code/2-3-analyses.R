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
df$milevel_step2 <- as.numeric(df$milevel_step2)
df$milevel_rep <- as.numeric(df$milevel_rep)
df$open_group <- as.numeric(df$open_group)
df$open_scale <- as.numeric(df$open_scale)
df$n_res <-as.numeric(df$n_res)

# count comparisons, articles, studies
nrow(df)                                # we attempt to reproduce 40 comparisons
count_articles(df)                      # across 6 articles
count_studies(df)                       # across 6 studies

# Comparisons for which we could access the data
df1 <- filter(df, open_data == 1)       # filter out comparisons that had open data
nrow(df1)                               # 5 comparisons
nrow(df1) / nrow(df) * 100              # 13 % of comparisons had open data
count_articles(df1)                     # 4 articles had open data
count_studies(df1)                      # 4 studies had open data

# Comparisons for which we could make a grouping variable
df2 <- filter(df, open_group == 1)       # filter out comparisons that had open group
nrow(df2)                                # 5 comparisons
nrow(df2) / nrow(df) * 100               # 13 % of comparisons had open group
count_articles(df2)                      # 4 articles had open group
count_studies(df2)                       # 4 studies had open group

# Comparisons for which we could make a scaling variable
df3 <- filter(df, open_scale == 1)        # filter out comparisons that had open scale
nrow(df3)                                 # 4 comparisons
nrow(df3) / nrow(df) * 100                # 10% of comparisons had open scale
count_articles(df3)                       # 3 articles had open scale
count_studies(df3)                        # 3 studies had open scale

# Comparisons for which we could make a group & scaling variable
df4 <- filter(df, open_scale == 1  & open_group == 1)     # filter out comparisons that had open scale & open group
nrow(df4)                                                 # 4 comparisons
nrow(df4) / nrow(df) * 100                                # 10% of comparisons had open scale
count_articles(df4)                                       # 3 articles had open scale
count_studies(df4)                                        # 3 studies had open scale

# Comparisons for which we could do a MI test
df5 <- filter(df, mitest_step2 == 1)                      # filter out comparisons that could do a MI test
nrow(df5)                                                 # 4 comparisons
nrow(df5) / nrow(df) * 100                                # 10% of comparisons could to MI test
count_articles(df5)                                       # 3 articles could to MI test
count_studies(df5)                                        # 3 studies could to MI test

# Comparisons for which MI held
df6 <- filter(df, miresult_step2 == 1)                    # filter out comparisons that found MI
nrow(df6)                                                 # 0 comparison
nrow(df6) / nrow(df) * 100                                # 0% of comparisons MI held

# Comparisons for which MI did not hold
df7 <- filter(df, miresult_step2 != 1)                    # filter out comparisons that did not find MI
nrow(df7)                                                 # 4 comparisons did not find MI
nrow(df7) / nrow(df) * 100                                # 10% of comparisons did not find MI
count_articles(df7)                                       # 3 articles did not find MI
count_studies(df7)                                        # 3 studies did not find MI

# Comparisons that were reproducible
sum(df$reproduced_step2)                                  # 0 comparisons could be reproduced in step 2


# Results separated by journal --------------------------------------------
df <- read_excel("../data/codebook-main-step2step3.xlsx")

# Make certain columns numeric so we can use them in subsequent steps
df$measure_scale <- as.numeric(df$measure_scale)
df$width_scale_recoded <- as.numeric(df$width_scale_recoded)
df$no_items <- as.numeric(df$no_items)
df$n_rep <- as.numeric(df$n_rep)
df$mimethod_rep <- as.numeric(df$mimethod_rep)
df$miresult_step2 <- as.numeric(df$miresult_step2)
df$milevel_step2 <- as.numeric(df$milevel_step2)
df$milevel_rep <- as.numeric(df$milevel_rep)
df$open_group <- as.numeric(df$open_group)
df$open_scale <- as.numeric(df$open_scale)
df$n_res <-as.numeric(df$n_res)

# count comparisons, articles, studies
df0 <- filter(df, df$journal_id == 0)
df1 <- filter(df, df$journal_id == 1)

nrow(df0);nrow(df1)                      # we attempt to reproduce 34 + 6 comparisons
count_articles(df0);count_articles(df1)  # across 4 + 2 articles
count_studies(df0);count_studies(df1)    # across 4 + 2 studies

# Comparisons for which we could access the data
df00 <- filter(df0, open_data == 1)       # filter out comparisons that had open data
df11 <- filter(df1, open_data == 1)       # filter out comparisons that had open data
nrow(df00);nrow(df11)                     # 4+1 comparisons
count_articles(df00);count_articles(df11) # across 3 + 1 articles

# Comparisons for which we could make a grouping variable
df000 <- filter(df0, open_group == 1)       # filter out comparisons for which we could make a grouping var
df111 <- filter(df1, open_group == 1)       
nrow(df000);nrow(df111)                     # 4+1 comparisons

# Comparisons for which we could make a scaling variable
df0000 <- filter(df0, open_scale == 1)       # filter out comparisons for which we could make a scale var
df1111 <- filter(df1, open_scale == 1)       
nrow(df0000);nrow(df1111)                    # 3+1 comparisons

# Comparisons for which we could make a group & scaling variable
df0 <- filter(df0, open_scale == 1  & open_group == 1)     # filter out comparisons that had open scale & open group
df1 <- filter(df1, open_scale == 1  & open_group == 1)     # filter out comparisons that had open scale & open group
nrow(df0);nrow(df1)                                        # 3+1 comparisons

# Comparisons for which we could do a MI test
df0 <- filter(df0, mitest_step2 == 1)                      # filter out comparisons that could do a MI test
df1 <- filter(df1, mitest_step2 == 1)                      # filter out comparisons that could do a MI test
nrow(df0);nrow(df1)                                        # 3+1 comparisons
count_articles(df0);count_articles(df1)                    # across 2+1 articles

# Comparisons for which MI held
df0y <- filter(df0, miresult_step2 == 1)                    # filter out comparisons that found MI
df1y <- filter(df1, miresult_step2 == 1)                    # filter out comparisons that found MI
nrow(df0y);nrow(df1y)                                       # 0 comparison

# Comparisons for which MI did not hold
df0n <- filter(df0, miresult_step2 == 0)                    # filter out comparisons that did not find MI
df1n <- filter(df1, miresult_step2 == 0)                    # filter out comparisons that did not find MI
nrow(df0n);nrow(df1n)                                       # 3+1 comparison

# MI results
table(df0$milevel_step2)                                   # 3+1 configural non-invariant
table(df1$milevel_step2) 

# Comparisons that were reproducible
sum(df0$reproduced_step2)                                  # 0 comparisons could be reproduced in step 2
sum(df1$reproduced_step2)                                  # 0 comparisons could be reproduced in step 2


