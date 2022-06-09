### CODE FOR MAIN STUDY SYSTEMATIC REVIEW MEASUREMENT INVARIANCE ##
### This is code to analyze step 4 of our main study, in which we 
### conducted MI tests for comparisons that did not report on MI
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

# load in data file step 2+3 and step 4
df <- read_excel("../data/codebook-main-step4.xlsx")
step23 <- read_excel("../data/codebook-main-step2step3.xlsx")

# Make certain columns numeric so we can use them in subsequent steps
df$open_group <- as.numeric(df$open_group)
df$open_scale <- as.numeric(df$open_scale)
df$miresult_step4 <- as.numeric(df$miresult_step4)
df$milevel_step4 <- as.numeric(df$milevel_step4)
df$config_assumed <- as.numeric(df$config_assumed)

# count comparisons, articles, studies in step 4
nrow(df)                                # we attempt to test MI for 294 comparisons
count_articles(df)                      # across 35 articles
count_studies(df)                       # across 65 studies

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
nrow(df6) / nrow(df5) * 100                               # 46% of comparisons we tested had MI
count_articles(df6)                                       # 15 articles MI held
count_studies(df6)                                        # 26 studies MI held

# Comparisons for which MI did not hold
df7 <- filter(df, miresult_step4 != 1)                    # filter out comparisons that did not find MI
nrow(df7)                                                 # 85 comparisons did not find MI
nrow(df7) / nrow(df5) * 100                               # 54% of comparisons we tested had MI violations
count_articles(df7)                                       # 19 articles did not find MI
count_studies(df7)                                        # 41 studies did not find MI

# Levels of invariance 
mi_level_yes <- table(df5$milevel_step4)                  # nonconfigural = 85, configural = 13, metric = 13, scalar = 46
mi_level_yes                                              
mi_level_yes/nrow(df5)*100                                # of all tested variables, 54% nonconfigural, 18% configural, 18% metric, 29% scalar

# Subset by level of invariance to count articles and studies
config <- df6[df6$milevel_step4 == 1,]
metric <- df6[df6$milevel_step4 == 2,]
scalar <- df6[df6$milevel_step4 == 3,]
count_articles(config);count_studies(config) # 6 articles 10 studies
count_articles(metric);count_studies(metric) # 6 articles 8 studies
count_articles(scalar);count_studies(scalar) # 14 articles 21 studies

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
table(df_conf_assumed$milevel_step4)/nrow(df_conf_assumed)*100  # 18% configural, 20% metric, 62% scalar

# Check how many scales with scalar invariance are unique
df_scalar <- subset(df, as.numeric(df$milevel_step4) == 3)
length(df_scalar)
df_scalar$name_scale
unique(df_scalar$name_scale)
length(unique(df_scalar$name_scale))

# How many scalar comparisons belonged to PLOS and PS
length(which(df_scalar$journal_id == 0)) # 0 to PLOS
length(which(df_scalar$journal_id == 1)) # 46 to PS

# Comparisons including step 2 and step 3 ---------------------------------

# count comparisons, articles, studies in steps2-3-4
nrow(step23)                            # we attempted to reproduce 40 comparisons
count_articles(step23)                  # across 6 articles
count_studies(step23)                   # across 6 studies

# is there overlap in step2+3 and step4?
step23$article_id %in% df$article_id
df$article_id %in% step23$article_id

# count total number of articles, studies in step2+3+4
count_articles(df) + count_articles(step23)  # 41 articles
count_studies(df) + count_studies(step23)    # 71 studies
nrow(df) + nrow(step23)                      # 334 comparisons

# count total number of articles, studies in step2+3+4 that we could test for MI
dim(df[df$mitest_step4 == 1,])[1] + dim(step23[step23$mitest_step2 == 1,])[1] # 161 comparisons tested for MI across step 2-3-4

# tables for invariance levels across articles, studies in step2+3+4 for those we could test for MI 
step23$mitest_step3 <- as.numeric(step23$mitest_step3)    # make numeric
dft <- filter(df, mitest_step4 == 1)                      # filter out comparisons that could do a MI test
step23t <- filter(step23, mitest_step2 == 1) 

# MI level for step 2, 3, 4
table(step23t$milevel_step3)  # noninvariant = 4
table(dft$milevel_step4)      # noninvariant = 85, configural = 13, metric = 13, scalar = 46
table(step23t$milevel_step3)[1] + table(dft$milevel_step4)[1] # noninvariant 89

# count how many studies and articles per level of invariance
noncon <- dft[dft$milevel_step4 == 0,];nrow(noncon);count_articles(noncon);count_studies(noncon) # c = 85, k = 19, s = 41
con <- dft[dft$milevel_step4 == 1,];nrow(con);count_articles(con);count_studies(con) # c = 13, k = 6,  s = 10
met <- dft[dft$milevel_step4 == 2,];nrow(met);count_articles(met);count_studies(met) # c = 13, k  = 6, s = 8
scal <- dft[dft$milevel_step4 == 3,];nrow(scal);count_articles(scal);count_studies(scal) # c = 46, k = 14, s = 21

# make new table
tab <- matrix(c(89,13,13,46), ncol=4)
colnames(tab) <- c("Noninvariant","Configural","Metric","Scalar")
rownames(tab) <- "freq"
tab <- as.table(tab);tab       # 89 noninvariant, 13 configural, 13 metric, 46 scalar
round(tab / sum(tab) * 100,1)  # 55% noninvariant, 8% configural, 8% metric, 29% scalar

# how many articles and studies found scalar invariance?
#count_articles(df_scalar) # 14 articles
#count_studies(df_scalar) # 21 studies

# level of invariance categorized by type of group
step23$milevel_step3 <- as.numeric(step23$milevel_step3)         # make numeric

# assign levels of invariance
df$milevel_step4 <- c("non-invariance","configural","metric","scalar",NA)[ match( df$milevel_step4, c(0,1,2,3,NA))]
step23$milevel_step3 <- c("non-invariance","configural","metric","scalar",NA)[ match( step23$milevel_step3, c(0,1,2,3,NA))]

# combine data
cat_group <- c(step23$cat_group,df$cat_group)
type_scale <- c(step23$type_scale,df$type_scale)
mi_level <- c(step23$milevel_step3,df$milevel_step4)
step234 <- as.data.frame(cbind(cat_group,type_scale,mi_level))
step234

# detailed categories
table(step234$cat_group,step234$mi_level)[,c(3,1,2,4)]
addmargins(table(step234$cat_group,step234$mi_level)[,c(3,1,2,4)])

# collapsed categories
step234$cat_group[grepl("dem",step234$cat_group)] <- "demographic"
step234$cat_group[grepl("exp",step234$cat_group)] <- "experimental"
table(step234$cat_group,step234$mi_level)[,c(3,1,2,4)]
addmargins(table(step234$cat_group,step234$mi_level)[,c(3,1,2,4)])

# level of invariance categorized by type of scale
step234$type_scale <- c("existing","ad hoc")[ match(step234$type_scale, c(0,1))]
table(step234$type_scale,step234$mi_level)[,c(3,1,2,4)]
addmargins(table(step234$type_scale,step234$mi_level)[,c(3,1,2,4)])

