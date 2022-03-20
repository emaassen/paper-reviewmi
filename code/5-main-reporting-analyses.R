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
colnames(df) # check if column names are correct

# How many journals, articles, studies in total?
unique(df$journal_id)                             # 2 journals
count_articles(df)                                # 426 articles
count_studies(df)                                 # 841 studies
nrow(df)                                          # 1845 rows

# Quantitative data
temp <- filter(df, empirical == 1)
count_articles(temp)                                                # 340 articles
count_articles(temp) / count_articles(df) * 100                     # 80% of articles contain empirical data
count_studies(temp)                                                 # 746 studies
count_studies(temp) / count_studies(df) * 100                       # 89% of studies contain empirical data
nrow(temp)                                                          # 1749 rows

# Non-quantitative data
count_studies(df) - count_studies(temp)                             # 95 studies contain non-empirical data
(count_studies(df) - count_studies(temp)) / count_studies(df) * 100 # 11% of studies contain non-empirical data

# Filter out non-quantitative data
df.final <- filter(df, empirical == 1)

# Group comparison in relation to total sample
temp <- filter(df, compare_group == 1)
count_articles(temp)                                                # 275 articles
count_articles(temp) / count_articles(df) * 100                     # 65% of articles contain group comparisons
count_studies(temp)                                                 # 617 studies
count_studies(temp) / count_studies(df) * 100                       # 73% of studies contain group comparisons
nrow(temp)                                                          # 1620 rows

# Non-group comparisons in relation to total sample
count_studies(df) - count_studies(temp)                             # 224 studies do not contain group comparisons
(count_studies(df) - count_studies(temp)) / count_studies(df) * 100 # 27% of studies do not contain group comparisons

# Group comparison in relation to final sample
temp <- filter(df, compare_group == 1)
count_articles(temp)                                                # 275 articles
count_articles(temp) / count_articles(df) * 100                     # 65% of articles contain group comparisons
count_studies(temp)                                                 # 617 studies
count_studies(temp) / count_studies(df) * 100                       # 73% of studies contain group comparisons
nrow(temp)                                                          # 1620 rows

# Non-group comparisons in relation to total sample
count_studies(df) - count_studies(temp)                             # 224 studies do not contain group comparisons
(count_studies(df) - count_studies(temp)) / count_studies(df) * 100 # 27% of studies do not contain group comparisons

# Filter out non-group comparisons
df.rest <- filter(df.final, compare_group == 0)
count_articles(df.rest)                                               # 84 articles dropped out
count_studies(df.rest)                                                # 131 studies dropped out
count_studies(df.rest) / count_studies(df) * 100                      # 16% of studies dropped out from total sample bc no group comparison 

# Filter out group comparisons
df.final <- filter(df.final, compare_group == 1)

# Comparison made on scale 
temp <- filter(df, scale == 1)
count_articles(temp)                                                # 196 articles
count_articles(temp) / count_articles(df) * 100                     # 46% of articles contain scales
count_studies(temp)                                                 # 403 studies
count_studies(temp) / count_studies(df) * 100                       # 48% of studies contain scales
count_studies(df) - count_studies(temp)                             # 438 studies do not contain scales
(count_studies(df) - count_studies(temp)) / count_studies(df) * 100 # 52% of studies do not contain group comparisons
nrow(temp)                                                          # 1295 rows

# Filter out non-scales
df.rest <- filter(df.final, scale == 0)
count_articles(df.rest)                                               # 97 articles dropped out
count_studies(df.rest)                                                # 216 studies dropped out
count_studies(df.rest) / count_studies(df) * 100                      # 26% of studies dropped out from total sample bc no group comparison 

# Filter out scales
df.final <- filter(df.final, scale == 1)

# Comparison made on reflective scale 
temp <- filter(df, reflective == 1)
count_articles(temp)                                                # 97 articles
count_articles(temp) / count_articles(df) * 100                     # 23% of articles contain reflective scales
count_studies(temp)                                                 # 151 studies
count_studies(temp) / count_studies(df) * 100                       # 18% of studies contain reflective scales
count_studies(df) - count_studies(temp)                             # 690 studies do not contain group comparisons
(count_studies(df) - count_studies(temp)) / count_studies(df) * 100 # 82% of studies do not contain group comparisons
nrow(temp)                                                          # 921 rows

# Filter out non-scales
df.rest <- filter(df.final, reflective == 0)
count_articles(df.rest)                                               # 102 articles dropped out
count_studies(df.rest)                                                # 252 studies dropped out
count_studies(df.rest) / count_studies(df) * 100                      # 30% of studies dropped out from total sample bc no group comparison 

# Filter out scales
df.final <- filter(df.final, reflective == 1)
count_articles(df.final)                                               # 97 articles left
count_studies(df.final)                                                # 151 studies left
count_studies(df.final) / count_studies(df) * 100                      # 18% of studies left 

# Delete rows from dataframe that do not have a comparison on a reflective scale
# This is our main unit of analysis
df <- subset(df,reflective == 1)

# Type of group comparison
sum(df$type_group == 0)                     # 351 comparisons across existing groups
sum(df$type_group == 0) / nrow(df) * 100    # 38% of comparisons are made across existing groups
sum(df$type_group == 1)                     # 570 comparisons across new groups
sum(df$type_group == 1) / nrow(df) * 100    # 62% of comparisons are made across new groups

# Type of scale
sum(df$type_scale == 0)                     # 448 ad hoc scales
sum(df$type_scale == 0) / nrow(df) * 100    # 49% of comparisons have ad hoc scales
sum(df$type_scale == 1)                     # 473 existing scales
sum(df$type_scale == 1) / nrow(df) * 100    # 51% of comparisons have existing scales

# Measure scale 
table(df$measure_scale)                                # dichotomous = 27 / ordinal = 311 / continuous = 269
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
length(which(is.na(df$power))) + length(which(df$power == "NA"))            # 628 comparisons do not mention power
(length(which(is.na(df$power))) + length(which(df$power == "NA")))/nrow(df) # 68% of comparisons do not mention power
nopwr <- df[which(is.na(df$power)),]                                         
count_articles(nopwr)                                                        # 59 articles do not mention power
count_articles(nopwr)/count_articles(df)*100                                 # 61% of remaining 97 articles do not mention power
count_studies(nopwr)                                                         # 76 studies do not mention power 
count_studies(nopwr)/count_studies(df)*100                                   # 78% of remaining studies do not mention power.

# Power mentioned
pwr <- df[which(!is.na(df$power)),]                                              
count_articles(pwr)                                                        # 42 articles mention power
count_studies(pwr)                                                         # 75 studies mention power

# Sample size
sum(is.na(as.numeric(df$n_rep)) & is.na(as.numeric(df$n1_rep)) & is.na(as.numeric(df$n2_rep)) & is.na(as.numeric(df$n3_rep)) & is.na(as.numeric(df$n4_rep)) & is.na(as.numeric(df$n5_rep)))
# 45 did not report any sample size at all
sum(is.na(as.numeric(df$n_rep)) & is.na(as.numeric(df$n1_rep)) & is.na(as.numeric(df$n2_rep)) & is.na(as.numeric(df$n3_rep)) & is.na(as.numeric(df$n4_rep)) & is.na(as.numeric(df$n5_rep)))/nrow(df)*100
# 5% did not report any sample size at all
sum(is.na(as.numeric(df$reltot)) & is.na(as.numeric(df$rel1)) & is.na(as.numeric(df$rel2)) & is.na(as.numeric(df$rel3)) & is.na(as.numeric(df$rel4)) & is.na(as.numeric(df$rel5)))
# 538 did not report any reliability estimate at all
sum(is.na(as.numeric(df$reltot)) & is.na(as.numeric(df$rel1)) & is.na(as.numeric(df$rel2)) & is.na(as.numeric(df$rel3)) & is.na(as.numeric(df$rel4)) & is.na(as.numeric(df$rel5)))/nrow(df)*100
# 58% did not report any reliability estimate at all

# Range of reliability
df$n_rep <- as.numeric(df$n_rep)
range(df$n_rep, na.rm=T)
summary(df$n_rep)

# Sample size total
sum(is.na(as.numeric(df$n_rep)))               # 81 did not report total sample size
sum(is.na(as.numeric(df$n_rep)))/nrow(df)*100  # 81 (9%) did not report total sample size
summary(as.numeric(df$n_rep))

# Sample size per group
# temporarily delete the scales that do not report total sample size
temp <- subset(df,!is.na(as.numeric(df$n_rep)))
# If the sample size for group 1 is not reported, we know there are no group sample sizes reported overall
sum(is.na(as.numeric(temp$n1_rep)))        # 242 (31.7%) did not report total sample size
sum(is.na(as.numeric(temp$n1_rep)))/N*100  # 242 (31.7%) did not report total sample size

# Reliability total
sum(is.na(as.numeric(df$reltot)))        # 552 (59.9%) did not report total sample size
sum(is.na(as.numeric(df$reltot)))/N*100  # 552 (59.9%) did not report total sample size
summary(as.numeric(df$reltot))

# Reliability per group
# temporarily delete the scales that do not report total reliability
temp <- subset(df,!is.na(as.numeric(df$reltot)))
# If the reliability for group 1 is not reported, we know there are no group sample sizes reported overall
sum(is.na(as.numeric(temp$rel1)))        # 358 (38.8%) did not report total sample size
sum(is.na(as.numeric(temp$rel1)))/N*100  # 358 (38.8%) did not report total sample size

# Range of reliability
df$reltot <- as.numeric(df$reltot)
range(df$reltot, na.rm=T)
summary(df$reltot)

# MI tested
sum(df$mitest_rep)        # 41 scales reported MI testing
table(df$mitest_rep)      # 0 = 880, 1 = 41
sum(df$mitest_rep) /N*100 # 41 (4.5%) did report on MI

# Only select rows that report on MI
dfmi <- subset(df,mitest_rep == 1)
N <- nrow(dfmi)
count_articles(dfmi)
count_studies(dfmi)

# How many articles studied?
length(unique(dfmi$article_id_recoded)) # 7

# MI method
table(df$mimethod_rep, useNA="always")
sum(df$mimethod_rep=="1.0", na.rm=T)/sum(df$mitest_rep)*100 # 24.4% of 41 
sum(df$mimethod_rep=="2.0", na.rm=T)/sum(df$mitest_rep)*100 # 73.2% of 41 

# MI result
table(df$miresult_rep, useNA="always")
sum(df$miresult_rep=="0.0", na.rm=T)/sum(df$mimethod_rep == "1.0" | df$mimethod_rep == "2.0", na.rm=T)*100 # 25% of 40 
sum(df$miresult_rep=="1.0", na.rm=T)/sum(df$mimethod_rep == "1.0" | df$mimethod_rep == "2.0", na.rm=T)*100 # 75% of 40 

# MI level
df.mi <- subset(df, df$miresult_rep == "1.0")
table(df.mi$milevel_rep, useNA="always")
sum(df.mi$milevel_rep=="2.0", na.rm=T)/nrow(df.mi)*100 # 16.7% of 30 
sum(df.mi$milevel_rep=="3.0", na.rm=T)/nrow(df.mi)*100 # 10% of 30 
sum(df.mi$milevel_rep=="NA", na.rm=T)/nrow(df.mi)*100 # 16.7% of 30 

# Only select rows that have scalar invariance
dfmi.scalar <- subset(df,milevel_rep == "3.0")
N <- nrow(dfmi.scalar)

# How many articles studied?
length(unique(dfmi.scalar$article_id_recoded)) # 2








# End -----------------------------------------------------------------------------------------


















# Analysis of Pilot Data --------------------------------------------------







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





# code from presi -----------------------------------------------------------------------------

### CODE FOR MAIN STUDY SYSTEMATIC REVIEW MEASUREMENT INVARIANCE ##
rm(list = ls()) # clear workspace
options(scipen=999) # no scientific notation
library(fulltext)
library(tidytext)
library(pdftools)
library(XML)
library(tm)
library(readxl)
library(dplyr)
library(ggplot2)
library(viridis)

# Analysis of Pilot Data --------------------------------------------------
df <- read_excel("final-codebook.xlsx") # load data 
# remove empty columns
df <- df[,1:35]

colnames(df)

# How many articles and journals studied?
nrow(df) # 1854 studies in 426 articles
N <- nrow(df)

# number of rows by journal
df %>% group_by(journal_id) %>% summarise(count=n())

# Count number of empirical and non-empirical studies by journal (0 = plos, 1 = PS)
df %>% group_by(journal_id) %>% count(empirical)

# delete the studies that are non-empirical
df <- df[df$empirical != 0,]

# Count number of group comparisons by journal (0 = plos, 1 = PS)
df %>% group_by(journal_id) %>% count(compare_group)

# delete the studies that are non group comparisons
df <- df[df$compare_group != 0,]

# Count number of scales by journal (0 = plos, 1 = PS)
df %>% group_by(journal_id) %>% count(scale)

# delete the studies that have no scales
df <- df[df$scale != 0,]

# Count number of reflective scales by journal (0 = plos, 1 = PS)
df %>% group_by(journal_id) %>% count(reflective)

# delete the studies that have no reflective scales
df <- df[df$reflective != 0,]

# count number of comparisons and articles left by journal
length(unique(df[df$journal_id == 0,]$article_id))
length(unique(df[df$journal_id == 1,]$article_id))
df %>% group_by(journal_id) %>% summarise(count=n())

# type scales plot
df$journal_id[df$journal_id == 0] <- "PLoS ONE"
df$journal_id[df$journal_id == 1] <- "Psychological Science"
df$type_scale[df$type_scale == "0.0"] <- "Existing scale"
df$type_scale[df$type_scale == "1.0"] <- "Ad hoc scale"
df$type_scale[df$type_scale == "NA"] <- "Ad hoc scale"
df$mitest_rep[df$mitest_rep == 0] <- "No"
df$mitest_rep[df$mitest_rep == 1] <- "Yes"



# histogram type scale
colors <- viridis(5)[c(1,3)]

df %>% 
  ggplot(., aes(x = journal_id, fill = type_scale)) +
  geom_bar(position="dodge", stat="count", colour="grey20") +
  theme_classic() +
  scale_fill_manual(values = colors) +
  stat_count(aes(y=..count..,label=..count..),geom="text", size=10, position = position_dodge2(width = 0.9, preserve = "single"), vjust=-0.5) +
  ylab("Count") +
  xlab("") +
  theme(legend.text=element_text(size=20),
        legend.key.size = unit(1, "cm"),
        text = element_text(size=20), 
        axis.text=element_text(size=20)) + 
  ylim(0,410) +
  labs(fill = "Type of scale")

# histogram mi reported
df %>% 
  ggplot(., aes(x = journal_id, fill = mitest_rep)) +
  geom_bar(position="dodge", stat="count", colour="grey20") +
  theme_classic() +
  scale_x_discrete(limits = levels(df$journal_id)) +
  scale_fill_manual(values = colors) +
  stat_count(aes(y=..count..,label=..count..),geom="text", size=10, position = position_dodge2(width = 0.9, preserve = "single"), vjust=-0.5) + 
  ylab("Count") +
  xlab("") +
  theme(text = element_text(size=20), axis.text=element_text(size=20), legend.text=element_text(size=20)) + 
  ylim(0,650) + 
  labs(fill = "Measurement invariance reported?")

# Delete all studies that do not do a measurement invariance check
df <- df[df$mitest_rep != "No",]


# histogram measurement invariance found?

# how many unique articles left?
unique(df$article_id)

df$miresult_rep[df$miresult_rep == "0.0"] <- 0
df$miresult_rep[df$miresult_rep == "1.0"] <- 1
df$miresult_rep[df$miresult_rep == "NA"] <- 2

df$miresult_rep <- factor(df$miresult_rep, labels = c("No", "Yes", "Not reported"))

colors2 <- viridis(5)[c(1,3,5)]

df %>% 
  ggplot(., aes(x = journal_id, fill = miresult_rep)) +
  geom_bar(position="dodge", stat="count", colour="grey20") +
  theme_classic() +
  scale_x_discrete(limits = levels(df$journal_id)) +
  scale_fill_manual(values = colors2) +
  stat_count(aes(y=..count..,label=..count..),geom="text", size=7, position = position_dodge2(width = 0.9, preserve = "single"), vjust=-0.5) + 
  ylab("Count") +
  xlab("") +
  theme(text = element_text(size=16), axis.text=element_text(size=16), legend.text=element_text(size=16)) + 
  ylim(0,40) + 
  labs(fill = "Full measurement invariance found?")

# histogram type of mi found
df$milevel_rep[df$milevel_rep == "2.0"] <- 2
df$milevel_rep[df$milevel_rep == "3.0"] <- 3
df$milevel_rep[df$milevel_rep == "5.0"] <- 5
df$milevel_rep[df$milevel_rep == "NA"] <-  6

df$milevel_rep <- factor(df$milevel_rep, labels = c("Metric invariance", "Scalar invariance", "Partial invariance", "Not reported"))

colors3 <- viridis(5)[c(1,3,4,5)]

df %>% 
  ggplot(., aes(x = journal_id, fill = milevel_rep)) +
  geom_bar(position="dodge", stat="count", colour="grey20") +
  theme_classic() +
  scale_x_discrete(limits = rev(levels(df$journal_id))) +
  scale_fill_manual(values = colors3) +
  stat_count(aes(y=..count..,label=..count..),geom="text", size=10, position = position_dodge2(width = 0.9, preserve = "single"), vjust=-0.5) + 
  ylab("Count") +
  xlab("") +
  theme(text = element_text(size=20), axis.text=element_text(size=20), legend.text=element_text(size=20)) + 
  ylim(0,40) + 
  labs(fill = "Level of MI found")


# histogram method mi reported
df$mimethod_rep[df$mimethod_rep == "1.0"] <- "Scale-based"
df$mimethod_rep[df$mimethod_rep == "2.0"] <- "Item-based"
df$mimethod_rep[df$mimethod_rep == "NA"] <- "Not reported"

df %>% 
  ggplot(., aes(x = journal_id, fill = mimethod_rep)) +
  geom_bar(position="dodge", stat="count", colour="grey20") +
  theme_classic() +
  scale_x_discrete(limits = rev(levels(df$journal_id))) +
  stat_count(aes(y=..count..,label=..count..),geom="text", size=10, position = position_dodge2(width = 0.9, preserve = "single"), vjust=-0.5) + 
  ylab("Count") +
  xlab("") +
  theme(text = element_text(size=20), axis.text=element_text(size=20), legend.text=element_text(size=20)) + 
  ylim(0,40) + 
  labs(fill = "MI method used")






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

# From those that share their data, can we construct scales based on the data? / 15, 50%
sum(df$open_scale, na.rm=T);sum(df$open_scale, na.rm=T)/Nd

# From those that share their data, can we construct groups based on the data? / 22, 73%
sum(df$open_group, na.rm=T);sum(df$open_group, na.rm=T)/Nd

# From those that share their data, can we construct both scales and groups / 15, 50%
sum(df$open_scale == 1 & df$open_group == 1, na.rm=T)
sum(df$open_scale == 1 & df$open_group == 1, na.rm=T)/Nd

# From all studies that have a reflective scale (36), how many report MI? / 4, 11%
sum(df$mi_rep, na.rm=T); sum(df$mi_rep, na.rm=T)/Nfinal

# From all studies that have a reflective scale (36), how many test for MI? / 1, 2%
sum(df$mitest_rep, na.rm=T); sum(df$mitest_rep, na.rm=T)/Nfinal

# From all studies that test for MI, how many find MI? / 1, 100%
sum(df$mitest_rep == 1 & df$miresult_rep == 1, na.rm=T)

# From all studies that have reflective scales and share their data (30), 
# How many have interpretable data / 23, 77%
sum(df$data_usability, na.rm=T);sum(df$data_usability, na.rm=T)/Nd

# From all studies that have reflective scales share their data (30), for how many can we run MI analysis? / 13, 43%
sum(df$mitest, na.rm=T);sum(df$mitest, na.rm=T)/Nd

# From all studies that have reflective scales share their data, for how many do we find MI? / 11, 37%
sum(df$miresult, na.rm=T);sum(df$miresult, na.rm=T)/Nd

# From all studies that we checked MI (15), for how many do we find MI? / 11, 85%
sum(df$miresult, na.rm=T);sum(df$miresult, na.rm=T)/sum(df$mitest, na.rm=T)

# For how many studies do we reproduce their MI result? # 1
sum(df$reproduced, na.rm=T)

# Within Journals, how many studies found that compare reflective scales across groups?
table(df$journal_id,df$reflective)



