### CODE FOR MAIN STUDY SYSTEMATIC REVIEW MEASUREMENT INVARIANCE ##
### This is code to analyze the reporting results of our main study

rm(list = ls()) # clear workspace
options(scipen=999) # no scientific notation
library(readxl)

df <- read_excel("../data/codebook-main-reporting.xlsx") # load data 
colnames(df)

# How many articles and journals studied?
length(unique(df$article_id_recoded)) # 426
unique(df$journal_id) # 2
nrow(df) # 1845 comparisons
N <- nrow(df)

# Empirical data (sum of comparisons and percentage)
sum(df$empirical)       # 1749 
sum(df$empirical)/N*100 # 94.8% of 1845

# Group comparison
sum(df$compare_group, na.rm=T)                        # 1617
sum(df$compare_group, na.rm=T)/N*100                  # 87.6% of 1845
sum(df$compare_group, na.rm=T)/sum(df$empirical)*100  # 92.5% of 1749 (empirical)





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



