#rm(list = ls()) # clear workspace
options(scipen=999) # no scientific notation

# We will take a random sample of 40 articles from the JDM article set (total k = 224) for the pilot study.
k <- 1:223
set.seed(09041708)
pilot.sample <- sample(k,40)
pilot.sample <- sort(pilot.sample)
pilot.sample
 