### CODE FOR PILOT STUDY SYSTEMATIC REVIEW MEASUREMENT INVARIANCE ##
### This is code to sample articles to analyze in the reporting step of our pilot study

rm(list = ls()) # clear workspace
options(scipen=999) # no scientific notation

# Sampling studies --------------------------------------------------------

# We will take a random sample of 20 articles from the JDM article set (total k = 223) for the pilot study.
# We will take a random sample of 20 articles from the PS article set (total k = 144) for the pilot study.
# We will take a random sample of 20 articles from the PlosONE article set (total k = 252) for the pilot study.
k.jdm <- 1:223
k.ps <- 1:144
k.plos <- 1:252

set.seed(0904)
pilot.jdm <- sample(k.jdm,20)

set.seed(1708)
pilot.ps <- sample(k.ps,20)

set.seed(09041708)
pilot.plos <- sample(k.plos,20)

pilot.jdm <- sort(pilot.jdm)
pilot.ps <- sort(pilot.ps)
pilot.plos <- sort(pilot.plos)

# selected articles for pilot study
pilot.jdm
pilot.ps
pilot.plos

# these numbers represent the articles we sampled from our existing index and included in our pilot data codebook.

