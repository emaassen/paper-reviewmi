### CODE FOR PILOT STUDY SYSTEMATIC REVIEW MEASUREMENT INVARIANCE ##
#rm(list = ls()) # clear workspace
options(scipen=999) # no scientific notation
library(fulltext)
library(tidytext)
library(pdftools)
library(XML)
library(tm)
library(readxl)
library(dplyr)


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


# Analysis of Pilot Data --------------------------------------------------
df <- read_excel("../data/pilot-codebook.xlsx") # load data 

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

# Of those empirical studies that compare groups (85), how many use reflective scales / 37, 44%
sum(df$reflective, na.rm = T); sum(df$reflective, na.rm = T)/Ng

# How many of those scales (37) were reflective? / 36, 97%
sum(df$reflective, na.rm=T); sum(df$reflective, na.rm=T)/Ns
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



# Text analysis of pilot data ---------------------------------------------

# We downloaded the 20 .pdf files for JDM by hand. Links to these articles can be
# found in /data/pilot-codebook.xlsx, column 3 (article url).
# For the other 40 articles (for PS and Plos) we had the doi, below is code to download 
# these 40 articles automatically.
# We've made use of the information here to analyze these data: https://www.tidytextmining.com/

# load pilot codebook
df <- read_excel("../data/pilot-codebook.xlsx") # load data

# change working directory
getwd()
setwd("../data/data-pilot/fulltext/") # change this to folder where the text files are

# extract dois
colnames(df)
head(df$url_article)
set <- which(df$paper_id > 20) # only need to extract dois from PS and Plos
dois <- df$url_article[set]
dois <- unique(dois) # remove doubles
dois <- gsub('http://doi.org/', '', dois)
dois <- gsub('https://doi.org/', '', dois);dois

# download the articles (note that PS ones are behind a paywall)
#ft_get(dois) 

# load pdf files
pdffiles <- list.files(pattern = "pdf$")
pdffiles <- c(sort(pdffiles[21:40]),sort(pdffiles[1:20])) # put them in right order
# dit mag weg: pdfs <- lapply(pdffiles, pdf_text)

# load xml files
xmlfiles <- list.files(pattern = "xml$")
xmlfiles <- sort(xmlfiles)

# We want to analyze the differences in text use in articles that use a reflective scale
# between groups, compared to those studies that do not.
# article IDs that use a reflective scale
ref <- unique(subset(df$paper_id, df$reflective == 1)) 
# first 40 are for the pdf files (JDM and PS), last 20 are for the XML files (plos)
ref1 <- ref[ref <= 40]
ref2 <- ref[ref > 40]
ref2 <- ref2 - 40 # numbering starts again with 1 in the XML file

# Match the article IDs to the right pdf and xml files
match <- unique(df$url_article)
match <- c(sort(match[1:20]),sort(match[21:40]), sort(match[41:60])) # sort within journal

# Subset the 60 articles in those with and those without reflective scales
pdffiles.nref <- pdffiles[-ref1]
pdffiles.ref <- pdffiles[ref1]
xmlfiles.nref <- xmlfiles[-ref2]
xmlfiles.ref <- xmlfiles[ref2]

# there are in total 18 articles with one or more reflective scales that are compared
# between groups, and 42 without one (or without it being compared between groups). 

# Now we need to find out how often certain terms related to constructs are used
refterms.c1 <- c("KR-20", "factor analysis", "self-report", 
                "classical test", "sum score",
                "true score", "scale score", "t-test", "t test", "test retest", "test re-test")

refterms.c2 <- c("construct", "scale", "questionnaire", "reliability", "alpha", "cronbach", "chronbach", 
                "KR20", "factor", "analysis", "internal", "consistency", "selfreport", "items", "latent", "trait",
                "convergent", "validity", "CTT", "sumscore", "sum", "score",
                "t-test", "retest")


refterms.m1 <- c("measurement invariance", "non-invariance", "non invariance",  
                "non-equivalence", "differential item", "item functioning", "measurement model", 
                "measurement bias")

refterms.m2 <- c("measurement", "invariance", "noninvariance", 
                "equivalence", "nonequivalence", "partial", "DIF", 
                "constraint", 
                "bias")

refterms.g1 <- c("between-group", "between group", "within-group", "within group",
                "control group", "experimental group", "treatment group")           

refterms.g2 <- c("groups", 
                "control", "experimental", "treatment")   

# create corpus (database for text) for pdfs and xml
corppdf.nref <- Corpus(URISource(pdffiles.nref),
               readerControl = list(reader = readPDF))

corppdf.ref <- Corpus(URISource(pdffiles.ref),
                    readerControl = list(reader = readPDF))

corpxml.nref <- Corpus(URISource(xmlfiles.nref),
               readerControl = list(reader = readPlain))

corpxml.ref <- Corpus(URISource(xmlfiles.ref),
                readerControl = list(reader = readPlain))

corp.nref <- c(corppdf.nref,corpxml.nref)
corp.ref <- c(corppdf.ref,corpxml.ref)

### create term document matrix
# we want to analyze sets of words (bigram) instead of single words, so we need a BigramTokenizer
# set for 2 words now: (words(x), 2). We add "tokenize = BigramTokenizer" to the TermDocumentMatrix 
# function below

BigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

# remove punctuation
# remove stopwords (eg, the, of, in, etc.), 
# convert text to lower case, stem the words, 
# remove numbers
# remove english stop words
# use bigramtokanizer from above, indexing 2 words
# only count words that appear at least 3 times, add: "bounds = list(global = c(3, Inf))"

tdm.nref <- TermDocumentMatrix(corp.nref,
                          control = 
                            list(removePunctuation = TRUE,
                                 stopwords = TRUE,
                                 tolower = TRUE,
                                 stemming = TRUE,
                                 removeNumbers = TRUE,
                                 removeWords, stopwords("english"),
                                 tokenize = BigramTokenizer)) 

tdm.ref <- TermDocumentMatrix(corp.ref,
                               control = 
                                 list(removePunctuation = TRUE,
                                      stopwords = TRUE,
                                      tolower = TRUE,
                                      stemming = TRUE,
                                      removeNumbers = TRUE,
                                      removeWords, stopwords("english"),
                                      tokenize = BigramTokenizer)) 

# Different ways to display or analyze the data:
inspect(tdm.ref[1:10,])
inspect(removeSparseTerms(tdm.ref, 0.7))
findFreqTerms(tdm.ref, lowfreq = 100, highfreq = Inf)
ft.ref <- findFreqTerms(tdm.ref, lowfreq = 100, highfreq = Inf)
as.matrix(tdm.ref[ft.ref,]) 
sort(apply(tdm.ref, 1, sum), decreasing = TRUE)

# turn tdm into dense matrix and create frequency vector 
tdm.ref.freq <- rowSums(as.matrix(tdm.ref))
tdm.nref.freq <- rowSums(as.matrix(tdm.nref))


# DOUBLE WORD ANALYSIS ----------------------------------------------------

tdm.nref <- TermDocumentMatrix(corp.nref,
                               control = 
                                 list(removePunctuation = TRUE,
                                      stopwords = TRUE,
                                      tolower = TRUE,
                                      stemming = TRUE,
                                      removeNumbers = TRUE,
                                      removeWords, stopwords("english"),
                                      tokenize = BigramTokenizer)) 

tdm.ref <- TermDocumentMatrix(corp.ref,
                              control = 
                                list(removePunctuation = TRUE,
                                     stopwords = TRUE,
                                     tolower = TRUE,
                                     stemming = TRUE,
                                     removeNumbers = TRUE,
                                     removeWords, stopwords("english"),
                                     tokenize = BigramTokenizer)) 

# CONSTUCT TERMS
for (i in 1:length(refterms.c1)) {
  print(tdm.ref.freq[grep(refterms.c1[i], names(tdm.ref.freq))], na.rm=T)
}

# terms: testretest 1
# total terms: 1

for (i in 1:length(refterms.c1)) {
  print(tdm.nref.freq[grep(refterms.c1[i], names(tdm.nref.freq))], na.rm=T)
}

# terms: none that are explically related to our search terms
# total terms: 0




# MEASUREMENT TERMS
for (i in 1:length(refterms.m1)) {
  print(tdm.ref.freq[grep(refterms.m1[i], names(tdm.ref.freq))], na.rm=T)
}

# terms: differential item 1
# total terms: 1

for (i in 1:length(refterms.m1)) {
  print(tdm.nref.freq[grep(refterms.m1[i], names(tdm.nref.freq))], na.rm=T)
}

# terms: none that are explically related to our search terms
# total terms: 0




# GROUP TERMS
for (i in 1:length(refterms.g1)) {
  print(tdm.ref.freq[grep(refterms.g1[i], names(tdm.ref.freq))], na.rm=T)
}

# terms: 
# between group 1
# within group 1
# control group 23
# experimental group 1
# total terms: 26

for (i in 1:length(refterms.g1)) {
  print(tdm.nref.freq[grep(refterms.g1[i], names(tdm.nref.freq))], na.rm=T)
}

# terms: none that are explically related to our search terms

# terms: 
# between group 4
# within group 1
# control group 25
# experimental group 18
# treatment group 1
# total terms: 49


# SINGLE WORD ANALYSIS ----------------------------------------------------

# We create new term document matrices, now without the BigramTokanizer because
# we do not need double words.

tdm.nref <- TermDocumentMatrix(corp.nref,
                               control = 
                                 list(removePunctuation = TRUE,
                                      stopwords = TRUE,
                                      tolower = TRUE,
                                      stemming = TRUE,
                                      removeNumbers = TRUE,
                                      removeWords, stopwords("english"))) 

tdm.ref <- TermDocumentMatrix(corp.ref,
                              control = 
                                list(removePunctuation = TRUE,
                                     stopwords = TRUE,
                                     tolower = TRUE,
                                     stemming = TRUE,
                                     removeNumbers = TRUE,
                                     removeWords, stopwords("english")))

# Find most occuring terms (single words only)
terms.nref <- Terms(tdm.nref)
terms.ref <- Terms(tdm.ref)
terms.nref
terms.ref

# Tidy the term document matrix (single words only)
tidy.nref <- tidy(tdm.nref)
tidy.ref <- tidy(tdm.ref)
tidy.ref

# We may be interested in finding the words most specific to each of the documents 
# This could be quantified by calculating the tf-idf of each term-speech 
# pair using the bind_tf_idf() function.

tidy.nref.tfidf <- tidy.nref %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))

tidy.ref.tfidf <- tidy.ref %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))

tidy.nref.tfidf;tidy.ref.tfidf

# Check the frequency of word occurence

# CONSTUCT TERMS
tidy.ref.tfidf[which(tidy.ref.tfidf[,1] == refterms.c2),]

# terms
# selfreport 13
# scale 20 (2 papers)
# factor 7 
# total unique papers: 4 (out of 18 = 22%)

tidy.nref.tfidf[which(tidy.nref.tfidf[,1] == refterms.c2),]

# terms
# score 34
# scale 13
# selfreport 1
# trait 1
# total unique papers: 4 (out of 42 = 10%)



# MEASUREMENT TERMS
tidy.ref.tfidf[which(tidy.ref.tfidf[,1] == refterms.m2),]

# terms
# partial 18 (2 papers)
# total unique papers: 2 (out of 18 = 11%)

tidy.nref.tfidf[which(tidy.nref.tfidf[,1] == refterms.m2),]

# GROUP TERMS
# terms
# bias 23 (7 papers)
# construct 5 (2 papers)
# partial 5 (5 papers)
# total unique papers: 13 (out of 42 = 31%)




# GROUP TERMS
tidy.ref.tfidf[which(tidy.ref.tfidf[,1] == refterms.g2),]

# terms
# treatment 4 (2 papers)
# control 20 (3 papers)
# total unique papers: 5 (out of 18 = 28%)

tidy.nref.tfidf[which(tidy.nref.tfidf[,1] == refterms.g2),]

# terms
# control 75 (8 papers)
# treatment 4 (2 papers)
# total unique papers: 10 (out of 42 = 24%)


for (i in 1:length(refterms.c2)) {
print(tidy.ref.tfidf[which(tidy.ref.tfidf[,1] == refterms.c2[i]),])
  
}

# I don't know why the word construct isn't found in the above code. 
# Looping over the refterms vector also doesnt work.
tidy.ref.tfidf[which(tidy.ref.tfidf[,1] == "construct"),]
# 8 out of 18 = 44%

tidy.nref.tfidf[which(tidy.nref.tfidf[,1] == "construct"),]
# 18 out of 42 = 43%


tidy.ref.tfidf[which(tidy.ref.tfidf[,1] == "scale"),]
# 12 out of 18 = 67%

tidy.nref.tfidf[which(tidy.nref.tfidf[,1] == "scale"),]
# 27 out of 42 = 65%

tidy.ref.tfidf[which(tidy.ref.tfidf[,1] == "cronbach"),]
# 1 out of 18 = 6%

tidy.nref.tfidf[which(tidy.nref.tfidf[,1] == "cronbach"),]
# 1 out of 42 = 2%






## Irrelevant code


# By seeing how often word X is followed by word Y, we can then build a model of the 
# relationships between them. Setting n to the number of words we wish to capture in 
# each n-gram. When we set n to 2, we are examining pairs of two consecutive words, 
# often called "bigrams"

#tidy.nref %>%
#  unnest_tokens(word, text)

#aa<- austen_books() %>%
#  unnest_tokens(bigram, text, token = "ngrams", n = 2)

#tidy_books <- pdf.nref %>%
#  unnest_tokens(word, text)


#xmls <- lapply(xmlfiles,xmlParse)
#xmltext <- lapply(xmls,xmlRoot) #gives content of root
#xmlarticles <- list()


#for (i in length(xmltext)) { 
#  xmlarticles <- mapply(c, xmltext[[i]][["front"]][["article-meta"]][["abstract"]], xmltext[[i]][["body"]])
#}
