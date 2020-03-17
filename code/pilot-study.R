### CODE FOR PILOT STUDY SYSTEMATIC REVIEW MEASUREMENT INVARIANCE ##
#rm(list = ls()) # clear workspace
options(scipen=999) # no scientific notation
library(readxl) # packages

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
library(fulltext)
library(tidytext)
library(pdftools)
library(XML)
library(tm)

getwd()
setwd("../data/data-pilot/fulltext/") # change this to folder where the files are

# load pilot codebook
df <- read_excel("../data/pilot-codebook.xlsx") # load data

# extract dois
colnames(df)
head(df$url_article)
set <- which(df$paper_id > 20) # only need to extract dois from PS and Plos
dois <- df$url_article[set]
dois <- unique(dois) # remove doubles
dois <- gsub('http://doi.org/', '', dois)
dois <- gsub('https://doi.org/', '', dois);dois

# download the articles (note that PS ones are behind a paywall)
ft_get(dois) 

# load pdf files
pdffiles <- list.files(pattern = "pdf$")
pdffiles <- c(sort(pdffiles[21:40]),sort(pdffiles[1:20])) # put them in right order
pdfs <- lapply(pdffiles, pdf_text)

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
# between groups, and 42 without one (or one without it being compared between groups). 

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
# remove punctuation
# remove stopwords (eg, the, of, in, etc.), 
# convert text to lower case, stem the words, 
# remove numbers
# only count words that appear at least 3 times, add: "bounds = list(global = c(3, Inf))"

tdm.nref <- TermDocumentMatrix(corp.nref,
                          control = 
                            list(removePunctuation = TRUE,
                                 stopwords = TRUE,
                                 tolower = TRUE,
                                 stemming = TRUE,
                                 removeNumbers = TRUE)) 

tdm.ref <- TermDocumentMatrix(corp.ref,
                               control = 
                                 list(removePunctuation = TRUE,
                                      stopwords = TRUE,
                                      tolower = TRUE,
                                      stemming = TRUE,
                                      removeNumbers = TRUE)) 

#inspect(tdm[1:10,])
#findFreqTerms(tdm, lowfreq = 100, highfreq = Inf)
#ft <- findFreqTerms(tdm, lowfreq = 100, highfreq = Inf)
#as.matrix(tdm[ft,]) 
#sort(apply(tdm, 1, sum), decreasing = TRUE)

terms.nref <- Terms(tdm.nref)
terms.ref <- Terms(tdm.ref)
terms.nref
terms.ref

which(terms.nref == "construct")
which(terms.ref == "construct")

# Now we need to find out how often certain terms related to constructs are used


## Irrelevant code
#xmls <- lapply(xmlfiles,xmlParse)
#xmltext <- lapply(xmls,xmlRoot) #gives content of root
#xmlarticles <- list()


#for (i in length(xmltext)) { 
#  xmlarticles <- mapply(c, xmltext[[i]][["front"]][["article-meta"]][["abstract"]], xmltext[[i]][["body"]])
#}
