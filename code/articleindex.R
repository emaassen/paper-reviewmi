install.packages("rplos")
library("rplos")
library("rcrossref")
library("dplyr")
library("tidyr")

# Search for subject psychology
# Search in journal PLoSONE
# Search for research articles (no corrections, viewpoints etc.)
# Only show full text availability


res <- plossubject(q = "\"psychology\"", 
            fl=c('id',
                 'publication_date'),
            fq=list('journal_key:PLoSONE',
                    'article_type:"Research Article"',
                    "doc_type:full"),
            start = 0,
            limit = 0)

r <- res$meta$numFound

res <- plossubject(q = "\"psychology\"", 
                   fl=c('id',
                        'publication_date'),
                   fq=list('journal_key:PLoSONE',
                           'article_type:"Research Article"',
                           "doc_type:full"),
                   start = 0,
                   limit = r)

# back up this huge set before I make a mistake
backup <- res
backup2 <- backup
res <- backup

# fill blank spaces
res[res == ""] <- NA

# change publication date to only the year
res$data$publication_date <- substr(res$data$publication_date, 1, 4)

# filter year - without doctype full we have 5169 left
# filter year - with doctype full we have 5169 left
res$data <- res$data[which(res$data$publication_date >= 2019),]

# are there duplicates?
res$data$duplicate # this doesnt work

# find unique dois - still 5169
length(unique(res$data$id))


# Don't  be  surprised  if  queries  you  perform  in  a  scripting  
# language,  like  usingrplosin  R,  givedifferent results than when 
# searching for articles on the PLOS website.  I am not sure what 
# exactdefaults they use on their website.  There are a few things to 
# consider.  You can tweak which typesof articles are returned: Try 
# using the article_type filter in the fq parameter.  
# For which journalto search, e.g., do'journal_key:PLoSONE'. 
# Seejournalnamekey()for journal abbreviations

# ISSN PlosOne = 1932-6203 
# ISSN PsychScience = 1467-9280 (web, I find 166), 0956-7976 (print)

ps <- cr_journals(issn = c('1467-9280'), 
            filter = c(from_pub_date = '2019-01-01', 
                      until_pub_date = '2019-12-31'),  
            works = T,
            limit = 1000)

plos <- cr_journals(query = "psychology",
                    issn = c('1932-6203'), 
                    filter = c(from_pub_date = '2019-01-01', 
                               until_pub_date = '2019-12-31',
                               has_full_text = TRUE),  
                    works = T,
                    limit = 1000)

# we can't filter on subject so for Plos the number is way too large. 


# Get 2 random dois
cr_r(2)


