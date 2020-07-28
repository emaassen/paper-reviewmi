library("rplos")
library("rcrossref")
library("dplyr")
library("tidyr")

# Search for subject psychology
# Search in journal PLoSONE
# Search for research articles (no corrections, viewpoints etc.)
# Only show full text availability

# Check how many articles there are
res <- plossubject(q = "\"psychology\"", 
            fl=c('id',
                 'publication_date'),
            fq=list('journal_key:PLoSONE',
                    'article_type:"Research Article"',
                    "doc_type:full"),
            start = 0,
            limit = 0)

# Save number of articles
r <- res$meta$numFound

# Index all articles from 0 to r
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

# change publication date to only the year
res$data$publication_date <- substr(res$data$publication_date, 1, 4)
res

# filter year - without doctype full we have 5169 left
# filter year - with doctype full we have 5169 left
index2019 <- res$data[which(res$data$publication_date == 2019),]
index2018 <- res$data[which(res$data$publication_date == 2018),]

# find unique ids
length(unique(index2019$id)) # 4233
length(unique(index2018$id)) # 4122


# Don't  be  surprised  if  queries  you  perform  in  a  scripting  
# language,  like  using rplos in  R,  give different results than when 
# searching for articles on the PLOS website. I am not sure what 
# exact defaults they use on their website.

# Sample 99 articles from 2019, 114 from 2018
set.seed(17080904)
sample2019 <- sample(index2019$id,99, replace = F)
sample2018 <- sample(index2018$id,114, replace = F)
sample2019 <- cbind(sample2019,rep(2019,length(sample2019)))
sample2018 <- cbind(sample2018,rep(2018,length(sample2018)))
plossample <- as.data.frame(rbind(sample2018,sample2019))

# load the psychological science sample
pssample <- read.table("sample-ps.txt", sep = "")

names(pssample) <- names(plossample) <- c("doi","year")

# Two coders will code all articles, but we will randomize the order in which they will appear in the separate codebooks

# Randomize the rows in the PLOS and PS sample for both coders
rows.plos.em <- sample(nrow(plossample), replace = F)
rows.plos.edd <- sample(nrow(plossample), replace = F) 
rows.ps.em <- sample(nrow(pssample), replace = F)
rows.ps.edd <- sample(nrow(pssample), replace = F) 

# Reorder the dataframes based on the row order selected previously
sample.plos.em <- plossample[rows.plos.em, ]
sample.ps.em <- pssample[rows.ps.em,]
sample.plos.edd <- plossample[rows.plos.edd, ]
sample.ps.edd <- pssample[rows.ps.edd,]

# Combine the two samples 
sample.em <- rbind(sample.plos.em,sample.ps.em)
sample.edd <- rbind(sample.plos.edd,sample.ps.edd)

# Save to file to input in main codebook
writeLines(sample.em, "sample-em.txt")
writeLines(sample.edd, "sample-edd.txt")


