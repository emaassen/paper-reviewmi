### CODE FOR MAIN STUDY SYSTEMATIC REVIEW MEASUREMENT INVARIANCE ##
### This is code to sample articles from Plos One to analyze in the reporting step of our main study
### Since we include all articles with open data from Psychological Science from 2018 and 2019, we only
### need to sample from Plos One, not from Psychological Science

# Relevant packages
require("rplos")

# Search for subject psychology
# Search in journal PLoSONE
# Search for research articles (no corrections, viewpoints etc.)
# Only show full text availability

# Check how many articles there are
res <- plossubject(q = "psychology", 
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
res <- plossubject(q = "psychology", 
                   fl=c('id',
                        'publication_date',
                        'subject'),
                   fq=list('journal_key:PLoSONE',
                           'article_type:"Research Article"',
                           "doc_type:full"),
                   start = 0,
                   limit = r)

# reduce publication date to only the year
res$data$publication_date <- substr(res$data$publication_date, 1, 4)
head(res)

# filter years we need
index2019 <- res$data[which(res$data$publication_date == 2019),]
index2018 <- res$data[which(res$data$publication_date == 2018),]

# find unique ids
length(unique(index2019$id)) # 3219
length(unique(index2018$id)) # 3491

# From the rplos package documentation:
# "Don't  be  surprised  if  queries  you  perform  in  a  scripting  
# language,  like  using rplos in  R,  give different results than when 
# searching for articles on the PLOS website. I am not sure what 
# exact defaults they use on their website."

# Sample 99 articles from 2019, 114 from 2018 (i.e., the same amount of articles as Psychological Science)
set.seed(17080904)
sample2019 <- sample(index2019$id,99, replace = F)
sample2018 <- sample(index2018$id,114, replace = F)
sample2019 <- cbind(sample2019,rep(2019,length(sample2019)))
sample2018 <- cbind(sample2018,rep(2018,length(sample2018)))
plossample <- as.data.frame(rbind(sample2018,sample2019))

# Load the Psychological Science sample
pssample <- read.table("../data/sample-ps.txt", sep = "")

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
write.table(sample.em, file = "../data/sample-em.txt", sep = "")
write.table(sample.edd, file = "../data/sample-edd.txt", sep = "")