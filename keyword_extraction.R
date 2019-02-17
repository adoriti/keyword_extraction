# keyword extraction ----
# developer: Afroditi Doriti
# version: 1
# description: keyword clustering, subsequent keyword extraction
# input: polymers_renewable.csv
# data ownership: MAPEGY
# ------------------------------- 
# outputs:
# -------------------------------

# load packages
library("tidyverse")
library("tm")
library("SnowballC")

# list.of.packages <- c("ggplot2", "Rcpp")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)

# set wd
setwd("/home/adoriti/Documents/Keyword extraction")

# define input file:
input_file <- "polymers_renewable.csv"

# read file:
data <- read.csv(input_file, stringsAsFactors = FALSE, sep = ";")

# Create corpus
docs <- Corpus(DataframeSource(data))

#Transform to lower case
docs <- tm_map(docs,content_transformer(tolower))

#remove potentiallyy problematic symbols
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
# docs <- tm_map(docs, toSpace, "/|@|nn|")
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, "‘")
docs <- tm_map(docs, toSpace, "•")
docs <- tm_map(docs, toSpace, "•    ")
docs <- tm_map(docs, toSpace, " -")
docs <- tm_map(docs, toSpace, "“")
docs <- tm_map(docs, toSpace, "”")

#remove punctuation
docs <- tm_map(docs, removePunctuation)

#Strip digits
docs <- tm_map(docs, removeNumbers)

#remove stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))

#remove whitespace
docs <- tm_map(docs, stripWhitespace)

# create backup
backup <- docs

# stem the words - i.e. truncate words to their base form
docs <- tm_map(docs, stemDocument, mc.cores=1)
# if mc.cores=1 not used: Warning message:
#   In mclapply(content(x), FUN, ...) :
#   all scheduled cores encountered errors in user code

# create DocumentTermMatrix
dtm <- DocumentTermMatrix(docs)
dtm

#convert dtm to matrix
m <- as.matrix(dtm)
#write as csv file (optional)
write.csv(m,file="dtmatrix.csv")

#compute distance between document vectors
d <- dist(m)




















