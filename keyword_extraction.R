# keyword extraction ----
# developer: Afroditi Doriti
# version: 1.1
# changes: used only title and abstract
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

# create dataframe only with title and abstract
relevant_data <- data.frame(title = data$title, abstract = data$abstract, stringsAsFactors = FALSE)

# Create corpus
docs <- Corpus(DataframeSource(relevant_data))

#Transform to lower case
docs <- tm_map(docs, content_transformer(tolower))

#remove potentiallyy problematic symbols
toSpace <-
  content_transformer(function(x, pattern) {
    return (gsub(pattern, " ", x))
  })
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, "â")
docs <- tm_map(docs, toSpace, "â¢")
docs <- tm_map(docs, toSpace, "â¢    ")
docs <- tm_map(docs, toSpace, " -")
docs <- tm_map(docs, toSpace, "â")
docs <- tm_map(docs, toSpace, "â")

#remove punctuation
docs <- tm_map(docs, removePunctuation)

#Strip digits
docs <- tm_map(docs, removeNumbers)

# remove words in other alphabets
docs <- tm_map(docs, content_transformer(function(s){
  gsub(pattern = '[^a-zA-Z0-9\\s]+',
       x = s,
       replacement = " ",
       ignore.case = TRUE,
       perl = TRUE)}))

#remove stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))

#remove whitespace
docs <- tm_map(docs, stripWhitespace)

# create backup
backup <- docs

# stem the words - i.e. truncate words to their base form
docs <- tm_map(docs, stemDocument, mc.cores = 1)
# if mc.cores=1 not used: Warning message:
#   In mclapply(content(x), FUN, ...) :
#   all scheduled cores encountered errors in user code

# create DocumentTermMatrix
dtm <- DocumentTermMatrix(docs)
dtm

#convert dtm to matrix
m <- as.matrix(dtm)
#write as csv file (optional)
write.csv(m, file = "dtmatrix.csv")

#compute distance between document vectors
d <- dist(m)
