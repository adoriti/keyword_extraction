# keyword extraction ----
# developer: Afroditi Doriti
# version: 1.2
# changes: used only title and abstract, used a sample of 1000 papers
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
relevant_data <-
  data.frame(
    title = data$title,
    abstract = data$abstract,
    stringsAsFactors = FALSE
  )

# create a sample of 1000 documents to check
set.seed(1988)
sampled <- sample(nrow(relevant_data), 1000)

# Create corpus
docs <- Corpus(DataframeSource(relevant_data[sampled, ]))

#Transform to lower case
docs <- tm_map(docs, content_transformer(tolower))

#remove potentiallyy problematic symbols
toSpace <-
  content_transformer(function(x, pattern) {
    return (gsub(pattern, " ", x))
  })
docs <- tm_map(docs, toSpace, "http[[:alnum:][:punct:]]*")
docs <- tm_map(docs, toSpace, "www[[:alnum:][:punct:]]*")
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
docs <- tm_map(docs, content_transformer(function(s) {
  gsub(
    pattern = '[^a-zA-Z0-9\\s]+',
    x = s,
    replacement = " ",
    ignore.case = TRUE,
    perl = TRUE
  )
}))

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
write.csv(m, file = "dtmatrix1000.csv")

#compute distance between document vectors
d <- dist(m)

#run hierarchical clustering using Wardâs method
groups <- hclust(d, method = "ward.D")
#plot dendogram, use hang to ensure that labels fall below tree
plot(groups, hang = -1)

#cut into 5 subtrees â try 3 and 5
rect.hclust(groups, 5)

# cut the tree into 5 clusters
cut <- cutree(groups, k = 5)

# add cluster info to the dataframe
datadf <- cbind(relevant_data[sampled, ], cluster = cut)

# arrange according to cluster
datadf <- datadf %>% arrange(cluster)

# remove cluster
datadf$cluster <- NULL

# merge columns
to_analyze <- datadf %>% unite(title, abstract)

# concatenate results
text <- paste(to_analyze, collapse = " ")


