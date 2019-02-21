# ------ keyword extraction ----- #
# developer: Afroditi Doriti
# version: 6.0
# changes: used tidytext for keyword extraction, analyzed words,
#          bigrams, trigrams, and tetragrams
# description: keyword clustering, subsequent keyword extraction
# input: polymers_renewable.csv
# data ownership: MAPEGY
# ------------------------------- #
# outputs:
# ------------------------------- #

# load packages ----
library("tidyverse")
library("tm")
library("SnowballC")
library("doParallel")
library("tidytext")

# list.of.packages <- c("ggplot2", "Rcpp")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)

# use doParallel for speed ----
# Choose a number of cores to use (1 less than detected)
no_cores <- detectCores() - 1

# Create Cluster with desired number of cores. Don't use them all! Your computer is running other processes.
cl <- makeCluster(no_cores)

# Register Cluster
registerDoParallel(cl)

# to onfirm how many cores are now "assigned" to R and RStudio
# getDoParWorkers()

# set wd and import files ----
# set wd
setwd("/home/adoriti/Documents/Testing")

# define input file:
input_file <- "ABC_text_clustering.csv"

# read file:
data <- read.csv(input_file, stringsAsFactors = FALSE, sep = ";")

# Data preprocessing ----
# sort data according to score_relevance
data <- data %>% arrange(desc(score_relevance))

# create dataframe only with title and abstract
relevant_data <-
  data.frame(
    title = data$title[1:1000],
    abstract = data$abstract[1:1000],
    stringsAsFactors = FALSE
  )

# Create corpus
docs <- Corpus(DataframeSource(relevant_data))

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

# create backup
backup <- docs

# Hierarchical Clustering ----
# create DocumentTermMatrix
dtm <- DocumentTermMatrix(docs)

# convert dtm to matrix
m <- as.matrix(dtm)
# write as csv file (optional)
# write.csv(m, file = "dtmatrix1000.csv")

#compute distance between document vectors
d <- dist(m)

# run hierarchical clustering using Wardâs method
groups <- hclust(d, method = "ward.D")
# plot dendogram, use hang to ensure that labels fall below tree
# plot(groups, hang = -1)

# set number of subtrees to nrow/10
num_subtrees <- round(nrow(relevant_data) / 10)

# cut into subtrees
# rect.hclust(groups, num_subtrees)

# cut the tree into nrow/10 clusters
cut <- cutree(groups, k = num_subtrees)

# Analyze clusters with tf-idf ----
# add cluster info to the dataframe
datadf <- cbind(relevant_data, cluster = cut)

# arrange according to cluster
datadf <- datadf %>% arrange(cluster)

# merge title and abstract
to_analyze <- datadf %>% unite(texts, title, abstract, sep = " ")

# (optional) write the results in a csv
# write.csv(to_analyze, "to_analyze.csv")

# tokenize the texts and avoid stopwords
text_words <- to_analyze %>%
  unnest_tokens(words, texts) %>%
  filter(!words %in% stop_words$word) %>%
  count(cluster, words, sort = TRUE) %>%
  ungroup()

# check total words per cluster
total_words <- text_words %>%
  group_by(cluster) %>%
  summarize(total = sum(n))

text_words <- left_join(text_words, total_words)

# use tf-idf
text_words <- text_words %>%
  bind_tf_idf(words, cluster, n)

# sort according to tf-idf
text_words <- text_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# (optional) write csv with results
# write.csv(text_words, "tf-idf.csv")

# Bigrams, trigrams, and tetragrams ----
# check for bigrams
text_bigrams <- to_analyze %>%
  unnest_tokens(bigrams, texts, token = "ngrams", n = 2) %>%
  separate(bigrams, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,!word2 %in% stop_words$word) %>%
  count(cluster, word1, word2, sort = TRUE) %>%
  unite(bigrams, word1, word2, sep = " ")

# check for trigrams:
text_trigrams <- to_analyze %>%
  unnest_tokens(trigrams, texts, token = "ngrams", n = 3) %>%
  separate(trigrams, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,!word2 %in% stop_words$word,!word3 %in% stop_words$word) %>%
  count(cluster, word1, word2, word3, sort = TRUE) %>%
  unite(trigrams, word1, word2, word3, sep = " ")

# check for tetragrams:
text_tetragrams <- to_analyze %>%
  unnest_tokens(tetragrams, texts, token = "ngrams", n = 4) %>%
  separate(tetragrams, c("word1", "word2", "word3", "word4"), sep = " ") %>%
  filter(
    !word1 %in% stop_words$word,
    !word2 %in% stop_words$word,!word3 %in% stop_words$word,
    !word4 %in% stop_words$word
  ) %>%
  count(cluster, word1, word2, word3, word4, sort = TRUE) %>%
  unite(tetragrams, word1, word2, word3, word4, sep = " ")

# bigrams analysis with tf-idf
bigram_tf_idf <- text_bigrams %>%
  bind_tf_idf(bigrams, cluster, n) %>%
  arrange(desc(tf_idf))

# same for trigrams
trigram_tf_idf <- text_trigrams %>%
  bind_tf_idf(trigrams, cluster, n) %>%
  arrange(desc(tf_idf))

# same for tetragrams
tetragram_tf_idf <- text_tetragrams %>%
  bind_tf_idf(tetragrams, cluster, n) %>%
  arrange(desc(tf_idf))

# all important results ----
# text_words
#
# bigram_tf_idf
#
# trigram_tf_idf

# separate bigrams
bigrams_separated <- bigram_tf_idf %>%
  separate(bigrams, c("word1", "word2"), sep = " ")

# separate trigrams
trigrams_separated <- trigram_tf_idf %>%
  separate(trigrams, c("word1", "word2", "word3"), sep = " ")

# separate tetragrams
tetragrams_separated <- tetragram_tf_idf %>%
  separate(tetragrams, c("word1", "word2", "word3", "word4"), sep = " ")

# choose text_words to show
final_text_words <- text_words %>%
  filter(!words %in% bigrams_separated$word1[1:20]) %>%
  filter(!words %in% bigrams_separated$word2[1:20])

# choose bigrams to show
final_bigrams <- bigram_tf_idf %>%
  separate(bigrams, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% trigrams_separated$word1[1:20]) %>%
  filter(!word1 %in% trigrams_separated$word2[1:20]) %>%
  filter(!word1 %in% trigrams_separated$word3[1:20]) %>%
  filter(!word1 %in% tetragrams_separated$word1[1:10]) %>%
  filter(!word1 %in% tetragrams_separated$word2[1:10]) %>%
  filter(!word1 %in% tetragrams_separated$word3[1:10]) %>%
  filter(!word1 %in% tetragrams_separated$word4[1:10]) %>%
  unite(bigrams, word1, word2, sep = " ")


# show keywords with n >= 15
for(i in 1:10) {
  if (final_text_words$n[i] >= 15) {
    print(final_text_words$words[i])
  }
}

# show bigrams with n >= 10
for (i in 1:10) {
  if (final_bigrams$n[i] >= 10) {
    print(final_bigrams$bigrams[i])
  }
}

# show trigrams with n >= 10
for (i in 1:10) {
  if (trigram_tf_idf$n[i] >= 10) {
    print(trigram_tf_idf$trigrams[i])
  }
}

# optional, tetragrams with n >= 8
for (i in 1:10) {
  if (tetragram_tf_idf$n[i] >= 8) {
    print(tetragram_tf_idf$tetragrams[i])
  }
}

# Stop Cluster ----
stopCluster(cl)
