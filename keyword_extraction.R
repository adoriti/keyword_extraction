# ------ keyword extraction ----- #
# developer: Afroditi Doriti
# version: 5.0
# changes: did not concatenate the text, kept it in texts and checked for keywords
#          using tidytext and tf-idf
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
cl <- makeCluster(no_cores, type = "FORK")

# Register Cluster
registerDoParallel(cl)

# Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers()

# set wd and import files ----
# set wd
setwd("/home/adoriti/Documents/Keyword extraction")

# define input file:
input_file <- "polymers_renewable.csv"

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

# Hierarchical Clustering ----
# create DocumentTermMatrix
dtm <- DocumentTermMatrix(docs)
dtm

# convert dtm to matrix
m <- as.matrix(dtm)
# write as csv file (optional)
write.csv(m, file = "dtmatrix1000.csv")

#compute distance between document vectors
d <- dist(m)

# run hierarchical clustering using Wardâs method
groups <- hclust(d, method = "ward.D")
# plot dendogram, use hang to ensure that labels fall below tree
plot(groups, hang = -1)

# set number of subtrees to nrow/10
num_subtrees <- round(nrow(relevant_data) / 10)

# cut into subtrees
rect.hclust(groups, num_subtrees)

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
write.csv(to_analyze, "to_analyze.csv")

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

head(text_words)

# sort according to tf-idf
text_words <- text_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# plot most important words
text_words[text_words$cluster %in% 1:10, ] %>%
  mutate(words = factor(words, levels = rev(unique(words)))) %>%
  group_by(cluster) %>%
  top_n(5) %>%
  ungroup %>%
  ggplot(aes(words, tf_idf)) +
  geom_col(show.legend = FALSE, fill = "grey53") +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap( ~ cluster, ncol = 2, scales = "free") +
  coord_flip()

# write csv with results
write.csv(text_words, "tf-idf.csv")

# Bigrams ----
# check for bigrams
text_bigrams <- to_analyze %>%
  unnest_tokens(bigrams, texts, token = "ngrams", n = 2)

head(text_bigrams)
text_bigrams$bigrams

# sort the bigrams
text_bigrams %>%
  count(bigrams, sort = TRUE)

# remove bigrams with stopwords
bigrams_separated <- text_bigrams %>%
  separate(bigrams, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

bigram_counts

# unite the bigrams again:
bigrams_united <- bigrams_filtered %>%
  unite(bigrams, word1, word2, sep = " ")

head(bigrams_united, 50)

# also check for trigrams:
text_trigrams <- to_analyze %>%
  unnest_tokens(trigrams, texts, token = "ngrams", n = 3) %>%
  separate(trigrams, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,!word2 %in% stop_words$word,!word3 %in% stop_words$word) %>%
  count(cluster, word1, word2, word3, sort = TRUE) %>%
  unite(trigrams, word1, word2, word3, sep = " ")

head(text_trigrams)

# bigrams analysis with tf-idf
bigram_tf_idf <- bigrams_united %>%
  count(cluster, bigrams) %>%
  bind_tf_idf(bigrams, cluster, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

# same for trigrams
trigram_tf_idf <- text_trigrams %>%
  bind_tf_idf(trigrams, cluster, n) %>%
  arrange(desc(tf_idf))

trigram_tf_idf

# all important results ----
text_words

bigram_tf_idf

trigram_tf_idf

# show keywords


# Stop Cluster ----
stopCluster(cl)
