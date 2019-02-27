# ------ keyword extraction ----- #
# developer: Afroditi Doriti
# version: 11.0
# changes: used parallelDist and only patents
# description: keyword clustering, subsequent keyword extraction
# input: polymers_renewable.csv
# data ownership: MAPEGY
# ------------------------------- #
# outputs:
# ------------------------------- #

# load packages ----
if (!require("pacman"))
  install.packages("pacman")

pacman::p_load(
  "tidyverse",
  "tm",
  "SnowballC",
  "doParallel",
  "tidytext",
  "tictoc",
  "fastcluster",
  "parallelDist",
  "Matrix",
  "lsa",
  "proxy",
  "spacyr"
)

# start measuring time
tic("total")
tic("preprocessing")

# functions ----
# keyword tf-idf
keywords_tf_idf <- function(tokenized_search_query, dataframe) {
  # find keywords ----
  text_words <- dataframe %>%
    unnest_tokens(words, text) %>%
    filter(!words %in% stop_words$word) %>%
    filter(is.na(as.numeric(words))) %>%
    filter(str_detect(words, "^[^>]+[A-Za-z\\d]")) %>%
    count(doc_id, words, sort = TRUE) %>%
    ungroup()
  
  # check total words per cluster
  total_words <- text_words %>%
    group_by(doc_id) %>%
    summarize(total = sum(n))
  
  # add total words to the dataframe
  text_words <- left_join(text_words, total_words)
  
  # find bigrams
  text_bigrams <- dataframe %>%
    unnest_tokens(bigrams, text, token = "ngrams", n = 2) %>%
    separate(bigrams, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,!word2 %in% stop_words$word) %>%
    filter(is.na(as.numeric(word1)), is.na(as.numeric(word2))) %>%
    filter(str_detect(word1, "^[^>]+[A-Za-z\\d]"),
           str_detect(word2, "^[^>]+[A-Za-z\\d]")) %>%
    count(doc_id, word1, word2, sort = TRUE) %>%
    unite(bigrams, word1, word2, sep = " ")
  
  # check for trigrams:
  text_trigrams <- dataframe %>%
    unnest_tokens(trigrams, text, token = "ngrams", n = 3) %>%
    separate(trigrams, c("word1", "word2", "word3"), sep = " ") %>%
    filter(
      !word1 %in% stop_words$word,!word2 %in% stop_words$word,!word3 %in% stop_words$word
    ) %>%
    filter(is.na(as.numeric(word1)), is.na(as.numeric(word2)),
           is.na(as.numeric(word3))) %>%
    filter(
      str_detect(word1, "^[^>]+[A-Za-z\\d]"),
      str_detect(word2, "^[^>]+[A-Za-z\\d]"),
      str_detect(word3, "^[^>]+[A-Za-z\\d]")
    ) %>%
    count(doc_id, word1, word2, word3, sort = TRUE) %>%
    unite(trigrams, word1, word2, word3, sep = " ")
  
  # check for tetragrams:
  text_tetragrams <- dataframe %>%
    unnest_tokens(tetragrams, text, token = "ngrams", n = 4) %>%
    separate(tetragrams, c("word1", "word2", "word3", "word4"), sep = " ") %>%
    filter(
      !word1 %in% stop_words$word,
      !word2 %in% stop_words$word,!word3 %in% stop_words$word,
      !word4 %in% stop_words$word
    ) %>%
    filter(
      str_detect(word1, "^[^>]+[A-Za-z\\d]"),
      str_detect(word2, "^[^>]+[A-Za-z\\d]"),
      str_detect(word3, "^[^>]+[A-Za-z\\d]"),
      str_detect(word4, "^[^>]+[A-Za-z\\d]")
    ) %>%
    count(doc_id, word1, word2, word3, word4, sort = TRUE) %>%
    unite(tetragrams, word1, word2, word3, word4, sep = " ")
  
  # tf-idf ----
  # use tf-idf for text_words
  text_words <- text_words %>%
    bind_tf_idf(words, doc_id, n)
  
  # sort text_words according to tf-idf
  text_words_tf_idf <- text_words %>%
    select(-total) %>%
    arrange(desc(tf_idf))
  
  # bigrams analysis with tf-idf
  bigram_tf_idf <- text_bigrams %>%
    bind_tf_idf(bigrams, doc_id, n) %>%
    arrange(desc(tf_idf))
  
  # same for trigrams
  trigram_tf_idf <- text_trigrams %>%
    bind_tf_idf(trigrams, doc_id, n) %>%
    arrange(desc(tf_idf))
  
  # same for tetragrams
  tetragram_tf_idf <- text_tetragrams %>%
    bind_tf_idf(tetragrams, doc_id, n) %>%
    arrange(desc(tf_idf))
  
  # POS tagging ----
  # for text_words
  POS_text_words <-
    spacy_parse(
      text_words_tf_idf$words[1:40],
      pos = TRUE,
      lemma = FALSE,
      entity = FALSE
    )
  
  # for bigrams
  POS_bigrams <-
    spacy_parse(
      bigram_tf_idf$bigrams[1:40],
      pos = TRUE,
      lemma = FALSE,
      entity = FALSE
    )
  
  # for trigrams
  POS_trigrams <-
    spacy_parse(
      trigram_tf_idf$trigrams[1:40],
      pos = TRUE,
      lemma = FALSE,
      entity = FALSE
    )
  
  # for tetragrams
  POS_tetragrams <-
    spacy_parse(
      tetragram_tf_idf$tetragrams[1:40],
      pos = TRUE,
      lemma = FALSE,
      entity = FALSE
    )
  
  # allowed entities (only keywords or keyphrases that end with a noun are allowed) ----
  # allowed text_words
  allowed_text_words <-
    as.numeric(rownames(POS_text_words[POS_text_words$pos %in%
                                         c("NOUN", "PRON", "PRORN", "PART"), ]))
  
  # allowed bigrams
  allowed_bigrams <-
    as.numeric(rownames(POS_bigrams[POS_bigrams$token_id == 2 &
                                      POS_bigrams$pos %in%
                                      c("NOUN", "PRON", "PRORN", "PART"), ])) /
    2
  
  # allowed trigrams
  allowed_trigrams <-
    as.numeric(rownames(POS_trigrams[POS_trigrams$token_id == 3 &
                                       POS_trigrams$pos %in%
                                       c("NOUN", "PRON", "PRORN", "PART"), ])) /
    3
  
  # allowed tetragrams
  allowed_tetragrams <-
    as.numeric(rownames(POS_tetragrams[POS_tetragrams$token_id == 4 &
                                         POS_tetragrams$pos %in%
                                         c("NOUN", "PRON", "PRORN", "PART"), ])) /
    4
  
  # separate n-grams (to use in the next part to avoid duplicates) ----
  # separate bigrams
  bigrams_separated <- bigram_tf_idf %>%
    separate(bigrams, c("word1", "word2"), sep = " ")
  
  # separate trigrams
  trigrams_separated <- trigram_tf_idf %>%
    separate(trigrams, c("word1", "word2", "word3"), sep = " ")
  
  # separate tetragrams
  tetragrams_separated <- tetragram_tf_idf %>%
    separate(tetragrams, c("word1", "word2", "word3", "word4"), sep = " ")
  
  # final keywords to use ----
  # here we avoid duplicates in the different n-grams, e.g.
  # you don't repeat a bigram as part of a trigram, etc.
  # choose text_words to show
  final_text_words <- text_words_tf_idf[allowed_text_words,] %>%
    filter(!words %in% bigrams_separated$word1[1:10]) %>%
    filter(!words %in% bigrams_separated$word2[1:10]) %>%
    mutate(stemWord = wordStem(words)) %>%
    filter(!stemWord %in% tokenized_search_query$stemWord)
  
  # choose bigrams to show
  final_bigrams <- bigram_tf_idf[allowed_bigrams,] %>%
    separate(bigrams, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% trigrams_separated$word1[1:10]) %>%
    filter(!word1 %in% trigrams_separated$word2[1:10]) %>%
    filter(!word1 %in% trigrams_separated$word3[1:10]) %>%
    filter(!word1 %in% tetragrams_separated$word1[1:10]) %>%
    filter(!word1 %in% tetragrams_separated$word2[1:10]) %>%
    filter(!word1 %in% tetragrams_separated$word3[1:10]) %>%
    filter(!word1 %in% tetragrams_separated$word4[1:10]) %>%
    unite(bigrams, word1, word2, sep = " ")
  
  # choose trigrams to show
  final_trigrams <- trigram_tf_idf[allowed_trigrams,] %>%
    separate(trigrams, c("word1", "word2", "word3"), sep = " ") %>%
    filter(!word1 %in% tetragrams_separated$word1[1:5]) %>%
    filter(!word1 %in% tetragrams_separated$word2[1:5]) %>%
    filter(!word1 %in% tetragrams_separated$word3[1:5]) %>%
    filter(!word1 %in% tetragrams_separated$word4[1:5]) %>%
    unite(trigrams, word1, word2, word3, sep = " ")
  
  # choose tetragrams to show
  final_tetragrams <- tetragram_tf_idf[allowed_tetragrams,]
  
  # results ----
  result <-
    c(
      unique(final_text_words$words[1:3]),
      unique(final_bigrams$bigrams[1:2]),
      unique(final_trigrams$trigrams[1:2]),
      unique(text_tetragrams$tetragrams[1:2])
    )
  
  return(result)
}

# doParallel ----
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
setwd("/home/adoriti/Documents/Keyword extraction")

# define input file:
input_file <- "polymers_renewable.csv"

# search query
search_query <- "polymers renewable"
search_query <- as.data.frame(search_query)
search_query$search_query <- as.character(search_query$search_query)

# read file:
data <- read.csv(input_file, stringsAsFactors = FALSE, sep = ";")

# Data preprocessing ----
# sort data according to score_relevance
data <- data %>% arrange(desc(score_relevance)) %>%
  filter(doc_type == "PATENT") %>% # select only patents
  distinct(title, abstract, .keep_all = TRUE) %>% # remove duplicates of text
  unite(text, title, abstract, sep = " ")

# create dataframe only with title and abstract
relevant_data <-
  data.frame(
    doc_id = 1:1000,
    text = data$text[1:1000],
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
docs <- tm_map(docs, stemDocument)
# if mc.cores=1 not used: Warning message:
#   In mclapply(content(x), FUN, ...) :
#   all scheduled cores encountered errors in user code

# create backup
backup <- docs

# Hierarchical Clustering ----
# create DocumentTermMatrix
dtm <- DocumentTermMatrix(docs)

# make a tf-idf weighted dtm
dtm_tfidf <- weightTfIdf(dtm)

# remove sparse terms
dtm_tfidf <- tm::removeSparseTerms(dtm_tfidf, 0.999)

# make the dtm into a matrix
tfidf_matrix <- as.matrix(dtm_tfidf)

# Cosine distance matrix (from pachage "proxy")
dist_matrix <- dist(tfidf_matrix, method = "cosine")

# hierarchical clustering
groups <- hclust(dist_matrix, method = "ward.D")

# plot dendogram, use hang to ensure that labels fall below tree
# plot(groups, hang = -1)

# calculate number of clusters according to square root of (number of document/2)
num_subtrees <- round((nrow(tfidf_matrix) / 2) ** 0.5)

# cut into subtrees
# rect.hclust(groups, num_subtrees)

# cut the tree into num_subtrees clusters
cut <- cutree(groups, k = num_subtrees)

# time for preprocessing
preprocessing <- toc()

# time for keyword extraction
tic("keyword extraction")

# Analyze clusters with tf-idf ----
# add cluster info to the dataframe
to_analyze <- cbind(relevant_data, cluster = cut)

# (optional) write the results in a csv
write.csv(to_analyze, "to_analyze_90.csv")

# tokenize search_query
tokenized_search_query <- search_query %>% unnest_tokens(words,
                                                         search_query) %>%
  mutate(stemWord = wordStem(words))

# Keywords per cluster ----
# per cluster
for (i in 1:num_subtrees) {
  clustered <- to_analyze$cluster == i
  print(i)
  print(suppressWarnings(keywords_tf_idf(tokenized_search_query,
                                         to_analyze[clustered, ])))
}

# optionally for specific clusters
suppressWarnings(keywords_tf_idf(tokenized_search_query,
                                 to_analyze[to_analyze$cluster == 2, ]))

# total time elapsed()
total_time <- toc()

# Stop Cluster ----
stopCluster(cl)
