library(fs)
library(tidyverse)
library(tidytext)
library(widyr)
library(tm)
library(dplyr)
library(readr)
library(ggplot2)
library(SnowballC)
library(stringr)
library(igraph)
library(ggraph)
library(wordcloud)
library(plotly)
library(syuzhet)

#############################################################################################
# function to clean Mercola Website files w/unnest_tokens

mercola_clean_text <- function(mydataset) {
  to_clean <- mydataset
  
  # transform table into one-word-per-line tidytext format
  to_clean <- to_clean %>%
    unnest_tokens(word, text)
  
  # remove stop words
  data(stop_words)
  to_clean <- to_clean %>%
    anti_join(stop_words)
  
  # remove numbers
  nums <- to_clean %>% filter(str_detect(word, "^[0-9]")) %>% select(word) %>% unique()
  to_clean <- to_clean %>%
    anti_join(nums, by = "word")
  
  # remove other words
  uni_sw <- data.frame(word = c("percent", "study", "day", "lower", "reduced", "found", "dr",
                                "including", "reduce", "time", "level", "report", "u.s", "http",
                                "because", "sites", "article", "archive", "levels", "note", "studies",
                                "www.mercola.com", "online", "articles.mercola.com", "mercola", 
                                "people", "recommend", "person", "www.nvic.org", "www.nvicadvocacy.org",
                                "effect", "effects", "effecting", "effected", "increase", "increased", 
                                "increasing", "increases", "mercola"))
  to_clean <- to_clean %>%
    anti_join(uni_sw, by = "word")
  
  to_clean <- to_clean %>%
    mutate(word = wordStem(word))
}


####################################################################
# function to visualize top words

visualize_top_words <- function(cleaned_data, wordamt, title) {
  data <- cleaned_data
  data %>%
    count(word, sort = TRUE) %>%
    filter(n > wordamt) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    ylab(title) +
    coord_flip()
}

############################################################################
# produces visualization of 9 most freqent words and the words that correlate
# with them
############################################################################
visualize_word_cors <- function(cleaned_data, topWords, twTitle){
  
  dataset <- cleaned_data
  wordlist <- topWords
  title <- twTitle
  
  word_cors <- dataset %>%
    group_by(word) %>%
    filter(n() >= 20) %>%
    pairwise_cor(word, url, sort = TRUE)

  # produce graph comparing 4 words of importance to their most correlated words
  word_cors %>%
    filter(item1 %in% wordlist) %>%
    group_by(item1) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(item2 = reorder(item2, correlation)) %>%
    ggplot(aes(item2, correlation)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ item1, scales = "free") +
    coord_flip() +
    ggtitle(title)
}

########################################################################
# function to visualize bigram relationship
########################################################################
count_bigrams <- function(dataset) {
  uni_sw <- data.frame(word = c("percent", "found", "dr", "including", "u.s", "http", "because", "sites", "article", "archive",
                                "www.mercola.com", "online", "articles.mercola.com", "mercola" ))
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word,
           !word1 %in% uni_sw$word,
           !word2 %in% uni_sw$word
           ) %>%
    count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams, title) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 2) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1, size=3) +
    theme_void() +
    ggtitle(title)
}

###########################################################################
# Function for Word cloud with Sentiment Analysis
###########################################################################

word_cloud_Merc <- function(dataset, title){
  data <- dataset
 
  #cleaning
  data$title <- iconv(data$title, "UTF-8", "ASCII", sub="") 
  
  # clean data
  data$title <- data$title %>%
    removePunctuation() %>%
    removeNumbers() %>%
    tolower() %>%
    removeWords(stopwords("SMART")) %>%
    #stemDocument() %>%
    stripWhitespace()
  
  #cleaning
  data$text <- iconv(data$text, "UTF-8", "ASCII", sub="")
  
  # Emotions for each tweet using NRC dictionary
  emotions <- get_nrc_sentiment(data$title)
  
  #create word cloud columns
  wordcloud_tweet = c(
    paste(data$title[emotions$anger > 0], collapse=" "),
    paste(data$title[emotions$anticipation >0], collapse=" "),
    paste(data$title[emotions$disgust > 0], collapse=" "),
    paste(data$title[emotions$fear > 0], collapse=" "),
    paste(data$title[emotions$joy > 0], collapse=" "),
    paste(data$title[emotions$sadness > 0], collapse=" "),
    paste(data$title[emotions$surprise > 0], collapse=" "),
    paste(data$title[emotions$trust > 0], collapse=" ")
  )
  
  # create corpus and covert to matrix
  corpus = Corpus(VectorSource(wordcloud_tweet))
  tdm = as.matrix(TermDocumentMatrix(corpus))
  tdmnew <- tdm[nchar(rownames(tdm)) < 15,]
  
  # column name binding
  colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
  colnames(tdmnew) <- colnames(tdm)
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5, y=0.5, title)
  comparison.cloud(tdmnew, random.order=FALSE,
                   colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                   title.size=1, max.words=250, scale=c(5, 0.3),rot.per=0.4, main="Title")
}

################################################################################
# function to clean Mercola Facebook files w/unnest_tokens
################################################################################

mercola_FBclean_text <- function(mydataset) {
  to_clean <- mydataset
  
  to_clean$Message <- iconv(to_clean$Message, "UTF-8", "ASCII", sub="")
  
  # transform table into one-word-per-line tidytext format
  to_clean <- to_clean %>%
    unnest_tokens(word, Message)
  
  # remove stop words
  data(stop_words)
  to_clean <- to_clean %>%
    anti_join(stop_words)
  
  # remove numbers
  nums <- to_clean %>% filter(str_detect(word, "^[0-9]")) %>% select(word) %>% unique()
  to_clean <- to_clean %>%
    anti_join(nums, by = "word")
  
  # remove other words
  uni_sw <- data.frame(word = c("mercola", "dr", "http", "np", "utm_source", "utm_medium", "utm_content",
                                "utm_campaign", "https", "sites", "click", "bit.ly", "facebookmercola_ranart", "u.", "html",
                                "dont", "mercola", "u.s", "u.s.a", "youve", "rt", "t.co", "twitter", "message",
                               "sharyl", "content", "continue", "select"))
  to_clean <- to_clean %>%
    anti_join(uni_sw, by = "word")
  
  to_clean <- to_clean %>%
    mutate(word = wordStem(word))  

}

############################################################################
# produces visualization of 9 most freqent words and the words that correlate
# with them (for Facebook Posts)
############################################################################
FB_visualize_word_cors <- function(cleaned_data, topWords, twTitle, nwords){
  number <- nwords
  dataset <- cleaned_data
  wordlist <- topWords
  title <- twTitle
  
  word_cors <- dataset %>%
    group_by(word) %>%
    filter(n() >= number) %>%
    pairwise_cor(word, url, sort = TRUE)
  
  # produce graph comparing 4 words of importance to their most correlated words
  word_cors %>%
    filter(item1 %in% wordlist) %>%
    group_by(item1) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(item2 = reorder(item2, correlation)) %>%
    ggplot(aes(item2, correlation)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ item1, scales = "free") +
    coord_flip() +
    ggtitle(title)
}

######################################################################
# function to visualize Facebook posts bigram relationships
######################################################################
fb_count_bigrams <- function(dataset) {
  uni_sw <- data.frame(word = c("percent", "found", "dr", "including", "u.s", "http", "because", "sites", "article", "archive",
                                "www.mercola.com", "online", "articles.mercola.com", "mercola", "rt", "htt", "t.co", 
                                "tag", "twit", "https", "page", "posting", "click", "select", "ca"))
  #cleaning
  dataset$Message <- iconv(dataset$Message, "UTF-8", "ASCII", sub="")
  
  dataset %>%
    unnest_tokens(bigram, Message, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word,
           !word1 %in% uni_sw$word,
           !word2 %in% uni_sw$word,
    ) %>%
    count(word1, word2, sort = TRUE)
}

fb_visualize_bigrams <- function(bigrams, title) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 2) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1, size=3) +
    theme_void() +
    ggtitle(title)
}

###########################################################################
# Function for FB Word cloud with Sentiment Analysis
###########################################################################

FB_word_cloud_Merc <- function(dataset, title){
  data <- dataset
  
  #cleaning
  data$Message <- iconv(data$Message, "UTF-8", "ASCII", sub="") 
  
  # clean data
  data$Message <- data$Message %>%
    removePunctuation() %>%
    removeNumbers() %>%
    tolower() %>%
    removeWords(stopwords("SMART")) %>%
    #stemDocument() %>%
    stripWhitespace()
  
  #cleaning
  data$Message <- iconv(data$Message, "UTF-8", "ASCII", sub="")
  
  # Emotions for each tweet using NRC dictionary
  emotions <- get_nrc_sentiment(data$Message)
  
  #create word cloud columns
  wordcloud_Message = c(
    paste(data$Message[emotions$anger > 0], collapse=" "),
    paste(data$Message[emotions$anticipation >0], collapse=" "),
    paste(data$Message[emotions$disgust > 0], collapse=" "),
    paste(data$Message[emotions$fear > 0], collapse=" "),
    paste(data$Message[emotions$joy > 0], collapse=" "),
    paste(data$Message[emotions$sadness > 0], collapse=" "),
    paste(data$Message[emotions$surprise > 0], collapse=" "),
    paste(data$Message[emotions$trust > 0], collapse=" ")
  )
  
  # create corpus and covert to matrix
  corpus = Corpus(VectorSource(wordcloud_Message))
  tdm = as.matrix(TermDocumentMatrix(corpus))
  tdmnew <- tdm[nchar(rownames(tdm)) < 15,]
  
  # column name binding
  colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
  colnames(tdmnew) <- colnames(tdm)
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5, y=0.5, title)
  comparison.cloud(tdmnew, random.order=FALSE,
                   colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                   title.size=1, max.words=250, scale=c(5, 0.3),rot.per=0.4, main="Title")
}

############################################################################
# produces visualization of 9 most freqent words and the words that correlate
# with them (for Tweets)
############################################################################
TW_visualize_word_cors <- function(cleaned_data, topWords, twTitle, nwords){
  number <- nwords
  dataset <- cleaned_data
  wordlist <- topWords
  title <- twTitle
  
  word_cors <- dataset %>%
    group_by(word) %>%
    filter(n() >= number) %>%
    pairwise_cor(word, tweet.datetime, sort = TRUE)
  
  # produce graph comparing 4 words of importance to their most correlated words
  word_cors %>%
    filter(item1 %in% wordlist) %>%
    group_by(item1) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(item2 = reorder(item2, correlation)) %>%
    ggplot(aes(item2, correlation)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ item1, scales = "free") +
    coord_flip() +
    ggtitle(title)
}
