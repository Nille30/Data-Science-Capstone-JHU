#Loading libraries
library(RWekajars)
library(qdapDictionaries)
library(qdapRegex)
library(qdapTools)
library(RColorBrewer)
library(qdap)
library(NLP)
library(tm)
library(SnowballC)
library(slam)
library(RWeka)
library(rJava)
library(wordcloud)
library(stringr)
library(DT)
library(stringi)

## Sampeling
## Loading the original data set
blogs_data <- readLines("./Data/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul=TRUE)
news_data <- readLines("./Data/en_US/en_US.news.txt", encoding = "UTF-8", skipNul=TRUE)
twitter_data <- readLines("./Data/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul=TRUE)

## Generating a random sapmle of all sources
twitter_sample <- twitter_data[sample(1:length(twitter_data),10000)]
news_sample <- news_data[sample(1:length(news_data),10000)]
blogs_sample <- blogs_data[sample(1:length(blogs_data),10000)]
combined_sample <- c(twitter_sample,news_sample,blogs_sample)

## Saving sample
writeLines(combined_sample, "./combined_sample.txt")


## Checking the size and length of the files and calculate the word count
blogs_size <- file.info("./Data/en_US/en_US.blogs.txt")$size / 1024.0^2
news_size <- file.info("./Data/en_US/en_US.news.txt")$size / 1024.0^2
twitter_size <- file.info("./Data/en_US/en_US.twitter.txt")$size / 1024.0^2
blogs_length <- length(blogs_data)
news_length <- length(news_data)
twitter_length <- length(twitter_data)
blogs_no_words <- sum(sapply(gregexpr("\\S+", blogs_data), length))
news_no_words <- sum(sapply(gregexpr("\\S+", news_data), length))
twitter_no_words <- sum(sapply(gregexpr("\\S+", twitter_data), length))

summary_matrix <- data.frame(
  file_name = c("Blogs","News","Twitter"),
  file_size = c(round(blogs_size, digits = 2), round(news_size,digits = 2), round(twitter_size, digits = 2)),
  line_count = c(blogs_length, news_length, twitter_length),
  word_count = c(blogs_no_words, news_no_words, twitter_no_words))

colnames(summary_matrix) <- c("File Name", "File Size (MB)", "No. of Lines", "No. of Words")
saveRDS(summary_matrix, file = "./data_summary.Rda")
summary_df <- readRDS("./data_summary.Rda")

## Building a clean corpus
sample_con <- file("./combined_sample.txt")
sample_text <- readLines(sample_con)
close(sample_con)
profanity_words <- read.table("./Data/profanity_words.txt", header = FALSE)

## Build the corpus, and specify the source to be character vectors 
cleaned_sample <- Corpus(VectorSource(sample_text))
rm(sample_text)
cleaned_sample <- tm_map(cleaned_sample, content_transformer(function(x) iconv(x, to="UTF-8", sub="byte")))

## Converting to lower case
cleaned_sample <- tm_map(cleaned_sample, content_transformer(tolower))

## Removing punction, numbers, URLs, stop, profanity and stem wordson
cleaned_sample <- tm_map(cleaned_sample, content_transformer(removePunctuation))
cleaned_sample <- tm_map(cleaned_sample, content_transformer(removeNumbers))
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x) 
cleaned_sample <- tm_map(cleaned_sample, content_transformer(removeURL))
cleaned_sample <- tm_map(cleaned_sample, stripWhitespace)
cleaned_sample <- tm_map(cleaned_sample, removeWords, stopwords("english"))
cleaned_sample <- tm_map(cleaned_sample, removeWords, profanity_words)
cleaned_sample <- tm_map(cleaned_sample, stemDocument)
cleaned_sample <- tm_map(cleaned_sample, stripWhitespace)

saveRDS(cleaned_sample, file = "./Data/corp_final.RDS")

# Exploratory analysis
## Budilding n-grams
corp_final <- readRDS("./Data/corp_final.RDS")
corp_final_df <-data.frame(text=unlist(sapply(corp_final,`[`, "content")), 
                           stringsAsFactors = FALSE)

## Building tokenization function for the n-grams
ngram_tokenizer <- function(corp, ngramCount) {
  ngram_func <- ngram_tokenizer(corp, 
                                  Weka_control(min = ngramCount, max = ngramCount, 
                                               delimiters = " \\r\\n\\t.,;:\"()?!"))
  ngram_func <- data.frame(table(ngram_func))
  ngram_func <- ngram_func[order(ngram_func$Freq, 
                                       decreasing = TRUE),][1:10,]
  colnames(ngram_func) <- c("String","Count")
  ngram_func
}

uni_gram <- ngram_tokenizer(corp_final_df, 1)
saveRDS(tri_gram, file = "./Data/uni_gram.RDS")
bi_gram <- ngram_tokenizer(corp_final_df, 2)
saveRDS(bi_gram, file = "./Data/bi_gram.RDS")
tri_gram <- ngram_tokenizer(corp_final_df, 3)
saveRDS(tri_gram, file = "./Data/tri_gram.RDS")

## uni_gram plot
uni_gram <- readRDS("./Data/uni_gram.RDS")
uni_gram_plot <- gvisColumnChart(uni_gram, "String", "Count",                  
                               options=list(legend="none"))
## bi_gram plot
bi_gram <- readRDS("./Data/bi_gram.RDS")
bi_gram_plot <- gvisColumnChart(bi_gram, "String", "Count",                  
                              options=list(legend="none"))
## tri_gram plot
tri_gram <- readRDS("./Data/tri_gram.RDS")
tri_gram_plot <- gvisColumnChart(tri_gram, "String", "Count",                  
                               options=list(legend="none"))

## Creating wordcloud
trigramTDM <- TermDocumentMatrix(corp_final)
word_cloud <- as.matrix(trigramTDM)
sorted_tri_gram <- sort(rowSums(word_cloud),decreasing=TRUE)
sorted_tri_gram_df <- data.frame(word = names(sorted_tri_gram),freq=sorted_tri_gram)
wordcloud(sorted_tri_gram_df$word,sorted_tri_gram_df$freq, c(5,.3),50, random.order=FALSE, colors=brewer.pal(8, "Accent"))
