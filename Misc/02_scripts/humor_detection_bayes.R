library(tidyverse) #metapackage
library(tm) #text mining

data<-read_csv('./data/humor_detection_dataset.csv')
str(data)
data$humor <- factor(data$humor)
str(data$humor)
table(data$humor)
data_corpus <- VCorpus(VectorSource(data$text))

print(data_corpus)
inspect(data_corpus[1:2])

as.character(data_corpus[[1]])
lapply(data_corpus[1:2], as.character)

data_corpus_clean <- tm_map(data_corpus, content_transformer(tolower))

as.character(data_corpus[[1]])
as.character(data_corpus_clean[[1]])

data_corpus_clean <- tm_map(data_corpus_clean, removeNumbers)
data_corpus_clean <- tm_map(data_corpus_clean, removeWords, stopwords())
data_corpus_clean <- tm_map(data_corpus_clean, removePunctuation)

replacePunctuation <- function(x) { gsub("[[:punct:]]+", " ", x) }
data_corpus_clean <- tm_map(data_corpus_clean, stemDocument)
data_corpus_clean <- tm_map(data_corpus_clean, stripWhitespace)
lapply(data_corpus[1:3], as.character)
lapply(data_corpus_clean[1:3], as.character)

data_dtm <- DocumentTermMatrix(data_corpus_clean)

data_dtm_train <- data_dtm[1:24000, ]
data_dtm_test  <- data_dtm[24001:30000, ]

data_train_labels <- data[1:24000, ]$humor
data_test_labels  <- data[24001:30000, ]$humor

prop.table(table(data_train_labels))
prop.table(table(data_test_labels))

library(wordcloud)

humor <- subset(data, humor == "TRUE")
nothumor<- subset(data, humor == "FALSE")
wordcloud(humor$text, max.words = 40, scale = c(3, 0.5))
wordcloud(nothumor$text, max.words = 40, scale = c(3, 0.5))

data_dtm_freq_train <- removeSparseTerms(data_dtm_train, 0.999)
data_dtm_freq_train

findFreqTerms(data_dtm_train, 5)

data_freq_words <- findFreqTerms(data_dtm_train, 5)
str(data_freq_words)

data_dtm_freq_train <- data_dtm_train[ , data_freq_words]
data_dtm_freq_test <- data_dtm_test[ , data_freq_words]

convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}
data_train <- apply(data_dtm_freq_train, MARGIN = 2, convert_counts)
data_test  <- apply(data_dtm_freq_test, MARGIN = 2, convert_counts)

library(e1071)
data_classifier <- naiveBayes(data_train, data_train_labels)
data_test_pred <- predict(data_classifier, data_test)

library(gmodels)
CrossTable(data_test_pred, data_test_labels,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
