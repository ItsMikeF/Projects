library(tidyverse) #metapackage
library(tm) #text mining

data<-read_csv('./data/humor_detection_dataset.csv')
str(data)
data$humor <- factor(data$humor)
str(data$humor)
table(data$humor)
data_corpus <- VCorpus(VectorSource(data$text))