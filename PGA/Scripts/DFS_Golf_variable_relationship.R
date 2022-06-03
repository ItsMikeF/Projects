library(tidyverse)  #metapackage
library(xgboost, warn.conflicts = F)
library(readr, warn.conflicts = F)
library(stringr, warn.conflicts = F)
library(caret, warn.conflicts = F)
library(car, warn.conflicts = F)

library(janeaustenr) #Jane Austen's complete novels
library(tidytext) #text mining
library(wordcloud2) #word viz
library(readxl) #read excel files
library(fastDummies) #dummy binary columns from categorical variables
library(reshape2) #restructure and aggregate data via melt() and dcast()
library(factoextra) #packages to cluster
library(jiebaR) #packages to cut the words

setwd("C:/Users/mikef/Documents/GitHub/Projects/Golf/results")

#split into training (80%) and testing set (20%)
data <- read.csv("golfers_results.csv") %>% 
  drop_na()

regulation<-function(x){
  return( (x - min(x,na.rm = T))/( max(x, na.rm = T) - min(x, na.rm = T)) )
}

data <- data[,c(4:29)]
data <- data[,c(4,8,10,11,13:15,22,23,25,28,29)]

for(i in 1:length(data)){
  data[,i] = regulation(data[,i])
}

#Relationship between variables
data %>%
  cor() %>%
  melt() %>%
  ggplot(aes(Var1, Var2, fill=value)) +
  geom_tile(color='white') +
  scale_fill_distiller(palette = 'GnBu', direction = 1) +
  geom_text(aes(label=paste(round(value,2)*100,'%')), size=2.5, color='black') +
  labs(x='',y='',fill='correlations', title='Relationship between golfer variables') +
  theme(axis.text.x = element_text(angle = 90, vjust = .5))
