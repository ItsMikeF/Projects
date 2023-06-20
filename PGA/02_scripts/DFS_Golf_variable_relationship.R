#load packages
suppressMessages({
  library(tidyverse) #metapackage
  library(xgboost)
  library(caret)

  library(tidytext) #text mining
  library(wordcloud2) #word viz
  library(readxl) #read excel files
  library(fastDummies) #dummy binary columns from categorical variables
  library(reshape2) #restructure and aggregate data via melt() and dcast()
  library(factoextra) #packages to cluster
  library(jiebaR) #packages to cut the words
})

#split into training (80%) and testing set (20%)
data <- read.csv("./Results/golfers_results_no_odds.csv") %>% 
  drop_na()

regulation <- function(x){
  return( (x - min(x,na.rm = T))/( max(x, na.rm = T) - min(x, na.rm = T)) )
}

#data <- data %>% select(ceil, Salary, residuals, AvgPointsPerGame, win, odds_close, odds_delta_per, total_pts)
data <- data[,c(3:36)]

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
