library(tidyverse) #metapackage
library(janeaustenr) #Jane Austen's complete novels
library(tidytext) #text mining
library(wordcloud2) #word viz
library(readxl) #read excel files
library(fastDummies) #dummy binary columns from categorical variables
library(reshape2) #restructure and aggregate data via melt() and dcast()
library(factoextra) #packages to cluster
library(jiebaR) #packages to cut the words

setwd(paste0("C://Users//",unlist(strsplit(getwd(), "/"))[3],"//Documents//GitHub//Projects//NLP//Spotify//"))

#load spotify data
spotify <- read.csv("spotify_2010_2019_top_100.csv")
spotify <- read_excel("spotify_2010_2019_top_100.xlsx")

# 1. Relationship between each genres -------------------------------------

#Normalize each variables
genre<-spotify[,c(3,6:17)]

regulation <- function(x){
  return( (x - min(x,na.rm = T)) / ( max(x, na.rm = T) - min(x, na.rm = T)) )
}

for(i in 2:12){
  genre[,i] = regulation(genre[,i])
}

genre2<-genre[, 13] %>% 
  dummy_cols(remove_selected_columns = T)

genre<-genre[, -13] %>%
  cbind(genre2)

#Relationship between music features
genre[,c(2:16)] %>%
  cor() %>%
  melt() %>%
  ggplot(aes(Var1, Var2, fill=value)) +
  geom_tile(color='white') +
  scale_fill_distiller(palette = 'GnBu', direction = 1) +
  geom_text(aes(label=paste(round(value,2)*100,'%')), size=2.5, color='black') +
  labs(x='',y='',fill='correlations', title='Relationship between music features') +
  theme(axis.text.x = element_text(angle = 90, vjust = .5))


# 2. Top Artist in each year ----------------------------------------------

top_artist <- spotify %>%
  group_by(`top year`) %>%
  count(artist) %>%
  mutate(prop=n / sum(n))

top_artist[order(top_artist$n, decreasing=TRUE)[1:30], ] %>%
  ggplot(aes(as_factor(`top year`), prop, fill=artist)) +
  geom_bar(stat='identity',  color = 'white', show.legend = F) +
  geom_text(aes(label=paste(artist)), size=2.5, color='black',
            position = position_stack(vjust = .5))+
  theme_bw() +
  labs(title='Hot artists in each year', y='Percent', x='Year')

# 3. Top genre in each year -----------------------------------------------

top_genre<-spotify %>%
  group_by(`top year`) %>%
  count(`top genre`) %>%
  mutate(prop=n/sum(n))

top_genre[order(top_genre$n, decreasing=TRUE)[1:40], ] %>%
  ggplot(aes(as_factor(`top year`), prop, fill=`top genre` )) +
  geom_bar(stat='identity',  color = 'white', show.legend = F) +
  geom_text(aes(label=paste(`top genre` )), size=2.5, color='black',
            position = position_stack(vjust = .5))+
  theme_bw() +
  labs(title='Hot genre in each year', y='Percent', x='Year')

spotify%>%
  group_by(`top year`) %>%
  count(`top genre`) %>%
  filter(`top genre` == 'dance pop')%>%
  ggplot(aes(as_factor(`top year`), n)) +
  geom_point(color='lightblue') +
  geom_line(group=1, color='lightblue') +
  theme_bw() +
  labs(title='Number of Dance pop in each year', y='Number of Dance pop', x='Year')

# 4. Predict the top song in future year ----------------------------------

#1. Segment different type of songs

set.seed(10)
fviz_nbclust(genre[,c(2:16)], 
             FUNcluster = kmeans,
             method = "wss",
             k.max = 12)+
  labs(title="Elbow Method for K-Means") +
  geom_vline(xintercept = 4,
             linetype = 2)

fviz_nbclust(genre[,c(2:16)], 
             FUNcluster = hcut,
             method = "wss",
             k.max = 12)+
  labs(title="Elbow Method for Hclust") +
  geom_vline(xintercept = 4,
             linetype = 2)

#K-means and Hclust result: 4 clusters is the optimal number
cluster <- kmeans(genre[,c(2:16)], 4)

spotify <- spotify %>%
  mutate(group=cluster$cluster)

spotify$group <- as_factor(spotify$group)

#2. Different group vs Artist Type
spotify %>%
  ggplot(aes(as_factor(`artist type`))) +
  geom_bar(aes(fill=group)) +
  labs(title='Different group vs Artist Type', fill='group', x='Artist Type') +
  theme_bw()

#3. Correlation of variables in Different group
group<-spotify[,18 ]%>% dummy_cols(remove_selected_columns = T)

genre3<-genre %>% cbind(group)

genre3[,c(2:20)]%>%
  cor()%>%
  melt()%>%
  filter(str_detect(Var1, 'group') & !str_detect(Var2, 'group'))%>%
  ggplot(aes(Var1, Var2, fill=value))+
  geom_tile(color='white')+
  scale_fill_distiller(palette = 'GnBu', direction = 1)+
  geom_text(aes(label=paste(round(value,2)*100,'%')), size=2.5, color='black')+
  labs(x='',y='',fill='correlations', title='Relationship between Group')+
  theme_bw()

#Group 4: Number of hot songs in spotify
spotify%>%
  filter(group==4)%>%
  ggplot(aes(as_factor(`top year`)))+
  geom_bar(fill='lightblue')+
  labs(x='Year',y='Count', title='Group 3: Number of hot songs in spotify')+
  theme_bw()

#Group 4: Song title
cut<-worker()

group4<-spotify %>%
  filter(group== 4) %>%
  select(title)

tibble(title=segment(group4$title, cut)) %>%
  count(title) %>%
  wordcloud2()

#Group 4: Who is popular cooperation prater?
name<-c('feat.+', 'with.+')

featSinger<-str_extract(group4$title, paste(name, sep='|'))
featSinger<-tibble(singer = featSinger)%>%
  filter(!is.na(singer))

featSinger<-str_remove(featSinger$singer, 'feat. ')
featSinger<-str_remove(featSinger, 'with ')
featSinger<-str_remove(featSinger, '\\)')

tibble(singer = featSinger)%>%
  count(singer)%>%
  arrange(desc(n))
