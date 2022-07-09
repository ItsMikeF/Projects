#load packages
suppressMessages({
  library(tidyverse) #metapackage
  library(ggwordcloud) #word cloud geometry 
})

#exploratory data analysis
data <- read_csv("./data/humor_detection_dataset.csv")
summary(data['humor'])
ggplot(data) + 
  geom_bar(aes(humor,fill=humor)) + 
  guides(fill=F)

#pie chart
ques_data<- data %>% 
  filter(str_detect(text,regex('\\?')))
notques_data <- data %>%
  filter(!str_detect(text,regex('\\?')))

m <- ques_data %>% 
  group_by(humor) %>% 
  summarize(count=n()) %>% 
  mutate(category='question')

k <- notques_data %>% 
  group_by(humor) %>% 
  summarize(count=n()) %>%
  mutate(category='not question')

sm_category<-rbind(m,k) %>%
  mutate(percent=round(count/sum(count),2))
sm_category[1] <- ifelse(sm_category$humor=='TRUE','humor','not humor')
sm_category2 <- unite(sm_category,class,humor,category,sep = ',')

pie(x=sm_category2$percent,
    labels = paste(sm_category2$class, sep = " ",
                   sm_category2$percent, "%"))
sm_category%>%group_by(category)%>%summarize(humor=humor,p=count/sum(count))

#box plot
data<-data%>%mutate(chrlength=' ')
data[[1]]<-gsub("[[:punct:]]",'',data[[1]])
data[[3]]<-as.integer(nchar(data[[1]]))
data_humor<-data%>%filter(humor=='TRUE')
data_nothumor<-data%>%filter(humor=='FALSE')

word_h<-strsplit(data_humor[[1]],' ')
word_n<-strsplit(data_nothumor[[1]],' ')
word_h.freq<-table(unlist(word_h))
word_n.freq<-table(unlist(word_n))

ggplot(data)+geom_boxplot(aes(humor,chrlength))

#word bar chart
humor_freq<-as.data.frame(cbind(names(word_h.freq),as.integer(word_h.freq)))
nothumor_freq<-as.data.frame(cbind(names(word_n.freq),as.integer(word_n.freq)))
humor_freq[[2]]<-as.integer(humor_freq[[2]])
nothumor_freq[[2]]<-as.integer(nothumor_freq[[2]])
humor_freq%>%filter(V1!=' ',V2>5000)%>%ggplot()+geom_bar(aes(reorder(V1,V2,median),V2,fill=V1),stat = 'identity')+
  labs(title='Top humor words',x='words',y='count')+guides(fill=F)
a<-humor_freq%>%filter(V1!=' ',V2>1000)
nothumor_freq%>%filter(V1!=' ',V2>5000)%>%ggplot()+geom_bar(aes(reorder(V1,V2,median),V2,fill=V1),stat = 'identity')+
  labs(title='Top not humor words',x='words',y='count')+guides(fill=F)
b<-nothumor_freq%>%filter(V1!=' ',V2>1000)

#word cloud
ggplot(a, aes(label = V1, size = V2,color =V1)) +
  geom_text_wordcloud_area(rm_outside = TRUE) +
  scale_size_area(max_size = 100) +
  theme_minimal()+labs(title='Humor words')
ggplot(b, aes(label = V1, size = V2,color =V1)) +
  geom_text_wordcloud_area(rm_outside = TRUE) +
  scale_size_area(max_size = 100) +
  theme_minimal()+labs(title='Not humor words')