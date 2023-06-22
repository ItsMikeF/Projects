#Supermarket branches segmentation analysis by R
#https://www.kaggle.com/code/teresawu726/supermarket-branches-segmentation-analysis-by-r/notebook

library(tidyverse) 
library(fastDummies)
library(reshape2)

#1. Exploratory Data Analysis

store <- read_csv("./Stores.csv")
options(repr.plot.width=10, repr.plot.height = 10, repr.plot.res = 150)

store %>% 
  distinct(Store_Area) %>%
  count()

regulation<-function(x){
  return(( x-min(x, na.rm=T) )/( max(x, na.rm=T) - min(x, na.rm=T)))
}

store2<-store%>%
  mutate(Store_Area = regulation(Store_Area),
         Items_Available = regulation(Items_Available),
         Daily_Customer_Count = regulation(Daily_Customer_Count),
         Store_Sales = regulation(Store_Sales))
store2<-store2[,-1]

cor(store2) %>% 
  melt() %>%
  ggplot(aes(Var1, Var2, fill=value))+
  geom_tile(color='white')+
  geom_text(aes(label = paste(round(value*100,1),'%')),
            color = 'white', size = 3)+
  theme_bw()+
  labs(x='',y='',
       title = 'Relationship between each variables')

#2. Clustering - Using Hclust

group<-store2[,-1] #Delete one of the variable

set.seed(1)
hcut<-hclust(dist(group), method = 'ward.D2')
plot(hcut)

store<-store%>%
  mutate(group = as_factor(cutree(hcut, 4)))

#3. Analysis
#Different groups and their Store Sales

store%>%
  ggplot(aes(Store_Sales, reorder(group, Store_Sales, method = 'median')))+
  geom_boxplot(fill='lightblue')+
  theme_bw()+
  labs(title='Boxplot: Different groups and their Store Sales',
       x = 'Store Sales',
       y = 'Group')

#4. Create New Variables: Conversion Rate

store<-store%>%
  mutate(Conversion_Rate = Store_Sales/Daily_Customer_Count)

store%>%
  ggplot(aes(log(Conversion_Rate)))+
  geom_density(aes(color = group, fill=group), alpha = 1/3 )+
  theme_bw()+
  labs(title='Density Plot: Different groups and the Conversion Rate',
       x = 'Conversion Rate(log)',
       y = 'Density')

store%>%
  ggplot(aes(Store_Area))+
  geom_density(aes(color = group, fill=group), alpha = 1/3 )+
  theme_bw()+
  labs(title='Density Plot: Different groups and the Store Area',
       x = 'Store Area',
       y = 'Density')