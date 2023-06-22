library(dplyr)
library(forecast)

train <- read.csv("./DailyDelhiClimateTrain.csv")
test <- read.csv("./DailyDelhiClimateTest.csv")

train_temp <- train %>% 
  select(meantemp)

temp_ts <- ts(train_temp, frequency=365, start=c(2013,1))
plot(temp_ts)

sarima = function(df,frequency,start,drop=0,test_count=10){
  df = df[(drop+1):nrow(df),]
  df_ts <- ts(df,frequency=frequency ,start=start)
  A= auto.arima(df_ts)
  A= as.data.frame(A[7])
  print("step1")
  p= A[1,1]
  q= A[2,1]
  P= A[3,1]
  Q= A[4,1]
  d= A[6,1]
  D= A[7,1]
  arimafit=arima(df_ts,order=c(p,d,q),seasonal=c(P,D,Q))
  print("step2")
  arimafuture=forecast:::forecast.Arima(arimafit,h=test_count)
  plot(arimafuture)
  pred = arimafuture[[4]]
  
  return(pred)
}

pred = sarima(train_temp,365,c(2013,1),test_count=nrow(test))
pred