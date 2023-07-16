#model uv index 
#LOESS (locally estimated scatterplot smoothing) and LOWESS (locally weighted scatterplot smoothing)

#load packages
suppressMessages({
  library(tidyverse) #metapackage
})

#load uv data
uv <- read_csv("./data/uv_hourly.csv")

#linear model of the uv data
lm <- summary(lm(uv$uv ~ uv$hour + I(uv$hour^2) + I(uv$hour^3)+ I(uv$hour^4)+ I(uv$hour^5)+ I(uv$hour^6)))

#import coefficients into a formula to calculate
poly_calc <- function(x){
  df <- as.tibble(lm$coefficients)
  return(round(
    (df$Estimate[7]*x^6) +
    (df$Estimate[6]*x^5) +
    (df$Estimate[5]*x^4) +
    (df$Estimate[4]*x^3) +
    (df$Estimate[3]*x^2) +
    (df$Estimate[2]*x^1) +
    (df$Estimate[1]*x^0) 
  , digits = 2))
}

uv <- uv %>% 
  mutate(uv_poly = poly_calc(hour))

ggplot(uv, aes(x = hour, y = uv)) +
  geom_point() +
  labs(
    x = "Time of Day", 
    y = "UV Index", 
    title = "UV Index vs Time of Day", 
    caption = "Data from WillyWeather"
  ) +
  theme_minimal() +
  geom_smooth(method = "loess", se = F, color ="red")