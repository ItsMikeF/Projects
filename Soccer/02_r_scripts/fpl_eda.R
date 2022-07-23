#testing out newly discovered soccer packages

#load packages
suppressMessages({
  library(tidyverse) #metapackage
  library(worldfootballR) #extract and lcean world soccer data
  library(engsoccerdata) #English and European Soccer Results
  library(socceR) #Evaluating Sport Tournament Predictions
  library(ggsoccer) #Plot Soccer Event Data
  library(footBayes) #Fitting Bayesian and MLE Football Models
  library(itscalledsoccer) #American Soccer Analysis API Client
  library(FPLdata) #Read in Fantasy Premier League Data
  library(EUfootball) #Football Match Data of European Leagues
})

fpl_data <- FPLdata()

head(fpl_data)
summary(fpl_data)
str(fpl_data)

fpl_data %>%
  group_by(web_name) %>%
  summarise("mean_next_gw_points" = mean(next_gw_points, na.rm = TRUE)) %>%
  arrange(-mean_next_gw_points)
