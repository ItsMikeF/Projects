#testing out newly discovered soccer packages

#load packages
suppressMessages({
  library(tidyverse) #metapackage
  library(gt) #Easily Create Presentation-Ready Display Tables
  library(ggsci) #Scientific Journal and Sci-Fi Themed Color Palettes for 'ggplot2'
  library(reshape2) #flexibly Reshape Data
  library(ggrepel) #Automatically Position Non-Overlapping Text Labels with 'ggplot2'
  library(webshot)
  library(DescTools) # Tools for Descriptive Statistics
  
  library(worldfootballR) #extract and clean world soccer data
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

fpl_data %>%
  group_by(web_name) %>%
  summarise("mean_next_gw_points" = mean(next_gw_points, na.rm = TRUE)) %>%
  arrange(-mean_next_gw_points)

{test %>%
    ggplot(aes(x = Draft.Rank, y = npxG_plus_xA_per_min)) +
    geom_smooth(method=loess, se=F) +
    geom_point(alpha = 0.7, cex = 3) +
    geom_text_repel(aes(label=Player)) +
    labs(x = "Draft.Rank",
         y = "npxG_plus_xA_per_90_adj_min",
         title = "2022/2023 FPL Value Finder",
         caption = "Twitter: Its_MikeF | Data: FBRef") +
    theme_bw() +
    theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
  }
