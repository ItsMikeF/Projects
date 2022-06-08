#Import packages
library(tidyverse, warn.conflicts = F) #Metapackage
library(xgboost, warn.conflicts = F)
library(readr, warn.conflicts = F)
library(stringr, warn.conflicts = F)
library(caret, warn.conflicts = F)
library(car, warn.conflicts = F)

#make this example reproducible
set.seed(0)

#Define training and test data
train <- read.csv("./Results/golfers_results.csv") %>% 
  drop_na(total_pts) %>% 
  drop_na(observed_finish)

golfers <- read.csv("./Results/golfers.csv")
test <- golfers %>% 
  drop_na()

#train plot
train_plot <- train %>% 
    filter(odds_delta_per < 1) %>% 
    filter(Salary > 7000)

{train_plot %>%
    ggplot(aes(x = odds_delta_per , y = observed_finish)) +
    geom_hline(yintercept = mean(train_plot$observed_finish), color = "red", linetype = "dashed", alpha=0.5) +
    geom_vline(xintercept =  mean(train_plot$odds_delta_per), color = "red", linetype = "dashed", alpha=0.5) +
    geom_point(aes(color = Salary), alpha = 0.7, cex = 3) +
    scale_color_gradient(low = "red", high = "green", guide = "colourbar") +
    labs(x = "odds_delta_per",
         y = "observed_finish",
         title = paste("Training Data", "Golfers"),
         caption = "Twitter: Its_MikeF | Data: DraftKings") +
    theme_bw() +
    theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
    }

#define predictor and response variables in training set
train_x <- data.matrix(train %>% select(ceil,Salary, residuals, AvgPointsPerGame, win, odds_close, odds_delta_per))
train_y <- train[,29]

#define predictor and response variables in testing set
test_x <- data.matrix(test %>% select(ceil,Salary, residuals, AvgPointsPerGame, win, odds_close, odds_delta_per))
test_y <- test[,25]

#define final training and testing sets
xgb_train <- xgb.DMatrix(data = train_x, label = train_y)
xgb_test <- xgb.DMatrix(data = test_x, label = test_y)

#define watchlist
watchlist <- list(train=xgb_train, test=xgb_test)

#define final model
final <- xgboost(data = xgb_train, max.depth = 2, nrounds = 11, print_every_n = 1)

#use model to make predictions on test data
pred_y <- predict(final, xgb_test)

#Create dataframe to copy
xgb_fpts = cbind(pred_y, test_y)
xgb_fpts[,1]

golfers$total_pts <- round(xgb_fpts[,1], digits = 2)

write.csv(golfers, file = "./Results/golfers.csv")
write.csv(golfers, file = paste0(list.dirs()[20],"/golfers.csv"))
