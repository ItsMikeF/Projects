# Install and load the nflverse package (if you haven't already)
library(nflverse)

# Load the passing and rushing statistics
pbp <- load_pbp(seasons = 2022)

# Filter the dataset for quarterback plays only
qb_data <- pbp[pbp$passer_player_name != "" | pbp$rusher_player_name != "", ]

# Group by player and summarize the relevant statistics
qb_stats <- qb_data %>%
  group_by(passer_player_name) %>%
  summarize(
    pass_yards = sum(passing_yards),
    pass_tds = sum(passing_touchdown),
    interceptions = sum(interception),
    rush_yards = sum(rushing_yards),
    rush_tds = sum(rushing_touchdown)
  )

# Calculate fantasy points based on a standard scoring system
# (Note: You can adjust these values based on your league's scoring system)
qb_stats$fantasy_points <- with(qb_stats,
                                0.04*pass_yards + 4*pass_tds - 2*interceptions +
                                  0.1*rush_yards + 6*rush_tds)

# Split data into training and testing sets (for this example, we'll use all data for training)
train_data <- qb_stats

# Build the linear regression model
model <- lm(fantasy_points ~ pass_yards + pass_tds + interceptions + rush_yards + rush_tds, data=train_data)

# Print the model summary
summary(model)

# To make predictions:
# predicted_points <- predict(model, newdata=your_data_frame)
