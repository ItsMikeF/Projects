library(tidyverse, warn.conflicts = F)
library(ggrepel)
library(lubridate, warn.conflicts = F)
library(utils)
library(filesstrings, warn.conflicts = F)
library(xtable)
library(tictoc)
library(lpSolve)
library(stats)
library(XML)
library(binr)
library(httr)

### Modift Results
results <- read.csv(list.files(pattern = "draftkings_pga_"))
results <- results %>%
  select(player_name, ownership, total_pts) %>%
  separate(player_name, into = c("last", "first"), sep = ",")

results$Player <- trimws(paste(results$first, results$last))
results <- results %>% select(Player, ownership, total_pts)

#Add Results to Golfers
golfers <- golfers %>% left_join(results, by = c("Name" = "Player"))

golfers3 <- golfers
#golfers3 <- golfers %>% filter(Salary >= salary_filter)
golfers3$date <- date
golfers3$tournament <- tournament
#write.csv(golfers3, file = "golfers_results.csv")

model <- lm(FPTS ~ adj_own + odds_delta + residuals + odds_delta_per, data = golfers3)
hist(residuals(model), col = "steelblue")
summary(model)

golfers3 %>%
  ggplot(aes(x = adj_own , y = FPTS)) +
  geom_hline(yintercept = mean(golfers3$FPTS, na.rm = TRUE), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(golfers3$adj_own, na.rm = TRUE), color = "red", linetype = "dashed", alpha=0.5) +
  
  geom_point(aes(color = FPTS), alpha = 0.7, cex = 6) +
  scale_color_gradient(low = "red", high = "green", guide = "colourbar") +
  
  geom_text_repel(aes(label=Name)) +
  labs(x = "adj_own",
       y = "FPTS",
       title = paste("Golf", tournament),
       caption = "Twitter: Its_MikeF | Data: DK") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
