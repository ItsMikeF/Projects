# compare nfl epa play over seasons

# ================== EPA per play + Era Analysis (Parallel, robust) =================

suppressPackageStartupMessages({
  library(nflreadr)   # load_pbp(), load_schedules()
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(purrr)
  library(future)
  library(furrr)
  library(glue)
})

seasons <- 1999:2024

pbp <- load_pbp(seasons) 

# calc off epa
epa_off <- function(seasons){
  
  # off pass epa
  pbp_off_pass <- pbp %>% 
    filter(pass == 1 ) %>% 
    group_by(season) %>% 
    summarize(off_pass_epa = round(mean(epa, na.rm = T), digits = 3),
              pass_plays = n()) 
  
  # off rush epa
  pbp_off_rush <- pbp %>% 
    filter(rush == 1) %>% 
    group_by(season) %>% 
    summarize(off_rush_epa = round(mean(epa, na.rm = T), digits = 3),
              rush_plays = n()) 
  
  pbp_off <<- pbp_off_pass %>% 
    left_join(pbp_off_rush, by = c('season'))
}
epa_off(seasons)


# 2.0 plot  ---------------------------------------------------------------


# Reshape data to long format for plotting
pbp_off_long <- pbp_off %>%
  pivot_longer(cols = c(off_pass_epa, off_rush_epa),
               names_to = "play_type",
               values_to = "epa_per_play") %>%
  mutate(play_type = ifelse(play_type == "off_pass_epa", "Pass", "Rush"))

# value for plot title, used in glue
seasons_dash <- paste(min(seasons), max(seasons), sep = "-")

# Generate bar plot on the same y-axis with data labels, improved delineation, and trend lines
nfl_epa_plot <- ggplot(pbp_off_long, aes(x = as.factor(season), y = epa_per_play, fill = play_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) + # Adjusted width for spacing
  geom_text(aes(label = sprintf("%.3f", epa_per_play), vjust = ifelse(epa_per_play >= 0, -0.5, 1.5)),
            position = position_dodge(width = 0.9), size = 3) +
  #geom_smooth(aes(group = play_type), method = "loess", se = FALSE, color = "black", size = 0.2,
              #position = position_dodge(width = 0.9)) + # Add trend lines for each play type
  labs(title = glue("NFL EPA per Play: Pass vs Rush ({seasons_dash})"),
       x = "Season",
       y = "EPA per Play",
       fill = "Play Type", 
       caption = "Twitter: @ItsMikeF | Github: @ItsMikeF") +
  scale_fill_manual(values = c("Pass" = "blue", "Rush" = "red")) +
  scale_y_continuous(limits = c(-0.15, 0.15),
                     breaks = c(-0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15)) + # Adjust y-axis limits to include all data
  theme_minimal() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for readability
        panel.grid.major.x = element_line(color = "gray80", size = 0.5), 
        plot.title = element_text(size = 16)) # Add vertical grid lines
nfl_epa_plot


seasons_glue <- paste(min(seasons), max(seasons), sep = "_")

# Save plot with 1080p resolution (1920x1080)
ggsave( plot = nfl_epa_plot, 
        file = glue("./03_plots/era_analysis/nfl_epa_per_play_{seasons_glue}.png"), 
        width = 16, height = 9, units = "in", dpi = 300)

