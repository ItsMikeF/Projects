library(tidyverse)
library(ggrepel)
library(glue)
library(lubridate)
library(gganimate)

setwd("C://Users//Mike Francis//Documents//")

qb_results <- read.csv("nfl_qb.csv")

qb_results$week_name_salary_own <- paste(qb_results$week, qb_results$Name, qb_results$Salary, qb_results$Own)

summary(lm(FPTS ~ sum_sd, data = qb_results))

qb_coefs <- coef(lm(FPTS ~ sum_sd, data = qb_results))

qb_results %>%
  ggplot(aes(x = sum_sd , y = FPTS)) +
  geom_hline(yintercept = mean(qb_results$FPTS, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(qb_results$sum_sd, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  geom_point(color = rainbow(dim(qb_results)[1]), cex = (25*qb_results$FPTS_vs_proj/max(qb_results$FPTS_vs_proj, na.rm = T)), alpha = .6) +
  geom_smooth(method = "lm", na.rm = T) +
  geom_text_repel(aes(label=week_name_salary_own)) +
  annotate("text", x = 1.5, y = 1, label = paste("r2 =",round(summary(lm(FPTS ~ sum_sd, data = qb_results))$r.squared, digits =3))) +
  annotate("text", x = 1.5, y = 5, label = paste("FPTS =", "(", round(qb_coefs[2], digits =2), "* sum_sd ) +", round(qb_coefs[1], digits = 2))) +
  labs(x = "sum_sd",
       y = "FPTS",
       title = paste("QBs, NFL Weeks 15 - 17"),
       caption = paste("Twitter: Its_MikeF | Data: DraftKings | Date:", today())) +
  theme_gray() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), sec.axis = sec_axis(trans = ~.*1 , name = "FPTS", breaks = scales::pretty_breaks(n = 10))) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

qb_fpts_lm <- function(sum_sd) {
  fpts <- (qb_coefs[2] * sum_sd) + qb_coefs[1]
  return(fpts)
}

my.animation <- 
  ggplot(qb_results, aes(x = sum_sd , y = FPTS)) + 
  geom_hline(yintercept = mean(qb_results$FPTS, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(qb_results$sum_sd, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  geom_point(color = rainbow(dim(qb_results)[1]), cex = (25*qb_results$FPTS_vs_proj/max(qb_results$FPTS_vs_proj, na.rm = T)), alpha = .6) +
  geom_text_repel(aes(label=week_name_salary_own)) +
  annotate("text", x = 1.5, y = 1, label = paste("r2 =",round(summary(lm(FPTS ~ sum_sd, data = qb_results))$r.squared, digits =3))) +
  annotate("text", x = 1.5, y = 5, label = paste("FPTS =", "(", round(qb_coefs[2], digits =2), "* sum_sd ) +", round(qb_coefs[1], digits = 2))) +
  labs(x = "sum_sd",
       y = "FPTS",
       caption = paste("Twitter: Its_MikeF | Data: DraftKings | Date:", today())) +
  theme_gray() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), sec.axis = sec_axis(trans = ~.*1 , name = "FPTS", breaks = scales::pretty_breaks(n = 10))) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  # Here comes the gganimate code
  ggtitle(paste('Now showing NFL QBs Week {closest_state}'),
          subtitle = 'Frame {frame} of {nframes}') +
  transition_states(
    week,
    transition_length = 2,
    state_length = 7) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')

animate(my.animation, duration = 25, height = 1000, width = 1000)
anim_save("test.gif")