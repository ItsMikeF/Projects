library(tidyverse)
library(ggrepel)
library(glue)
library(stringr)
library(pracma)

###Game log###

game_log <- read.csv("ridder_game_log.csv", header = F)

game_log <- subset(game_log, V1 !="@")

log_team <- tibble(game_log[seq(1, nrow(game_log), 34), ])
log_POS <- tibble(game_log[seq(2, nrow(game_log), 34), ])
log_S <- tibble(game_log[seq(3, nrow(game_log), 34), ])
log_DB <- tibble(as.numeric(game_log[seq(4, nrow(game_log), 34), ]))
log_ATT <- tibble(as.numeric(game_log[seq(5, nrow(game_log), 34), ]))
log_COM <- tibble(as.numeric(game_log[seq(6, nrow(game_log), 34), ]))
log_COM_per <- tibble(as.numeric(game_log[seq(7, nrow(game_log), 34), ]))
log_YDS <- tibble(as.numeric(game_log[seq(8, nrow(game_log), 34), ]))
log_YPA <- tibble(as.numeric(game_log[seq(9, nrow(game_log), 34), ]))
log_TD <- tibble(as.numeric(game_log[seq(10, nrow(game_log), 34), ]))
log_INT <- tibble(as.numeric(game_log[seq(11, nrow(game_log), 34), ]))
log_OFF <- tibble(as.numeric(game_log[seq(12, nrow(game_log), 34), ]))
log_PASS <- tibble(as.numeric(game_log[seq(13, nrow(game_log), 34), ]))
log_RUN <- tibble(as.numeric(game_log[seq(14, nrow(game_log), 34), ]))
log_FUM <- tibble(as.numeric(game_log[seq(15, nrow(game_log), 34), ]))
log_BTT <- tibble(as.numeric(game_log[seq(16, nrow(game_log), 34), ]))
log_BTT_per <- tibble(as.numeric(game_log[seq(17, nrow(game_log), 34), ]))
log_TWP <- tibble(as.numeric(game_log[seq(18, nrow(game_log), 34), ]))
log_TWP_per <- tibble(as.numeric(game_log[seq(19, nrow(game_log), 34), ]))
log_ADOT <- tibble(as.numeric(game_log[seq(20, nrow(game_log), 34), ]))
log_ADJ_per <- tibble(as.numeric(game_log[seq(21, nrow(game_log), 34), ]))
log_DRP <- tibble(as.numeric(game_log[seq(22, nrow(game_log), 34), ]))
log_DROP_per <- tibble(as.numeric(game_log[seq(23, nrow(game_log), 34), ]))
log_BAT <- tibble(as.numeric(game_log[seq(24, nrow(game_log), 34), ]))
log_HAT <- tibble(as.numeric(game_log[seq(25, nrow(game_log), 34), ]))
log_TA <- tibble(as.numeric(game_log[seq(26, nrow(game_log), 34), ]))
log_DPR <- tibble(as.numeric(game_log[seq(27, nrow(game_log), 34), ]))
log_SK <- tibble(as.numeric(game_log[seq(28, nrow(game_log), 34), ]))
log_P2S <- tibble(as.numeric(game_log[seq(29, nrow(game_log), 34), ]))
log_TTT <- tibble(as.numeric(game_log[seq(30, nrow(game_log), 34), ]))
log_SCR <- tibble(as.numeric(game_log[seq(31, nrow(game_log), 34), ]))
log_1ST <- tibble(as.numeric(game_log[seq(32, nrow(game_log), 34), ]))
log_NFL <- tibble(as.numeric(game_log[seq(33, nrow(game_log), 34), ]))
log_PEN <- tibble(as.numeric(game_log[seq(34, nrow(game_log), 34), ]))

game_log <- cbind(log_team, log_POS, log_S, log_DB, log_ATT, log_COM, log_COM_per, log_YDS, log_YPA, log_TD, log_INT, log_OFF, log_PASS, log_RUN, log_FUM, log_BTT, log_BTT_per, 
           log_TWP, log_TWP_per, log_ADOT, log_ADJ_per, log_DRP, log_DROP_per, log_BAT, log_HAT, log_TA, log_DPR, log_SK, log_P2S, log_TTT, log_SCR, log_1ST, log_NFL, log_PEN)

names(game_log) <- c("TEAM", "POS", "S", "DB", "ATT", "COM", "COM%", "YDS", "YPA", "TD", "INT", "OFF", "PASS", "RUN", "FUM", "BTT", "BTT%", "TWP", "TWP%", "ADOT", "ADJ%", "DRP", "DROP%", "BAT", "HAT", "TA", "DPR", "SK", "P2S", "TTT", "SCR", "1ST", "NFL", "PEN")

mean(game_log$YDS)

###Defense###

cfb_pff_def <- read.csv("defense_summary.csv")

cfb_schools <- read.csv("CFB_Schools.csv")

cfb_def <- cfb_pff_def %>%
  group_by(team_name) %>%
  summarise(def = round(weighted.mean(grades_defense, snap_counts_defense), digits = 1),
            rdef = round(weighted.mean(grades_run_defense, snap_counts_run_defense), digits = 1),
            tack = round(weighted.mean(grades_tackle, snap_counts_defense, na.rm = TRUE), digits = 1),
            prsh = round(weighted.mean(grades_pass_rush_defense, snap_counts_pass_rush), digits = 1),
            cov = round(weighted.mean(grades_coverage_defense, snap_counts_coverage), digits =1))

cfb_def$def_rank <- round(rank(-cfb_def$def), digits = 0)
cfb_def$rdef_rank <- round(rank(-cfb_def$rdef), digits = 0)
cfb_def$tack_rank <- round(rank(-cfb_def$tack), digits = 0)
cfb_def$prsh_rank <- round(rank(-cfb_def$prsh), digits = 0)
cfb_def$cov_rank <- round(rank(-cfb_def$cov), digits = 0)

cfb_def <- cfb_def %>%
  left_join(cfb_schools, by = c('team_name' = 'School'))

game_log <- game_log %>% 
  left_join(cfb_def, by = c('TEAM' = 'team_name'))

###Game log chart###

summary(lm(def_rank ~ poly(PASS, 3, raw = T), data = game_log))

game_log %>%
  ggplot(aes(x = cov_rank , y = PASS)) +
  geom_hline(yintercept = mean(game_log$PASS, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(game_log$cov_rank, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  geom_point(cex = 10 *game_log$DB / max(game_log$DB), alpha = .6) +
  geom_smooth(method = "lm") +
  geom_text_repel(aes(label=TEAM)) +
  annotate("text", x = mean(game_log$cov_rank, na.rm = T), y = min(game_log$PASS), label = paste("r2 =",round(summary(lm(cov_rank ~ PASS, data = game_log))$r.squared, digits =3))) +
  labs(x = "cov_rank",
       y = "PASS",
       title = paste("Game Log"),
       caption = "Twitter: Its_MikeF | Data: PFF") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
