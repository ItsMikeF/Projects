library(tidyverse)
library(ggrepel)
library(glue)
library(lubridate)

setwd("C://Users//Mike Francis//Documents//")

nfl_salaries <- read.csv("DKSalaries.csv")
nfl_standings <- read.csv(list.files(path = getwd(), pattern = "contest-standings"))
nfl_pff_dk_own <- read.csv("dk-ownership.csv")
nfl_team_table <- read.csv("NFL_Team_Table.csv")

###Adjustments###

nfl_standings <- nfl_standings %>%
  rename(Own = X.Drafted)

###Results###

nfl_results <- nfl_standings %>%
  select(Player, 
         Roster.Position, 
         Own,
         FPTS) %>% 
  drop_na()

nfl_results$Own <- round(as.numeric(sub("%","",nfl_results$Own)), digits = 1)
nfl_results$FPTS <- round(as.numeric(nfl_results$FPTS), digits = 1)
nfl_results$ppo <- round(nfl_results$FPTS / nfl_results$Own, digits = 1)

nfl_pff_pblk <- replace(nfl_pff_pblk, nfl_pff_pblk == 'ARZ', 'ARI')

#nfl_results <- replace(nfl_results, nfl_results == list(nfl_team_table$Team), list(nfl_team_table$team_name))

###Lineups###

nfl_lineups <- nfl_standings %>%
  select(EntryId,
         EntryName,
         Points, 
         Lineup) %>% 
  filter(Points > 200)

nfl_lineups <- separate(nfl_lineups, Lineup, into = c(letters), sep = "FLEX")
nfl_lineups$c <- paste("RB",nfl_lineups$b, sep = "")
nfl_lineups <- separate(nfl_lineups, c, into = c(letters[-(1:2)]), sep = " ")
nfl_lineups$f <- replace(nfl_lineups$f, nfl_lineups$f == "FLEX","")
nfl_lineups$f <- replace(nfl_lineups$f, nfl_lineups$f == "QB","")
nfl_lineups$f <- replace(nfl_lineups$f, nfl_lineups$f == "RB","")
nfl_lineups$f <- replace(nfl_lineups$f, nfl_lineups$f == "WR","")
nfl_lineups$f <- replace(nfl_lineups$f, nfl_lineups$f == "TE","")
nfl_lineups$f <- replace(nfl_lineups$f, nfl_lineups$f == "DST","")
nfl_lineups$c <- paste(nfl_lineups$d, nfl_lineups$e)
nfl_lineups <- nfl_lineups[,1:6]
nfl_lineups <- nfl_lineups %>% 
  left_join(nfl_salaries, by = c("c" = "Name"))
nfl_lineups$b <- paste(str_trim(nfl_lineups$Position, side = "both"), str_trim(nfl_lineups$b, side = "both"))
nfl_lineups <- nfl_lineups[,1:5]
nfl_lineups$a <- paste(nfl_lineups$a, nfl_lineups$b)
nfl_lineups <- nfl_lineups[,1:4]

nfl_lineups <- separate(nfl_lineups, a, into = c(letters), sep = "QB")
nfl_lineups <- separate(nfl_lineups, b, into = c(letters), sep = "RB")
nfl_lineups <- separate(nfl_lineups, c, into = c(letters[-(1:4)]), sep = "WR")
nfl_lineups <- separate(nfl_lineups, h, into = c(letters[-(1:8)]), sep = "TE")
#nfl_lineups <- separate(nfl_lineups, g, into = c(letters[-(1:10)]), sep = "FLEX")
nfl_lineups <- separate(nfl_lineups, k, into = c(letters[-(1:10)]), sep = "DST")

#view(nfl_lineups)

nfl_lineups$Points <- round(nfl_lineups$Points, digits = 1) 

###Winning Lineup###

winning_lineup <- tibble(c((nfl_lineups$a[1]),
                           (nfl_lineups$b[1]),
                           (nfl_lineups$e[1]),
                           (nfl_lineups$f[1]),
                           (nfl_lineups$g[1]),
                           (nfl_lineups$i[1]), 
                           (nfl_lineups$j[1]),
                           (nfl_lineups$k[1]),
                           (nfl_lineups$l[1])))

winning_lineup <- tibble(c(str_trim((nfl_lineups$a[1]),side = "both"),
                           str_trim((nfl_lineups$b[1]),side = "both"),
                           str_trim((nfl_lineups$e[1]),side = "both"),
                           str_trim((nfl_lineups$f[1]),side = "both"),
                           str_trim((nfl_lineups$g[1]),side = "both"), 
                           str_trim((nfl_lineups$i[1]),side = "both"),
                           str_trim((nfl_lineups$j[1]),side = "both"),
                           str_trim((nfl_lineups$k[1]),side = "both"),
                           paste(str_trim((nfl_lineups$l[1]),side = "both"),"")))

names(winning_lineup)[1] <- "Player"

winning_lineup <- winning_lineup %>% 
  left_join(nfl_salaries, by = c('Player' = 'Name'))

winning_lineup <- winning_lineup %>% 
  left_join(nfl_results, by = c("Player" = "Player"))

#view(winning_lineup)

winning_lineup <- winning_lineup %>% 
  left_join(nfl_pff_dk_own, by = c("Player" = "player"))

winning_lineup <- winning_lineup %>% 
  left_join(nfl_pff_projections, by = c("Player" = "playerName"))

winning_lineup$Own_delta <- winning_lineup$Own - winning_lineup$ownership
winning_lineup$FPTS_delta <- winning_lineup$FPTS - winning_lineup$fantasyPoints

replace_na(winning_lineup, "")

winning_lineup %>% 
  select(Player, 
         Position, 
         Salary, 
         Own,
         Own_delta,
         FPTS,
         FPTS_delta,
         opponent) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~""))) %>% 
  view(title = "Winning lu")

###QB Results Check###

nfl_qb <- nfl_qb %>%
  left_join(nfl_results, by = c('Name' = 'Player'))

nfl_qb$FPTS_dollar <- round(nfl_qb$FPTS / nfl_qb$Salary, digits = 5)

nfl_qb$FPTS_vs_proj <- round(nfl_qb$FPTS / nfl_qb$fantasyPoints, digits = 1)

nfl_qb$name_salary_own <- paste(nfl_qb$name_salary, nfl_qb$Own)

nfl_qb %>%
  filter(Position == "QB") %>% 
  select(Name,
         week,
         TeamAbbrev,
         Salary,
         Own,
         FPTS,
         FPTS_vs_proj,
         sum_sd,
         opponent,
         def_pass_epa_rank,
         def_rank, 
         cov_rank,
         grades_pass,
         grades_pass_multiplier,
         FPTS_dollar,
         ppo, 
         name_salary_own) %>%
  arrange(-FPTS) %>%
  view(title = "QBs Results") %>% 
  #write.table("nfl_qb.csv", sep = ",", col.names = !file.exists("nfl_qb.csv"), append = T)
  #write.csv(file = paste("nfl_qb_week_", max(nfl_2021$week)-1,".csv", sep = ""))

#nfl_qb$Name2 <- separate(nfl_qb, Name , into = c("Name2"), sep = " ")
#nfl_qb$Name2 <- nfl_qb$Name
#nfl_qb$Name_week <- paste(nfl_qb$Name, nfl_qb$week)

###QB Results Chart###

summary(lm(FPTS ~ poly(sum_sd, 3, raw = T), data = nfl_qb))

nfl_qb_chart <- nfl_qb %>% 
  filter(FPTS > 0)

nfl_qb_chart$name_salary_own <- paste(nfl_qb$name_salary, nfl_qb$Own)

nfl_qb_chart %>%
  ggplot(aes(x = sum_sd , y = FPTS)) +
  geom_hline(yintercept = mean(nfl_qb_chart$FPTS, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(nfl_qb_chart$sum_sd, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  geom_point(color = nfl_qb_chart$team_color, cex = (25*nfl_qb_chart$FPTS_vs_proj/max(nfl_qb_chart$FPTS_vs_proj, na.rm = T)), alpha = .6) +
  geom_smooth(method = "lm") +
  geom_text_repel(aes(label=name_salary_own)) +
  annotate("text", x = (0.8* max(nfl_qb_chart$sum_sd)), y = min(nfl_qb_chart$FPTS, na.rm = T), label = paste("r2 =",round(summary(lm(FPTS ~ sum_sd, data = nfl_qb_chart))$r.squared, digits =3))) +
  labs(x = "sum_sd",
       y = "FPTS",
       title = paste("QBs, NFL Week",max(nfl_2021$week)),
       caption = paste("Twitter: Its_MikeF | Data: DraftKings | Date:", today())) +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), sec.axis = sec_axis(trans=~.*1, name="FPTS", breaks = scales::pretty_breaks(n = 10))) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

###RB Results Check###

nfl_rb <- nfl_rb %>%
  left_join(nfl_results, by = c('Name' = 'Player'))

nfl_rb$FPTS_dollar <- round(nfl_rb$FPTS / nfl_rb$Salary, digits = 5)

nfl_rb$FPTS_vs_proj <- round(nfl_rb$FPTS / nfl_rb$fantasyPoints, digits = 1)

nfl_rb$name_salary_own <- paste(nfl_rb$name_salary, nfl_rb$Own)

nfl_rb %>%
  filter(Position == "RB") %>% 
  select(Name,
         week,
         TeamAbbrev,
         def_rush_epa_rank,
         opponent,
         rdef_rank,
         Salary,
         Own,
         FPTS,
         sum_sd,
         FPTS_vs_proj,
         FPTS_dollar,
         ppo, 
         runBlockAdv) %>%
  arrange(-FPTS) %>%
  view(title = "RBs Results") 
  #write.csv(file = paste("nfl_rb_week_", max(nfl_2021$week)+1,".csv", sep = ""))

###RB Results Chart###

summary(lm(FPTS ~ poly(sum_sd, 3, raw = T), data = nfl_rb))

nfl_rb_chart <- nfl_rb %>% 
  filter(FPTS > 0)

nfl_rb_chart %>%
  ggplot(aes(x = sum_sd , y = FPTS)) +
  geom_hline(yintercept = mean(nfl_rb_chart$FPTS, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(nfl_rb_chart$sum_sd, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  geom_point(color = nfl_rb_chart$team_color.x, cex = (25*nfl_rb_chart$FPTS_vs_proj/max(nfl_rb_chart$FPTS_vs_proj, na.rm = T)), alpha = .6) +
  geom_smooth(method = "lm") +
  geom_text_repel(aes(label=name_salary_own)) +
  annotate("text", x = max(nfl_rb_chart$sum_sd, na.rm = T)-.1, y = 1, label = paste("r2 =",round(summary(lm(FPTS ~ sum_sd, data = nfl_rb_chart))$r.squared, digits =3))) +
  labs(x = "sum_sd",
       y = "FPTS",
       title = paste("RBs, NFL Week",max(nfl_2021$week)),
       caption = "Twitter: Its_MikeF | Data: DraftKings") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

###WR Results Check###

nfl_wr <- nfl_wr %>%
  left_join(nfl_results, by = c('Name' = 'Player'))

nfl_wr$FPTS_dollar <- round(nfl_wr$FPTS / nfl_wr$Salary, digits = 5)

nfl_wr$FPTS_vs_proj <- round(nfl_wr$FPTS / nfl_wr$fantasyPoints, digits = 1)

nfl_wr$name_salary_own <- paste(nfl_wr$name_salary, nfl_wr$Own)

nfl_wr %>%
  filter(Position == "WR") %>%
  select(Name,
         week,
         TeamAbbrev,
         Salary,
         Own, 
         FPTS,
         sum_sd,
         FPTS_vs_proj,
         FPTS_dollar,
         ppo,
         advantage,
         yprr, 
         man_yprr,
         man_rank,
         opponent) %>%
  arrange(-FPTS) %>%
  view(title = "WRs Results")
  #write.csv(file = paste("nfl_wr_week_", max(nfl_2021$week)+1,".csv", sep = ""))

###WR Results Chart###

nfl_rb_chart <- nfl_rb %>% 
  filter(FPTS > 0)

nfl_wr %>%
  ggplot(aes(x = sum_sd , y = FPTS)) +
  geom_hline(yintercept = mean(nfl_wr$FPTS, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(nfl_wr$sum_sd, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  geom_point(color = nfl_wr$team_color, cex = (25*nfl_wr$FPTS_vs_proj/max(nfl_wr$FPTS_vs_proj, na.rm = T)), alpha = .6) +
  geom_smooth(method = "lm") +
  geom_text_repel(aes(label=name_salary_own)) +
  annotate("text", x = 1, y = 1, label = paste("r2 =",round(summary(lm(FPTS ~ sum_sd, data = nfl_wr))$r.squared, digits =3))) +
  labs(x = "sum_sd",
       y = "FPTS",
       title = paste("WRs, NFL Week",max(nfl_2021$week)),
       caption = "Twitter: Its_MikeF | Data: DraftKings") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

###TE Results Check###

nfl_te <- nfl_te %>%
  left_join(nfl_results, by = c('offPlayer' = 'Player'))

nfl_te$fpts_adv <- round(nfl_te$FPTS / nfl_te$adv, digits = 1)

nfl_te$FPTS_dollar <- round(nfl_te$FPTS / nfl_te$Salary, digits = 5)

nfl_te$name_salary_own <- paste(nfl_te$offPlayer, nfl_te$Salary, nfl_te$Own)

nfl_te %>%
  select(offPlayer,
         TeamAbbrev,
         Salary,
         Own, 
         FPTS,
         FPTS_dollar,
         ppo,
         offYprr, 
         adv, 
         fpts_adv) %>%
  arrange(-FPTS) %>%
  view(title = "TEs Results")

###TE Results Chart###

nfl_te %>%
  ggplot(aes(x = adv , y = FPTS)) +
  geom_hline(yintercept = mean(nfl_te$FPTS, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(nfl_te$adv, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  geom_point(cex = 25 * (nfl_te$Own / max(nfl_te$Own)), alpha = 0.6) +
  geom_smooth(method = "lm") +
  geom_text_repel(aes(label=name_salary_own)) +
  annotate("text", x = .7* max(nfl_te$adv, na.rm = T), y = 1, label = paste("r2 =",round(summary(lm(FPTS ~ adv, data = nfl_te))$r.squared, digits =3))) +
  labs(x = "adv",
       y = "FPTS",
       title = paste("TEs, NFL Week",max(nfl_2021$week)),
       caption = "Twitter: Its_MikeF | Data: DraftKings") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

###Def Results Check###

nfl_results %>% 
  filter(Roster.Position == "DST") %>% 
  arrange(-FPTS) %>% 
  view(title = "DEF Results")
