#trying to learn this script from this link
#https://nflseedr.com/articles/nflsim.html

library(nflseedR)
library(dplyr)
options(digits = 3)

set.seed(4)
sims <- simulate_nfl(
  nfl_season = 2020, 
  fresh_season = T, 
  simulations = 100
)

sims$teams %>% 
  dplyr::filter(team == "CHI") %>% 
  select(sim, team, wins, seed, draft_order) %>% 
  head(6) %>% 
  knitr:: kable()

sims$games %>% filter(sim == 1, game_type != "REG") %>% knitr::kable()

summary(sims)

stupid_games_model <- function(teams, games, week_num, ...) {
  # make the earlier alphabetical team win 90% of the time
  games <- games %>%
    dplyr::mutate(
      result = dplyr::case_when(
        !is.na(result) | week != week_num ~ result,
        away_team < home_team ~ sample(c(-3, 3), n(), prob = c(0.9, 0.1), replace = TRUE),
        away_team > home_team ~ sample(c(-3, 3), n(), prob = c(0.1, 0.9), replace = TRUE),
        TRUE ~ 0
      )
    )
  
  # return values
  return(list(teams = teams, games = games))
}

sims2 <- simulate_nfl(
  nfl_season = 2020, 
  process_games = stupid_games_model, 
  fresh_season = T, 
  simulations = 100
)

sims2$overall %>% arrange(team) %>% head() %>% knitr::kable()
sims2$overall %>% arrange(team) %>% tail() %>% knitr::kable()

biased_games_model <- function(teams, games, week_num, ...) {
  
  # arguments
  args <- list(...)
  best <- ""
  worst <- ""
  
  # best team?
  if ("best" %in% names(args)) {
    best <- args$best
  }
  
  # worst team?
  if ("worst" %in% names(args)) {
    worst <- args$worst
  }
  
  # make the best team always win and the worst team always lose
  # otherwise, make the earlier alphabetical team win 90% of the time
  games <- games %>%
    dplyr::mutate(
      result = dplyr::case_when(
        !is.na(result) | week != week_num ~ result,
        away_team == best | home_team == worst ~ -3,
        away_team == worst | home_team == best ~ 3,
        away_team < home_team ~ sample(c(-3, 3), n(), prob = c(0.9, 0.1), replace = TRUE),
        away_team > home_team ~ sample(c(-3, 3), n(), prob = c(0.1, 0.9), replace = TRUE),
        TRUE ~ 0
      )
    )
  
  # return values
  return(list(teams = teams, games = games))
}

sims3 <- simulate_nfl(
  nfl_season = 2020, 
  process_games = biased_games_model, 
  fresh_season = T, 
  simulations = 25, 
  best = "CHI", 
  worst = "GB"
)

summary(sims3)

elo_model <- function(teams, games, week_num, ...) {
  
  # round out (away from zero)
  # this way the simulator never simulates a tie
  # the simulator will still allow ties to be simulated if you want
  # ... but not on playoff games
  round_out <- function(x) {
    x[!is.na(x) & x < 0] <- floor(x[!is.na(x) & x < 0])
    x[!is.na(x) & x > 0] <- ceiling(x[!is.na(x) & x > 0])
    return(x)
  }
  
  # we're going to store elo as a new columns in the teams data
  # it won't start off there of course, so we need to determine it
  # from our arguments
  if (!("elo" %in% colnames(teams))) {
    args <- list(...)
    if ("elo" %in% names(args)) {
      # pull the elo info from custom arguments
      teams <- teams %>%
        dplyr::inner_join(args$elo %>% dplyr::select(team, elo), by = c("team" = "team"))
    } else {
      # error with a friendly error message if no elo data is passed in
      stop("Pass in a tibble `elo` as an argument to `simulate_nfl()`")
    }
  }
  
  # isolate the ratings data by sim and by team only
  # we will want to join to the games data later and don't want excess columns
  ratings <- teams %>% dplyr::select(sim, team, elo)
  
  # simulate game outcomes
  games <- games %>%
    # add in the away team's elo to the game data
    # note we join on both `sim` and the team
    # always join on `sim` to make sure each sim cares about only its data
    dplyr::inner_join(ratings, by = c("sim" = "sim", "away_team" = "team")) %>%
    dplyr::rename(away_elo = elo) %>%
    # repeat for the home team as well
    dplyr::inner_join(ratings, by = c("sim" = "sim", "home_team" = "team")) %>%
    dplyr::rename(home_elo = elo) %>%
    dplyr::mutate(
      # calculate the elo difference
      elo_diff = home_elo - away_elo,
      # add in a small HFA amount if played at home
      elo_diff = elo_diff + ifelse(location == "Home", 20, 0),
      # make an adjustment for rest
      elo_diff = elo_diff + (home_rest - away_rest) / 7 * 25,
      # playoff games swing elo more
      elo_diff = elo_diff * ifelse(game_type == "REG", 1, 1.2),
      # from elo, we calculate the home team's win percentage
      wp = 1 / (10^(-elo_diff / 400) + 1),
      # we also can calculate the estimate (mean points home team wins by)
      estimate = elo_diff / 25,
      result = dplyr::case_when(
        # !!! ALWAYS DO THIS NEXT LINE IN YOUR `result` CHANGES !!!
        # you have to make sure you're only changing unfinished games in current week
        # if you don't do this, it will usually error out on a friendly error message
        is.na(result) & week == week_num ~ 
          as.integer(round_out(rnorm(n(), estimate, 13))),
        # if not this week or known result, leave as-is
        TRUE ~ as.integer(result)
      ),
      # simplify to 1 = win, 0 = loss, 0.5 = tie to help calculate elo shift
      outcome = dplyr::case_when(
        is.na(result) ~ NA_real_,
        result > 0 ~ 1,
        result < 0 ~ 0,
        TRUE ~ 0.5
      ),
      # calculate the amount to adjust home team's elo by
      elo_input = dplyr::case_when(
        is.na(result) ~ NA_real_,
        result > 0 ~ elo_diff * 0.001 + 2.2,
        result < 0 ~ -elo_diff * 0.001 + 2.2,
        TRUE ~ 1.0,
      ),
      elo_mult = log(pmax(abs(result), 1) + 1.0) * 2.2 / elo_input,
      elo_shift = 20 * elo_mult * (outcome - wp)
    ) %>%
    # we don't want these columns in `games` any more
    # remove any columns you don't need when you're done
    # otherwise the next week they'll get joined as `col.x` and `col.y`
    # which will almost certainly break your script
    dplyr::select(
      -away_elo, -home_elo, -elo_diff, -wp, -estimate,
      -outcome, -elo_input, -elo_mult
    )
  
  # apply elo shifts
  teams <- teams %>%
    # join games results from this week to away teams (within same sim!)
    # note this is a LEFT join, we don't want to remove any teams rows
    dplyr::left_join(games %>%
                       dplyr::filter(week == week_num) %>%
                       dplyr::select(sim, away_team, elo_shift),
                     by = c("sim" = "sim", "team" = "away_team")
    ) %>%
    # away team's elo gets subtracted by elo amount
    # if the team wasn't an away team, do nothing
    dplyr::mutate(elo = elo - ifelse(!is.na(elo_shift), elo_shift, 0)) %>%
    # we don't want to keep `elo_shift` in `teams` either, remove it
    dplyr::select(-elo_shift) %>%
    # repeat the above except now do it for the home team
    dplyr::left_join(games %>%
                       dplyr::filter(week == week_num) %>%
                       dplyr::select(sim, home_team, elo_shift),
                     by = c("sim" = "sim", "team" = "home_team")
    ) %>%
    # note that a team on a bye will have `elo_shift` as NA for both joins
    # this means it won't change, which is what we want
    dplyr::mutate(elo = elo + ifelse(!is.na(elo_shift), elo_shift, 0)) %>%
    dplyr::select(-elo_shift)
  
  # we need to keep `elo_shift` out of `games` too and we're done with it
  games <- games %>%
    dplyr::select(-elo_shift)
  
  # return the updated teams and games information
  # note that `teams` will now have an updated `elo` column which will
  # be used for the next week's games
  # note that starting `elo` values are the same per-team... 
  # ... but after that will differ per sim depending on that sim's results
  return(list(teams = teams, games = games))
}

initial_elo <- tibble::tibble(
  team = unique(nflseedR::divisions$team),
  elo = rnorm(length(unique(nflseedR::divisions$team)), 1500, 150)
)
test <- simulate_nfl(
  nfl_season = 2020,
  process_games = elo_model,
  elo = initial_elo,
  fresh_season = TRUE,
  test_week = 3
)

test$teams %>%
  dplyr::filter(team == "CHI") %>%
  utils::head() %>%
  knitr::kable()

test$games %>%
  filter(sim == 1) %>%
  filter(away_team == "CHI" | home_team == "CHI")
