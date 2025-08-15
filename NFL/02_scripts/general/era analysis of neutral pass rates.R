# ================== Neutral Pass Rate + Era Analysis (Parallel, robust) =================
# First-time installs (if needed):
# install.packages(c("nflreadr","dplyr","tidyr","ggplot2","purrr","future","furrr","changepoint","strucchange"))

suppressPackageStartupMessages({
  library(nflreadr)   # load_pbp(), load_schedules()
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(purrr)
  library(future)
  library(furrr)
})

# ----------------------------- Parallel helpers --------------------------------
# Parallel-safe season loader (uses future/furrr). No progress bars.
load_pbp_parallel <- function(
    seasons,
    use_parallel = TRUE,
    workers = max(1, parallel::detectCores(logical = TRUE) - 2),
    select_cols = c("season","posteam","pass","rush","down","wp",
                    "half_seconds_remaining","qtr")
) {
  if (use_parallel && length(seasons) > 1) {
    furrr::future_map_dfr(
      seasons,
      ~ nflreadr::load_pbp(.x) %>% dplyr::select(dplyr::any_of(select_cols)),
      .options = furrr::furrr_options(seed = TRUE)
    )
  } else {
    purrr::map_dfr(
      seasons,
      ~ nflreadr::load_pbp(.x) %>% dplyr::select(dplyr::any_of(select_cols))
    )
  }
}

# Build team-season records (REG only): win% and point diff
build_team_records <- function(seasons) {
  sched <- nflreadr::load_schedules(seasons) %>% dplyr::filter(game_type == "REG")
  
  home <- sched %>%
    transmute(season,
              team = home_team,
              points_for = home_score,
              points_against = away_score,
              win = as.integer(home_score > away_score))
  
  away <- sched %>%
    transmute(season,
              team = away_team,
              points_for = away_score,
              points_against = home_score,
              win = as.integer(away_score > home_score))
  
  bind_rows(home, away) %>%
    group_by(season, team) %>%
    summarise(
      win_pct    = mean(win, na.rm = TRUE),
      point_diff = sum(points_for - points_against, na.rm = TRUE),
      .groups = "drop"
    )
}

# ---------------------------- Neutral filter -----------------------------------
neutral_filter <- function(pbp,
                           wp_low = 0.25, wp_high = 0.75,
                           min_half_sec = 120,
                           downs = 1:3,
                           include_q4_if_neutral = TRUE) {
  pbp %>%
    filter((pass == 1 | rush == 1)) %>%
    filter(down %in% downs) %>%
    filter(wp > wp_low, wp < wp_high) %>%
    filter(half_seconds_remaining > min_half_sec) %>%
    filter(if (include_q4_if_neutral) TRUE else qtr <= 3)
}

# Safe weighted mean: fall back to unweighted mean if weights unusable
safe_weighted_mean <- function(x, w) {
  if (length(x) == 0) return(NA_real_)
  if (all(is.na(w)) || sum(w, na.rm = TRUE) == 0) {
    return(mean(x, na.rm = TRUE))
  }
  out <- suppressWarnings(stats::weighted.mean(x, w = w, na.rm = TRUE))
  if (is.nan(out) || is.na(out) || !is.finite(out)) return(mean(x, na.rm = TRUE))
  out
}

# ------------------------- Core Functions ---------------------------------------
# 1) Team neutral pass rate table
team_neutral_pass_rate <- function(seasons = 2024,
                                   wp_low = 0.25, wp_high = 0.75,
                                   min_half_sec = 120,
                                   downs = 1:3,
                                   include_q4_if_neutral = TRUE,
                                   use_parallel = TRUE,
                                   workers = max(1, parallel::detectCores(logical = TRUE) - 2)) {
  
  pbp <- load_pbp_parallel(seasons, use_parallel = use_parallel, workers = workers)
  
  neut <- neutral_filter(
    pbp,
    wp_low = wp_low, wp_high = wp_high,
    min_half_sec = min_half_sec,
    downs = downs,
    include_q4_if_neutral = include_q4_if_neutral
  )
  
  neut %>%
    group_by(season, posteam) %>%
    summarise(
      pass_plays = sum(pass == 1, na.rm = TRUE),
      rush_plays = sum(rush == 1, na.rm = TRUE),
      plays = n(),
      .groups = "drop"
    ) %>%
    mutate(neutral_pass_rate = round(pass_plays / pmax(plays, 1), 3)) %>%
    arrange(season, desc(neutral_pass_rate))
}

# 2) Era analysis using league NPR with optional weighting (robust, no progress)
# weight_by: "none", "win_pct", or "point_diff"
era_neutral_pass_analysis <- function(seasons = 2012:2024,
                                      wp_low = 0.25, wp_high = 0.75,
                                      min_half_sec = 120,
                                      downs = 1:3,
                                      include_q4_if_neutral = TRUE,
                                      weight_by = c("none", "win_pct", "point_diff"),
                                      penalty = "BIC",
                                      use_parallel = TRUE,
                                      workers = max(1, parallel::detectCores(logical = TRUE) - 2)) {
  weight_by <- match.arg(weight_by)
  
  # Load data
  pbp <- load_pbp_parallel(seasons, use_parallel = use_parallel, workers = workers)
  team_records <- build_team_records(seasons)
  
  # Neutral plays
  neut <- neutral_filter(
    pbp,
    wp_low = wp_low, wp_high = wp_high,
    min_half_sec = min_half_sec,
    downs = downs,
    include_q4_if_neutral = include_q4_if_neutral
  )
  
  team_npr <- neut %>%
    group_by(season, posteam) %>%
    summarise(
      pass_plays = sum(pass == 1, na.rm = TRUE),
      plays = n(),
      neutral_pass_rate = pass_plays / pmax(plays, 1),
      .groups = "drop"
    ) %>%
    left_join(team_records, by = c("season", "posteam" = "team"))
  
  # Robust seasonal aggregation
  season_rates <- team_npr %>%
    group_by(season) %>%
    summarise(
      neutral_pass_rate = dplyr::case_when(
        weight_by == "none"       ~ mean(neutral_pass_rate, na.rm = TRUE),
        weight_by == "win_pct"    ~ safe_weighted_mean(neutral_pass_rate, w = win_pct),
        weight_by == "point_diff" ~ safe_weighted_mean(neutral_pass_rate, w = pmax(point_diff, 0))
      ),
      .groups = "drop"
    ) %>%
    arrange(season) %>%
    # final guard against NA/NaN/Inf
    mutate(neutral_pass_rate = ifelse(is.finite(neutral_pass_rate),
                                      neutral_pass_rate,
                                      mean(neutral_pass_rate[is.finite(neutral_pass_rate)], na.rm = TRUE)))
  
  # Changepoint detection (only on finite values)
  sr <- season_rates %>% filter(is.finite(neutral_pass_rate))
  breaks <- integer(0)
  if (nrow(sr) >= 3) {
    if (requireNamespace("changepoint", quietly = TRUE)) {
      fit <- changepoint::cpt.mean(sr$neutral_pass_rate, method = "PELT", penalty = penalty)
      breaks <- sr$season[changepoint::cpts(fit)]
    } else if (requireNamespace("strucchange", quietly = TRUE)) {
      bf <- strucchange::breakpoints(neutral_pass_rate ~ 1, data = sr)
      k  <- which.min(BIC(bf))
      if (length(k) && is.finite(k)) {
        bp  <- stats::breakpoints(bf, breaks = k)
        idx <- bp$breakpoints
        breaks <- sr$season[idx]
      }
    }
  }
  
  # Era labels
  if (length(breaks)) {
    cut_points <- sort(unique(breaks))
    season_rates <- season_rates %>%
      mutate(
        era_id = cut(
          season,
          breaks = c(min(season_rates$season) - 1, cut_points, max(season_rates$season)),
          labels = paste0("Era ", seq_len(length(cut_points) + 1)),
          right = TRUE
        )
      )
  } else {
    season_rates <- season_rates %>% mutate(era_id = factor("Era 1"))
  }
  
  eras <- season_rates %>%
    group_by(era_id) %>%
    summarise(
      start_season = min(season),
      end_season   = max(season),
      avg_npr      = mean(neutral_pass_rate),
      .groups = "drop"
    )
  
  p <- ggplot(season_rates, aes(season, neutral_pass_rate)) +
    geom_line() +
    geom_point() +
    labs(
      title = paste("League Neutral Pass Rate by Season",
                    ifelse(weight_by == "none", "(Unweighted)",
                           paste0("(Weighted by ", weight_by, ")"))),
      x = "Season",
      y = "Neutral Pass Rate"
    ) +
    scale_x_continuous(breaks = season_rates$season) +
    theme_minimal()
  
  if (length(breaks)) {
    p <- p + geom_vline(xintercept = breaks, linetype = "dashed")
  }
  
  list(
    season_rates = season_rates,
    breaks = breaks,
    eras = eras,
    plot = p
  )
}

# 3) Convenience: overlay weighted vs unweighted on one chart
era_compare_plot <- function(seasons = 2012:2024,
                             use_parallel = TRUE,
                             workers = max(1, parallel::detectCores(logical = TRUE) - 2),
                             ...) {
  unw <- era_neutral_pass_analysis(seasons, weight_by = "none",
                                   use_parallel = use_parallel, workers = workers, ...)
  win <- era_neutral_pass_analysis(seasons, weight_by = "win_pct",
                                   use_parallel = use_parallel, workers = workers, ...)
  pd  <- era_neutral_pass_analysis(seasons, weight_by = "point_diff",
                                   use_parallel = use_parallel, workers = workers, ...)
  
  comb <- bind_rows(
    unw$season_rates %>% mutate(series = "Unweighted"),
    win$season_rates %>% mutate(series = "Weighted (Win%)"),
    pd$season_rates  %>% mutate(series = "Weighted (Point Diff)")
  )
  
  ggplot(comb, aes(season, neutral_pass_rate, group = series)) +
    geom_line() +
    geom_point() +
    facet_wrap(~series, ncol = 1, scales = "free_y") +
    labs(title = "Neutral Pass Rate by Season: Unweighted vs Weighted",
         x = "Season", y = "Neutral Pass Rate") +
    scale_x_continuous(breaks = sort(unique(comb$season))) +
    theme_minimal()
}

# ----------------------------- EXAMPLES -----------------------------------------
# Pick workers automatically (leave 1–2 for OS)
workers <- max(1, parallel::detectCores(logical = TRUE) - 2)

# Turn ON parallel plan once
future::plan(future::multisession, workers = workers)

# Team table (2024)
team_2024 <- team_neutral_pass_rate(2024, use_parallel = TRUE, workers = workers)
print(team_2024 %>% arrange(desc(neutral_pass_rate)))

# Era analysis (unweighted, then weighted)
era_unw <- era_neutral_pass_analysis(2012:2024, weight_by = "none",
                                     use_parallel = TRUE, workers = workers)
era_win <- era_neutral_pass_analysis(2012:2024, weight_by = "win_pct",
                                     use_parallel = TRUE, workers = workers)
era_pd  <- era_neutral_pass_analysis(2012:2024, weight_by = "point_diff",
                                     use_parallel = TRUE, workers = workers)

print(era_unw$eras); print(era_win$eras); print(era_pd$eras)
print(era_unw$plot); print(era_win$plot); print(era_pd$plot)

# Combined comparison plot
p_compare <- era_compare_plot(2012:2024, use_parallel = TRUE, workers = workers)
print(p_compare)

# Optional: reset to sequential when done
future::plan(future::sequential)
# ===============================================================================#
