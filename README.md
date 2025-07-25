# Sports Analytics Projects

A collection of sports analytics projects focusing on NFL, MLB, and college basketball using R and Python.

## Overview
This repository contains data analysis and predictive modeling projects for sports analytics, including:
- NFL fantasy stat projections using `nflverse` and `cfbfastR`.
- MLB pitcher and batter performance models using `baseballr` and Statcast data.
- College basketball analytics via KenPom data scraping.

## Projects
- **NFL Fantasy Projections** (`/NFL/02_scripts/projections`): Predicts passing, rushing, and receiving stats for NFL rookies by integrating college stats with NFL draft data.
- **MLB Pitcher Projections** (`/MLB`): Builds per-start ERA and strikeout predictions using FanGraphs and Statcast data.
- **KenPom Basketball Scraper** (`/NBA`): Scrapes college basketball team performance data from KenPom.
- **College Football Analytics** (`/CFB`): Analyzes game IDs and player stats using `cfbfastR`.

## Setup
To run these projects, install the required R packages:
```R
install.packages(c("nflreadr", "tidymodels", "tidyverse", "baseballr"))
