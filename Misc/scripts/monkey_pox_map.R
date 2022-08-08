#load packages
suppressMessages({
  library(ggplot2) #	Create Elegant Data Visualisations Using the Grammar of Graphics
  library(dplyr) #A Grammar of Data Manipulation
  library(maps) # Draw Geographical Maps
  library(viridis) # Colorblind-Friendly Color Maps for R
})

case_series <- read.csv("https://raw.githubusercontent.com/globaldothealth/monkeypox/main/timeseries-country-confirmed.csv")
head(case_series)

world_map <- map_data("world")
head(world_map)

plot_case_map <- function(date, xlim, ylim) {
  # Pre-process case and map data
  case_map <- case_series[which(case_series$Date == date), c(4, 3)]
  colnames(case_map)[1] <- "region"
  case_map$region[which(case_map$region == "United States")] <- "USA"
  case_map$region[which(case_map$region == "United Kingdom")] <- "UK"
  case_map$region[which(case_map$region == "Democratic Republic Of The Congo")] <- "Democratic Republic of the Congo"
  case_map$region[which(case_map$region == "Bosnia And Herzegovina")] <- "Bosnia and Herzegovina"
  if ("Gibraltar" %in% case_map$region) {
    case_map <- case_map[-which(case_map$region == "Gibraltar"), ]
  }
  if (length(setdiff(world_map$region, case_map$region)) > 0) {
    case_map_other <- as.data.frame(cbind(setdiff(world_map$region, case_map$region), NA))
    colnames(case_map_other) <- c("region", "Cumulative_cases")
    case_map <- rbind(case_map, case_map_other)
  }
  case_map$Cumulative_cases <- as.numeric(case_map$Cumulative_cases)
  case_map <- left_join(case_map, world_map, by = "region")
  # Plot case map
  ggplot(case_map, aes(long, lat, group = group)) +
    geom_polygon(aes(fill = Cumulative_cases), color = "white", size = 0.2) +
    scale_fill_viridis_c() +
    theme_linedraw() +
    theme(legend.position = "right") +
    labs(fill = "Cumulative cases") +
    theme(legend.direction = "vertical") +
    coord_map(xlim = xlim, ylim = ylim)
}
plot_case_map("2022-07-29", c(-180, 180), c(-55, 90))
