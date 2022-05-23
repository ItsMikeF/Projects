### Odds Table via loop ###

golfer_odds_loop <- data.frame()
book_abbrev <- c("dk", "fd", "fb", "bs", "ts", "ub", "bf")

for (i in 1:length(content[[1]][[8]][[3]][[4]][[1]][[2]])) {
  
  for (j in c(2:length(content[[1]][[8]]))) {
    assign(paste0("golfer_names_", book_abbrev[1]), data.frame())
    get(paste0("golfer_names_", book_abbrev[1]))[i,1] <- content[[1]][[8]][[j]][[4]][[1]][[2]][[i]]$name
    assign(paste0("golfer_names_", book_abbrev[1]))[i,2] <- content[[1]][[8]][[j]][[4]][[1]][[2]][[i]]$price
  }
  golfer_names_dk <- golfer_names_dk %>% 
    left_join(get(paste0("golfer_names_", book_abbrev[1])), by=c("V1"))
}

get(paste0("golfer_names_", book_abbrev[1]))[1,1] <- content[[1]][[8]][[1]][[4]][[1]][[2]][[i]]$name

assign(paste0("golfer_names_", book_abbrev[1]))[i,1] <- content[[1]][[8]][[1]][[4]][[1]][[2]][[1]]$name
