###Bracket with a loop###

rounds <- c("r64", "r32", "r16", "e8", "f4", "c2")
rounds_loop <- c("r32", "r16", "e8", "f4", "c2")

predictions <- data.frame(matrix(ncol = 6, nrow = 32))
names(predictions) <- rounds

for (i in rounds){
  predictions[,(which(rounds == i))] <- get(i)[,51]
}

### Make this into a loop so the predicted bracket is easier to read ###
### No duplicates

for (i in rounds){
  winner <- data.frame(r64$winner)
  
  predictions <- cbind("id"=rownames(predictions), predictions)
  predictions$id <- as.numeric(predictions$id)
  
  winner <- cbind("id"=rownames(winner), winner)
  winner$id <- as.numeric(winner$id)
  
  merge <- merge(predictions, winner, all = T) 
}
