#lets move and rename files ffrom contests to training data

#load packages
library(glue)
library(purrr)


# 1.0 Define the files to be copied and moved -----------------------------

# Create a vector of all of the files
files <- list.files(path = "./01_data/contests/2023_w2/pff")

# Define the strings to remove
remove_strings <- c("dk-ownership.csv", "te_matchup_chart.csv", "wr_cb_matchup_chart.csv")

# Create a regular expression that matches any of the remove strings
remove_regex <- paste(remove_strings, collapse = "|")

# Find the indices of the elements to remove
remove_indices <- grep(remove_regex, files)

# Remove hte elements from the vector using negative indexing
files <- files[-remove_indices]

#edit the string names to remove the .csv
files <- unlist(map(files, ~substr(.x, 1, nchar(.x)-4)))

# 2.0 Copy and paste the files to the directory ---------------------------

# week is current game week
for (i in 1:5) {
  
  for (j in 1:25) {
    #copy the files
    file.copy(glue("./01_data/contests/2023_w{i}/pff/{files[j]}.csv"), 
              glue("./01_data/training_data/2023/{files[j]} ({i}).csv"))
  }
  
}
