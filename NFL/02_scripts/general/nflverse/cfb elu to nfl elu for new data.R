# lets project cfb elu to nfl elu

# load packages
suppressMessages({
  library(dplyr)
  library(tidyr)
  library(glue)
})

# load model
rb_elu <- readRDS(file = "./04_models/rb_elu.rds")

# print the model
rb_elu$call

# define new data
new_data = tibble(col1 = 92, 
                  col2 = 96)

# define column names
col1 <- rb_elu$terms[[3]][[2]]
col2 <- rb_elu$terms[[3]][[3]]

# add col cnames
names(new_data) <- c(col1,col2)

# predict rb elu
predict(rb_elu, newdata = new_data)
