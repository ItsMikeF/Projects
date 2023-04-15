#load packages
library(tidyverse)
library(gt)

#import lift data
lift <- read_csv("lift.csv")

#data wrangling
colnames(lift) <- gsub(" ","_",tolower(colnames(lift)))

lift$product_name <- gsub(" ","_",lift$product_name)
lift$product_name <- gsub("___","_", lift$product_name)
lift$product_name <- gsub("___","_", lift$product_name)
lift$product_name <- gsub("__","_", lift$product_name)
lift$product_name <- gsub("__","_", lift$product_name)

lift <- separate(lift, col = "port_name",into = c("city", "state"), sep = "[(]")

lift$state <- gsub("[)]", "", lift$state)

lift <- lift %>% 
  drop_na()

lift$city <- trimws(lift$city)

states <- tibble(sort(unique(lift$state)))
colnames(states)[1] <- "state"
states

state_abbrev <- read.csv("state_abbrev.csv")

states <- states %>% 
  left_join(state_abbrev, by=c("state")) %>% 
  drop_na() 

lift$state[lift$city == "NEWARK - ELIZABETH"] <- "New Jersey"

lift <- lift %>% 
  left_join(states, by=c("state"))

#change date formats
lift$del_conf_date <- as.Date(lift$del_conf_date, tryFormats = "%d/%m/%Y")
lift$required_del._date <- as.Date(lift$required_del._date, tryFormats = "%d/%m/%Y")
lift$pricing_date <- as.Date(lift$pricing_date, tryFormats = "%d/%m/%Y")

lift$month <- as.numeric(substr(lift$required_del._date,6,7))
lift$year <- as.numeric(substr(lift$required_del._date,1,4))

#by city 
lift_city <- lift %>% 
  group_by(city) %>% 
  summarise(
    total_weight = sum(net_weight),
    total_amount = sum(net_amount)
  ) %>% 
  arrange(-total_amount) 

lift_city$total_weight <- formatC(lift_city$total_weight, format = "d", big.mark = ",")
lift_city$total_amount <- formatC(lift_city$total_amount, format = "d", big.mark = ",")

gt_lift_city <- lift_city %>% 
  #top_n(10) %>% 
  gt(rowname_col = "port_name") %>% 
  tab_header(
    title = "Top 10 Port Lifting Data",
    subtitle = "Data from Jan to June 2022"
  ) %>% 
  tab_stubhead(label = "port") 

gt_lift_city

#by city and customer
lift_city_customer <- lift %>% 
  group_by(city, customer_name) %>% 
  summarise(
    n = n(),
    total_weight = sum(net_weight),
    total_amount = sum(net_amount), 
    amount_per_delivery = round(total_amount / n, digits = 0)
  ) %>% 
  arrange(-total_amount)
lift_city_customer

#by product 
lift_product <- lift %>% 
  group_by(product_name) %>% 
  summarise(
    total_weight = sum(net_weight),
    total_amount = sum(net_amount)
  ) %>% 
  arrange(-total_amount)
lift_product

#by product and customer
lift_product_customer <- lift %>% 
  group_by(product_name, customer_name) %>% 
  summarise(
    total_weight = sum(net_weight),
    total_amount = sum(net_amount)
  ) %>% 
  arrange(-total_amount)
lift_product_customer

#by plants
lift_plants <- lift %>% 
  group_by(plant) %>% 
  summarise(
    total_weight = sum(net_weight),
    total_amount = sum(net_amount)
  ) %>% 
  arrange(-total_amount) %>% 
  drop_na() %>% 
  left_join(read.csv("plant_codes.csv"), by=c("plant"="p3b")) %>% 
  select(plant_name, plant, total_weight, total_amount)

lift_plants$total_weight <- formatC(lift_plants$total_weight, format = "d", big.mark = ",")
lift_plants$total_amount <- formatC(lift_plants$total_amount, format = "d", big.mark = ",")

lift_plants

#by customer
lift_customer <- lift %>% 
  group_by(customer_name) %>% 
  summarise(
    deliveries = n(),
    total_weight = sum(net_weight),
    total_amount = sum(net_amount)
  ) %>% 
  mutate(amount_per_delivery = round(total_amount / deliveries, digits = 0)) %>% 
  arrange(-total_amount)

lift_customer$total_weight <- formatC(lift_customer$total_weight, format = "d", big.mark = ",")
lift_customer$total_amount <- formatC(lift_customer$total_amount, format = "d", big.mark = ",")
lift_customer$amount_per_delivery <- formatC(lift_customer$amount_per_delivery, format = "d", big.mark = ",")
lift_customer

#by state
lift_state <- lift %>% 
  group_by(abbrev) %>% 
  summarise(
    total_amount = sum(net_amount)
  ) %>% 
  arrange(-total_amount)
lift_state

#by month
lift_month <- lift %>% 
  group_by(city, month, year) %>% 
  summarise(
    total_amount = sum(net_amount)
  ) %>% 
  arrange(-total_amount)
lift_month

#summary
print(paste("# of ports:", dim(lift_port)[1]))
print(paste("$ of products:", dim(lift_product)[1]))
print(paste("# of customers", dim(lift_customer)[1]))
