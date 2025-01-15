# The URL you want to open
url <- ("https://premium.pff.com/nfl/positions/2024/REGPO/passing?position=QB",
        "https://premium.pff.com/nfl/positions/2024/REGPO/passing-depth?position=WR,TE,RB",
        "https://premium.pff.com/nfl/positions/2024/REGPO/passing-pressure?position=QB",
        
        "https://premium.pff.com/nfl/positions/2024/REGPO/receiving?position=ED,LB,CB,S",
        "https://premium.pff.com/nfl/positions/2024/REGPO/receiving-depth?position=WR,TE,RB",
        "https://premium.pff.com/nfl/positions/2024/REGPO/receiving-concept?position=WR,TE,RB",
        "https://premium.pff.com/nfl/positions/2024/REGPO/receiving-scheme?position=WR,TE,RB", 
        
        "https://premium.pff.com/nfl/positions/2024/REGPO/rushing?position=WR,TE,RB",
        
        "https://premium.pff.com/nfl/positions/2024/REGPO/offense-blocking?position=HB,FB", 
        "https://premium.pff.com/nfl/positions/2024/REGPO/offense-pass-blocking?position=T,G,C,TE,RB",
        "https://premium.pff.com/nfl/positions/2024/REGPO/offense-run-blocking?position=T,G,C,TE,RB", 
        "https://premium.pff.com/nfl/positions/2024/REGPO/ol-pass-blocking-efficiency?position=T,G,C,TE,RB", 
        
        "https://premium.pff.com/nfl/positions/2024/REGPO/defense", 
        "https://premium.pff.com/nfl/positions/2024/REGPO/defense-pass-rush?position=DI,ED,LB,CB,S", 
        "https://premium.pff.com/nfl/positions/2024/REGPO/defense-coverage?position=DI,ED,LB,CB,S")


# Load the necessary library
library(RSelenium)

# Start RSelenium
rD <- rsDriver(browser = "chrome", port = 4445L, verbose = FALSE)
remDr <- rD$client

# Open the webpage
url <- "https://premium.pff.com/nfl/positions/2024/REGPO/passing?position=QB"
remDr$navigate(url)

# Pause to allow the page to load
Sys.sleep(5)

# Find and click the CSV button
# (Update 'css_selector' to match the CSV button's actual CSS selector)
csv_button <- remDr$findElement(using = "css selector", value = ".csv-download-link")  # Replace with the correct CSS selector for the CSV button
csv_button$click()

# Optional: Wait for download to complete
Sys.sleep(10)

# Close the session
remDr$close()
rD$server$stop()
