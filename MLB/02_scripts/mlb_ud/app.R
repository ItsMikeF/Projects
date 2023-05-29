#Lets build a shiny web for underdog mlb draft analysis

# LIBRARIES ----

# Shiny
library(shiny)
library(bslib)
library(gt)

# Define UI ----
ui <- fluidPage(
  titlePanel("MLB Underdog Draft Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "df_select", 
                  label = "Select a dataframe:",
                  choices = names(exp_list), 
                  selected = "686 4 10 3 Shohei Ohtani")
    ),
    mainPanel(
      gt_output(outputId = "gt_table")
    )
  )
)

# Define server ----
server <- function(input, output) {
  
  # Display the selected dataframe in a table
  output$gt_table <- render_gt({
    df <- exp_list[[input$df_select]] %>% 
      select(name, team_logo_espn, espn_headshot, Pick.Number, adp, value, rel_value, projectedPoints) %>% 
      gt() %>% 
      gt_img_rows(columns = team_logo_espn, height = 50) %>% 
      gt_img_rows(columns = espn_headshot, height = 50) %>% 
      gt_color_rows(rel_value, palette = c("red","green"), domain = c(-.5,.5)) %>% 
      gt_theme_dark()
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
