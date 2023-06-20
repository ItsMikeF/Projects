#lets analyze the nfl dfs slate with shiny

#load packages
suppressMessages({
  library(nflfastR) #nflfastr nflseedr nflplotr
  library(tidyverse) #ggplot2 dplyr tibble tidyr purrr forecats 
  library(ggrepel) #automatically position non-overlapping text labels
  library(glue) #interpreted literal strings
  
  library(shiny) #web application framework in r
  library(bslib) #custom bootstrap sass tehmes for shiny and markdown
  library(DataExplorer) #automate data exploration
  library(plotly) #create interactive web graphics via plotly.js
})

# 0.0 load datasets -------------------------------------------------------

data_list = list(
  "best" = best,
  "drafts" = drafts,
  "drafts" = drafts_by_date,
  "exposure_adp" = exposure_adp
)

# 1.0 user interface ------------------------------------------------------

ui <- navbarPage(
  title = "UD EDA",
  tabPanel(
    title = "Underdog Explatory Data Analysis", 
    sidebarLayout(
      sidebarPanel(
        width = 3, 
        h1("Explore a dataset"),
        selectInput(
          inputId = "dataset_choice", 
          label = "Data Connection", 
          choices = names(data_list)
        ),
        hr(),
        h3("My first web app!"),
        p("lets crush this slate")
      ),
      mainPanel(
        h1("Correlation"), 
        plotlyOutput("corrplot", height = 700)
      )
    )
  )
)

# 2.0 server --------------------------------------------------------------

server <- function(input, output) {
  
  rv <- reactiveValues()
  
  observe({
    rv$data_set <- data_list %>% pluck(input$dataset_choice)
  })
  
  output$corrplot <- renderPlotly({
    g <- DataExplorer::plot_correlation(rv$data_set)
    plotly::ggplotly(g)
  })
  
}

shinyApp(ui = ui, server = server)
