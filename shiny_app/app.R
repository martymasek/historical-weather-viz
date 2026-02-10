library(shiny)
library(bslib)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(httr2)
library(jsonlite)
library(purrr)
library(tidygeocoder) # to get lat and long from an address
library(shinycssloaders)

source("functions.R")

# Define UI for app that draws a histogram ----
ui <- page_sidebar(
  # App title ----
  title = "Historical Weather Metrics",
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    width = "350px",
    ## Card 1 - city, dates, submit ----
    card(
      textInput(
        "location_nm",
        label = NULL, # "Enter location",
        placeholder = "City, State and/or Country"
      ),
      dateRangeInput(
        inputId = "dates",
        label = NULL, # "Select dates",
        start = paste0(year(Sys.Date()) - 15, "-01-01"),
        end   = Sys.Date() - days(1),
        min   = "1940-01-01",
        max   = Sys.Date() - days(1)
      ),
      selectInput(
        "temp_unit",
        label = NULL , # "Select temperatue unit",
        choices = list("Fahrenheit", "Celsius"), 
        selected = "Fahrenheit"
      ),
      actionButton(
        inputId = "submit_city_dates", 
        label = "Submit"
      )
    ),
    ## Month of interest ----
    selectInput(
      "month",
      label = "Select month",
      choices = as.list(month.name)
    )
  ),
  # Output ----
  navset_tab(
    
    ## Temperature ----
    nav_panel(
      "Temperature",
      plotOutput("plot_temp", height = "600px") |> shinycssloaders::withSpinner()
    ),
    
    ## Sun duration ----
    nav_panel(
      "Sun Duration",
      plotOutput("plot_sun", height = "600px") |> shinycssloaders::withSpinner()
    ),
    
    ## Source info
    nav_panel(
      "Source",
      uiOutput("source_info")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # get the location info: coords, city name, country, etc.
  location_df <- eventReactive(
    input$submit_city_dates,
    {
      get_location(input$location_nm)
    }
  )
  
  # ensure valid coordinates were returned
  location_valid <- reactive({
    !is.na(location_df()$lat)
  })
  
  # ensure date range is at least 1 day
  date_range_valid <- eventReactive(
    input$submit_city_dates,
    {
      as.numeric(input$dates[2] - input$dates[1]) >= 0
    }
  )
  
  # get the weather data
  weather_df <- eventReactive(
    input$submit_city_dates,
    {
      
      # must have a valid location, otherwise halt execution
      req(location_valid())
      # must have at least 1 day in date range
      req(date_range_valid())
      
      get_weather_data(
        location_df = location_df(),
        start_dt    = input$dates[1],
        end_dt      = input$dates[2],
        temp_unit   = input$temp_unit
      )
    }
  )
  
  month_df <- reactive({
    
    filter_to_month(
      df       = weather_df(),
      my_month = input$month
    )
    
  })
  
  # ensure month is contained within date range
  month_valid <- reactive({
    nrow(month_df()) > 0
  })
  
  output$plot_temp <- renderPlot({
    
    # handle errors
    shiny::validate(
      need(location_valid(), "Enter a valid location.")
    )
    
    shiny::validate(
      need(date_range_valid(), "Ensure that the date range spans at least 1 day.")
    )
    
    shiny::validate(
      need(month_valid(), "Ensure that the selected month is within the selected date range.")
    )
    
    plot_temp(df = month_df())
    
  })
  
  output$plot_sun <- renderPlot({
    
    plot_sun(df = month_df())
    
  })
  
  output$source_info <- renderUI({

    tagList(
      tags$br(),
      "Shiny App code by Marty Masek with contributions by James Ahloy. See the ",
      tags$a(
        href = "https://github.com/martymasek/historical-weather-viz",
        "GitHub repo.",
        target = "_blank"
      ),
      tags$br(),
      "Historical weather data from ",
      tags$a(
        href = "https://open-meteo.com/en/docs/historical-weather-api",
        "Open-Meteo's Historical Weather API.",
        target = "_blank"
      ),
      tags$br(),
      "Geocoding by",
      tags$a(
        href = "https://jessecambon.github.io/tidygeocoder/",
        "tidygeocoder.",
        target = "_blank"
      )
    )

  })
  
}

shinyApp(ui = ui, server = server)
