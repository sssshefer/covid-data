# ================================
# R Script Template For Lectures
# Lecture number and name
# 
# https://github.com/rstudio/cheatsheets
# ================================

# 1. Setup
# Install necessary packages
# install.packages(c("dplyr", "tidyr", "lubridate", "ggplot2", "shiny", "bslib", "leaflet", "rnaturalearth", "sf", "rnaturalearthdata"))

# Load necessary packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(shiny)
library(bslib)
library(leaflet)
library(rnaturalearth)
library(sf)
library(rnaturalearthdata)

# Load data
tblBase <- read.csv("worldometer_coronavirus_daily_data.csv")

# Convert date column to Date type
tblBase$date <- as.Date(tblBase$date, format = "%Y-%m-%d")

# Load world boundaries data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge COVID-19 data with geographic boundaries by country name
world_data <- world %>%
  left_join(tblBase %>% filter(date == max(date, na.rm = TRUE)), by = c("name" = "country"))

# Define UI for app with time series plot and choropleth map
ui <- page_sidebar(
  # App title
  title = "Covid Data Visualization",
  
  # Sidebar panel for inputs
  sidebar = sidebar(
    # Display total new cases and new deaths with dynamic percentage change
    div(
      h4("New Positive Cases"),
      textOutput("total_new_cases"),
      textOutput("cases_change"),
      style = "font-size: 20px; padding: 10px; background-color: #f0f0f0; margin-bottom: 10px; border-radius: 5px;"
    ),
    
    div(
      h4("New Deaths"),
      textOutput("total_new_deaths"),
      textOutput("deaths_change"),
      style = "font-size: 20px; padding: 10px; background-color: #f0f0f0; margin-bottom: 10px; border-radius: 5px;"
    ),
    
    # Input: Select country for time series plot
    selectInput(
      inputId = "country",
      label = "Select country:",
      choices = unique(tblBase$country),
      selected = "Afghanistan"
    ),
    
    # Input: Select column for time series plot
    selectInput(
      inputId = "column",
      label = "Select column to plot:",
      choices = c("cumulative_total_cases", "daily_new_cases", "active_cases", "cumulative_total_deaths", "daily_new_deaths"),
      selected = "daily_new_cases"
    ),
    
    # Input: Select new or cumulative data for map
    selectInput(
      inputId = "map_data_type",
      label = "Select data type:",
      choices = c("New" = "new", "Cumulative" = "cumulative"),
      selected = "new"
    ),
    
    # Input: Select metric (positive cases or deaths) for map
    selectInput(
      inputId = "map_metric",
      label = "Select metric:",
      choices = c("Positive Cases" = "cases", "Deaths" = "deaths"),
      selected = "cases"
    )
  ),
  
  # Output: Time series plot
  plotOutput(outputId = "timeSeriesPlot"),
  
  # Output: Leaflet map
  leafletOutput(outputId = "covidMap", height = "600px")
)

# Define server logic for time series plot and choropleth map
server <- function(input, output) {
  
  # Calculate total new cases for the latest date and compare with the previous day
  output$total_new_cases <- renderText({
    latest_date <- max(tblBase$date, na.rm = TRUE)
    total_cases <- tblBase %>%
      filter(date == latest_date) %>%
      summarise(total_new_cases = sum(daily_new_cases, na.rm = TRUE)) %>%
      pull(total_new_cases)
    paste(total_cases)
  })
  
  output$cases_change <- renderText({
    latest_date <- max(tblBase$date, na.rm = TRUE)
    previous_date <- latest_date - 1
    latest_cases <- tblBase %>% filter(date == latest_date) %>% summarise(total = sum(daily_new_cases, na.rm = TRUE)) %>% pull(total)
    previous_cases <- tblBase %>% filter(date == previous_date) %>% summarise(total = sum(daily_new_cases, na.rm = TRUE)) %>% pull(total)
    if (!is.na(previous_cases) && previous_cases != 0) {
      change_percent <- ((latest_cases - previous_cases) / previous_cases) * 100
      paste0(round(change_percent, 1), "% vs previous day (", previous_cases, ")")
    } else {
      "No data for previous day"
    }
  })
  
  output$total_new_deaths <- renderText({
    latest_date <- max(tblBase$date, na.rm = TRUE)
    total_deaths <- tblBase %>%
      filter(date == latest_date) %>%
      summarise(total_new_deaths = sum(daily_new_deaths, na.rm = TRUE)) %>%
      pull(total_new_deaths)
    paste(total_deaths)
  })
  
  output$deaths_change <- renderText({
    latest_date <- max(tblBase$date, na.rm = TRUE)
    previous_date <- latest_date - 1
    latest_deaths <- tblBase %>% filter(date == latest_date) %>% summarise(total = sum(daily_new_deaths, na.rm = TRUE)) %>% pull(total)
    previous_deaths <- tblBase %>% filter(date == previous_date) %>% summarise(total = sum(daily_new_deaths, na.rm = TRUE)) %>% pull(total)
    if (!is.na(previous_deaths) && previous_deaths != 0) {
      change_percent <- ((latest_deaths - previous_deaths) / previous_deaths) * 100
      paste0(round(change_percent, 1), "% vs previous day (", previous_deaths, ")")
    } else {
      "No data for previous day"
    }
  })
  
  output$timeSeriesPlot <- renderPlot({
    country_data <- tblBase %>% filter(country == input$country)
    column_data <- as.numeric(country_data[[input$column]])
    valid_data <- country_data %>% mutate(selected_column = column_data) %>% filter(!is.na(selected_column) & is.finite(selected_column))
    if (nrow(valid_data) == 0) {
      plot.new()
      text(0.5, 0.5, "No valid numeric data available in the selected column. Choose a different column.", cex = 1.5, col = "red")
    } else {
      ggplot(valid_data, aes(x = date, y = selected_column)) +
        geom_line(color = "#007bc2") +
        labs(x = "Date", y = input$column, title = paste(input$column, "over time for", input$country)) +
        theme_minimal()
    }
  })
  
  output$covidMap <- renderLeaflet({
    latest_date <- max(tblBase$date, na.rm = TRUE)
    map_data <- tblBase %>% filter(date == latest_date)
    map_column <- if (input$map_metric == "cases") {
      if (input$map_data_type == "new") "daily_new_cases" else "cumulative_total_cases"
    } else {
      if (input$map_data_type == "new") "daily_new_deaths" else "cumulative_total_deaths"
    }
    pal <- colorNumeric("YlOrRd", domain = map_data[[map_column]], na.color = "transparent")
    leaflet(world_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(world_data[[map_column]]),
        weight = 0.5,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~paste(name, ": ", round(world_data[[map_column]], 0)),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal,
        values = world_data[[map_column]],
        opacity = 0.7,
        title = paste(input$map_data_type, input$map_metric),
        position = "bottomright"
      )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
