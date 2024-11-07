# ================================
# R Script Template For Lectures
# Lecture number and name
# 
# https://github.com/rstudio/cheatsheets
# ================================

# 1. Setup
# Install necessary packages
# install.packages(c("dplyr", "tidyr", "lubridate", "ggplot2", "patchwork", "ggtext", "ragg", "magick", "readxl", "tidyverse", "hrbrthemes", "plotly", "babynames", "viridis", "gridExtra", "shiny", "bslib"))

# Load necessary packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(shiny)
library(bslib)

# Load data
tblBase <- read.csv("C:/Users/arkad/Desktop/XAMK/XAMK_3_YEAR/BigData/Covid-interactive-project/worldometer_coronavirus_daily_data.csv")

# Convert date column to Date type
tblBase$date <- as.Date(tblBase$date, format = "%Y-%m-%d")

# Define UI for app that draws a time series plot
ui <- page_sidebar(
  # App title
  title = "Covid Data Visualization",
  
  # Sidebar panel for inputs
  sidebar = sidebar(
    # Input: Select country
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
    )
  ),
  
  # Output: Time series plot
  plotOutput(outputId = "timeSeriesPlot")
)

# Define server logic required to draw a time series plot
server <- function(input, output) {
  
  # Time series plot of selected column data from tblBase for selected country
  output$timeSeriesPlot <- renderPlot({
    
    # Filter data for selected country
    country_data <- tblBase %>% filter(country == input$country)
    
    # Extract and convert the selected column to numeric, ignoring non-numeric values
    column_data <- as.numeric(country_data[[input$column]])
    
    # Filter out NA and infinite values after conversion to numeric
    valid_data <- country_data %>% 
      mutate(selected_column = column_data) %>% 
      filter(!is.na(selected_column) & is.finite(selected_column))
    
    # Check if there is valid data to plot
    if (nrow(valid_data) == 0) {
      plot.new()
      text(0.5, 0.5, "No valid numeric data available in the selected column. Choose a different column.", cex = 1.5, col = "red")
    } else {
      # Create time series plot
      ggplot(valid_data, aes(x = date, y = selected_column)) +
        geom_line(color = "#007bc2") +
        labs(x = "Date", y = input$column, title = paste(input$column, "over time for", input$country)) +
        theme_minimal()
    }
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
