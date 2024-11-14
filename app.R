# Code: # ================================
# R Script Template For Lectures
# Lecture number and name
# 
# https://github.com/rstudio/cheatsheets
# ================================

# 1. Setup
# Install necessary packages if needed
# install.packages(c("dplyr", "tidyr", "lubridate", "ggplot2", "shiny", "bslib", "leaflet", "rnaturalearth", "sf", "rnaturalearthdata", "plotly"))

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
library(plotly)  # New package for interactive bar plot

# Load data
tblBase <- read.csv("worldometer_coronavirus_daily_data.csv")
# Convert date column to Date type
tblBase$date <- as.Date(tblBase$date, format = "%Y-%m-%d")

tblBase <- tblBase %>%
  mutate(
    country = ifelse(country == "USA", "United States of America",
                     ifelse(country == "UK", "United Kingdom", country))
  )

# Load world boundaries data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge COVID-19 data with geographic boundaries by country name
world_data <- world %>%
  left_join(tblBase %>% filter(date == max(date, na.rm = TRUE)), by = c("name" = "country"))


View(world_data)
# Define UI for app with time series plot and choropleth map
ui <- page_sidebar(
  title = div(
    style = "background-color: #2E3B4E; padding: 15px; color: white; font-size: 24px; font-weight: bold; display: flex; justify-content: space-between; align-items: center; width: 100vw; margin-left: calc(-50vw + 50%);",
    
    # Left section with title and date
    div(
      style = "display: flex; flex-direction: column;",
      "Global COVID-19 Tracker",
      span("January 21, 2020 - April 29, 2022", style = "font-size: 14px; font-weight: normal; color: #B0B0B0;")
    ),
    
    # Right section with map image and label
    div(
      style = "display: flex; flex-direction: column; align-items: center;",
      img(src = "www/global_map_image.png", height = "30px", style = "margin-top: 5px;"),
      span("Global", style = "font-size: 12px; color = #B0B0B0;")
    )
  ),
  
  sidebar = sidebar(
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
    )
  ),
  
  # Dropdowns aligned horizontally above the map
  div(
    style = "display: flex; justify-content: center; gap: 20px; margin-top: 15px; margin-bottom: 15px;",
    selectInput(
      inputId = "map_data_type",
      label = "Select data type:",
      choices = c("New" = "new", "Cumulative" = "cumulative"),
      selected = "new",
      width = "200px"
    ),
    
    selectInput(
      inputId = "map_metric",
      label = "Select metric:",
      choices = c("Positive Cases" = "cases", "Deaths" = "deaths"),
      selected = "cases",
      width = "200px"
    )
  ),
  
  # Leaflet output for the map
  leafletOutput(outputId = "covidMap", height = "600px"),
  
  # Plotly output for the bar plot
  plotlyOutput(outputId = "barPlot")
)


# Define server logic for bar plot and choropleth map
server <- function(input, output) {
  
  # Display total new cases for the latest date
  output$total_new_cases <- renderText({
    latest_date <- max(tblBase$date, na.rm = TRUE)
    total_cases <- tblBase %>%
      filter(date == latest_date) %>%
      summarise(total_new_cases = sum(daily_new_cases, na.rm = TRUE)) %>%
      pull(total_new_cases)
    paste(total_cases)
  })
  
  # Display percentage change in new cases
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
  
  # Display total new deaths for the latest date
  output$total_new_deaths <- renderText({
    latest_date <- max(tblBase$date, na.rm = TRUE)
    total_deaths <- tblBase %>%
      filter(date == latest_date) %>%
      summarise(total_new_deaths = sum(daily_new_deaths, na.rm = TRUE)) %>%
      pull(total_new_deaths)
    paste(total_deaths)
  })
  
  # Display percentage change in new deaths
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
  
  # Render the interactive bar plot
  output$barPlot <- renderPlotly({
    latest_date <- max(tblBase$date, na.rm = TRUE)
    plot_data <- tblBase %>% filter(date == latest_date)
    
    # Check if we have any data for the plot
    if (nrow(plot_data) == 0) {
      return(NULL)  # If no data, return NULL to prevent plot rendering
    }
    
    # Choose the appropriate column based on input values
    plot_column <- if (input$map_metric == "cases") {
      if (input$map_data_type == "new") "daily_new_cases" else "cumulative_total_cases"
    } else {
      if (input$map_data_type == "new") "daily_new_deaths" else "cumulative_total_deaths"
    }
    
    # Check if the selected column exists in the data
    if (!(plot_column %in% colnames(plot_data))) {
      stop(paste("Error: column", plot_column, "does not exist in the dataset."))
    }
    
    # Filter out rows with NA values for the selected plot column
    plot_data <- plot_data %>%
      select(country, !!sym(plot_column), daily_new_cases, cumulative_total_cases, daily_new_deaths, cumulative_total_deaths) %>%
      filter(!is.na(!!sym(plot_column))) %>%
      arrange(desc(!!sym(plot_column))) %>%
      slice(1:20)  # Show only top 20 countries
    
    # Create the bar plot with ggplot2, positioning country names on the left
    p <- ggplot(plot_data, aes(x = reorder(country, !!sym(plot_column)), y = !!sym(plot_column))) +
      geom_bar(stat = "identity", fill = "#00a9e0") +  # Blue color for the bars
      geom_text(aes(label = country), hjust = 1.05, vjust = 0.5, size = 3.5, color = "black") +  # Align country names to the left
      coord_flip() +
      labs(x = NULL, y = NULL, title = if (input$map_metric == "cases") {
        if (input$map_data_type == "new") "New Positive Cases" else "Cumulative Positive Cases"
      } else {
        if (input$map_data_type == "new") "New Deaths" else "Cumulative Deaths"
      }) +  # Simplified axis labels
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.text.y = element_blank(),  # Remove y-axis text (countries) to avoid duplication
        axis.text.x = element_text(size = 8),  # Values size
        panel.grid.major = element_blank(),  # Remove grid lines
        panel.grid.minor = element_blank()
      ) +
      scale_y_continuous(labels = scales::comma)  # Add comma formatting for large numbers
    
    # Convert ggplot to plotly for interactivity and hover details
    ggplotly(p, tooltip = "text") %>%
      layout(
        yaxis = list(
          tickvals = plot_data$country,
          title = list(standoff = 20, font = list(size = 12))
        ),
        xaxis = list(title = NULL),
        margin = list(l = 150)  # Adjust left margin for country names
      ) %>%
      config(displayModeBar = FALSE) %>%
      layout(showlegend = FALSE) %>%
      # Add custom hover text
      style(
        hovertext = paste(
          "Country: ", plot_data$country, "<br>",
          "New Cases: ", plot_data$daily_new_cases, "<br>",
          "Cumulative Cases: ", plot_data$cumulative_total_cases, "<br>",
          "New Deaths: ", plot_data$daily_new_deaths, "<br>",
          "Cumulative Deaths: ", plot_data$cumulative_total_deaths
        )
      )
  })
  
  
  
  # Render the Leaflet map
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