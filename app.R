# ================================
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

# Clearing the data to fit shiny library map naming
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

unique_countries <- unique(tblBase$country)


ui <- fluidPage(
  titlePanel(
    div(
      style = "background-color: #2E3B4E; padding: 15px; color: white; font-size: 24px; font-weight: bold; display: flex; justify-content: space-between; align-items: center; width: 100vw; margin-left: calc(-50vw + 50%);",
      
      # Left section with title and date
      div(
        style = "display: flex; flex-direction: column;",
        "Global COVID-19 Tracker",
        span("January 21, 2020 - April 29, 2022", style = "font-size: 14px; font-weight: normal; color: #B0B0B0;")
      )
    )
  ),
  
  # Dropdowns aligned horizontally above the main content
  fluidRow(
    column(12, align = "center",
           div(
             style = "display: flex; justify-content: center; gap: 20px; margin-top: 15px; margin-bottom: 15px;",
             selectInput(
               inputId = "map_data_type",
               label = "New or Cumulative",
               choices = c("New" = "new", "Cumulative" = "cumulative"),
               selected = "new"
             ),
             
             selectInput(
               inputId = "map_metric",
               label = "Positive Cases or Deaths",
               choices = c("Positive Cases" = "cases", "Deaths" = "deaths"),
               selected = "cases"
             ),
             
             selectInput(
               inputId = "map_data_country",
               label = "Country",
               choices = c("Global" = "Global", unique_countries),
               selected = "Global"
             )
           )
    )
  ),
  
  # Main content divided into three sections
  fluidRow(
    # Left section: summary
    column(3,
           div(
             h4(textOutput("positive_cases_title")),
             textOutput("total_new_cases"),
             textOutput("cases_change"),
             style = "font-size: 20px; padding: 10px; background-color: #f0f0f0; margin-bottom: 10px; border-radius: 5px;"
           ),
           
           plotlyOutput(outputId = "new_cases_plot"),
           
           div(
             h4(textOutput("deaths_title")),
             textOutput("total_new_deaths"),
             textOutput("deaths_change"),
             style = "font-size: 20px; padding: 10px; background-color: #f0f0f0; margin-bottom: 10px; border-radius: 5px;"
           )
    ),
    
    # Center section: map
    column(5,
           leafletOutput(outputId = "covidMap", height = "600px", width = "600px")
    ),
    
    # Right section: bar plot
    column(4,
           plotlyOutput(outputId = "barPlot")
    )
  )
)

# Define server logic for bar plot and choropleth map
server <- function(input, output) {
  # Reactive dataset based on selected country
  dataset <- reactive({
    if (is.null(input$map_data_country) || input$map_data_country == "Global") {
      tblBase
    } else {
      tblBase %>% filter(country == input$map_data_country)
    }
  })
  
  # ------------------------------------------------------------
  # POSITIVE CASES TITLE
  # (NEW OR CUMULATIVE)
  output$positive_cases_title <- renderText({
    type <- input$map_data_type  # "new" or "cumulative"
    
    if (type == "new") {
      return("New Positive Cases")
    } else if (type == "cumulative") {
      return("Cumulative Positive Cases")
    }
  })
  
  # POSITIVE CASES VALUE
  # (NEW OR CUMULATIVE) (SPECIFIC COUNTRY OR GLOBAL)
  output$total_new_cases <- renderText({
    # Get the dataset based on the selected country
    data <- dataset()
    
    # Find the latest date in the filtered dataset
    latest_date <- max(data$date, na.rm = TRUE)
    
    # Determine if "new" or "cumulative" data should be shown
    type <- input$map_data_type  # "new" or "cumulative"
    
    if (type == "new") {
      # Summarize total new cases for the latest date
      total_cases <- data %>%
        filter(date == latest_date) %>%
        summarise(total_new_cases = sum(daily_new_cases, na.rm = TRUE)) %>%
        pull(total_new_cases)
    } else if (type == "cumulative") {
      # Calculate cumulative total cases (sum of all cases up to the latest date)
      total_cases <- data %>%
        filter(date <= latest_date) %>%
        summarise(total_cumulative_cases = sum(daily_new_cases, na.rm = TRUE)) %>%
        pull(total_cumulative_cases)
    }
    
    # Display total cases, handling the case if there are no new cases
    if (is.na(total_cases)) {
      return("No data available.")
    } else {
      return(paste(total_cases))
    }
  })
  
  # PERCENTAGE CHANGE IN POSITIVE CASES 
  # (NEW OR CUMULATIVE) (SPECIFIC COUNTRY OR GLOBAL)
  output$cases_change <- renderText({
    data <- dataset()
    latest_date <- max(data$date, na.rm = TRUE)
    previous_date <- latest_date - 1
    type <- input$map_data_type  # "new" or "cumulative"
    
    # Determine the total cases based on the type (new or cumulative)
    if (type == "new") {
      latest_cases <- data %>%
        filter(date == latest_date) %>%
        summarise(total = sum(daily_new_cases, na.rm = TRUE)) %>%
        pull(total)
      
      previous_cases <- data %>%
        filter(date == previous_date) %>%
        summarise(total = sum(daily_new_cases, na.rm = TRUE)) %>%
        pull(total)
      
    } else if (type == "cumulative") {
      latest_cases <- data %>%
        filter(date <= latest_date) %>%
        summarise(total = sum(daily_new_cases, na.rm = TRUE)) %>%
        pull(total)
      
      previous_cases <- data %>%
        filter(date <= previous_date) %>%
        summarise(total = sum(daily_new_cases, na.rm = TRUE)) %>%
        pull(total)
    }
    
    # Calculate percentage change if previous data exists
    if (!is.na(previous_cases) && previous_cases != 0) {
      change_percent <- ((latest_cases - previous_cases) / previous_cases) * 100
      paste0(round(change_percent, 1), "% vs previous day (", previous_cases, ")")
    } else {
      "No data for previous day"
    }
  })
  
  # HORIZONTAL PLOT POSITIVE 
  # (NEW OR COMULATIVE) (SPECIFIC COUNTRY OR GLOBAL)
  # HORIZONTAL PLOT POSITIVE (NEW OR CUMULATIVE) (SPECIFIC COUNTRY OR GLOBAL)
  output$new_cases_plot <- renderPlotly({
    data <- dataset()
    
    # Get the map_data_type (either "new" or "cumulative")
    type <- input$map_data_type  # "new" or "cumulative"
    
    # Ensure the data is sorted by date for correct timeline visualization
    data_sorted <- data %>%
      group_by(date) %>%
      summarise(total_new_cases = sum(daily_new_cases, na.rm = TRUE)) %>%
      arrange(date)
    
    # Convert 'date' to Date class for better handling
    data_sorted$date <- as.Date(data_sorted$date)
    
    if (type == "new") {
      # For "new", we show the daily new cases
      p <- ggplot(data_sorted, aes(x = total_new_cases, y = reorder(date, total_new_cases))) +
        geom_bar(stat = "identity", fill = "skyblue") +
        coord_flip() +  # Horizontal bar plot
        labs(
          title = "New Cases Over Time",
          x = "New Cases",
          y = "Date"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          axis.text.y = element_text(size = 8)
        )
    } else if (type == "cumulative") {
      # For "cumulative", we calculate cumulative cases over time
      data_sorted <- data_sorted %>%
        mutate(cumulative_cases = cumsum(total_new_cases))
      
      p <- ggplot(data_sorted, aes(x = cumulative_cases, y = reorder(date, cumulative_cases))) +
        geom_bar(stat = "identity", fill = "lightcoral") +
        coord_flip() +  # Horizontal bar plot
        labs(
          title = "Cumulative Cases Over Time",
          x = "Cumulative Cases",
          y = "Date"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          axis.text.y = element_text(size = 8)
        )
    }
    
    # Adjust y-axis to show only every third month
    p <- p + scale_y_discrete(
      breaks = data_sorted$date[seq(1, nrow(data_sorted), by = 3)],  # Every 3rd date
      labels = format(data_sorted$date[seq(1, nrow(data_sorted), by = 3)], "%b %Y")  # Format dates as "Month Year"
    )
    
    # Convert ggplot to plotly for interactivity and hover details
    ggplotly(p)
  })
  

  # ------------------------------------------------------------
  # DEATHS TITLE
  # (NEW OR CUMULATIVE)
  output$deaths_title <- renderText({
    type <- input$map_data_type  # "new" or "cumulative"
    
    if (type == "new") {
      return("New Deaths")
    } else if (type == "cumulative") {
      return("Cumulative Deaths")
    }
  })
  
  # DISPLAY DEATHS VALUE 
  # (NEW OR CUMULATIVE)
  output$total_new_deaths <- renderText({
    data <- dataset()
    type <- input$map_data_type  # "new" or "cumulative"
    
    latest_date <- max(data$date, na.rm = TRUE)
    
    if (type == "new") {
      # Calculate total new deaths for the latest date
      total_deaths <- data %>%
        filter(date == latest_date) %>%
        summarise(total_new_deaths = sum(daily_new_deaths, na.rm = TRUE)) %>%
        pull(total_new_deaths)
    } else if (type == "cumulative") {
      # Calculate cumulative deaths up to the latest date
      total_deaths <- data %>%
        filter(date <= latest_date) %>%
        summarise(total_cumulative_deaths = sum(daily_new_deaths, na.rm = TRUE)) %>%
        pull(total_cumulative_deaths)
    }
    
    # Return total deaths
    paste(total_deaths)
  })
  
  # PERCENTAGE CHANGE IN DEATHS 
  # (NEW OR CUMULATIVE)
  output$deaths_change <- renderText({
    data <- dataset()
    type <- input$map_data_type  # "new" or "cumulative"
    
    latest_date <- max(data$date, na.rm = TRUE)
    previous_date <- latest_date - 1
    
    if (type == "new") {
      # Get total new deaths for the latest and previous date
      latest_deaths <- data %>%
        filter(date == latest_date) %>%
        summarise(total = sum(daily_new_deaths, na.rm = TRUE)) %>%
        pull(total)
      
      previous_deaths <- data %>%
        filter(date == previous_date) %>%
        summarise(total = sum(daily_new_deaths, na.rm = TRUE)) %>%
        pull(total)
    } else if (type == "cumulative") {
      # Get cumulative deaths up to the latest and previous date
      latest_deaths <- data %>%
        filter(date <= latest_date) %>%
        summarise(total = sum(daily_new_deaths, na.rm = TRUE)) %>%
        pull(total)
      
      previous_deaths <- data %>%
        filter(date <= previous_date) %>%
        summarise(total = sum(daily_new_deaths, na.rm = TRUE)) %>%
        pull(total)
    }
    
    # Calculate and display percentage change
    if (!is.na(previous_deaths) && previous_deaths != 0) {
      change_percent <- ((latest_deaths - previous_deaths) / previous_deaths) * 100
      paste0(round(change_percent, 1), "% vs previous day (", previous_deaths, ")")
    } else {
      "No data for previous day"
    }
  })
  
  # ------------------------------------------------------------
  # Render the interactive bar plot
  output$barPlot <- renderPlotly({
    latest_date <- max(tblBase$date, na.rm = TRUE)
    plot_data <- tblBase %>% filter(date == latest_date)
    
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
        if (input$map_data_type == "new") "New Positive Cases" else "Cumulative total cases"
      } else {
        if (input$map_data_type == "new") "Daily new deaths" else "Cumulative total deaths"
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
  
  # ------------------------------------------------------------
  # Render the Leaflet map with bubbles in the center of each country
  output$covidMap <- renderLeaflet({
    # Get the latest date from the dataset
    latest_date <- max(tblBase$date, na.rm = TRUE)
    
    # Filter data for the latest date and join with geographic boundaries
    map_data <- world_data %>% 
      filter(date == latest_date)
    
    # Choose the column to display based on input
    map_column <- if (input$map_metric == "cases") {
      if (input$map_data_type == "new") "daily_new_cases" else "cumulative_total_cases"
    } else {
      if (input$map_data_type == "new") "daily_new_deaths" else "cumulative_total_deaths"
    }
    
    # Calculate centroids of countries for bubble placement
    centroids <- st_centroid(world_data$geometry)
    
    # Create the bubble map with bubbles centered on each country's centroid
    leaflet(world_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = "transparent",  # Make polygons transparent
        weight = 0.5,
        color = "white",
        fillOpacity = 0
      ) %>%
      addCircles(
        lng = st_coordinates(centroids)[, 1],  # Longitude of centroid
        lat = st_coordinates(centroids)[, 2],  # Latitude of centroid
        fillColor = "#48caf4",  # Fixed color for bubbles
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        radius = ~sqrt(world_data[[map_column]] / max(map_data[[map_column]], na.rm = TRUE)) * 1000000,  # Adjust bubble size
        label = ~paste(name, ": ", round(world_data[[map_column]], 0)),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        colors = "#48caf4",
        labels = paste(input$map_data_type, input$map_metric),
        opacity = 0.7,
        title = paste(input$map_data_type, input$map_metric),
        position = "bottomright"
      )
  })
  
}


# Run the application
shinyApp(ui = ui, server = server)
