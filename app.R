# ================================
# R Script Template For Lectures
# Lecture number and name
# 
# https://github.com/rstudio/cheatsheets
# ================================

# 1. Setup
# Install necessary packages if needed
# install.packages(c("dplyr", "tidyr", "lubridate", "ggplot2", "shiny", "bslib", "leaflet", "rnaturalearth", "sf", "rnaturalearthdata", "plotly"))
# install.packages('rsconnect')
library(rsconnect)

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
library(rsconnect)
rsconnect::setAccountInfo(name='shefer', token='67EC11FA6C1EF1C314741BBC6B1F4321', secret='obHvBpK31cFJQAYRukGQc+tSVamOAyMgBVOC4/S2')
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

colnames(world_data)

unique_countries <- unique(tblBase$country)

ui <- fluidPage(
  titlePanel(
    div(
      style = "background-color: #2E3B4E; padding: 15px; margin-top: -20px; margin-bottom: -10px; color: white; font-size: 24px; font-weight: bold; display: flex; justify-content: space-between; align-items: center; width: 100vw; margin-left: calc(-50vw + 50%);",
      
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
             style = "display: flex; justify-content: center; gap:20px; padding-top: 10px; padding-bottom: 10px; margin-bottom: 10px; background-color: #f1f2f3; margin-left: -15px; margin-right: -15px;",
             
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
    column(
      3,  
      div(
        h2(textOutput("positive_cases_title"), style = "font-size: 20px; font-weight: bold; margin-bottom: 5px;"),
        h4(textOutput("total_new_cases"), style = "font-size: 14px; margin-bottom: 5px;"),
        textOutput("cases_change"),
        style = "padding: 10px; background-color: #f0f0f0; margin-bottom: 10px; border-radius: 5px;"
      ),
      
      div(
        style = "margin-bottom: 10px;",
        plotlyOutput(outputId = "new_cases_plot", height = "200px")
      ),      
      div(
        h2(textOutput("deaths_title"), style = "font-size: 20px; font-weight: bold; margin-bottom: 5px;"),
        h4(textOutput("total_new_deaths"), style = "font-size: 14px; margin-bottom: 5px;"),
        textOutput("deaths_change"),
        style = "padding: 10px; background-color: #f0f0f0; margin-bottom: 10px; border-radius: 5px;"
      ),
      
      plotlyOutput(outputId = "deaths_graph", height = "200px")  # Smaller plot height
    ),
    
    # Center section: map
    column(6,
           div(
             style = "display: flex; flex-direction: column;",
             uiOutput("mapTitle"),
             span("Select a Country to see more details", style = "font-size: 20px; font-weight: normal; color: #B0B0B0;")
           ),
           leafletOutput(outputId = "covidMap", height = "600px", width = "100%")
    ),
    
    # Right section: bar plot
    column(
      3,
      div(
        style = "overflow-y: scroll; width: calc(100% - 10px); height: 600px; text-align: left; padding: 0; margin: 0; margin-top:16px;",  # Align content to the left and shrink spacing
        plotlyOutput(outputId = "barPlot", height = "4000px", width = "100%")
      )
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
    
    # Initialize total_cases as NA
    total_cases <- NA
    
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
    
    # Skip if total_cases is NA
    if (is.na(total_cases)) {
      return("No valid data available.")
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
  # (NEW OR CUMULATIVE) (SPECIFIC COUNTRY OR GLOBAL)
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
        geom_bar(stat = "identity", fill = "#48caf4") +
        coord_flip() +  # Horizontal bar plot
        labs(
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
        geom_bar(stat = "identity", fill = "#48caf4") +
        coord_flip() +  # Horizontal bar plot
        labs(
          title = "Cumulative Positive Cases",
          x = "Cumulative Cases",
          y = "Date"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          axis.text.y = element_text(size = 8)
        ) +
        scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M"))
    }
    
    # Adjust y-axis to show only every third month
    breaks <- seq(1, nrow(data_sorted), by = 3)  # Every 3rd date
    labels <- format(data_sorted$date[breaks], "%b %Y")  # Format dates as "Month Year"
    
    p <- p + scale_y_discrete(
      breaks = data_sorted$date[breaks],  # Every 3rd date
      labels = labels  # Display formatted month-year labels
    ) + scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M"))
    
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
  
  # HORIZONTAL PLOT DEATHS
  # (NEW OR CUMULATIVE) (SPECIFIC COUNTRY OR GLOBAL)
  output$deaths_graph <- renderPlotly({
    data <- dataset()
    
    # Get the selected data type: "new" or "cumulative"
    type <- input$map_data_type  # "new" or "cumulative"
    
    # Prepare and sort the data for visualization
    data_sorted <- data %>%
      group_by(date) %>%
      summarise(total_deaths = if (type == "new") {
        sum(daily_new_deaths, na.rm = TRUE)
      } else {
        cumsum(sum(daily_new_deaths, na.rm = TRUE))
      }) %>%
      arrange(date)
    
    # Ensure the date is in Date format
    data_sorted$date <- as.Date(data_sorted$date)
    
    # Create the plot based on the data type
    p <- ggplot(data_sorted, aes(x = total_deaths, y = reorder(date, total_deaths))) +
      geom_bar(stat = "identity", fill = if (type == "new") "#48caf4" else "#48caf4") +  # Red shades for deaths
      coord_flip() +  # Horizontal bar plot
      labs(
        x = if (type == "new") "Daily Deaths" else "Cumulative Deaths",
        y = "Date"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.text.y = element_text(size = 8)  # Smaller text for better readability
      )
    
    # Adjust y-axis to display only significant dates
    p <- p + scale_y_discrete(
      breaks = data_sorted$date[seq(1, nrow(data_sorted), by = 3)],  # Show every 3rd date
      labels = format(data_sorted$date[seq(1, nrow(data_sorted), by = 3)], "%b %Y")  # Format as "Month Year"
    ) + scale_x_continuous(labels = scales::label_number(scale = 0.001, suffix = "K"))
    
    # Convert ggplot to Plotly for interactivity
    ggplotly(p)
  })
  
  
  # ------------------------------------------------------------
  # Render the interactive bar plot
  output$barPlot <- renderPlotly({
    # Get the latest date in the dataset
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
    
    # Filter and prepare data, selecting top 20 countries by the chosen metric
    plot_data <- plot_data %>%
      select(
        country, !!sym(plot_column), 
        daily_new_cases, cumulative_total_cases, 
        daily_new_deaths, cumulative_total_deaths
      ) %>%
      filter(!is.na(!!sym(plot_column))) %>%
      arrange(desc(!!sym(plot_column))) %>%
      slice(1:194)
    
    # Add hover text with the required details
    plot_data <- plot_data %>%
      mutate(hover_text = paste0(
        "<b>", country, "</b><br>",
        "Cumulative Positive Cases: ", scales::comma(cumulative_total_cases), "<br>",
        "New Positive Cases: ", scales::comma(daily_new_cases), "<br>",
        "Cumulative Deaths: ", scales::comma(cumulative_total_deaths), "<br>",
        "New Deaths: ", scales::comma(daily_new_deaths)
      ))
    
    # Create the bar plot with ggplot2
    p <- ggplot(plot_data, aes(
      x = reorder(country, !!sym(plot_column)), 
      y = !!sym(plot_column), 
      text = hover_text
    )) +
      geom_bar(stat = "identity", fill = "#48caf4") +  # Set bar color
      coord_flip() +  # Flip coordinates for horizontal bars
      labs(
        x = NULL, y = NULL,
        title = if (input$map_metric == "cases") {
          if (input$map_data_type == "new") "New Positive Cases" else "Cumulative Positive Cases"
        } else {
          if (input$map_data_type == "new") "New Deaths" else "Cumulative Deaths"
        }
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 1),  # Title aligned left
        axis.text.y = element_text(size = 10),  # Adjust font size
        axis.text.x = element_text(size = 8),  # Format numbers
        panel.grid = element_blank()           # Remove gridlines
      ) +
      scale_y_continuous(labels = scales::comma)  # Format y-axis with commas
    
    
    # Convert ggplot to Plotly for interactivity
    ggplotly(p, tooltip = "text") %>%
      layout(
        yaxis = list(title = NULL, tickfont = list(size = 10)),
        xaxis = list(title = NULL),
        margin = list(l = 40)  # Reduce left margin to bring the bars closer
      ) %>%
      config(displayModeBar = FALSE) %>%
      layout(showlegend = FALSE)
  })
  
  # ------------------------------------------------------------
  # MAP
  output$mapTitle <- renderUI({
    # Dynamically generate the title
    map_title <- if (input$map_metric == "cases") {
      if (input$map_data_type == "new") "New Positive Cases" else "Cumulative Positive Cases"
    } else {
      if (input$map_data_type == "new") "New Deaths" else "Cumulative Deaths"
    }
    
    # Return the title wrapped in HTML for styling
    tags$h3(map_title, style = "text-align: center; font-weight: bold; color: #333;")
  })
  
  # MAP
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
    
    # Define the bounds to constrain the map to a global view
    world_bounds <- list(
      lng1 = -180, lat1 = -90,
      lng2 = 180, lat2 = 90
    )
    
    # Create the bubble map
    leaflet(world_data, options = leafletOptions(
      dragging = FALSE,
      zoomControl = TRUE,
      minZoom = 2,
      maxZoom = 10,
      worldCopyJump = FALSE
    )) %>%
      addTiles() %>%
      fitBounds(world_bounds$lng1, world_bounds$lat1, world_bounds$lng2, world_bounds$lat2) %>%
      addPolygons(
        fillColor = "transparent",
        weight = 0.5,
        color = "white",
        fillOpacity = 0
      ) %>%
      addCircleMarkers(
        lng = st_coordinates(centroids)[, 1],
        lat = st_coordinates(centroids)[, 2],
        fillColor = "#48caf4",
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        radius = ~sqrt(world_data[[map_column]]) / sqrt(max(world_data[[map_column]], na.rm = TRUE)) * 50, # Adjust radius scaling
        label = ~paste(name, ": ", round(world_data[[map_column]], 0)),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      )  %>%
      addLegend(
        colors = "#48caf4",
        labels = paste(input$map_data_type, input$map_metric),
        opacity = 0.7,
        title = NULL,
        position = "bottomright"
      )
  })
}


# Run the application
shinyApp(ui = ui, server = server)
