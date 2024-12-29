# Interactive COVID Map

This project provides an interactive COVID-19 global map that allows users to visualize the spread of COVID-19 cases and deaths worldwide. Users can explore the map with customizable metrics, including the option to view new or cumulative cases and deaths for each country or globally. The project is implemented using R, the Shiny package for interactive web applications, and the Leaflet library for rendering the map.

## Table of Contents
- [Theory Notes](#theory-notes)
  - [R Overview](#r-overview)
- [Features and Functionality](#features-and-functionality)
- [Implementation](#implementation)
- [Running the Project Locally](#running-the-project-locally)


## Theory Notes

### R Overview
R is a programming language and free software environment used for statistical computing and graphics. In this project, we utilize R's ability to handle large datasets, perform statistical analysis, and produce interactive web applications with Shiny. Key packages used in this project include:

- **Shiny**: Enables interactive web applications.
- **ggplot2** and **Plotly**: Used for data visualization.
- **Leaflet**: Allows for interactive maps.
- **dplyr** and **tidyr**: Help with data manipulation.
- **sf**: Provides support for spatial data.

### Additional Concepts
If necessary, further theoretical topics can be added to help users unfamiliar with R or the specifics of COVID-19 data handling.

## Features and Functionality
- **Interactive Map**: Users can view COVID-19 data on a global map, with the option to select the data type (new or cumulative) and metrics (positive cases or deaths).
- **Real-time Data Updates**: The data updates automatically, showing the most recent information available.
- **Customizable Views**: Dropdown menus allow users to select countries and different data types, adjusting the map and accompanying charts accordingly.
- **Data Visualization**: Visualizations include a choropleth map, bar plots for cases and deaths, and the ability to track trends over time.

## Implementation
The implementation of this project is dynamic and relies on reactive programming provided by the Shiny framework. The map and data visualizations are updated in real-time based on user inputs. This ensures that the map reflects the current state of the pandemic across the globe, as well as detailed metrics for specific countries.

The project uses a combination of R packages for data handling, map rendering, and interactive plotting:
- **Leaflet**: For rendering the interactive map with COVID-19 case and death data.
- **Plotly and ggplot2**: For interactive visualizations, such as bar charts and line plots.
- **dplyr and tidyr**: For cleaning and manipulating the COVID-19 dataset.

## Running the Project Locally
  1) Install R and RStudio
  2) Install Dependencies
  3) Install the required R packages by running the following command in your RStudio console:
```r
install.packages(c("dplyr", "tidyr", "lubridate", "ggplot2", "shiny", "bslib", "leaflet", "rnaturalearth", "sf", "rnaturalearthdata", "plotly"))
```
## Run the Application:
Clone the repo and open the project by clicking on the .R file.
> [!IMPORTANT]
> Ensure that you open the file directly from the file explorer rather than from within RStudio. This ensures that the local file paths are set correctly for the dataset.
> If you want to avoid needing to open the file from the file explorer each time, you can modify the dataset path to an absolute path in the script.

By following these steps, you should be able to run the interactive COVID map locally on your machine.



