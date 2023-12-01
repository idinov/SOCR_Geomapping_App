# SOCR_Geomapping_App

## Overview
This Shiny web application provides a user interface to explore, visualize, and analyze demographic data for the city of Barcelona. The app is divided into three main tabs: Data Selection, Data Visualization, and Regression Analysis. This application also allows the user to upload their datasets (including geojson file) to explore their own data. 

## Libraries Used
- `shiny`: Provides the framework for building the Shiny web application.
- `shinydashboard`: Creates a dashboard layout for the Shiny app.
- `plotly`: Enables interactive and dynamic plotting.
- `tidyverse`: A collection of packages for data manipulation and visualization.
- `DT`: Renders interactive data tables.
- `forecast`: Used for time series forecasting.
- `gganimate`: Adds animation to ggplot2 visualizations.
- `leaflet`: Creates interactive maps.
- `corrplot`: Generates correlation plots.
- `caret`: Implements machine learning tools.
- `stargazer`: Produces statistical tables.
- `shinycssloaders`: Provides loading animations.
- `shinythemes`: Adds additional themes for Shiny apps.
- `datadigest`: Creates a digest of data for exploration. Download the package through this: https://cran.r-project.org/src/contrib/Archive/datadigest/ 

## Data Loading
The application loads the dataset from the "Reduced_Data_Demographic.csv" file, excluding the first column (X). It also loads geographical data for Barcelona districts from "districtes.geojson."

```r
df <- read.csv("Reduced_Data_Demographic.csv") %>% select(-X)
barcelona <- rjson::fromJSON(file = "districtes.geojson")
```

## User Interface
### Data Selection Tab (tab1)
Users can select age groups and columns for analysis. Buttons for downloading the selected data and updating the dataset are provided.

### Data Visualization Tab (tab2)
Users can select a specific year using a slider. A dynamic plot using Plotly shows the chosen variable’s distribution across Barcelona districts for the selected year.

### Regression Analysis Tab (tab3)
Users can select variables for linear regression analysis. Options for choosing X and Y variables, setting a train/test split percentage, and viewing model summaries are available. Multiple panels for exploring data, summary statistics, correlation plots, and model results are included.

## Server Functions
### Data Selection Tab
Reactive function filtereddata filters the dataset based on user inputs. Download button (input$downloadData) exports the selected data to “Barcelona_write_csv.csv.”

### Data Visualization Tab
Reactive function output$time_dynamic_plot generates a choropleth map using Plotly based on the selected variable and year.

### Regression Analysis Tab
Reactive functions handle data splitting, model training, and prediction. Various outputs provide information on data summary, correlation plots, linear regression model details, variable importance, and prediction plots.

## Linear Regression
Linear regression is performed on the selected X and Y variables. Model results, including coefficients and statistics, are displayed. Variable importance is computed, and the top variables are presented.

## Prediction
The app includes a plot showing the best fit line between actual and predicted values. Residual plots are displayed for diagnostic purposes.

## Additional Features
The app includes loading spinners (withSpinner) to indicate when data is being processed. The datadigest package is used to create a data digest for exploration in the “Data Structure” tab (currently commented out in the code).