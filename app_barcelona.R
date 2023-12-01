
library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(DT)
library(tseries)
library(forecast)
library(gganimate)

library(scales)
library(lme4)
library(gee)
library(rjson)
library(maps)
library(dplyr)
library(leaflet)
library(ggplot2)
library(corrplot)
library(caret)
library(stargazer)
library(shinycssloaders)
library(shinythemes)
library(datadigest)
library(rio)

# load the dataset
df <- read.csv("Reduced_Data_Demographic.csv") %>% select(-X)
da <- df %>% select(Death_Number:Unemployment_Number_Female_Unemployment.demand)
barcelona <- 
  rjson::fromJSON(file="districtes.geojson")
namedListOfFeatures <- function (dataset) {
  namedList         <- as.list(colnames(dataset))
  names (namedList) <- colnames(dataset)
  return(namedList)
}

ui <- dashboardPage(
  dashboardHeader(title = "Data Visualization App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload Data", tabName = "tabUpload"),
      menuItem("Data Selection", tabName = "tab1"),
      menuItem("DataVisualization", tabName = "tab2"), 
      menuItem("Regression Analysis", tabName = "tab3"),
      menuItem("Monte Carlo Simulation", tabName = "tab4")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "tabUpload",
        fluidPage(
          sidebarLayout(
            sidebarPanel(
              fileInput("file1", "Choose CSV File", accept = c(".csv")),
              fileInput("file2", "Choose GeoJSON File", accept = c(".geojson"))
            ), 
            mainPanel()
          )
        )
      ),
      tabItem(
        tabName = "tab1",
        fluidPage(
          sidebarLayout(
            
            # Sidebar panel for input controls
            sidebarPanel(
              selectInput("age", "Age", choices =c(distinct(df, Age)), multiple = TRUE), 
              selectInput("Var_Name", "Column", choices = namedListOfFeatures(da)), 
              actionButton("downloadData", "Download"), 
              actionButton("update", "Update Dataset")
            ), 
            mainPanel(
              # Output table for selected dataset
              DT::dataTableOutput("dataset_table")
            )
          )
        )
      ), 
      tabItem(
        tabName = "tab2", 
        fluidPage(
          sidebarLayout(
            
            # Sidebar panel for input controls
            sidebarPanel(
              sliderInput("year", "Year:",
                          min = 2015, max = 2017,
                          value = 2015, step = 1),
            ), 
            mainPanel(
              # Output table for selected dataset
               plotlyOutput("time_dynamic_plot"),
            )
          )
        )
      ), 
      tabItem(
        tabName = "tab3", 
        fluidPage(
          box(
            selectInput(
              "SelectX",
              label = "Select variables:",
              choices = names(df),
              multiple = TRUE,
              selected = NULL,
            ),
            solidHeader = TRUE,
            width = "3",
            status = "primary",
            title = "X variable"
          ),
          box(
            selectInput("SelectY", label = "Select variable to predict:", choices = names(df)),
            solidHeader = TRUE,
            width = "3",
            status = "primary",
            title = "Y variable"
          ), 
          box(
            sliderInput( 
            "Slider1",
            label = h3("Train/Test Split %"),
            min = 0,
            max = 100,
            value = 75
          ),
          textOutput("cntTrain"),
          textOutput("cntTest"),
          br()
        )
        ), 
        fluidPage(
          tabBox(
            id = "tabset1",
            height = "1000px",
            width = 12,
            
            tabPanel("Data",
                     box(withSpinner(DTOutput(
                       "Data"
                     )), width = 12)),
            tabPanel(
              "Data Summary",
              box(withSpinner(verbatimTextOutput("Summ")), width = 6),
              box(withSpinner(verbatimTextOutput("Summ_old")), width = 6)
            ),
            
            # 
            # tabPanel("Data Strucure",
            #          # box(
            #          #   withSpinner(verbatimTextOutput("structure")), width = "100%"
            #          # ),
            #          explorerOutput("digest")
            #          ),
            tabPanel("Plots",
                     box(withSpinner(plotOutput(
                       "Corr"
                     )), width = 12)),
            #box(withSpinner(verbatimTextOutput("CorrMatrix")), width = 12),
            tabPanel(
              "Model",
              box(
                withSpinner(verbatimTextOutput("Model")),
                width = 6,
                title = "Model Summary"
              ),
              # box(
              #   withSpinner(verbatimTextOutput("Model_new")),
              #   width = 6,
              #   title = "Model Summary"
              # ),
              # 
              box(
                withSpinner(verbatimTextOutput("ImpVar")),
                width = 5,
                title = "Variable Importance"
              )
            ),
            #textOutput("correlation_accuracy"),
            tabPanel(
              "Prediction",
              box(withSpinner(plotOutput("Prediction")), width = 6, title = "Best Fit Line"),
              box(withSpinner(plotOutput("residualPlots")), width = 6, title = "Diagnostic Plots")
            )
          )
        )
      ), 
      tabItem(
        tabName = "tab4", 
        fluidPage(
          sidebarLayout(
            
            # Sidebar panel for input controls
            sidebarPanel(
              
            ), 
            mainPanel(
              
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session){
  # User selected age
  data <- reactive({
    df
  })
  
  #select variable
  filtereddata <- eventReactive(input$update, {
    data()[, colnames(data()) %in% c("Year", "District.Name", "Neighborhood.Name", "Age", input$Var_Name)] %>% 
      filter(Age %in% input$age)
  })
  
  eventReactive(input$downloadData, {
    csv_down <- filtereddata()
    write_csv(csv_down, "Barcelona_write_csv.csv")
  })
  
  output$dataset_table <-  DT::renderDataTable({
    filtereddata()
  })
  
  #filter plotly data and plot
  output$time_dynamic_plot <- renderPlotly({
    data_Barcelona <- filtereddata()

    data_Barcelona <- data_Barcelona %>% filter(Year == input$year) %>%
      group_by(District.Name) %>%
      summarize(Sum_Col = sum(!!sym(input$Var_Name)))
    
    

    g <- list(
      fitbounds = "locations",
      visible = FALSE
    )

    fig <- plot_ly()


    fig <- fig %>% add_trace(
      type="choropleth",
      geojson=barcelona,
      locations=data_Barcelona$District.Name,
      z=data_Barcelona$Sum_Col,
      colorscale="Viridis",
      featureidkey="properties.NOM"
    )
    fig <- fig %>% layout(
      geo = g
    )
    fig <- fig %>% colorbar(title = toString(input$Var_Name))
    fig <- fig %>% layout(
      title = toString(input$Var_Name)
    )
  })
  
  # ———————————— Linear Regression Part 
  
  InputDataset <- reactive({
    df
  })
  
  InputDataset_model <- reactive({
    if (is.null(input$SelectX)) {
      dt <- df
    }
    else{
      dt <- df[, c(input$SelectX, input$SelectY)]
    
    }
  })
  
  
  observe({
    lstname <- names(InputDataset())
    updateSelectInput(session = session,
                      inputId = "SelectY",
                      choices = lstname)
  })
  
  splitSlider <- reactive({
    input$Slider1 / 100
  })
  output$Summ <-
    renderPrint(
      stargazer(
        InputDataset(),
        type = "text",
        title = "Descriptive statistics",
        digits = 1,
        out = "table1.txt"
      )
    )
  output$Summ_old <- renderPrint(summary(InputDataset()))
  output$structure <- renderPrint(str(InputDataset()))
  
  set.seed(100)  # setting seed to reproduce results of random sampling
  trainingRowIndex <-
    reactive({
      sample(1:nrow(InputDataset_model()),
             splitSlider() * nrow(InputDataset_model()))
    })# row indices for training data
  
  trainingData <- reactive({
    tmptraindt <- InputDataset_model()
    tmptraindt[trainingRowIndex(), ]
  })
  
  testData <- reactive({
    tmptestdt <- InputDataset_model()
    tmptestdt[-trainingRowIndex(),]
  })
  
  
  
  output$cntTrain <-
    renderText(paste("Train Data:", NROW(trainingData()), "records"))
  output$cntTest <-
    renderText(paste("Test Data:", NROW(testData()), "records"))
  
  output$Data <- renderDT(InputDataset())
  
  
  cormat <- reactive({
    round(cor(InputDataset_model()), 1)
  })
  output$Corr <-
    renderPlot(corrplot(
      cormat(),
      type = "lower",
      order = "hclust",
      method = "number"
    ))
  
  
  #Code section for Linear Regression-----------------------------------------------------------------------------
  
  f <- reactive({
    as.formula(paste(input$SelectY, "~."))
  })
  
  
  Linear_Model <- reactive({
    lm(f(), data = trainingData())
  })
  
  output$Model <- renderPrint(summary(Linear_Model()))
  output$Model_new <-
    renderPrint(
      stargazer(
        Linear_Model(),
        type = "text",
        title = "Model Results",
        digits = 1,
        out = "table1.txt"
      )
    )
  
  Importance <- reactive({
    varImp(Linear_Model(), scale = FALSE)
  })
  
  tmpImp <- reactive({
    
    imp <- as.data.frame(varImp(Linear_Model()))
    imp <- data.frame(overall = imp$Overall,
                      names   = rownames(imp))
    imp[order(imp$overall, decreasing = T),]
    
  })
  
  output$ImpVar <- renderPrint(tmpImp())
  
  price_predict <- reactive({
    predict(Linear_Model(), testData())
  })
  
  tmp <- reactive({
    tmp1 <- testData()
    tmp1[, c(input$SelectY)]
  })
  
  
  actuals_preds <-
    reactive({
      data.frame(cbind(actuals = tmp(), predicted = price_predict()))
    })
  
  Fit <-
    reactive({
      (
        plot(
          actuals_preds()$actuals,
          actuals_preds()$predicted,
          pch = 16,
          cex = 1.3,
          col = "blue",
          main = "Best Fit Line",
          xlab = "Actual",
          ylab = "Predicted"
        )
      )
    })
  
  output$Prediction <- renderPlot(Fit())
  
  output$residualPlots <- renderPlot({
    par(mfrow = c(2, 2)) # Change the panel layout to 2 x 2
    plot(Linear_Model())
    par(mfrow = c(1, 1)) # Change back to 1 x 1
    
  })
  
  output$digest <- renderExplorer({
    
    explorer(data = dd$data, demo = F)
    
  }) 
  
}


# Run the APP 
shinyApp(ui = ui, server = server)

