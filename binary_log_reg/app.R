#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#



#load libraries
library(here)
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(leaflet)
library(shinydashboard)
library(leaflet.extras)
library(plotly)
library(shinymaterial)
library(shinyWidgets)
library(janitor)
library(caret)
library(DT)
library(plantr)



# Simulated dataset
generate_data <- function(n = 200) {
  data.frame(
    characteristic1 = runif(n, 0, 10),
    characteristic2 = runif(n, 0, 10),
    characteristic3 = runif(n, 0, 10),
    characteristic4 = runif(n, 0, 10),
    characteristic5 = runif(n, 0, 10),
    fire_resilient = sample(c(0, 1), n, replace = TRUE)
  )
}

data <- generate_data()

ui <- fluidPage(
  titlePanel("Fire Resilience Prediction"),
  sidebarLayout(
    sidebarPanel(
      selectInput("predictors", "Select Predictors:", 
                  choices = names(data)[1:5], multiple = TRUE, selected = names(data)[1:3]),
      actionButton("run_model", "Run Model")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Exploratory Data Analysis",
                 plotOutput("histogram"),
                 plotOutput("boxplot")
        ),
        tabPanel("Model Training",
                 verbatimTextOutput("model_summary"),
                 DTOutput("model_table")
        ),
        tabPanel("Best Model Comparison",
                 verbatimTextOutput("best_model"))
      )
    )
  )
)

server <- function(input, output) {
  selected_data <- reactive({
    req(input$predictors)
    data[, c(input$predictors, "fire_resilient")]
  })
  
  output$histogram <- renderPlot({
    ggplot(data, aes_string(x = input$predictors[1])) +
      geom_histogram(binwidth = 1, fill = "blue", alpha = 0.7) +
      labs(title = "Histogram of Selected Characteristic")
  })
  
  output$boxplot <- renderPlot({
    ggplot(data, aes(x = as.factor(fire_resilient), y = data[[input$predictors[1]]])) +
      geom_boxplot(fill = "red", alpha = 0.7) +
      labs(title = "Boxplot of Selected Characteristic by Fire Resilience")
  })
  
  model_results <- eventReactive(input$run_model, {
    set.seed(123)
    train_control <- trainControl(method = "cv", number = 10)
    models <- list(
      logistic = train(fire_resilient ~ ., data = selected_data(), method = "glm", 
                       family = "binomial", trControl = train_control),
      rf = train(fire_resilient ~ ., data = selected_data(), method = "rf", 
                 trControl = train_control),
      svm = train(fire_resilient ~ ., data = selected_data(), method = "svmRadial", 
                  trControl = train_control)
    )
    models
  })
  
  output$model_summary <- renderPrint({
    req(model_results())
    summary(model_results()$logistic$finalModel)
  })
  
  output$model_table <- renderDT({
    req(model_results())
    res <- data.frame(
      Model = names(model_results()),
      Accuracy = sapply(model_results(), function(m) max(m$results$Accuracy))
    )
    datatable(res)
  })
  
  output$best_model <- renderPrint({
    req(model_results())
    best_model <- names(which.max(sapply(model_results(), function(m) max(m$results$Accuracy))))
    paste("Best Model: ", best_model)
  })
}

shinyApp(ui, server)
