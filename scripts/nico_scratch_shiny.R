library(shiny)
library(tidymodels)
library(dplyr)
library(DT)

ui <- fluidPage(
  titlePanel("Fire Resistance Predictor (Modified Recipe 2)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("moisture_use", "Moisture Use:",
                  choices = c("Low", "Medium", "High")),
      selectInput("growth_period", "Growth Period:",
                  choices = c("Spring", "Summer", "Winter", "Year Round")),
      numericInput("height", "Height (in feet):", value = 10, min = 0, step = 0.1),
      numericInput("root_depth", "Root Depth (inches):", value = 5, min = 0, step = 0.5),
      actionButton("predict", "Predict Fire Resistance")
    ),
    mainPanel(
      h3("Prediction Output"),
      DT::dataTableOutput("prediction_table")
    )
  )
)

server <- function(input, output, session) {
  
  new_prediction <- eventReactive(input$predict, {
    # Create a new data frame from manual inputs.
    new_data <- tibble(
      moisture_use = factor(input$moisture_use, levels = c("Low", "Medium", "High")),
      growth_period = factor(input$growth_period, levels = c("Spring", "Summer", "Winter", "Year Round")),
      height = input$height,
      root_depth = input$root_depth
    )
    
    # Generate predictions using the pre-trained modified Recipe 2 workflow (log_fit2)
    pred_class <- predict(log_fit2, new_data = new_data)
    pred_prob  <- predict(log_fit2, new_data = new_data, type = "prob")
    
    list(class = pred_class$.pred_class, probability = pred_prob)
  })
  
  output$prediction_table <- DT::renderDataTable({
    req(new_prediction())
    result <- new_prediction()
    
    # Format the probabilities as percentages with no decimal points
    df <- tibble(
      "Predicted Fire Resistance" = result$class,
      "Probability (No)" = paste0(round(result$probability$.pred_No * 100), "%"),
      "Probability (Yes)" = paste0(round(result$probability$.pred_Yes * 100), "%")
    )
    
    DT::datatable(df, options = list(pageLength = 5, dom = 't'))
  })
}

shinyApp(ui, server)
