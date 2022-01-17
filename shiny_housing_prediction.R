library(shiny)
library(dplyr)
library(ggplot2)

options(scipen=999)

housing_predictions <- readRDS("train_modeling.rds")
results_all <- read.csv("results_all.csv")
results_all <- results_all[,-1]
variable_selection_cat <- colnames(housing_predictions[c(-1,-2),3:48])
variable_selection_contin <- colnames(housing_predictions[-1,49:67])

# Define UI
ui <- fluidPage(
  tabsetPanel(
    tabPanel("House Price Prediction",
             titlePanel("Regression Analysis"),
             textOutput("mlr_summary"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("continuous_vars", label = "Select Continuous", choices = variable_selection_contin),
                 selectInput("categorical_vars", label = "Select Categorical", choices = variable_selection_cat),
               tableOutput("results_all")
               ),
               
               mainPanel(
                 fluidRow(column(width = 6, plotOutput("continuous_plot")),
                          column(width = 6, plotOutput("continuous_hist"))),
                 plotOutput("catetgorical_box"),
                 imageOutput("model_comparison")
                 
               )
             )
    )
)
)
# Define server logic
server <- function(input, output) {

  output$continuous_plot <- renderPlot({
    housing_predictions <- housing_predictions %>% select(input$continuous_vars, SalePrice)
    plot(housing_predictions)
  })
  
  output$continuous_hist <- renderPlot({
    housing_predictions <- housing_predictions %>% select(input$continuous_vars)
    hist(housing_predictions[,1], main = input$continuous_vars, xlab = input$continuous_vars)
  })
  
  output$catetgorical_box <- renderPlot({
    housing_predictions <- housing_predictions %>% select(input$categorical_vars, SalePrice)
    ggplot(housing_predictions, aes(x = housing_predictions[,1], y = housing_predictions[,2])) + geom_boxplot() + xlab(input$categorical_vars) + ylab("SalePrice")
  })
  
  output$model_comparison <- renderImage(list(
    src = file.path("model_comparison.png"),
    contentType = "image/png"), deleteFile = FALSE)
  
  output$results_all <- renderTable(results_all)
}

# Run the application 
shinyApp(ui = ui, server = server)
