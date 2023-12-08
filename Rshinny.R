# Load necessary libraries
library(shiny)
library(randomForest)
library(ggplot2)

# Define UI
ui <- fluidPage(
  theme = shinythemes::shinytheme("united"),
  titlePanel("Health Insurance Expense Prediction"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV file"),
      br(),
      checkboxGroupInput("variables", "Select variables to include in the model:",
                         choices = c("age", "sex", "bmi", "children", "smoker", "region"),
                         selected = c("age", "bmi", "children", "smoker", "region")),
      br(),
      actionButton("submit", "Run Model", class = "btn btn-primary btn-lg btn-block")
    ),
    mainPanel(
      h3("Model Results", align = "center"),
      hr(),
      fluidRow(
        column(width = 6, 
               wellPanel(
                 verbatimTextOutput("mse_output"),
                 style = "background-color: #f5f5f5; padding: 15px; border-radius: 10px; box-shadow: 0px 0px 10px #bbb;"
               )
        ),
        column(width = 6,
               wellPanel(
                 verbatimTextOutput("accuracy_output"),
                 style = "background-color: #f5f5f5; padding: 15px; border-radius: 10px; box-shadow: 0px 0px 10px #bbb;"
               )
        )
      ),
      div(
        plotOutput("prediction_plot", height = "400px"),
        class = "plot-wrapper"
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  observeEvent(input$submit, {
    req(data())
    selected_vars <- input$variables
    
    # Check if 'expenses' column exists in the dataset
    if (!("expenses" %in% colnames(data()))) {
      return(showNotification("Please upload a file with 'expenses' column.", type = "warning"))
    }
    
    formula <- as.formula(paste("expenses ~", paste(selected_vars, collapse = " + ")))
    model <- randomForest(formula, data = data())
    
    predictions <- predict(model, newdata = data())
    mse <- mean((predictions - data()$expenses)^2)
    accuracy <- 100 * (1 - mse / var(data()$expenses))
    
    output$mse_output <- renderPrint({
      paste("Mean Squared Error: ", round(mse, 2))
    })
    
    output$accuracy_output <- renderPrint({
      paste("Accuracy: ", round(accuracy, 2), "%")
    })
    
    output$prediction_plot <- renderPlot({
      ggplot(data = data(), aes_string(x = "predictions", y = "expenses")) +
        geom_point(color = "#4285F4", alpha = 0.7, size = 3) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#EA4335") +
        labs(x = "Predicted Expenses", y = "Actual Expenses", title = "Predicted vs Actual Expenses") +
        theme_minimal()
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
