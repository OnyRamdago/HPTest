# Shiny Web App for Hypothesis Testing - Myttest
#
library(shiny)
library(HPTest)

# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("Two Sample t-test"),
  sidebarLayout(
    sidebarPanel(
      numericInput("n", "Length of the vectors:", value = 30),
      numericInput("meanx", "Mean of x:", value = 8),
      numericInput("meany", "Mean of y:", value = 8),
      numericInput("sdx", "Standard deviation of x:", value = 15),
      numericInput("sdy", "Standard deviation of y:", value = 15),
      numericInput("alpha", "Alpha:", value = 0.05),
      checkboxInput("paired", "Paired:", value = FALSE),
      actionButton("ttest", "Create vectors and perform t-test")

    ),
    mainPanel(
      plotOutput("boxplot"),
      verbatimTextOutput("ttest_result") # Add a panel to show t-test results
    )
  )
)

# Define server logic
server <- function(input, output) {

  # Create reactive values to store x and y vectors
  x <- reactiveVal(NULL)
  y <- reactiveVal(NULL)
  tt <- reactive(NULL)
  # Define function to create x and y vectors
  create_vectors <- function() {
    x_val <- rnorm(input$n, mean = input$meanx, sd = input$sdx)
    y_val <- rnorm(input$n, mean = input$meany, sd = input$sdy)
    x(x_val)
    y(y_val)
  }

  # Perform t-test
  tt_test <- eventReactive(input$ttest, {
    create_vectors()
    tt_testing = myttest(x(), y(), alpha = input$alpha, paired = input$paired)
    output$boxplot <- renderPlot({plot(tt_testing)})
    output$ttest_result <- renderPrint({print(tt_testing)})
  })


  # Create vectors and perform t-test
  observeEvent(input$ttest, {
    tt_test()
  })

}

# Run the Shiny app
shinyApp(ui = ui, server = server)
