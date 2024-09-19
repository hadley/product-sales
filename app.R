library(shiny)
library(ggplot2)
library(bslib)

# Generate random sales data
set.seed(123)
sales_data <- data.frame(
  Date = rep(seq(as.Date("2023-01-01"), by = "month", length.out = 12), 3),
  Product = rep(c("Product A", "Product B", "Product C"), each = 12),
  Sales = sample(1000:5000, 36, replace = TRUE)
)

# Define UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),

  titlePanel("Sales Data Visualization"),

  sidebarLayout(
    sidebarPanel(
      selectInput("product", "Choose Product:",
                  choices = c("All Products", unique(sales_data$Product)),
                  selected = "All Products"),
      dateRangeInput("date_range", "Select Date Range:",
                     start = min(sales_data$Date),
                     end = max(sales_data$Date))
    ),

    mainPanel(
      plotOutput("sales_plot")
    )
  )
)

# Define server
server <- function(input, output) {
  filtered_data <- reactive({
    if (input$product == "All Products") {
      data <- sales_data
    } else {
      data <- sales_data[sales_data$Product == input$product, ]
    }
    data[data$Date >= input$date_range[1] &
           data$Date <= input$date_range[2], ]
  })

  output$sales_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Date, y = Sales, color = Product)) +
      geom_line() +
      geom_point() +
      labs(title = if(input$product == "All Products") "Sales Data for All Products" else paste("Sales Data for", input$product),
           x = "Date", y = "Sales") +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
