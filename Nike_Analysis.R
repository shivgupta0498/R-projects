#install.packages("shinyWidgets")
#install.packages("shinythemes")
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(tidyverse)
library(tidyquant)
library(DT)

nike_vs_competitors_tickers <- c("NKE","SKX","UAA","ADS.DE","PUM.DE")
nike_vs_etf_tickers <- c("NKE","XLY")
nike_vs_SPY_tickers <- c("NKE","SPY")

#Data
data1 <- tq_get(nike_vs_competitors_tickers, 
                get = "stock.prices",
                from = today() - months(24),
                to = today()) %>%
  select(symbol,date, close)


data2 <- tq_get(nike_vs_etf_tickers, 
                get = "stock.prices",
                from = today() - months(24),
                to = today()) %>%
  select(symbol,date, close)


data3 <- tq_get(nike_vs_SPY_tickers, 
                get = "stock.prices",
                from = today() - months(24),
                to = today()) %>%
  select(symbol,date, close)

data <- rbind(data1, data2, data3)

#Summary_Statistics
get_summary_stats <- function(data) {
  summary_stats <- summary(data)
  return(summary_stats)
}

#ggplot
ggplot_visual <- function(data) {
  ggplot(data, aes(x = date, y = close, color = symbol)) +
    geom_point() +
    labs(title = "Dataset", x = "date", y = "close")
}

#plotly
plotly_visual <- function(data) {
  plot_ly(data, x = ~date, y = ~close, color = ~factor(symbol), type = 'scatter', mode = 'markers')
  
}

# Additional Shiny Visualization #1
bar_chart_visual <- function(data) {
  ggplot(data, aes(x = date, y = close, fill = symbol)) +
    geom_bar(stat = "identity") +
    labs(title = "Nike vs Competitors (Bar Chart)", x = "date", y = "close")
}

# Additional Shiny Visualization #2
line_chart_visual <- function(data) {
  ggplot(data, aes(x = date, y = close, color = symbol)) +
    geom_line() +
    labs(title = "Nike vs Competitors (Line Chart)", x = "date", y = "close")
}

# Additional Shiny Visualization #3
density_visual <- function(data) {
  ggplot(data, aes(x = close, color = symbol)) +
    geom_density(alpha = 0.5) +
    labs(title = "Density Plot of Nike vs Competitors", x = "close", y = "density")
}

# UI
ui <- fluidPage(
  titlePanel("Nike Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Select dataset:",
                  choices = c("Nike vs Competitors" = "data1", "Nike vs etf" = "data2", "Nike vs SPY" = "data3"),
                  selected = "data1")
    ),
    mainPanel(
      h4("Summary Statistics"),
      dataTableOutput("summary_table"),
      h4("Static ggplot Visual"),
      plotOutput("ggplot_visual"),
      h4("Dynamic plotly Visual"),
      plotlyOutput("plotly_visual"),
      h4("Table Output"),
      DT::dataTableOutput("table_output"),
      h4("Bar Chart"),
      plotOutput("bar_chart"),
      h4("Line Chart"),
      plotOutput("line_chart"),
      h4("Density Plot"),
      plotOutput("density_plot")
    )
  )
)

# SERVER
server <- function(input, output) {
  # Summary Statistics
  selected_data <- reactive({get(input$dataset)})
  output$summary_table <- renderDataTable(get_summary_stats(selected_data()))
  
  # Static ggplot visual
  output$ggplot_visual <- renderPlot({
    ggplot_visual(selected_data())
  })
  
  # Dynamic plotly visual
  output$plotly_visual <- renderPlotly({
    plotly_visual(selected_data())
  })
  
  # Table output
  output$table_output <- DT::renderDataTable({
    DT::datatable(selected_data())
  })
  
  # Line chart output
  output$line_chart <- renderPlot({
    data <- selected_data()
    ggplot(data, aes(x = date, y = close, color = symbol)) +
      geom_line() +
      labs(title = "Line Chart", x = "date", y = "close")
  })
  
  # Bar chart output
  output$bar_chart <- renderPlot({
    data <- selected_data()
    ggplot(data, aes(x = symbol, y = close)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = "Bar Chart", x = "symbol", y = "close")
  })
  
  #Density Plot Output
  output$density_plot <- renderPlot({
    data <- get(input$dataset)
    ggplot(data, aes(x = close, fill = symbol)) +
      geom_density(alpha = 0.5) +
      ggtitle("Density Plot") +
      xlab("Close Price") +
      ylab("Density")
  })
}

# Run the app
shinyApp(ui = ui, server = server)
