library(shiny)
library(forecast)
library(tseries)
library(readxl)
library(rugarch)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Time Series Forecasting - CAC II"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Excel File", accept = ".xlsx"),
      numericInput("forecast_periods", "Number of Forecast Periods", value = 10),
      actionButton("analyze", "Run Analysis")
    ),
    
    mainPanel(
      plotOutput("tsPlot"),
      verbatimTextOutput("stationarityTest"),
      plotOutput("decompPlot"),
      plotOutput("acfPlot"),
      plotOutput("pacfPlot"),
      verbatimTextOutput("modelOutput"),
      plotOutput("forecastPlot"),
      verbatimTextOutput("accuracyOutput"),
      verbatimTextOutput("garchOutput")
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$analyze, {
    
    req(input$file)
    
    df <- read_excel(input$file$datapath)
    ts_data <- ts(df[[1]], frequency = 12)
    
    output$tsPlot <- renderPlot({
      plot(ts_data, main = "Original Time Series", ylab = "Value", xlab = "Time")
    })
    
    output$stationarityTest <- renderPrint({
      adf.test(ts_data)
    })
    
    output$decompPlot <- renderPlot({
      decomp <- decompose(ts_data, type = "multiplicative")
      plot(decomp)
    })
    
    output$acfPlot <- renderPlot({
      acf(ts_data)
    })
    
    output$pacfPlot <- renderPlot({
      pacf(ts_data)
    })
    
    # Transformation & Differencing
    diff_ts <- diff(log(ts_data))
    model <- auto.arima(diff_ts)
    
    output$modelOutput <- renderPrint({
      model
    })
    
    forecast_result <- forecast(model, h = input$forecast_periods)
    
    output$forecastPlot <- renderPlot({
      autoplot(forecast_result)
    })
    
    output$accuracyOutput <- renderPrint({
      accuracy(forecast_result)
    })
    
    output$garchOutput <- renderPrint({
      log_returns <- diff(log(ts_data))
      spec <- ugarchspec(
        variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
        mean.model = list(armaOrder = c(1, 1), include.mean = TRUE)
      )
      fit <- tryCatch({
        ugarchfit(spec, data = log_returns)
      }, error = function(e) "GARCH Model failed")
      fit
    })
  })
}

shinyApp(ui = ui, server = server)
