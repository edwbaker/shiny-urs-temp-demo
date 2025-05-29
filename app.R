#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(NHMDE)
library(lubridate)

load("sensors.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NHM Urban Research Station Soil Temperature Demo"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          dateRangeInput(
            "dates",
            "Date Range",
            start = "2025-05-15",
            end = "2025-05-28",
            min = NULL,
            max = NULL,
            format = "yyyy-mm-dd",
            startview = "month",
            weekstart = 0,
            language = "en",
            separator = " to ",
            width = NULL,
            autoclose = TRUE
          ),
          selectInput(
            "sensors",
            "Sensors",
            choices = unique(data$sensor_id),
            selected = c("28-00000f9d74ea")
          ),
          checkboxInput(
            "DeltaT",
            "Show Delta T",
            value = FALSE
          ),
          checkboxInput(
            "DailyExtremes",
            "Show daily extremes",
            value = FALSE
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("timeSeries"),
           htmlOutput("timeSeriesNotes"),
           htmlOutput("AccD"),
           htmlOutput("deltaThead"),
           plotOutput("deltaT")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$timeSeries <- renderPlot({
      start <- as.POSIXct(input$dates[1])
      end <- as.POSIXct(input$dates[2])+86400
      plot_data <- data[data$timestamp >= start & data$timestamp <= end, ]
      plot_data <- plot_data[plot_data$sensor_id %in% input$sensors, ]
      plot(
        plot_data$timestamp,
        plot_data$value,
        col=factor(plot_data$sensor_id),
        type="p",
        cex=0.3,
        xlab="Date",
        ylab="Temperature"
        )
      if (input$DailyExtremes) {
        dw <- daily_delete_missing(plot_data, "timestamp", 60*60)

        de <- daily_extremes(dw$value, dw$timestamp, breaks=T)

        abline(v=de$date[de$type=="max"], col="red", lwd=1)
        abline(v=de$date[de$type=="min"], col="blue", lwd=1)
      }
    })
    output$timeSeriesNotes <- renderUI({
      if (input$DailyExtremes) {
        p("Daily extremes are only shown for days with complete data.")
      }
    })
    output$AccD <- renderUI({
      start <- as.POSIXct(input$dates[1])
      second(start) <- 0
      minute(start) <- 0
      hour(start) <- 0
      end <- as.POSIXct(input$dates[2])
      second(end) <- 59
      minute(end) <- 59
      hour(end) <- 23
      plot_data <- data[data$timestamp >= start & data$timestamp <= end, ]
      plot_data <- plot_data[plot_data$sensor_id %in% input$sensors, ]
      tags$div(
        tags$h3("Accumulated Degree-Days"),
        tags$p(paste0("Total Accumulated Degree Days: ", round(AccDD(plot_data$timestamp, plot_data$value), 2)))
      )
    })
    output$deltaThead <- renderUI({
      if (input$DeltaT) {
        tags$div(
          tags$h3("Delta T"),
          tags$p("Boxplot of the time difference between consecutive readings for the selected sensors.")
        )
      }
    })
    output$deltaT <- renderPlot({
      if (input$DeltaT) {
        start <- as.POSIXct(input$dates[1])
        end <- as.POSIXct(input$dates[2])+86400
        plot_data <- data[data$timestamp >= start & data$timestamp <= end, ]
        plot_data <- plot_data[plot_data$sensor_id %in% input$sensors, ]
        boxplot(diff(as.numeric(plot_data$timestamp)), horizontal=T)
      }
    })
}

# Run the application
shinyApp(ui = ui, server = server)
