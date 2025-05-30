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
library(ggplot2)
library(bslib)
library(sonicscrewdriver)
library(plotrix)

load("sensors.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NHM Urban Research Station Soil Temperature Demo"),

    navset_tab(
      nav_panel(
        "Time Series",
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
            checkboxGroupInput(
              "sensors",
              "Sensors",
              choices = unique(data$sensor_id),
              selected = c("28-00000f9d74ea")
            )

          ),

          # Show a plot of the generated distribution
          mainPanel(
            navset_tab(
              nav_panel(
                "Waveform",
                plotOutput("timeSeries"),
                htmlOutput("timeSeriesNotes"),
                selectInput(
                  "DailyExtremes",
                  "Show daily extremes",
                  choices = c("None",unique(data$sensor_id))
                )
              ),
              nav_panel(
                "Temporal Coverage",
                htmlOutput("deltaThead"),
                plotOutput("deltaT")
              ),
              nav_panel(
                "Degree-Days",
                htmlOutput("AccD")
              )
            )
          )
        )
      ),
      nav_panel(
        "Diel Plot",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              "sensors",
              "Sensors",
              choices = unique(data$sensor_id),
              selected = c("28-00000f9d74ea")
            )
          ),
          mainPanel(
            plotOutput("dielPlot"),
            selectInput(
              "DailyExtremes",
              "Show daily extremes",
              choices = c("None",unique(data$sensor_id))
            )
          )
        )
      ),
      nav_panel(
        "Summary Stats",
        mainPanel(
          htmlOutput("summaryStats")
        )
      )
    ),
    id = "tab"
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$timeSeries <- renderPlot({
      start <- as.POSIXct(input$dates[1])
      end <- as.POSIXct(input$dates[2])+86400
      plot_data <- data[data$timestamp >= start & data$timestamp <= end, ]
      plot_data <- plot_data[plot_data$sensor_id %in% input$sensors, ]

      plot <- ggplot(data=plot_data, aes(x=timestamp, y=value, colour=sensor_id)) + geom_point(size=0.05) + guides(colour = guide_legend(override.aes = list(size=5)))

      if (input$DailyExtremes != "None") {
        start <- as.POSIXct(input$dates[1])
        end <- as.POSIXct(input$dates[2])+86400
        plot_data <- data[data$timestamp >= start & data$timestamp <= end, ]
        dw <- plot_data[plot_data$sensor_id == input$DailyExtremes, ]
        dw <- daily_delete_missing(dw, "timestamp", 60*60)

        de <- daily_extremes(dw$value, dw$timestamp, breaks=T)

        plot <- plot +
          geom_vline(xintercept=de$date[de$type=="max"], colour="red")  +
          geom_vline(xintercept=de$date[de$type=="min"], colour="blue")

      }

      print(plot)
    })
    output$timeSeriesNotes <- renderUI({
      if (input$DailyExtremes != "None") {
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

      title <- tags$h3("Accumulated Degree-Days")

      tags_list <- lapply(unique(plot_data$sensor_id), function(item) {
        tags$p(
          paste(#
            item,
            ": ",
            round(
              AccDD(
                plot_data$timestamp[plot_data$sensor_id == item],
                plot_data$value[plot_data$sensor_id == item]
              ),
              2
            ),
            "°C days"
          )
        )
      })
      tags_list <- c(list(title), tags_list)

      do.call(tagList, tags_list)



    })
    output$deltaThead <- renderUI({
        tags$div(
          tags$h3("Delta T"),
          tags$p("Boxplot of the time difference between consecutive readings for the selected sensors.")
        )
    })
    output$deltaT <- renderPlot({
        start <- as.POSIXct(input$dates[1])
        end <- as.POSIXct(input$dates[2])+86400
        plot_data <- data[data$timestamp >= start & data$timestamp <= end, ]
        plot_data <- plot_data[plot_data$sensor_id %in% input$sensors, ]
        boxplot(diff(as.numeric(plot_data$timestamp)), horizontal=T)
    })
    output$summaryStats <- renderUI({
      tags$div(
        tags$h3("Summary Counts"),
        tags$table(
          tags$tr(
            tags$td("Total Sensors:"),
            tags$td(length(unique(data$sensor_id)))
          ),
          tags$tr(
            tags$td("Total Readings:"),
            tags$td(nrow(data))
          )
        )
      )
    })
    output$dielPlot <- renderPlot({
      start <- as.POSIXct(input$dates[1])
      end <- as.POSIXct(input$dates[2])+86400
      plot_data <- data[data$timestamp >= start & data$timestamp <= end, ]
      plot_data <- plot_data[plot_data$sensor_id %in% input$sensors, ]

      emptyDiel()

      for (i in unique(plot_data$sensor_id)) {
        angles <- dielFraction(plot_data[plot_data$sensor_id==i, "timestamp"])
        values <- plot_data[plot_data$sensor_id==i, "value"]
        values <- plot_data$value - min(plot_data$value, na.rm = TRUE)
        values <- 2*values/max(values, na.rm = TRUE)
        lines(x=cos(angles)*values, y=sin(angles)*values)
      }


      if (input$DailyExtremes != "None") {
        start <- as.POSIXct(input$dates[1])
        end <- as.POSIXct(input$dates[2])+86400
        plot_data <- data[data$timestamp >= start & data$timestamp <= end, ]
        dw <- plot_data[plot_data$sensor_id == input$DailyExtremes, ]
        dw <- daily_delete_missing(dw, "timestamp", 60*60)

        de <- daily_extremes(dw$value, dw$timestamp, breaks=T)

        draw.radial.line(0,2, angle=dielFraction(de$date[de$type=="max"]), col="red")
        draw.radial.line(0,2, angle=dielFraction(de$date[de$type=="min"]), col="blue")

      }
    })
}

# Run the application
shinyApp(ui = ui, server = server)
