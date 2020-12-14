#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)


citibike <- read.csv("citibike_cleaned.csv")
day_weather_data <- citibike %>%
    group_by(date) %>%
    summarize(mean(TAVG), 
              mean(tripduration), 
              count = n()) %>%
    rename(tavg = "mean(TAVG)",
           tripduration = "mean(tripduration)")


ui <- fluidPage(
    titlePanel("CitiBike Weather Dashboard"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("tempInput", "Temperature", 0, 90, c(45,60), pre = "F"),
            
            
        ),
        mainPanel(
            plotOutput("coolplot"),
            br(), br(),
            tableOutput("results")
        )
    )
)

server <- function(input, output) {
    output$coolplot <- renderPlot({
        filtered <-
            day_weather_data %>%
                filter(tavg >= input$tempInput[1],
                       tavg <= input$tempInput[2],
        )
            
        ggplot(filtered, aes(x = tavg, y = count)) +
            geom_point() + geom_smooth() + 
            labs(title = "Number of Rides Based Off Temperature") + xlab("Average Temperature (F)") + ylab("Number of Rides")
        
    })
}

shinyApp(ui = ui, server = server)