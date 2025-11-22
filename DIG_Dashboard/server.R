#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

## app.R ##
library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(ggplot2)

server <- function(input, output, session) {
  output$Baseline_Values_plot <- renderPlot({
    ggplot(dig.df, aes(x = TRTMT, y = .data[[input$features]], fill = TRTMT)) +
      geom_boxplot() +
      scale_fill_manual(values=c("seagreen2", "cornflowerblue")) +
      geom_jitter(alpha = 0.2) +
      labs(title = "Figure 1: Patient Ages per Feature",
           x = "Treatment Group",
           y = "Feature",
           fill = "Treatment Group",
           shape = "Treatment Group")
  })
  
}


 