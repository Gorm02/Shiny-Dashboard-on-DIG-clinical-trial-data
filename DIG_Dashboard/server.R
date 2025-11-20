#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)

# Define server logic required to draw a histogram
function(input, output) {
  data <- reactive({
    req(input$dig.df)
    read.csv(input$dig.df$datapath)
  })
  
  output$table1 <- renderDataTable({
    data()
  })
}


 