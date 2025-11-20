  #
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
dashboardPage(
  dashboardHeader(title = "Digoxin Clinical Trial Interactive Application", titleWidth = 450),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th")),
      menuItem("Upload Data", tabName = "Upload", icon = icon("upload")),
      
      fileInput(
        inputId = "dig.df", "Insert DIG data", accept = ".csv"
        
        
      )
    )
  ),
  dashboardBody(
    dataTableOutput("table1")
  )
)
    

