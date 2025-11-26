  #
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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

ui <- dashboardPage(
  dashboardHeader(title = "Digoxin Clinical Trial Interactive Application", titleWidth = 450),
  
  dashboardSidebar(
    sidebarMenu(
      # use menuItem to make tabs for different components of the study we want to discuss:
      menuItem("Study Overview", tabName = "study_overview", icon = icon("tree")),
      menuItem("Substudies", icon = icon("th"), tabName = "substudies", badgeLabel = "new",
               badgeColor = "green"),
      menuItem("Key Takeaways", icon = icon("book"), tabName = "takeaway", badgeLabel = "Important",
               badgeColor = "red")
    )
  ),
  
  dashboardBody(
    # input the bodies for the different tabs (above, in menuItem):
    tabItems(
      tabItem("study_overview",
              box(plotOutput("Baseline_Values_plot")),
              box(selectInput("features", "Features:",
                              c("AGE", "BMI")))
      ),
      # Here are the labels for the Dosage plot we talked about. Add more inputs if necessary.
      tabItem("study_overview",
              box(plotOutput("Dosage_Plot")),
              box(selectInput("outcome", "Outcome:",
                              c("WHF", "HOSP")))
              ),
      
      tabItem("substudies",
              fluidPage(
                h1("3 Patient Substudies"),
                h2("Three substudies were conducted. The quality of life/6-minute walk test substudy determined the effect of treatment on a patient's well-being, daily activities, and functional status. The Holter/signal averaging electrocardiogram substudy examined the pathophysiology of sudden cardiac death. The neurohormonal substudy determined whether long-term administration of digoxin attenuated the neuroendocrine response in patients with heart failure.")
              ))
    )
  )
)

