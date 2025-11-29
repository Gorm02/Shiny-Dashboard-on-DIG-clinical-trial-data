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
               badgeColor = "red"),
      menuItem("Baseline Characteristics", tabName = "cat_base_char", icon = icon("circle"))
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
      # mortality plot in the new tab key takeaways
      tabItem("takeaway",
              fluidPage(plotOutput("Mortality_Plot"),
                        plotOutput("Hospitalisation_Plot"),
                        selectInput("features", "Features:",
                              c("WHF", "CVD")))),
      
      tabItem("substudies",
              fluidPage(
                h2("Study Background"),
                h4("The digitalis Investigation Group (DIG) study investigated the capacity of the cardiac glycoside, digoxin, to treat systolic heart failure. Glycosides, such as digoxin work by increasing the amount of intracellular sodium retained, enabling the accumulation of intracellular calcium, resulting in stronger cardiac contractions.")
              )),
      
      tabItem("cat_base_char",
              box(plotOutput("categorical_baseline_plot")),
              box(selectInput("features", "Features:",
                                    c("SEX", "HYPERTEN"))))
    )
  )
)

