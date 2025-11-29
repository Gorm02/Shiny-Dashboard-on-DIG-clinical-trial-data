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
  
  # Popup Model that will display critical information and support information
  
  observe({
    showModal(
      modalDialog(
        title = "Welcome to our interactive app exploring the DIG clinical trial data",
        easyClose = T,
        p("Thank you for showing interest in our interactive app exploring the DIG dataset. Digoxin is one of the oldest drugs used to treat heart failure
        and concerns about its safty and efficacy is put into question. This apps purpose is to visualize the reported findings to paint a clearer picture than
        static spreadsheets and graphs."),
        
        p(
        tags$strong("For support, contact:"),
        tags$br(),
        "Dylan House: D.House1@universityofgalway.ie",
        tags$br(),
        "Clodagh Gormley: C.Gormley8@universityofgalway.ie"
      )
     )
    )
   }
  )
  
  # CONTINUOUS BASELINE
  # Boxplot of continuous baseline characteristics
  
  output$Baseline_Values_plot <- renderPlot({
    ggplot(data = dig.df, aes(x = TRTMT, y = .data[[input$features]], fill = TRTMT)) +
      geom_boxplot() +
      scale_fill_manual(values=c("cadetblue1", "firebrick4")) +
      geom_jitter(alpha = 0.1) +
      labs(title = "Figure 1: Patient INSERT_VARIABLE_NAME_HERE in Each Treatment Group",
           x = "Treatment Group",
           y = input$features,
           fill = "Treatment Group") +
      theme_minimal()
  })
  
  # CATEGORICAL BASELINE
  # Barplot of categorical baseline characteristics
  output$categorical_baseline_plot <- renderPlot({
    ggplot(data = dig.df[!is.na(dig.df[[input$features]]), ], aes(x = .data[[input$features]], fill = TRTMT)) +
      geom_bar(position = "dodge",
               alpha = 0.75,
               colour = "black") +
      #      facet_wrap(~TRTMT) +
      scale_fill_manual(values = c("yellow", "purple") ) +
      theme_bw() + 
      labs(title = "Baseline INSERT_VARIABLE_NAME_HERE  in Each Treatment Group",
           fill = "Treatment Group",
           x = input$features,
           y = "Number of Patients") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
  })
  
  # CATEGORICAL DEATHS
  # Barplot of patient mortality for placebo/treatment and an effector (CVD, WHF etc)
  
  output$Mortality_Plot <- renderPlot({
    ggplot(data = dig.df[!is.na(dig.df[[input$features]]), ], aes(x = .data[[input$features]], fill = DEATH)) +
      geom_bar(position = "dodge",
               alpha = 0.75,
               colour = "black") +
      facet_wrap(~TRTMT) +
      scale_fill_manual(values = c("pink", "maroon") ) +
      theme_bw() + 
      labs(title = "INSERT_VARIABLE_NAME_HERE and Deaths in Each Treatment Group",
           fill = "Patient Mortality",
           x = input$features,
           y = "Number of Patients") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
  })
  
  # CATEGORICAL HOSPITALISATIONS
  # Barplot of patient hospitalisation for placebo/treatment and an effector (CVD, WHF etc)
  
  output$Hospitalisation_Plot <- renderPlot({
    ggplot(data = dig.df[!is.na(dig.df[[input$features]]), ], aes(x = .data[[input$features]], fill = HOSP)) +
      geom_bar(position = "dodge",
               alpha = 0.75,
               colour = "black") +
      facet_wrap(~TRTMT) +
      scale_fill_manual(values = c("lightgrey", "blue") ) +
      theme_bw() + 
      labs(title = "INSERT_VARIABLE_NAME_HERE and Hospitalisations in Each Treatment Group",
           fill = "Patient Hospitalisations",
           x = input$features,
           y = "Number of Patients") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
  })
  
  # CONTINUOUS DEATHS
  output$continuous_deaths_plot <- renderPlot({
    ggplot(data = dig.df, aes(x = DEATH, y = .data[[input$features]], fill = DEATH)) +
      geom_boxplot() +
      facet_wrap(~TRTMT) +
      scale_fill_manual(values=c("pink", "maroon") ) +
      geom_jitter(alpha = 0.1) +
      labs(title = "Figure 1: Patient Mortality in Each Treatment Group by INSERT_VARIABLE_NAME_HERE",
           x = "Treatment Group",
           y = input$features,
           fill = "Treatment Group") +
      theme_minimal()
  })
  # CONTINUOUS HOSPITALISATIONS
  output$continuous_hospitalisations_plot <- renderPlot({
    ggplot(data = dig.df, aes(x = HOSP, y = .data[[input$features]], fill = DEATH)) +
      geom_boxplot() +
      facet_wrap(~TRTMT) +
      scale_fill_manual(values=c("pink", "maroon") ) +
      geom_jitter(alpha = 0.1) +
      labs(title = "Figure 1: Patient Hospitalisations in Each Treatment Group by INSERT_VARIABLE_NAME_HERE",
           x = "Treatment Group",
           y = input$features,
           fill = "Treatment Group") +
      theme_minimal()
  })
  # DEATH RISK PLOT
}


 