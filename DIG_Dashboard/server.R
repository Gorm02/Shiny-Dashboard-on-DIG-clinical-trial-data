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
  
  dig.df <- reactive({
    df <- input$dig.df
    
    df$TRTMT <- factor(df$TRTMT,
                       levels = c("Placebo", "Treatment"))
    
    df$SEX <- factor(df$SEX,
                     levels = c(1, 2),
                     labels = c("Male", "Female"))
    
    df$DEATH <- factor(df$DEATH,
                       levels = c("FALSE", "TRUE"),
                       labels = c("Alive", "Death"))
    
    df$HYPERTEN <- factor(df$HYPERTEN,
                          levels = c("FALSE", "TRUE"),
                          labels = c("No History of Hypertension",
                                     "History of Hypertension"))
    
    df$CVD <- factor(df$CVD,
                     levels = c("FALSE", "TRUE"),
                     labels = c("No Cardiovascular Disease",
                                "Cardiovascular Disease"))
    
    df$WHF <- factor(df$WHF,
                     levels = c("FALSE", "TRUE"),
                     labels = c("No Worsening Heart Failure",
                                "Worsening Heart Failure"))
    
    df$DIG <- factor(df$DIG,
                     levels = c("FALSE", "TRUE"),
                     labels = c("No Digoxin Toxicity",
                                "Digoxin Toxicity"))
    
    df$HOSP <- factor(df$HOSP,
                      levels = c("FALSE", "TRUE"),
                      labels = c("Not Hospitalized",
                                 "Hospitalized"))
    
    df
  })
  output$Baseline_Values_plot <- renderPlot({
    df <- as.data.frame(dig.df())
    ggplot(df, aes(x = TRTMT, y = .data[[input$features]], fill = TRTMT)) +
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


 