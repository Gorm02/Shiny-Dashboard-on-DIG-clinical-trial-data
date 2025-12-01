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
library(bslib)
library(plotly)

server <- function(input, output, session) {
  
  #
  output$res <- renderText({
    paste("You've selected:", input$tabs)
  })
  
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
      labs(title = paste("Figure 1: Patient ", input$features, " in Each Treatment Group"),
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
      labs(title = paste("Baseline ", input$feature, " in Each Treatment Group"),
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
      labs(title = paste(input$features, " and Deaths in Each Treatment Group"),
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
      labs(title = paste(input$features, " and Hospitalisations in Each Treatment Group"),
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
      labs(title = paste("Figure 1: Patient Mortality in Each Treatment Group by ", input$features),
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
      labs(title = paste("Figure 1: Patient Hospitalisations in Each Treatment Group by ", input$features),
           x = "Treatment Group",
           y = input$features,
           fill = "Treatment Group") +
      theme_minimal()
  })
  
  # DEATH RISK PLOT
  output$basic_surv_plot <- renderPlot({
    ggsurvplot(Mort_TRTMT_Fit,
               pval = TRUE, conf.int = TRUE,
               conf.int.style = "step",
               xlab = "Time in Months",
               ylab = "Risk of Mortality",
               break.time.by = 6,
               risk.table = "abs_pct", 
               risk.table.col = "strata", 
               risk.table.y.text = FALSE,
               linetype = "strata",
               ncensor.plot = TRUE,  
               censor.shape="|",
               censor.size = 3,
               legend.labs = 
                 c("Placebo", "Treatment"),
               palette = c("cadetblue1", "firebrick4"),
               title = paste("Risk of Mortality Over Time for Patients With ", input$features),
               subtitle = "Within each Treatment Group",
               font.title = c(22, "bold", "black"),
               ggtheme = theme_classic() + 
                 theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
                 theme(plot.subtitle = element_text(hjust = 0.5, size = 16, face = "italic")),
               risk.table.height = 0.25,
               risk.table.fontsize = 3.0)
  })
  
  # Preparing for the interactive survival plot (reactive expression):
  survfit_reactive_model <- reactive({
    f <- as.formula(paste("Surv(Month, DEATH) ~ TRTMT +", input$features))
     return(survfit(f, data = survfit.df))
  })
  
  output$survPlot_main <- renderPlotly({
    p <- ggsurvplot(
      survfit_reactive_model(),
      pval = TRUE,
      conf.int = TRUE,
      conf.int.style = "step",
      xlab = "Time in Months",
      ylab = "Risk of Mortality",
      break.time.by = 6,
      risk.table = "abs_pct",
      risk.table.col = "strata",
      risk.table.text = FALSE,
      linetype = "strata",
      ncensor.plot = TRUE,
      censor.shape = "|",
      censor.size = 3,
      legend.labs = c(
        "Placebo + No CVD", 
        "Placebo + CVD",
        "Treatment + No CVD", 
        "Treatment + CVD"
      ),
      palette = "Awtools",
      title = "Figure 17: Risk of Mortality Over Time",
      subtitle = "Within each Treatment Group",
      font.title = c(22, "bold", "black"),
      ggtheme = theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 16, face = "italic")
        ),
      risk.table.height = 0.25,
      risk.table.fontsize = 2.0
    )
    ggplotly(p$plot)
  })  
  
  # plot showing number of patients per group
  output$trtmt_plot <- renderPlot({
    ggplot(q1, aes(x = Treatment_Group, y = Number_of_Patients, fill = Treatment_Group)) + 
      geom_col(stat = "identity",
               color = "black") +
      scale_fill_manual(values = c("cadetblue1", "firebrick4") ) +
      theme(legend.position="none") +
      ggtitle("Number of Patients in Each Treatment Group") +
      xlab("Treatment Group") +
      ylab("Number of Patients") +
      geom_text(label = q1$Number_of_Patients) +
      # to make the columns touch the x-axis:
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_classic()
  })
  
  # par co plot showing predictors in recommended dig dose
  output$predict_dig_dose <- renderPlotly({
    plot_ly(
      type = "parcoords",
      line = list(color = ~ as.numeric(parco.df$SEX),
                  colorscale = list(c(0,'blue'),c(1,"magenta"))),
      dimensions = list(
        list(tickvals = c(1,2), ticktext = c("Male", "Female"),
             label = "Sex", values = parco.df$SEX),
        list(range = c(min(parco.df$AGE), max(parco.df$AGE)),
             label = "Age", values = parco.df$AGE),
        list(range = c(min(parco.df$BMI), max(parco.df$BMI)),
             label = "BMI", values = parco.df$BMI),
        list(range = c(min(parco.df$CREAT), max(parco.df$CREAT)),
             label = "Serum Creatinine (mg/dL)", values = parco.df$CREAT),
        list(range = c(min(parco.df$DIGDOSER), max(parco.df$DIGDOSER)),
             label = "Recommended Digoxin Dose", values = parco.df$DIGDOSER),
        list(range = c(min(parco.df$DIGDOSE), max(parco.df$DIGDOSE)),
             label = "Adjusted Digoxin Dose", values = parco.df$DIGDOSE)
      )
    ) %>%
      layout(title = "Predicting the Digoxin Dose of the Treatment Group")
  })
}


 