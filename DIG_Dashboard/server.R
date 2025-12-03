
library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(ggplot2)
library(bslib)
library(plotly)
library(ggplot2)
library(survival)
library(survminer)


server <- function(input, output, session) {
  
  # Text notifying user which tab they have selected.
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
  

  
 
# Table Output
  output$table <- renderDataTable({
    dig.df
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
      theme(axis.line = element_line(colour = "black"), 
            axis.text = element_text(size = 12),
            axis.title = element_text(face = "bold", size = 14)) + 
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
      theme(axis.line = element_line(colour = "black"), 
            axis.text = element_text(size = 12),
            axis.title = element_text(face = "bold", size = 14)) + 
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
           fill = "Patient Mortality") +
      theme_minimal()
  })
  # CONTINUOUS HOSPITALISATIONS
  output$continuous_hospitalisations_plot <- renderPlot({
    ggplot(data = dig.df, aes(x = HOSP, y = .data[[input$features]], fill = HOSP)) +
      geom_boxplot() +
      facet_wrap(~TRTMT) +
      scale_fill_manual(values=c("lightgrey", "blue") ) +
      geom_jitter(alpha = 0.1) +
      labs(title = paste("Figure 1: Patient Hospitalisations in Each Treatment Group by ", input$features),
           x = "Treatment Group",
           y = input$features,
           fill = "Patient Hospitalisation") +
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
               title = paste("Risk of Mortality Over Time for Patients in Each Treatment Group"),
               subtitle = "Within each Treatment Group",
               font.title = c(22, "bold", "black"),
               ggtheme = theme_classic() + 
                 theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
                 theme(plot.subtitle = element_text(hjust = 0.5, size = 16, face = "italic")),
               risk.table.height = 0.25,
               risk.table.fontsize = 3.0)
  })
  
 
  # plot showing number of patients per group
  output$trtmt_plot <- renderPlot({
    ggplot(data = q1, aes(x = Treatment_Group, y = Number_of_Patients, fill = Treatment_Group)) + 
      geom_col(stat = "identity",
               color = "black") +
      scale_fill_manual(values = c("cadetblue1", "firebrick4") ) +
      theme(legend.position="none") +
      ggtitle("Number of Patients in Each Treatment Group") +
      xlab("Treatment Group") +
      ylab("Number of Patients") +
      #geom_text(label = Number_of_Patients, vjust = -0.5) +
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
  
  # basic plot showing patient deaths per treatment group
  output$trtmt_deaths <- renderPlot({
    ggplot(data = dig.df, aes(x = DEATH, fill = TRTMT)) +
      geom_bar(position = "dodge",
               color = "black") +
      scale_fill_manual(values = c("cadetblue1", "firebrick4") ) +
      theme_classic() + 
      labs(title = "Deaths per Treatment Group",
           x = "Deaths",
           y = "Number of Patients",
           fill = "Treatment Group") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
  })
  
  # basic plot showing patient hospitalisations per treatment group
  output$trtmt_hosps <- renderPlot({
    ggplot(data = dig.df, aes(x = HOSP, fill = TRTMT)) +
      geom_bar(position = "dodge",
               color = "black") +
      scale_fill_manual(values = c("cadetblue1", "firebrick4") ) +
      theme_classic() + 
      labs(title = "Hospitalizations and Treatment Groups",
           fill = "Treatment Group",
           x = "Hospitalized during the Trial",
           y = "Number of Patients") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
  })
  
  # Baseline characteristics plot in subtab "Baseline characteristics
  
  dig.df_complete1 <- reactive({
    
    req(input$age, input$sex, input$bmi, input$TRTMT)
    
    out <- dig.df_complete %>%
      filter(AGE >= input$age[1], AGE <= input$age[2]) %>%
      filter(SEX %in% input$sex) %>%
      filter(BMI >= input$bmi[1], BMI <= input$bmi[2]) %>%
      filter(TRTMT %in% input$TRTMT)
  })
  
  
  # Plotly diagram
  output$baseline_plotly <- renderPlotly({
    df <- dig.df_complete1()
   # if (nrow(df) == 0) return(NULL)
    
    df <- df %>% mutate(across(everything(), as.numeric))
    
    plot_ly(
      type = "parcoords",
      line = list(
        color = df$TRTMT,
        colorscale = list(
          list(0, "steelblue"),
          list(1, "darkred")
        )
      ),
      dimensions = list(
        list(range = range(df$AGE),values = df$AGE,label = "Age"),
        list(range = range(df$BMI),values = df$BMI,label = "BMI"),
        list(range = range(df$KLEVEL),values = df$KLEVEL,label = "KLEVEL"),
        list(range = range(df$CREAT),values = df$CREAT,label = "Creatinine"),
        list(range = range(df$DIABP),values = df$DIABP,label = "Diastolic BP"),
        list(range = range(df$SYSBP),values = df$SYSBP,label = "Systolic BP"),
        list(range = c(1, 2),values = df$SEX,label = "Sex")
      )
    )
  })
  
  ####################################################################
  # # Interactive mortality plot
  # Reactive expression to generate the survival fit model
  reactive_survfit_input <- reactive({
    
    # to make the input reactive, it has to be in a formula format.
    # simply doing fit <- survfit(Surv(Month, DEATH) ~ TRTMT + input$features, data = survfit.df) does not seem to work
    # as survfit works as "survfit(formula, data, \dots)", I have tried specifying the formula with paste to make it reactive outside of the survfit()
    
    survfit_formula <- as.formula(paste("Surv(Month, DEATH) ~ TRTMT +", input$features))
    fit <- survfit(survfit_formula, data = survfit.df)
    
    # now we need to change it from the probability of surviving to the risk of dying.
    fit$surv <- 1 - fit$surv
    # flip the confidence interval
    fit$upper <- 1 - fit$lower
    fit$lower <- 1 - fit$upper
    
    return(fit)
  })
  
  
  output$survPlot <- renderPlot({
    # # within the survplot, set fit_2 as the input data  - for some reason you can't use reactive_survfit_input() directly.
    fit_2 <- reactive_survfit_input()
    
    # set 4 colourblind-safe colours as the line colours
    plot_colors <- c("magenta", "orange", "aquamarine", "blue")
    
    # use plot() to plot the risk of dying in each combination of factors:
    # for some reason the surv plot could not be made reactive, there was always an error, but it works with the plot() function, there just isn't a risk table
    plot(fit_2, 
         main = paste("Survival Curve for Treatment and ", input$features), 
         col = plot_colors,
         xlab = "Time in Months",
         ylab = "Risk of Death",
         lwd = 2,
         xlim = c(0, 60),
         ylim = c(0,1))
    
    # label each lines using paste so they change with input
    legend_labels <- c(paste("Placebo + No", input$features), 
                       paste("Placebo + ", input$features), 
                       paste("Treatment + No", input$features), 
                       paste("Treatment + ", input$features))
    # set the positions of the legends/text manually so they don't overlap
    x_positions <- c(0,0,0,0)
    y_positions <- c(0.9, 0.8, 0.7, 0.6)
    
    # use a for loop to add the legend/text to each line
    # there seems to be a problem making a legend so I used text() instead.
    for (single_legend in 1:length(legend_labels)) {
      text(x = x_positions[single_legend],
           y = y_positions[single_legend],
           pos = 4,
           labels = legend_labels[single_legend], 
           col = plot_colors[single_legend], 
           cex = 1)
    }
    
  })
}



