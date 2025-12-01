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
library(bslib)
library(plotly)
ui <- dashboardPage(
  dashboardHeader(title = "Digoxin Clinical Trial Interactive Application", titleWidth = 450,
                  tags$li(class = "dropdown",
                  tags$img(src = "NUIG.png", height = "40px")
                  )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      #To let the user know what tab they have selected.
      
      id = "tabs",
      
      # use menuItem to make tabs for different components of the study we want to discuss:
      menuItem("Study Information", icon = icon("th"), tabName = "study_info", badgeLabel = "new",
               badgeColor = "green"),
      menuItem("Study Overview", tabName = "study_overview", icon = icon("tree")),
      menuItem("Baseline Characteristics", tabName = "cat_base_char", icon = icon("circle")),
      menuItem("Key Takeaways", icon = icon("book"), tabName = "takeaway", badgeLabel = "Important",
               badgeColor = "red"),
      menuItem("Continuous Deaths", tabName = "cont_death", icon = icon("circle")),
      menuItem("Continuous Hospitalisations", tabName = "cont_hosp", icon = icon("circle")),
      menuItem("Basic Mortality", tabName = "bas_mort", icon = icon("circle")),
      menuItem("Interactive Mortality", tabName = "interact_surv", icon = icon("circle"))
    ),
    
    textOutput("res")
    
  ),

  
  dashboardBody(
    # Changes Title Font to Times New Roman (integrated CSS file)
    tags$head(tags$style(HTML('
                              .main-header .logo {
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 22px;
                              }'
                              )
                         )
              ),
    
    # Using custom CSS to change colour of title background identical. (Not sure why but .skin-blue is important here. Might be because of default settings?)
    tags$head(tags$style(HTML('
        .skin-blue .main-header .logo {
          background-color: #66b2b2;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #66b2b2 ; }
         
         .skin-blue .main-header .navbar {
         background-color: #66b2b2;
         } 
         .skin-blue .main-header .navbar sidebar-toggle:hover {
         background-color: #66b2b2:)
         }'
        )
      )
),
   
    
    # input the bodies for the different tabs (above, in menuItem):
    tabItems(
      tabItem("study_info",

                h2("Study Background"),
                h4("The digitalis Investigation Group (DIG) study investigated the capacity of the cardiac glycoside, digoxin, to treat systolic heart failure. Glycosides, such as digoxin work by increasing the amount of intracellular sodium retained, enabling the accumulation of intracellular calcium, resulting in stronger cardiac contractions. Despite this known health outcome, at the time of the study it was unknown whether digoxin treatment reduced mortality for patients with heart failure."),
                
                h2("Study Eligibility"),
                h4("The main DIG study took place across 302 health centers in Canada and the United states (US) from February of 1991 and September of 1993."),
                h4("The main trial investigated patients with left ventricular ejection fractions of 45% or less."),
                h4("In total, 6800 patients were recruited to the main trial and randomised to either digoxin treatment or placebo."),
                box(plotOutput("trtmt_plot")),

                h4("Patients in the digoxin group were given an initial recommended dose by an algorithm, which took into account the patient’s age, weight, sex and renal function."),
                h4("The medical practitioners within this study took into account other factors, including the previous dose of digoxin, and adjuvant drugs which could impact digoxin pharmacokinetics, and adjusted digoxin doses accordingly. The average digoxin dose given was 0.25 mg/day."),
                box(plotlyOutput("predict_dig_dose")),  
              
                h2("Study Outcomes"),
                h4("This was the first study to investigate whether digoxin impacted individuals with heart failure’s quality of life (by the number of hospitalisations) and quantity of life (by patient deaths)."),
                h4("In the central trial, 1194 (35.1%) of those in the placebo group, and 1181 (38.4%) of those treated with digoxin died during the trial timeline. In other words, patient mortality was not affected by digoxin treatment."),
                h4("However, in the placebo group, 2282 (67.1%) compared to 2184 (64.3%) patients in the digoxin treatment group were hospitalised throughout the trial period. This in itself was not a major difference, but digoxin treatment decreased hospitalisations more significantly for patients with comorbidities such as worsening heart failure."),
                h4(em("Interact with the data in the other tabs to see for yourself!"), style = "color: maroon;"),
                
                h2("Conclusions"),
                h4("Although digoxin treatment did not reduce patient mortality as a whole, it did decrease the rate of hospitalization. This benefit was increased in patients with worsening heart failure. Therefore, digoxin treatment can be used as a method of improving the quality rather than the quantity of life for patients with chronic heart failure, and especially those with worsening heart failure.")
              ),
      
      
      tabItem("study_overview",
              h2("Study Overview"),
              h4("Baseline characteristics are the demographic, medical, and other descriptive data collected prior to the study. It's imperitive
                 to record this information to ensure groups are comparable, identifying potential confounders, or assessing the impact on randomization."),
                fluidRow(
                  box(width = 4,title = "Boxplot of Baseline Characteristics", collapsible = T, status = "warning", solidHeader = T, plotOutput("Baseline_Values_plot")),
              box(width = 2, title = "Select Feature", collapsible = T, status = "warning", solidHeader = T, selectInput("features", "Features:",
                              c("Age" = "AGE", "BMI", "Serum Potassium Level" = "KLEVEL", "Serum Creatinine (mg/dL)" = "CREAT", "Ejection Fraction Percent" = "EJF_PER", "Diastolic Blood Pressure" = "DIABP", "Systolic Blood Pressure" = "SYSBP"),
                              selected = "AGE"))),
              box(width = 8, title = "Key Findings", 
                  "Boxplots were used to visualize the spread of the data between patients assigned to placebo and control. Each feature analysed 
                  showed relative similarity between groups suggesting the study population is suitable for this trial.")),
      
      
      
      # mortality plot in the new tab key takeaways
      tabItem("takeaway",
              fluidPage(plotOutput("Mortality_Plot"),
                        plotOutput("Hospitalisation_Plot"),
                        selectInput("features", "Features:",
                              c("WHF", "CVD", "Sex" = "SEX", "History of Hypertension" = "HYPERTEN", "Race" = "RACE"),
                              selected = "WHF"))),
      
      
      tabItem("cat_base_char",
              box(plotOutput("categorical_baseline_plot")),
              box(selectInput("features", "Features:",
                              c("Sex" = "SEX", "History of Hypertension" = "HYPERTEN", "Race" = "RACE"),
                              selected = "Sex"))),
      
      tabItem("cont_death",
              box(plotOutput("continuous_deaths_plot")),
              box(selectInput("features", "Features:",
                              c("Age" = "AGE", "BMI", "Serum Potassium Level" = "KLEVEL", "Serum Creatinine (mg/dL)" = "CREAT", "Ejection Fraction Percent" = "EJF_PER"),
                              selected = "Age"))),
      tabItem("cont_hosp",
              box(plotOutput("continuous_hospitalisations_plot")),
              box(selectInput("features", "Features:",
                              c("Age" = "AGE", "BMI", "Serum Potassium Level" = "KLEVEL", "Serum Creatinine (mg/dL)" = "CREAT", "Ejection Fraction Percent" = "EJF_PER"),
                              selected = "Age"))),
      tabItem("bas_mort",
              box(plotOutput("basic_surv_plot")),
             # box(plotOutput("trtmt_plot"))
              ),
      
      tabItem("interact_surv",box(
                width = 12,
                plotlyOutput("survPlot_main", height = "500px")
              ),
              box(selectInput("features", "Features:", c("CVD", "WHF"), selected = "CVD")))
      )
    )
  )


