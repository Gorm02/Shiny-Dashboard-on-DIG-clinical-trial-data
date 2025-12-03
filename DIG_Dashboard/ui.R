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
      menuItem("Final - Study Information", icon = icon("th"), tabName = "study_info", badgeLabel = "new",
               badgeColor = "green"),
      menuItem("Final - Graphs", tabname = "Graphs", icon = icon("star"),
               menuSubItem("Baseline Characteristics", tabName = "baseline_characteristics"),
               menuSubItem("Patient Hospitalisations", tabName = "patient_hospitalisations"),
               menuSubItem("Patient Deaths", tabName = "patient_deaths")
          ),
      menuItem("Final - Table", tabName = "table", icon = icon("star")),
      menuItem("Continuous Hospitalisations", tabName = "cont_hosp", icon = icon("circle")),
      menuItem("Basic Mortality", tabName = "bas_mort", icon = icon("circle")),
      menuItem("Interactive Mortality", tabName = "interact_surv", icon = icon("circle"))
    ),
    
    textOutput("res")
    
  ),

  
  dashboardBody(
    #test for sidebar
    tags$head(
      tags$style(HTML(".sidebar {
                      height: 90vh; overflow-y: auto;
                    }"
      )       
      )            
    ),             
    
    
    
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
              fluidPage(
              box( width = 12,
                h2("Study Background"),
                h4("The digitalis Investigation Group (DIG) study investigated the capacity of the cardiac glycoside, digoxin, to treat systolic heart failure [1]. Glycosides, such as digoxin work by increasing the amount of intracellular sodium retained, enabling the accumulation of intracellular calcium, resulting in stronger cardiac contractions [2]. Despite this known health outcome, at the time of the study it was unknown whether digoxin treatment reduced mortality for patients with heart failure [1].")
              ),
              box(width = 12,
                h2("Study Eligibility"),
                h4("The main DIG study took place across 302 health centers in Canada and the United states (US) from February of 1991 and September of 1993.[1]"),
                h4("The main trial investigated patients with left ventricular ejection fractions of 45% or less.[1]"),
                h4("In total, 6800 patients were recruited to the main trial and randomised to either digoxin treatment or placebo."),
                box(plotOutput("trtmt_plot"))
              ),
              
              box(width = 12,
                h4("Patients in the digoxin group were given an initial recommended dose by an algorithm, which took into account the patient’s age, weight, sex and renal function [1]."),
                h4("The medical practitioners within this study took into account other factors, including the previous dose of digoxin, and adjuvant drugs which could impact digoxin pharmacokinetics, and adjusted digoxin doses accordingly. The average digoxin dose given was 0.25 mg/day [1]."),
                box(width = 12, plotlyOutput("predict_dig_dose"))
              ),
              
              box(width = 12,
                h2("Study Outcomes")
              ),
              box(width = 6,
                h4("This was the first study to investigate whether digoxin impacted individuals with heart failure’s quality of life (by the number of hospitalisations), quantity of life (by patient deaths), and the impact of co-morbidities on the two [3]."),
                h4("In the central trial, 1194 (35.1%) of those in the placebo group, and 1181 (38.4%) of those treated with digoxin died during the trial timeline. In other words, patient mortality was not affected by digoxin treatment."),
                box(width = 12, plotOutput("trtmt_deaths"))
              ),
              box(width = 6,
                h4("However, in the placebo group, 2282 (67.1%) compared to 2184 (64.3%) patients in the digoxin treatment group were hospitalised throughout the trial period. This in itself was not a major difference, but digoxin treatment decreased hospitalisations more significantly for patients with comorbidities such as worsening heart failure."),
                h4(em("Interact with the data in the other tabs to see for yourself!"), style = "color: maroon;"),
                box(width = 12, plotOutput("trtmt_hosps"))
              ),
              
              box(width = 12,
                h2("Conclusions"),
                h4("Although digoxin treatment did not reduce patient mortality as a whole, it did decrease the rate of hospitalization. This benefit was increased in patients with worsening heart failure. Therefore, digoxin treatment can be used as a method of improving the quality rather than the quantity of life for patients with chronic heart failure, and especially those with worsening heart failure.")
              ),
              box(width = 12,
                  h2("References"),
                  h4("1. Abdul-Rahim, A. H., Macisaac, R. L., Jhund, P. S., Petrie, M. C., Lees, K. R., & McMurray, J. J. V. (2016). Efficacy and safety of digoxin in patients with heart failure and reduced ejection fraction according to diabetes status: An analysis of the Digitalis Investigation Group (DIG) trial. International Journal of Cardiology, 209. https://doi.org/10.1016/j.ijcard.2016.02.074 "),
                  h4("2. Patocka, J., Nepovimova, E., Wu, W., & Kuca, K. (2020). Digoxin: Pharmacology and toxicology—A review. In Environmental Toxicology and Pharmacology (Vol. 79). https://doi.org/10.1016/j.etap.2020.103400 "),
                  h4("3. The Effect of Digoxin on Mortality and Morbidity in Patients with Heart Failure. (1997). New England Journal of Medicine, 336(8). https://doi.org/10.1056/nejm199702203360801 "))
              )),
      
      
      tabItem("baseline_characteristics",
              h2("Study Overview"),
              h4("Baseline characteristics are the demographic, medical, and other descriptive data collected prior to the study. It's imperitive
                 to record this information to ensure groups are comparable, identifying potential confounders, or assessing the impact on randomization. Below represents a parallel coordinates plot
                 visualising each patients baseline characteristic to disern if both groups are comparable. We can see that outside of a few outliers, the consensus
                 of the population is relative similarity between these characteristics. Feel free to explore the dataset with the sliders shown on this page!"),
                fluidRow(
                  
              # Baseline Plotly
              box(width = 8, title = "Interactive plot comparing baseline characteristics between gender and treatment group", collapsible = T, status = "warning", solidHeader = T,
                  plotlyOutput("baseline_plotly")),
              box(width = 4, title = "Select Charactertistic", collapsible = T, status = "warning", solidHeader = T,
                  sliderInput("bmi", "Body Mass index:",
                               min = min(dig.df_complete$BMI), max = max(dig.df_complete$BMI), value = c(min(dig.df_complete$BMI), max(dig.df_complete$BMI))),
                  sliderInput("age", "Select Age Range:",
                              min = 21, max = 90, value = c(30, 50)),
                  checkboxGroupInput("sex", "Sex:", choices = c("Male" = 1, "Female" = 2), selected = c(1,2)),
                  checkboxGroupInput("TRTMT", "Treatment", choices =  c("Placebo" = 1, "Treatment" = 2), selected = c(1,2))),
              
              box(width = 8, title = "Key Findings", 
                  "Boxplots were used to visualize the spread of the data between patients assigned to placebo and control. Each feature analysed 
                  showed relative similarity between groups suggesting the study population is suitable for this trial."))
      ),
      
      tabItem("table",
              dataTableOutput("table")),
      
      # mortality plot in the new tab key takeaways. 
      tabItem("patient_hospitalisations",
              fluidPage(
                h2("Comparing number of hospitalisations and deaths across different factors"),
                h4("This tab is dedicated to visualising the effect features such as worsening heart failure, history of cardiovascular disease, history of hypertension, and race
                   have on the number of hospitalisations and deaths. Select a feature to view relevent bar charts."),
                box(width = 8, title = "Mortality plot", collapsible = T, status = "warning", solidHeader = T,
                    plotOutput("Mortality_Plot")),
                box(width = 4, title = "Select Feature:", collapsible = T, status = "warning", solidHeader = T,
                    selectInput("features", "Features:",
                                c("WHF", "CVD", "Sex" = "SEX", "History of Hypertension" = "HYPERTEN", "History of Diabetes" = "DIABETES", "Race" = "RACE"),
                                selected = "WHF")),
                box(width = 8, title = "Mortality plot 2", collapsible = T, status = "warning", solidHeader = T,
                        plotOutput("Hospitalisation_Plot")),
                box(width = 12, title = "Key Insights",collapsible = F,
                    h4("Looking at prior history to cardiovascular disease, the presence of prior CVD did not significantly affect mortality rates within either the placebo or treatment groups.
                    In the placebo group, patients without a history of CVD showed a slightly lower proportion of deaths compared to those with CVD.
                    In contrast, within the treatment group, mortality appeared marginally higher among those without a CVD history,
                    suggesting only minor variation and no clear treatment-related effect.
                    The placebo group had a slightly higher proportion of hospitalizations compared to the treatment group. Conversely, the treatment group had a slightly higher
                    proportion of patients who were not hospitalized. This suggests that Digoxin potentially may reduce the risk of hospitalization. Between both treatment and placebo groups, worsening heart failure (WHF) is 100% correlated with the number of hospitalizations.
                    Patients who did not experience WHF showed slight variation in the percentage of hospitalizations in this study, suggesting that WHF is the key attributing factor to the number of hospitalizations in this study. 
                       Finally, both race and sex seem to have no effect on hospitalisations or death.")))),
      
      tabItem("patient_deaths",
              box(width = 8,
                  plotOutput("basic_surv_plot")),
              box(width = 12,
                  box(plotOutput("survPlot")),
                  box(selectInput("features", "Features:",
                                  c("CVD", "WHF", "DIABETES"), selected = "CVD")))
              ),
              
              
              
              
              # fluidPage(
              #   box(width = 8, title = "Mortality Plot", collapsible = T, status = "warning", solidHeader = T,
              #       radioButtons("mort_op", "Select your factor:", c("Worsening Heart Failure" = "WHF","History of Cardiovascular Disease"= "CVD")),
              #       plotlyOutput("surv_plotly"),
              #       )
              # )),
      
     
      tabItem("cont_hosp",
              box(plotOutput("continuous_hospitalisations_plot")),
              box(selectInput("features", "Features:",
                              c("Age" = "AGE", "BMI", "Serum Potassium Level" = "KLEVEL", "Serum Creatinine (mg/dL)" = "CREAT", "Ejection Fraction Percent" = "EJF_PER"),
                              selected = "Age"))),
      # tabItem("bas_mort",
      #         box(plotOutput("basic_surv_plot"))
      #         ),
      
      tabItem("interact_surv",
              box(
                width = 12,
                plotlyOutput("survPlot_main", height = "500px")
              ),
              box(selectInput("featuresmort", "Features:", c("CVD", "WHF"), selected = "CVD")))
      )
  )
)



