library(tidyverse)
library("survival")
library("survminer")
library("plotly")

dig.df <- read_csv("DIG.csv", 
                   col_names = TRUE, 
                   col_select = c(ID, TRTMT, AGE, SEX, BMI, KLEVEL, CREAT, DIABP, SYSBP, HYPERTEN, CVD, WHF, DIG, HOSP, HOSPDAYS, DEATH, DEATHDAY, DIGDOSER, DIGDOSE, RACE, EJF_PER),
                   col_types = cols(
                     ID = col_integer(),
                     TRTMT = col_logical(),
                     AGE = col_integer(),
                     SEX = col_factor(),
                     BMI = col_double(),
                     KLEVEL = col_double(),
                     CREAT = col_double(),
                     DIABP = col_integer(),
                     SYSBP = col_integer(),
                     HYPERTEN = col_logical(),
                     CVD = col_logical(),
                     WHF = col_logical(),
                     DIG = col_logical(),
                     HOSP = col_logical(),
                     HOSPDAYS = col_integer(),
                     DEATH = col_logical(),
                     DEATHDAY = col_integer(),
                     DIGDOSER = col_double(),
                     DIGDOSE = col_double(),
                     RACE = col_factor(),
                     EJF_PER = col_double()))

## label the factors described in the codebook: 
dig.df$TRTMT <- factor(dig.df$TRTMT,
                       levels = c("FALSE", "TRUE"),
                       labels = c("Placebo", "Treatment"))

dig.df$SEX <- factor(dig.df$SEX,
                     levels = c(1, 2),
                     labels = c("Male", "Female"))

dig.df$DEATH <- factor(dig.df$DEATH,
                       levels = c("FALSE", "TRUE"),
                       labels = c("Alive", "Death"))

# More factor variables have been labelled here:
dig.df$HYPERTEN <- factor(dig.df$HYPERTEN,
                          levels = c("FALSE", "TRUE"),
                          labels = c("No History of Hypertension", "History of Hypertension"))
dig.df$CVD <- factor(dig.df$CVD,
                     levels = c("FALSE", "TRUE"),
                     labels = c("No Cardiovascular Disease", "Cardiovascular Disease"))
dig.df$WHF <- factor(dig.df$WHF,
                     levels = c("FALSE", "TRUE"),
                     labels = c("No Worsening Heart Failure", "Worsening Heart Failure"))
dig.df$DIG <- factor(dig.df$DIG,
                     levels = c("FALSE", "TRUE"),
                     labels = c("No Digoxin Toxicity", "Digoxin Toxicity"))

dig.df$HOSP <- factor(dig.df$HOSP,
                      levels = c("FALSE", "TRUE"),
                      labels = c("Not Hospitalized", "Hospitalized"))
dig.df$RACE <- factor(dig.df$RACE,
                      levels = c(1, 2),
                      labels = c("White", "Nonwhite"))

## mutate dig.df to include a new column,"Month":
dig.df <- dig.df %>%
  mutate(Month = round(DEATHDAY/30))

# Remove the row with the serum potassium outlier but not the na values (there are 801 NA values in this column)
dig.df <- dig.df %>%
  filter(KLEVEL != 434 | is.na(KLEVEL))


# Data for the survfit functionS:
survfit.df <- dig.df %>%
  mutate(DEATH  = case_when(
    DEATH == "Alive" ~ 0,
    DEATH == "Death" ~ 1
  ))

# Preparing for the basic survival plot
TRTMT_FIT <- survfit(Surv(Month, DEATH) ~ TRTMT, data = survfit.df)

# now we need to change it from the probability of surviving to the risk of dying.
Mort_TRTMT_Fit <- TRTMT_FIT
Mort_TRTMT_Fit$surv <- 1- TRTMT_FIT$surv
# flip the confidence interval
Mort_TRTMT_Fit$upper <- 1 - TRTMT_FIT$lower
Mort_TRTMT_Fit$lower <- 1 - TRTMT_FIT$upper


# Preparing for the basic plot showing the number of patients per treatment group:
q1 <- dig.df %>%
  group_by(TRTMT)%>%
  summarise(Number_of_Patients = n()) %>%
  mutate(Proportion_of_Patients = round(Number_of_Patients/sum(Number_of_Patients), 3))

names(q1) <- c("Treatment_Group", "Number_of_Patients", "Proportion_of_Patients")

# Preparing for the parallel coordinates plot:
parco.df <- read_csv("DIG.csv",
                     col_names = TRUE,
                     col_select = c(ID, TRTMT, SEX, AGE, BMI, CREAT, DIGDOSER, DIGDOSE),
                     col_types = cols(
                       ID = col_integer(),
                       TRTMT = col_integer(),
                       AGE = col_integer(),
                       SEX = col_factor(),
                       BMI = col_double(),
                       CREAT = col_double(),
                       DIGDOSER = col_double(),
                       DIGDOSE = col_double()
                     ))

parco.df <- na.omit(parco.df)


#

dig.df_complete <- na.omit(dig.df)
dig.df_complete$TRTMT <- as.numeric(factor(dig.df_complete$TRTMT))
dig.df_complete$SEX <- as.numeric(factor(dig.df_complete$SEX))

