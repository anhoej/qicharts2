## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, 
  comment = "#>",
  fig.width = 7.15)

## ------------------------------------------------------------------------
# Load qicharts2.
library(qicharts2)

# Lock random number generator to make examples reproducible.
seed <- 19
set.seed(seed)

# Generate 12 random numbers from a normal distribution.
y <- rnorm(24)

# Make run chart.
qic(y)
# Make I control chart.
qic(y, chart = 'i')

## ------------------------------------------------------------------------
# Add special cause to y in the form of a large, transient shift of 4 standard deviations.
set.seed(seed)
y[22] <- 4

qic(y, chart = 'i')

## ------------------------------------------------------------------------
# Add special cause to y in the form of a moderate, persistent shift of 2 standard deviations.
set.seed(seed)
y[13:24] <- rnorm(12, mean = 2)

qic(y, chart = 'i')

## ------------------------------------------------------------------------
head(cdi)

## ------------------------------------------------------------------------
qic(month, n,
    notes = notes,
    data  = cdi,
    main  = 'Hospital acquired Clostridium difficile infections',
    ylab  = 'Count',
    xlab  = 'Month',
    print.summary = T)

## ------------------------------------------------------------------------
qic(month, n,
    data        = cdi,
    freeze      = 24,
    part.labels = c('Before intervention', 'After intervention'),
    main        = 'Hospital acquired Clostridium difficile infections',
    ylab        = 'Count',
    xlab        = 'Month')

## ------------------------------------------------------------------------
qic(month, n,
    data         = cdi,
    break.points = 24,
    main         = 'Hospital acquired Clostridium difficile infections',
    ylab         = 'Count',
    xlab         = 'Month')

## ------------------------------------------------------------------------
qic(month, n,
    data         = cdi,
    chart        = 'c',
    break.points = 24,
    main         = 'Hospital acquired Clostridium difficile infections (C chart)',
    ylab         = 'Count',
    xlab         = 'Month')

## ------------------------------------------------------------------------
qic(month, n, 
    n            = days,
    data         = cdi,
    chart        = 'u',
    break.points = 24,
    multiply     = 10000, 
    main         = 'Hospital acquired Clostridium difficile infections (U chart)',
    ylab         = 'Count per 10,000 risk days',
    xlab         = 'Month')

## ------------------------------------------------------------------------
head(cabg)

## ------------------------------------------------------------------------
qic(age,
    data  = tail(cabg, 100), 
    chart = 'i',
    main  = 'Age of the last 100 patients (I chart)',
    ylab  = 'Years',
    xlab  = 'Patient #')

## ------------------------------------------------------------------------
qic(age,
    data  = tail(cabg, 100), 
    chart = 'mr',
    main  = 'Age of the last 100 patients (I chart)',
    ylab  = 'Years',
    xlab  = 'Patient #')

## ------------------------------------------------------------------------
qic(age,
    data  = tail(cabg, 100), 
    chart = 'i',
    exclude = c(45, 70),
    main  = 'Age of the last 100 patients (I chart)',
    ylab  = 'Years',
    xlab  = 'Patient #')

## ------------------------------------------------------------------------
suppressPackageStartupMessages(library(dplyr))

cabg <- cabg %>% 
  mutate(month = as.Date(cut(date, 'month')))

## ------------------------------------------------------------------------
qic(month, age,
    data  = cabg,
    chart = 'xbar',
    main  = 'Average age (Xbar chart)',
    ylab  = 'Years',
    xlab  = 'Month')

## ------------------------------------------------------------------------
qic(month, age,
    data  = cabg,
    chart = 's',
    main  = 'Age standard deviation (S chart)',
    ylab  = 'Years',
    xlab  = 'Month')

## ------------------------------------------------------------------------
cabg_by_month <- cabg %>% 
  group_by(month) %>% 
  summarise(deaths       = sum(death),
            readmissions = sum(readmission),
            n            = n())

head(cabg_by_month)

## ------------------------------------------------------------------------
qic(month, readmissions, 
    n         = n,
    data      = cabg_by_month,
    chart     = 'p',
    y.percent = TRUE,
    main      = 'Readmissions within 30 days (P chart)',
    ylab      = '',
    xlab      = 'Month')


## ------------------------------------------------------------------------
qic(month, deaths, 
    n         = n,
    data      = cabg_by_month,
    chart     = 'p',
    y.percent = TRUE,
    main      = '30 days mortality (P chart)',
    ylab      = '',
    xlab      = 'Month')

## ------------------------------------------------------------------------
fatalities <- cabg %>% 
  mutate(x = row_number()) %>% 
  filter(death) %>% 
  mutate(dd = x - lag(x),
         dt = date - lag(date))

## ------------------------------------------------------------------------
qic(dd,
    data  = fatalities,
    chart = 'g',
    main  = 'Surgeries between deaths (G chart)',
    ylab  = 'Count',
    xlab  = 'Death #')

## ------------------------------------------------------------------------
qic(dt,
    data  = fatalities,
    chart = 't',
    main  = 'Days between deaths (T chart)',
    ylab  = 'Days',
    xlab  = 'Death #')

## ------------------------------------------------------------------------
cabg_by_month_gender <- cabg %>% 
  group_by(month, gender) %>% 
  summarise(readmissions = sum(readmission),
            n            = n())

head(cabg_by_month_gender)

## ------------------------------------------------------------------------
qic(month, readmissions, 
    n         = n,
    data      = cabg_by_month_gender,
    facets    = ~ gender, 
    chart     = 'p',
    y.percent = TRUE,
    main      = 'Readmissions within 30 days (P chart)',
    ylab      = '',
    xlab      = 'Month')

## ------------------------------------------------------------------------
head(hospital_infections)

## ------------------------------------------------------------------------
qic(month, n, days,
    data     = hospital_infections,
    chart    = 'u',
    multiply = 10000,
    main     = 'Hospital acquired infections in the Capital Region of Denmark',
    ylab     = 'Count per 10,000 risk days',
    xlab     = 'Month')

## ---- fig.height=2-------------------------------------------------------
qic(month, n, days,
    data     = hospital_infections,
    facets   = ~ infection,
    chart    = 'u',
    multiply = 10000,
    main     = 'Hospital acquired infections in the Capital Region of Denmark',
    ylab     = 'Count per 10,000 risk days',
    xlab     = 'Month')

## ---- fig.height=4-------------------------------------------------------
qic(month, n, days,
    data     = hospital_infections,
    facets   = infection ~ hospital,
    chart    = 'u',
    multiply = 10000,
    scales   = 'free_y',
    x.angle  = 45,
    main     = 'Hospital acquired infections in the Capital Region of Denmark',
    ylab     = 'Count per 10,000 risk days',
    xlab     = 'Month')

