## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, 
  comment = "#>",
  fig.width = 7.15)

## ------------------------------------------------------------------------
# Load qicharts2.
library(qicharts2)

# Lock random number generator to make exampels reproducible.
set.seed(9)

# Generate 12 random numbers from a normal distribution.
y <- rnorm(12)

# Make run chart.
qic(y)

# Make I control chart.
qic(y, chart = 'i')

## ------------------------------------------------------------------------
# Load dataset.
data("hospital_infections")

# Make U control chart of hospital infections.
qic(month, n, days,
    data     = hospital_infections,
    chart    = 'u',
    multiply = 10000,
    main     = 'Hospital acquired infections in the Capital Region of Denmark',
    ylab     = 'Count per 10,000 risk days',
    xlab     = 'Month')

# Facet control chart by type of infection.
qic(month, n, days,
    data     = hospital_infections,
    facets   = ~ infection,
    chart    = 'u',
    multiply = 10000,
    main     = 'Hospital acquired infections in the Capital Region of Denmark',
    ylab     = 'Count per 10,000 risk days',
    xlab     = 'Month')

# Facet control chart by hospital (vertical) and type of infection (horizontal).
qic(month, n, days,
    data     = hospital_infections,
    facets   = infection ~ hospital,
    chart    = 'u',
    multiply = 10000,
    main     = 'Hospital acquired infections in the Capital Region of Denmark',
    ylab     = 'Count per 10,000 risk days',
    xlab     = 'Month')

# Free y axis and rotate x axis labels
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

