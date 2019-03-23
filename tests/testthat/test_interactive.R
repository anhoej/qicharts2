library(qicharts2)
context('Interactive tests')

# Single plot of hospital infections ----
test_that('Single plot of hospital infections', {
  expect_equal_to_reference(
    `$`(`$`(
      qic(month, n, days,
          data = hospital_infections,
          chart = 'u'), x), html),
    'interactive_1.rds'
  )
})

# Single facted plot of hospital infections ----
test_that('Interactive - Single facted plot of hospital infections', {
  expect_equal_to_reference(
    `$`(`$`(
      qic(month, n, days,
          data = hospital_infections,
          facets = ~ infection,
          chart = 'u'), x), html),
    'interactive_2.rds'
  )
})

# Double faceted plot of hospital infections ----
test_that('Interactive - Double faceted plot of hospital infections', {
  expect_equal_to_reference(
    `$`(`$`(
      qic(month, n, days,
          data = hospital_infections,
          facets = hospital ~ infection,
          chart = 'u'), x), html),
    'interactive_3.rds'
  )
})

# Funnel plot of hospital infections ----
test_that('Interactive - Funnel plot of hospital infections', {
  expect_equal_to_reference(
    `$`(`$`(
      qic(reorder(hospital, days), n, days,
          data = subset(hospital_infections, month >= '2016-07-01'),
          chart = 'u',
          facets = ~ infection,
          multiply = 10000), x), html),
    'interactive_4.rds'
  )
})
