library(qicharts2)
context('Hospital Infections')

# Single plot of hospital infections ----
test_that('Single plot of hospital infections', {
  expect_equal_to_reference(
    summary(
      qic(month, n, days,
          data = hospital_infections,
          chart = 'u')
    ),
    'hai_1.rds'
  )
})

# Single facted plot of hospital infections ----
test_that('Single facted plot of hospital infections', {
  expect_equal_to_reference(
    summary(
      qic(month, n, days,
          data = hospital_infections,
          facets = ~ infection,
          chart = 'u')
    ),
    'hai_2.rds'
  )
})

# Double faceted plot of hospital infections ----
test_that('Double faceted plot of hospital infections', {
  expect_equal_to_reference(
    summary(
      qic(month, n, days,
          data = hospital_infections,
          facets = hospital ~ infection,
          chart = 'u')
    ),
    'hai_3.rds'
  )
})

# Funnel plot of hospital infections ----
test_that('Funnel plot of hospital infections', {
  expect_equal_to_reference(
    summary(
      qic(reorder(hospital, days), n, days,
          data = subset(hospital_infections, month >= '2016-07-01'),
          chart = 'u',
          facets = ~ infection,
          multiply = 10000)
    ),
    'hai_4.rds'
  )
})
