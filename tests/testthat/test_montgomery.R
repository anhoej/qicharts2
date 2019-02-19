library(qicharts2)
context('Control charts from Montgomery')

# Example 6.3 ----
test_that('Example 6.3_1 is equal to reference', {
  skip_on_cran()
  skip_on_travis()
  expect_equal_to_reference(
    qic(sample, obs,
        data = tbl_06.03,
        chart = 'xbar')$data
    ,
    'ex_6.3_1.rds'
  )
  
  expect_equal_to_reference(
    qic(sample, obs,
        data = tbl_06.03,
        chart = 's')$data
    ,
    'ex_6.3_2.rds'
  )
})

# Example 6.4 ----
test_that('Example 6.4 is equal to reference', {
  skip_on_cran()
  skip_on_travis()
  expect_equal_to_reference(
    qic(sample, obs,
        data = tbl_06.04,
        chart = 'xbar')$data
    ,
    'ex_6.4_1.rds'
  )
  
  expect_equal_to_reference(
    qic(sample, obs,
        data = tbl_06.04,
        chart = 's')$data
    ,
    'ex_6.4_2.rds'
  )
})

# Example 6.5 ----
test_that('Example 6.5 is equal to reference', {
  skip_on_cran()
  skip_on_travis()
  expect_equal_to_reference(
    qic(weeks, cost,
        data = tbl_06.07,
        chart = 'i',
        freeze = 20)$data
    ,
    'ex_6.5_3.rds'
  )
  
  expect_equal_to_reference(
    qic(weeks, cost,
        data = tbl_06.07,
        chart = 'mr',
        freeze = 20)$data
    ,
    'ex_6.5_4.rds'
  )
})

# Example 7.1 ----
test_that('Example 7.1 is equal to reference', {
  skip_on_cran()
  skip_on_travis()
  expect_equal_to_reference(
    qic(sample, count, size, notes = note,
        data = tbl_07.03,
        chart = 'p',
        exclude = c(15, 23),
        part = 30)$data
    ,
    'ex_7.1_5.rds'
  )
})

# Table 7.4 ----
test_that('Table 7.4 is equal to reference', {
  skip_on_cran()
  skip_on_travis()
  expect_equal_to_reference(
    qic(sample, count, size,
        data = tbl_07.04,
        chart = 'p')$data
    ,
    'tbl_7.4_1.rds'
  )
})

# Example 7.3 ----
test_that('Example 7.3 is equal to reference', {
  skip_on_cran()
  skip_on_travis()
  expect_equal_to_reference(
    qic(sample, count,
        data = tbl_07.08,
        chart = 'c',
        exclude = c(6, 20),
        freeze = 26)$data
    ,
    'ex_7.3_3.rds'
  )
})

# Example 7.4 ----
test_that('Example 7.4 is equal to reference', {
  skip_on_cran()
  skip_on_travis()
  expect_equal_to_reference(
    qic(sample, count, size,
        data = tbl_07.10,
        chart = 'u')$data
    ,
    'ex_7.4_1.rds'
  )
})

# Example 7.5 ----
test_that('Example 7.5 is equal to reference', {
  skip_on_cran()
  skip_on_travis()
  expect_equal_to_reference(
    qic(roll, count, meters,
        data = tbl_07.11,
        chart = 'u',
        multiply = 50)$data
    ,
    'ex_7.5_1.rds'
  )
})

# Example 7.6 ----
test_that('Example 7.6 is equal to reference', {
  skip_on_cran()
  skip_on_travis()
  expect_equal_to_reference(
    qic(failure, hours,
        data = tbl_07.14,
        chart = 't')$data
    ,
    'ex_7.6_1.rds'
  )
})

# NA test ----
test_that('NA test is equal to reference', {
  skip_on_cran()
  skip_on_travis()
  expect_equal_to_reference(
    qic(sample, obs,
        data = NA_test,
        chart = 'xbar')$data
    ,
    'NA_test.rds'
  )
})
