library(testthat)
library(koo)

test_that("box_diagnostics generates a grid of diagnostic plots", {
  x <- rexp(100, rate = 0.5)

  expect_silent({
    box_diagnostics(x, lambda = 0.5)
  })
})

test_that("box_diagnostics calculates transformed data when not provided", {
  x <- rexp(100, rate = 0.5)

  expect_silent({
    box_diagnostics(x, lambda = 0.5)
  })
})

test_that("box_diagnostics works with pre-supplied transformed data", {
  x <- rexp(100, rate = 0.5)
  transformed_x <- box_transform(x, lambda = 0.5)

  expect_silent({
    box_diagnostics(x, transformed_x = transformed_x)
  })
})

test_that("box_diagnostics throws error for negative or zero values", {
  x <- c(-1, 2, 3, 4, 5)

  expect_error(box_diagnostics(x, lambda = 0.5),
               "All values in x must be positive for the Box-Cox transformation.")
})

test_that("box_diagnostics throws error for non-numeric input", {
  x <- c("a", "b", "c")

  expect_error(box_diagnostics(x, lambda = 0.5),
               "Input data x must be numeric.")
})

test_that("box_diagnostics throws error if neither lambda nor transformed_x is provided", {
  x <- rexp(100, rate = 0.5)

  expect_error(box_diagnostics(x),
               "If transformed_x is not provided, lambda must be specified.")
})
