library(testthat)
library(koo)
test_that("box_plot throws error for unsupported plot type", {
  x <- rexp(100, rate = 0.5)

  expect_error(
    box_plot(x, lambda = 0.5, plot_type = "unknown"),
    "Invalid plot_type. Choose 'scatter', 'histogram', or 'density'."
  )
})

test_that("box_plot throws error for invalid input types", {
  x <- c(-1, -2, -3)
  expect_error(box_plot(x, lambda = 0.5), "All values in x must be positive for the Box-Cox transformation.")

  x <- "not numeric"  # Invalid type
  expect_error(box_plot(x, lambda = 0.5), "Input data x must be numeric.")
})
