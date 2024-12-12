#' Box-Cox Diagnostic Plots
#'
#' This function generates diagnostic plots comparing the original data and the transformed data.
#' Includes QQ plots for normality checks and a box plot to highlight change in data spread and central tendecny
#'
#' @param x A numeric vector of the original data.
#' @param transformed_x An optional numeric vector of the transformed data. If not provided, the function will calculate it.
#' @param lambda Transformation parameter for the Box-Cox transformation (ignored if `transformed_x` is provided).
#'
#' @author Olivia Summerville
#'
#' @return A combined grid of diagnostic plots.
#'
#' @import ggplot2
#' @import gridExtra
#'
#' @examples
#' x <- c(1, 2, 3, 4, 10, 15, 25)
#' box_diagnostic_plot(x, lambda = 0.5)
#' @export
box_diagnostic_plot <- function(x, transformed_x = NULL, lambda = NULL) {
  if (any(x <= 0)) {
    stop("All values in x must be positive for the Box-Cox transformation.")
  }
  if (!is.numeric(x)) {
    stop("Input data x must be numeric.")
  }
  if (is.null(transformed_x)) {
    if (is.null(lambda)) {
      stop("If transformed_x is not provided, lambda must be specified.")
    }
    transformed_x <- box_transform(x, lambda)
  }

  qq_original <- ggplot(data.frame(Value = x), aes(sample = Value)) +
    stat_qq() +
    stat_qq_line() +
    theme_minimal() +
    labs(title = "QQ Plot: Original Data", x = "Theoretical Quantiles", y = "Sample Quantiles")

  qq_transformed <- ggplot(data.frame(Value = transformed_x), aes(sample = Value)) +
    stat_qq() +
    stat_qq_line() +
    theme_minimal() +
    labs(title = "QQ Plot: Transformed Data", x = "Theoretical Quantiles", y = "Sample Quantiles")

  boxplot_data <- data.frame(
    Value = c(x, transformed_x),
    Type = rep(c("Original", "Transformed"), each = length(x))
  )

  boxplot <- ggplot(boxplot_data, aes(x = Type, y = Value, fill = Type)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = "Boxplot: Original vs Transformed Data")

  grid.arrange(
    qq_original, qq_transformed, boxplot, ncol = 2
  )
}
