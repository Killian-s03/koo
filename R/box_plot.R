#' Box-Cox Transformation Visualization
#'
#' This function compares the distribution of the original data with the transformed data
#' using side-by-side histograms or density plots.
#'
#' @param x A numeric vector of the original data.
#' @param transformed_x An optional numeric vector of the transformed data. If not provided, the function will calculate it.
#' @param lambda Transformation parameter for the Box-Cox transformation (ignored if `transformed_x` is provided).
#' @param plot_type The type of plot to generate: "scatter" (default), histogram" or "density"
#'
#' @author Olivia Summerville
#'
#' @return A ggplot2 object displaying the comparison of original and transformed data.
#'
#' @import ggplot2
#' @import gridExtra
#'
#' @examples
#' x <- c(1, 2, 3, 4, 10, 15, 25)
#' box_plot(x, lambda = 0.5)
#' @export
box_plot <- function(x, transformed_x = NULL, lambda = NULL, plot_type = "histogram") {
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

  library(ggplot2)
  scatter_original <- ggplot(data.frame(Original = x), aes(x = Original, y = Original)) +
    geom_point(color = "cyan3") +
    theme_minimal() +
    labs(title = "Scatter Plot: Original Data", x = "Original Value", y = "Original Value")

  scatter_transformed <- ggplot(data.frame(Original = x, Transformed = transformed_x), aes(x = Original, y = Transformed)) +
    geom_point(color = "cyan3") +
    theme_minimal() +
    labs(title = "Scatter Plot: Transformed Data", x = "Original Value", y = "Transformed Value")

  hist_original <- ggplot(data.frame(Value = x), aes(x = Value)) +
    geom_histogram(binwidth = 1, fill = "cyan3", alpha = 0.6, aes(y = ..density..)) +
    theme_minimal() +
    labs(title = "Histogram: Original Data", x = "Value", y = "Density")

  hist_transformed <- ggplot(data.frame(Value = transformed_x), aes(x = Value)) +
    geom_histogram(binwidth = 1, fill = "lightpink", alpha = 0.6, aes(y = ..density..)) +
    theme_minimal() +
    labs(title = "Histogram: Transformed Data", x = "Value", y = "Density")

  density_original <- ggplot(data.frame(Value = x), aes(x = Value)) +
    geom_density(fill = "cyan3", alpha = 0.6) +
    theme_minimal() +
    labs(title = "Density Plot: Original Data", x = "Value", y = "Density")

  density_transformed <- ggplot(data.frame(Value = transformed_x), aes(x = Value)) +
    geom_density(fill = "lightpink", alpha = 0.6) +
    theme_minimal() +
    labs(title = "Density Plot: Transformed Data", x = "Value", y = "Density")

  if (plot_type == "scatter") {
    scatter_plots <- grid.arrange(scatter_original, scatter_transformed, ncol = 2)
    print(scatter_plots)
  } else if (plot_type == "histogram") {
    hist_plots <- grid.arrange(hist_original, hist_transformed, ncol = 2)
    print(hist_plots)
  } else if (plot_type == "density") {
    density_plots <- grid.arrange(density_original, density_transformed, ncol = 2)
    print(density_plots)
  } else {
    stop("Invalid plot_type. Choose 'scatter', 'histogram', or 'density'.")
  }
}
