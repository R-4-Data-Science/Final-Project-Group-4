#this is for function 6 (plotting metrix from confusion matrix)

#' @title Plotting Confusion Matrix Metrics
#'
#' @description Create a plot that depicts the metrics of model performance
#' produced by the 'matrix_metrics' function (prevalence, accuracy,
#' sensitivity, specificity, false discovery rate, and diagnostic odds ratio)
#' @param y A \code{vector} of binary response variables.
#' @param pi_prob A \code{vector} of predicted probabilities.
#' @param metric Specifies which metric you want to visualize.
#' @param cutoff_grid A range of cutoff points.
#' @return A plot of your chosen metric over a cutoff grid.
#' @author Group 4
#' @export
#' @examples
#' set.seed(123)
#' n <- 200
#' x <- cbind(1, rnorm(n))
#' beta_true <- c(-1, 2)
#' pi_true <- 1 / (1 + exp(-x %*% beta_true))
#' y <- rbinom(n, 1, pi_true)
#' plot_metrics(y, pi_prob, metric = "sensitivity")
#' plot_metrics(y, pi_prob, metric = "false_discovery_rate")
plot_metrics <- function(y, pi_prob, metric, cutoff_grid = seq(0.1, 0.9, by = 0.1)) {
  metrics_list <- sapply(cutoff_grid, function(cutoff) {
  metrics_result <- matrix_metrics(y, pi_prob, cutoff = cutoff)
  return(metrics_result$metrics[[metric]])
  })
  plot(cutoff_grid, metrics_list, type = "l", col = "blue", lwd = 2,
       xlab = "Cutoff", ylab = metric)
  title(paste("Plot of", tolower(metric), "over Cut-off Values"))
}

