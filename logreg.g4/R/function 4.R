#This is for function 4 (plotting the logistic curve)

#' @title Plotting a Logistic Curve
#'
#' @description Plot a logistic curve that fits to the responses from a logistic
#' regression, where the x-axis represents a sequence of predicted beta values.
#' @param x A \code{matrix} of predictor variables.
#' @param y A \code{vector} of binary response variables.
#' @param beta_hat A \code{vector} of regression coefficients.
#' @return A plot of a fitted logistic curve.
#' @author Group 4
#' @import MASS
#' @export
#' @examples
#' set.seed(123)
#' n <- 200
#' x <- cbind(1, rnorm(n))
#' beta_true <- c(-1, 2)
#' pi_true <- 1 / (1 + exp(-x %*% beta_true))
#' y <- rbinom(n, 1, pi_true)
#' result <- logreg(x, y)
#' plot_logistic_curve(x, result, y)
plot_logistic_curve <- function(x, beta_hat, y) {
  x_range <- seq(min(x %*% beta_hat), max(x %*% beta_hat), length.out = 100)
  fitted_probs <- 1 / (1 + exp(-x_range, y))
  plot(x_range, fitted_probs, type = "l", col = "blue",
       lwd = 3, xlab = "X Beta", ylab = "Probability (y)",
       main = "Fitted Logistic Curve")
  points(x %*% beta_hat, y, col = "orange", pch = 16)
}
