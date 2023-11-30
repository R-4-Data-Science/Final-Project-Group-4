#This is for function 4 (plotting the logistic curve)

#See Ch. 8.1.4 in the book for details on the first section here
#' @title Plotting a logistic curve
#'
#' @description enter description here.
#' @param enter parameters here
#'
#' @return enter outputs here
#' @author enter name here
#' @importFrom library(MASS) library(boot)
#' @export
#' @examples
#' enter examples here
set.seed(123)
n <- 100
X <- cbind(1, rnorm(n))
beta_true <- c(-1, 2)
pi_true <- 1 / (1 + exp(-X %*% beta_true))
y <- rbinom(n, 1, pi_true)

result <- logistic_regression(y, X)
plot_logistic_curve(X, result$coefficients, y)

y_pred_prob <- 1 / (1 + exp(-X %*% result$coefficients))
evaluation_result <- evaluate_classification(y, y_pred_prob)
print(evaluation_result$confusion_matrix)
print(evaluation_result$metrics)

plot_metrics_over_cutoff(y, y_pred_prob)

# Function to compute logistic regression coefficients
logistic_regression <- function(y, X, alpha = 0.05, num_bootstraps = 20) {
  # Initial values from least-squares formula
  initial_values <- solve(t(X) %*% X) %*% t(X) %*% y

  # Function to calculate logistic function
  logistic_function <- function(beta, X) {
    1 / (1 + exp(-X %*% beta))
  }

  # Function to calculate negative log-likelihood
  neg_log_likelihood <- function(beta, X, y) {
    p <- logistic_function(beta, X)
    -sum(y * log(p) + (1 - y) * log(1 - p))
  }

  # Optimize using the negative log-likelihood function
  result <- optim(par = initial_values, fn = neg_log_likelihood, X = X, y = y)
  beta_hat <- result$par

  # Bootstrap confidence intervals
  boot_func <- function(data, indices) {
    fit <- optim(par = initial_values, fn = neg_log_likelihood, X = X[indices, ], y = y[indices])
    return(fit$par)
  }
  boot_results <- boot(data = X, statistic = boot_func, R = num_bootstraps)
  ci <- t(sapply(1:ncol(boot_results$t), function(i) {
    quantile(boot_results$t[, i], c(alpha / 2, 1 - alpha / 2))
  }))

  return(list(
    coefficients = beta_hat,
    bootstrap_ci = ci,
    initial_values = initial_values
  ))
}

# Function to plot fitted logistic curve
plot_logistic_curve <- function(X, beta_hat, y) {
  x_range <- seq(min(X %*% beta_hat), max(X %*% beta_hat), length.out = 100)
  fitted_probs <- 1 / (1 + exp(-x_range))
  plot(x_range, fitted_probs, type = "l", col = "blue", lwd = 2, xlab = "X'beta", ylab = "Probability")
  points(X %*% beta_hat, y, col = "red", pch = 16)
}
