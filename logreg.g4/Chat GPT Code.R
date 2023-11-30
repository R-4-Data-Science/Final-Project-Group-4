# Install necessary packages if not already installed
if (!requireNamespace("MASS", quietly = TRUE)) {
  install.packages("MASS")
}

if (!requireNamespace("boot", quietly = TRUE)) {
  install.packages("boot")
}

# Load required libraries
library(MASS)
library(boot)

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

# Function to create confusion matrix and related metrics
evaluate_classification <- function(y_true, y_pred_prob, cutoff = 0.5) {
  y_pred <- ifelse(y_pred_prob > cutoff, 1, 0)
  confusion_matrix <- table(Actual = y_true, Predicted = y_pred)
  
  prevalence <- mean(y_true)
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  sensitivity <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
  false_discovery_rate <- confusion_matrix[1, 2] / sum(confusion_matrix[, 2])
  diagnostic_odds_ratio <- (sensitivity / (1 - specificity))
  
  metrics <- c(
    prevalence = prevalence,
    accuracy = accuracy,
    sensitivity = sensitivity,
    specificity = specificity,
    false_discovery_rate = false_discovery_rate,
    diagnostic_odds_ratio = diagnostic_odds_ratio
  )
  
  return(list(confusion_matrix = confusion_matrix, metrics = metrics))
}

# Function to plot metrics over a grid of cutoff values
plot_metrics_over_cutoff <- function(y_true, y_pred_prob, cutoff_grid = seq(0.1, 0.9, by = 0.1)) {
  metrics_list <- lapply(cutoff_grid, function(cutoff) {
    evaluate_classification(y_true, y_pred_prob, cutoff = cutoff)$metrics
  })
  
  metrics_df <- data.frame(matrix(unlist(metrics_list), nrow = length(cutoff_grid), byrow = TRUE))
  colnames(metrics_df) <- c("Prevalence", "Accuracy", "Sensitivity", "Specificity", "FalseDiscoveryRate", "DiagnosticOddsRatio")
  
  plot(cutoff_grid, metrics_df$Accuracy, type = "l", col = "blue", lwd = 2, xlab = "Cutoff", ylab = "Accuracy")
  lines(cutoff_grid, metrics_df$Sensitivity, col = "red", lty = 2)
  lines(cutoff_grid, metrics_df$Specificity, col = "green", lty = 2)
  legend("topright", legend = c("Accuracy", "Sensitivity", "Specificity"), col = c("blue", "red", "green"), lty = 1:2)
}

# Example usage:
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
