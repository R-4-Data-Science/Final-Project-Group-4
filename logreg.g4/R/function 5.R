#This is for function 5 (confusion matrix)

#' @title Creating Confusion Matrix
#'
#' @description Create a table for visualizing how well a model performs,
#' specifically in terms of prevalence, accuracy, sensitivity, specificity,
#' false discovery rate, and diagnostic odds ratio.
#' @param y A \code{vector} of binary response variables.
#' @param pi_prob A \code{vector} of predicted probabilities.
#' @param cutoff A cut-off value for prediction that assigns '1' to values
#' higher and '0' to values lower.
#' @return A confusion matrix and values for the 6 target metrics.
#' @author Group 4
#' @export
#' @examples
#' set.seed(123)
#' n <- 200
#' x <- cbind(1, rnorm(n))
#' beta_true <- c(-1, 2)
#' pi_true <- 1 / (1 + exp(-x %*% beta_true))
#' y <- rbinom(n, 1, pi_true)
#' result <- logreg(x, y)
#' pi_prob <- 1 / (1 + exp(-x %*% result))
#' metrics_result <- matrix_metrics(y, pi_prob)
#' print(metrics_result$confusion_matrix)
#' print(metrics_result$metrics)
matrix_metrics <- function(y, pi_prob, cutoff = 0.5) {
  pi <- ifelse(pi_prob > cutoff, 1, 0)
  confusion_matrix <- table(Actual = y, Predicted = pi)

  prevalence <- mean(y)
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
