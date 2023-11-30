#This is for function 5 (confusion matrix)

#' @title Creating Confusion Matrix
#'
#' @description enter description here.
#' @param enter parameters here
#'
#' @return enter outputs here
#' @author enter name here
#' @importFrom enter package name and functions needed here
#' @export
#' @examples
#' enter examples here


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
