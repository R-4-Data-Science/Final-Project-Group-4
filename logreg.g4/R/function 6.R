#this is for function 6 (plotting metrix from confusion matrix)

#' @title Plotting Confusion Matrix Metrics
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
