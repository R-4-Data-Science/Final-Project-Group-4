#This is for function 3 (bootstrapping)

#' @title Bootstrapping
#'
#' @description Generate confidence intervals using a bootstrap method for
#' the logistic regression coefficients.
#' @param x a \code{matrix} of predictor variables.
#' @param y a \code{vector} of binary response variables.
#' @param alpha a \code{numeric} of a significance level
#' @param bootstrap a \code{numeric} of the number of bootstraps to be performed
#' @return
#' @author Ava White
#' @author Bukola Ayodele
#' @importFrom
#' @export
#' @examples
#' x <-
#' y <-
#' alpha <- 0.05
#' bootstrap <- 20
#' bootstrap_CI(x, y, alpha, bootstrap)
bootstrap_CI <- function(x, y, alpha, bootstrap){

  set.seed(123)
  n <- length(x)
  beta_boot <- matrix(NA, ncol = bootstrap, nrow = length(logreg(x, y)))

  #begin bootstraps
  for(i in 1:bootstrap){

  #sampling with replacement
   index <- sample(1:n, replace = TRUE)
   xboot <- x[index, ]
   yboot <- y[index, ]

   beta_boot[, i] <- logreg(xboot, yboot)
  }

  #generate confidence intervals
  lowerbound <- quantile(beta_boot, 1 - (1 - alpha))
  upperbound <- quantile(beta_boot, (1 - alpha))

  #outputs
  bootstrap_result <- beta_boot
  return(beta_boot)
  CI_result <- c(lowerbound, upperbound)
  return(CI_result)
}
