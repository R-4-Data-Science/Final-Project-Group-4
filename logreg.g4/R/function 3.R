#This is for function 3 (bootstrapping)

#' @title Bootstrapping
#'
#' @description Complete a bootstrap with a defined number of bootstraps and
#' significance level (alpha) and generate confidence intervals.
#' @param x A \code{matrix} of predictor variables.
#' @param y A \code{vector} of binary response variables.
#' @param alpha The significance level
#' @param bootstrap The number of bootstraps to be performed
#' @return A \code{list} giving the values of \code{bootstrap_result} and
#' \code{CI_result}
#' @author Group 4
#' @export
#' @examples
#' #generate random data
#' set.seed(123)
#' n <- 200
#' p <- 2
#' x <- matrix(rnorm(n * p), nrow = n, ncol = p)
#' y <- rbinom(n, 1, 0.5)
#' alpha <- 0.05
#' bootstrap <- 20
#' bootstrap_result <- bootstrap_CI(x, y, alpha, bootstrap)
#' print(bootstrap_result)
bootstrap_CI <- function(x, y, alpha, bootstrap = 20){

  n <- nrow(x)
  beta_boot <- matrix(NA, ncol = bootstrap, nrow = length(logreg(x, y)))

  #begin bootstraps
  for(i in 1:bootstrap){

  #sampling with replacement
   index <- sample(1:n, replace = TRUE)
   xboot <- x[index, , drop = FALSE]
   yboot <- y[index]

   beta_boot[, i] <- logreg(xboot, yboot)
  }

  #generate confidence intervals
  lowerbound <- quantile(beta_boot, 1 - (1 - alpha))
  upperbound <- quantile(beta_boot, (1 - alpha))

  #outputs
  bootstrap_result <- beta_boot
  CI_result <- c(lowerbound, upperbound)
  return(list(bootstrap_result = bootstrap_result, CI_result = CI_result))
}
