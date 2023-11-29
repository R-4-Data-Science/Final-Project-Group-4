#This file is to code Function 1 (logistic regression).

#' @title Logistic Regression
#'
#' @description Perform a logistic regression using numerical optimization
#' to estimate the coefficient vector \code(beta) by minimizing the log-likelihood.
#' @param x A \code{matrix} of predictor variables.
#' @param y A \code{vector} containing binary response variables (0 or 1).
#' @param beta A \code{vector} that contains the coefficients of the regression.
#' @return A \code{numeric} giving the value of \code{beta}
#' @author Ava White
#' @author Bukola Ayodele
#' @importFrom
#' @export
#' @examples
#' set.seed(100)
#' x <- matrix(rnorm(200), nrow = 20)
#' y <- rnorm(200)
#' beta <-
#' logreg(x, y, beta)
logreg <- function(x, y, beta){

  #generate initial values with least squares
  least_squares <- solve(t(x)%*%x%*%t(x)%*%y)

  #compute pi
  probability_pi <- function(x, beta){
    pi <- 1 / (1 + exp(-x %*% beta))
    return(pi)
  }

  #compute log likelihood
  log_likelihood <- function(yi, pi){
  log_like <- sum(y * log(pi) + (1 - y) * log(1 - pi))
  return(log_like)
  }
}
