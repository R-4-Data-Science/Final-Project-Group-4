#This file is to code Function 1 (logistic regression).

#' @title Logistic Regression
#'
#' @description Perform a logistic regression using numerical optimization
#' to estimate the coefficient vector \code{beta_hat} by minimizing the log-likelihood.
#' @param x A \code{matrix} of predictor variables.
#' @param y A \code{vector} containing binary response variables (0 or 1).
#' @return A \code{numeric} giving the value of \code{beta_hat}
#' @author Ava White
#' @author Bukola Ayodele
#' @export
#' @examples
#' #generate random data
#' set.seed(123)
#' n <- 200
#' p <- 2
#' x <- matrix(rnorm(n * p), nrow = n, ncol = p)
#' y <- rbinom(n, 1, 0.5)
#' #use logreg function to predict beta
#' beta_hat <- logreg(x, y)
#' print(beta_hat)
logreg <- function(x, y){

  #generate initial values with least squares
  leastsquares <- function(x, y){
  initial_values <- solve(t(x) %*% x) %*% t(x) %*% y
  return(initial_values)
  }

  #compute pi
  probability_pi <- function(x, beta){
    pi <- 1 / (1 + exp(-x %*% beta))
    return(pi)
  }

  #compute log likelihood
  log_likelihood <- function(beta, x, y){
  pi <- probability_pi(x, beta)
  log_like <- -sum(y * log(pi) + (1 - y) * log(1 - pi))
  return(log_like)
  }

  result <- optim(par = leastsquares(x, y), fn = log_likelihood, x = x, y = y)
  beta_hat <- result$par
  return(beta_hat)

}
