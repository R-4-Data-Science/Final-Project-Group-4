#This file is to code Function 2 (least squares).

#' @title Least Squares
#'
#' @description Use a least squares formula to generate initial
#' values for optimization using the 'logreg' function.
#' @param x A \code{matrix} of predictor variables.
#' @param y A \code{vector} containing binary response variables (0 or 1).
#' @return A \code{numeric} giving the value of \code{initial_values}
#' @author Group 4
#' @export
#' @examples
#' #generate random data
#' set.seed(123)
#' n <- 200
#' p <- 2
#' x <- matrix(rnorm(n * p), nrow = n, ncol = p)
#' y <- rbinom(n, 1, 0.5)
#' #use leastsquares function to generate initial values
#' initial_values <- leastsquares(x, y)
#' print(initial_values)
leastsquares <- function(x, y){

initial_values <- solve(t(x) %*% x) %*% t(x) %*% y

return(initial_values)
}
