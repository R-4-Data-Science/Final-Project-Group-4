#This file is to code Function 1 (logistic regression).

#' @title Logistic Regression
#'
#' @description Perform a logistic regression using numerical optimization
#' to estimate the coefficient vector \code(beta).
#' @param predict A \code{matrix} of predictor variables.
#' @param response A \code{string} containing the function to be integrated. It
#' is assumed that \code{x} is used as the variable of interest.
#' @param beta A \code{vector} that contains the coefficients of the regression.
#' @param seed A \code{numeric} used to control the seed of the random number
#' generator used by this function.
#' @return A \code{list} containing the following attributes:
#' \describe{
#'      \item{B}{the estimated Beta coefficient}
#' @author Ava White
#' @importFrom stats
#' @export
#' @examples
#' set.seed(100)
#' logreg(predict = x, response = y, beta = Beta)
#' x <- matrix(rnorm(200), nrow = 20)
#' y <-
logreg
