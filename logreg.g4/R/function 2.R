#This is function 2 (least squares)

#' @title Least Squares Function
#'
#' @description
#' @param x_range A \code{vector} of dimension 2 used to denote the integration
#' region of interest, i.e. [a, b].
#' @param fun A \code{string} containing the function to be integrated. It
#' is assumed that \code{x} is used as the variable of interest.
#' @param B A \code{numeric} (integer) used to denote the number of simulations.
#' @param seed A \code{numeric} used to control the seed of the random number
#' generator used by this function.
#' @return A \code{list} containing the following attributes:
#' \describe{
#'      \item{I}{Estimated value of the integral}
#'      \item{var}{Estimated variance of the estimator}
#' }
#' @author Ava White
#' @importFrom stats runif
#' @export
#' @examples
#' mc_int(x_range = c(0,1), fun = "x^2", B = 10^5)
#' mc_int(x_range = c(0,1), fun = "x^2*sin(x^2/pi)", B = 10^5)
