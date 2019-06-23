
#' Simulated example data with continuous outcome and quadratic covariate effects
#'
#' A dataset containing simulated data where the outcome depends quadratically
#' on a partially observed covariate.
#'
#' @format A data frame with 1000 rows and 5 variables:
#' \describe{
#'   \item{y}{Continuous outcome}
#'   \item{z}{Fully observed covariate, with linear effect on outcome}
#'   \item{x}{Partially observed normally distributed covariate, with quadratic effect on outcome}
#'   \item{xsq}{The square of x, which thus has missing values also}
#'   \item{v}{An auxiliary variable (i.e. not contained in the substantive model)}
#' }
#'
"ex_linquad"

