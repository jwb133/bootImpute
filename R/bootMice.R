#' Bootstrap then impute using mice
#'
#' Bootstraps an incomplete dataset and then imputes each bootstrap a number
#' of times using the mice package. The resulting list of bootstrapped then
#' imputed datasets can be analysed with \code{\link{bootImputeAnalyse}}.
#' To run this function requires the \code{mice} package to be installed.
#'
#' @param obsdata The data frame to be imputed.
#' @param nBoot The number of bootstrap samples to take. It is recommended
#' that you use a minimum of 200.
#' @param nImp The number of times to impute each bootstrap sample. Two
#' is recommended.
#' @param ... Other arguments that are to be passed to \code{mice}.
#' @return A list of imputed datasets.
#'
#' @example data-raw/bootMiceExamples.r
#'
#' @export
bootMice <- function(obsdata, nBoot=200, nImp=2, ...) {
  if (!requireNamespace("mice", quietly = TRUE)) {
    stop("Package \"mice\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  bootImpute(obsdata, miceImpOnce, nBoot=nBoot, nImp=nImp, ...)
}

#a function that imputes once using mice with the specified options
#and returns the imputed dataset
miceImpOnce <- function(inputData, ...) {
  oneImp <- mice::mice(inputData, m=1, ...)
  mice::complete(oneImp)
}
