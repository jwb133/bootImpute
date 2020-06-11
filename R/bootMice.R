#' Bootstrap then impute using mice
#'
#' Bootstraps an incomplete dataset and then imputes each bootstrap a number
#' of times using the mice package. The resulting list of bootstrapped then
#' imputed datasets can be analysed with \code{\link{bootImputeAnalyse}}.
#' To run this function requires the \code{mice} package to be installed.
#'
#' @param obsdata The data frame to be imputed.
#' @param nBoot The number of bootstrap samples to take. It is recommended
#' that you use a minimum of 200. If you specify \code{nCores>1}, \code{nBoot} must
#' be a multiple of the specified \code{nCores} value.
#' @param nImp The number of times to impute each bootstrap sample. Two
#' is recommended.
#' @param nCores The number of CPU cores to use. If specified greater than one,
#' bootImpute will impute using the number of cores specified.
#' @param seed Random number seed.
#' @param ... Other arguments that are to be passed to \code{mice}.
#' @return A list of imputed datasets.
#'
#' @example data-raw/bootMiceExamples.r
#'
#' @export
bootMice <- function(obsdata, nBoot=200, nImp=2, nCores=1, seed=NULL, ...) {
  bootImpute(obsdata, miceImpOnce, nBoot=nBoot, nImp=nImp, nCores=nCores, seed=seed, ...)
}

#a function that imputes once using mice with the specified options
#and returns the imputed dataset
miceImpOnce <- function(inputData, ...) {
  oneImp <- mice::mice(inputData, m=1, ...)
  mice::complete(oneImp)
}
