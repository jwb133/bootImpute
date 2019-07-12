#' Bootstrap then impute using smcfcs
#'
#' Bootstraps an incomplete dataset and then imputes each bootstrap a number
#' of times using the smcfcs package. The resulting list of bootstrapped then
#' imputed datasets can be analysed with \code{\link{bootImputeAnalyse}}.
#' To run this function requires the \code{smcfcs} package to be installed.
#'
#' @param obsdata The data frame to be imputed.
#' @param nBoot The number of bootstrap samples to take. It is recommended
#' that you use a minimum of 200.
#' @param nImp The number of times to impute each bootstrap sample. Two
#' is recommended.
#' @param ... Other arguments that are to be passed to \code{smcfcs}.
#' @return A list of imputed datasets.
#'
#' @example data-raw/bootSmcfcsExamples.r
#'
#' @export
bootSmcfcs <- function(obsdata, nBoot=200, nImp=2, ...) {
  bootImpute(obsdata, smcfcsImpOnce, nBoot=nBoot, nImp=nImp, ...)
}

#a function that imputes once using smcfcs with the specified options
#and returns the imputed dataset
smcfcsImpOnce <- function(inputData, ...) {
  oneImp <- smcfcs::smcfcs(inputData, m=1, ...)
  oneImp$impDatasets[[1]]
}
