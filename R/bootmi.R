#' Bootstrap then impute a partially observed dataset
#'
#' @param obsdata The data frame to be imputed.
#' @param impfun A function which will will impute the missing values.
#' @param B The number of bootstrap samples to take.
#' @param M The number of times to impute each bootstrap sample.
#' @param ... Other parameters that are to be passed through to \code{impfun}.
#' @return A list of imputed datasets
#' @export
impute <- function(obsdata, impfun, nBoot=200, nImp=2, ...) {
  n <- dim(obsdata)[1]
  imps <- vector("list", nBoot*nImp)
  count <- 1
  for (b in 1:nBoot) {
    #take bootstrap sample
    bsIndices <- sample(1:n, replace=TRUE)
    #impute nImp times
    for (m in 1:nImp) {
      imps[[count]] <- impfun(obsdata[bsIndices,], ...)
      count <- count + 1
    }
  }
  attributes(imps) <- list(nBoot=nBoot, nImp=nImp)

  #return list of imputations
  imps
}

#' Analyse bootstrapped and imputed estimates
#'
#' @param imps The list of imputed datasets returned by \code{impute}
#' @param analysisfun A function which when applied to a single dataset returns
#' the estimate of the parameter(s) of interest.
#' @param ... Other parameters that are to be passed through to \code{analysisfun}.
#' @param quiet Specify whether to print output or not.
#' @return A vector containing the point estimate(s), variance estimates, and
#' degrees of freedom.
#' @export
bootmi_analyse <- function(imps, analysisfun, ..., quiet=FALSE) {
  nBoot <- attributes(imps)$nBoot
  nImp <- attributes(imps)$nImp
  ests <- array(0, dim=c(nBoot,nImp))
  count <- 1
  for (b in 1:nBoot) {
    for (m in 1:nImp) {
      ests[b,m] <- analysisfun(imps[[count]],...)
      count <- count + 1
    }
  }

  #fit one way model
  SSW <- sum((ests-rowMeans(ests))^2)
  SSB <- nImp*sum((rowMeans(ests)-mean(ests))^2)
  MSW <- SSW/(nBoot*(nImp-1))
  MSB <- SSB/(nBoot-1)
  resVar <- MSW
  randIntVar <- (MSB-MSW)/nImp
  if (randIntVar<0) {
    randIntVar <- 0
    resVar <- (SSW+SSB)/(nBoot*nImp-1)
  }
  pointEstimate <- mean(ests)
  varEstimate <- (1+1/nBoot)*randIntVar + resVar/(nBoot*nImp)
  df <- (varEstimate^2)/((((nBoot+1)/(nBoot*nImp))^2*MSB^2 / (nBoot-1)) + MSW^2/(nBoot*nImp^2*(nImp-1)))
  #prevent df from going below 3
  df <- max(3,df)

  if (quiet==FALSE) {
    print(paste("Boot MI point estimate: ", pointEstimate, sep=""))
    print(paste("Between bootstrap variance: ", randIntVar, sep=""))
    print(paste("Within bootstrap / between imputation variance: ", resVar, sep=""))
    print(paste("Degrees of freedom: ", df, sep=""))
  }

  result <- c(pointEstimate, varEstimate, randIntVar, resVar, df)
  names(result) <- c("Point estimate", "Variance", "Between bootstrap var.",
                    "Between imputation var.", "DF")
  result
}

