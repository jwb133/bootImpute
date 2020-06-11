#' Bootstrap then impute an incomplete dataset
#'
#' Bootstraps an incomplete dataset and then imputes each bootstrap a number
#' of times. The resulting list of bootstrapped then imputed datasets can
#' be analysed with \code{\link{bootImputeAnalyse}}.
#'
#' The \code{impfun} must be a function which when passed an incomplete datasets
#' and possibly additional arguments, returns a single imputed data frame. Depending
#' on what imputation function returns by default, you may need to write a small
#' wrapper function that calls the imputation procedure once and returns the
#' imputed dataset. See the Example for an illustration with the \code{mice}
#' package. To improve computation times, \code{bootImpute} now supports
#' multiple cores through the \code{nCores} argument which uses the \code{parallel}
#' package.
#'
#' @param obsdata The data frame to be imputed.
#' @param impfun A function which when passed an incomplete dataset will
#' return a single imputed data frame.
#' @param nBoot The number of bootstrap samples to take. It is recommended
#' that you use a minimum of 200. If you specify \code{nCores>1}, \code{nBoot} must
#' be a multiple of the specified \code{nCores} value.
#' @param nImp The number of times to impute each bootstrap sample. Two
#' is recommended.
#' @param nCores The number of CPU cores to use. If specified greater than one,
#' bootImpute will impute using the number of cores specified.
#' @param seed Random number seed.
#' @param ... Other parameters that are to be passed through to \code{impfun}.
#' @return A list of imputed datasets.
#'
#' @example data-raw/bootImputeExamples.r
#'
#' @export
bootImpute <- function(obsdata, impfun, nBoot=200, nImp=2, nCores=1, seed=NULL, ...) {
  if (nBoot<200) {
    warning("It is recommended to use at least 200 bootstraps.")
  }
  n <- dim(obsdata)[1]
  imps <- vector("list", nBoot*nImp)
  count <- 1

  if (nCores>1) {
    #use multiple cores
    if ((nBoot %% nCores)!=0) stop("nBoot must be a multiple of nCores.")
    nBootPerCore <- nBoot/nCores

    #the setup_strategy argument here is to temporarily deal with
    #this issue: https://github.com/rstudio/rstudio/issues/6692
    cl <- parallel::makeCluster(nCores, setup_strategy = "sequential")
    if (!is.null(seed)) {
      parallel::clusterSetRNGStream(cl, seed)
    }
    parallel::clusterExport(cl, c("obsdata", "impfun", "nBootPerCore", "nImp"),
                            envir=environment())
    parImps <- parallel::parLapply(cl, X=1:nCores, fun = function(no){
      bootImpute(obsdata, impfun, nBoot=nBootPerCore, nImp=nImp, nCores=1, ...)
    }, ...)
    parallel::stopCluster(cl)

    imps <- do.call(c, parImps)

  } else {
    if (!is.null(seed)) {
      set.seed(seed)
    }

    for (b in 1:nBoot) {
      #take bootstrap sample
      bsIndices <- sample(1:n, replace=TRUE)
      #impute nImp times
      for (m in 1:nImp) {
        imps[[count]] <- impfun(obsdata[bsIndices,], ...)
        count <- count + 1
      }
    }
  }
  attributes(imps) <- list(nBoot=nBoot, nImp=nImp)

  #return list of imputations
  imps
}

#' Analyse bootstrapped and imputed estimates
#'
#' Applies the user specified analysis function to each imputed dataset contained
#' in \code{imps}, then calculates estimates, confidence intervals and p-values
#' for each parameter, as proposed by von Hippel (2018).
#'
#' @param imps The list of imputed datasets returned by \code{\link{bootImpute}}
#' @param analysisfun A function which when applied to a single dataset returns
#' the estimate of the parameter(s) of interest.
#' @param ... Other parameters that are to be passed through to \code{analysisfun}.
#' @param quiet Specify whether to print a table of estimates, confidence intervals
#' and p-values.
#' @return A vector containing the point estimate(s), variance estimates, and
#' degrees of freedom.
#'
#' @references von Hippel PT, Bartlett JW. Maximum likelihood multiple imputation: faster,
#' more efficient imputation without posterior draws. arXiv, 2019, 1210.0870v10
#'  \url{https://arxiv.org/pdf/1210.0870v10.pdf}
#'
#' @import stats
#'
#' @example data-raw/bootImputeAnalyseExamples.r
#'
#'
#' @export
bootImputeAnalyse <- function(imps, analysisfun, ..., quiet=FALSE) {
  nBoot <- attributes(imps)$nBoot
  nImp <- attributes(imps)$nImp

  #find out how many parameters are returned by analysisFun
  firstResult <- analysisfun(imps[[1]],...)
  nParms <- length(firstResult)
  ests <- array(0, dim=c(nBoot,nImp,nParms))
  count <- 1
  for (b in 1:nBoot) {
    for (m in 1:nImp) {
      ests[b,m,] <- analysisfun(imps[[count]],...)
      count <- count + 1
    }
  }

  #fit one way model, separately for each parameter
  est <- array(0, dim=nParms)
  var <- array(0, dim=nParms)
  ci <- array(0, dim=c(nParms,2))
  df <- array(0, dim=nParms)

  for (i in 1:nParms) {
    SSW <- sum((ests[,,i]-rowMeans(ests[,,i]))^2)
    SSB <- nImp*sum((rowMeans(ests[,,i])-mean(ests[,,i]))^2)
    MSW <- SSW/(nBoot*(nImp-1))
    MSB <- SSB/(nBoot-1)
    resVar <- MSW
    randIntVar <- (MSB-MSW)/nImp
    if (randIntVar <= 0) {
      warning(paste("Parameter ",i," has an estimated between bootstrap variance of zero. You should re-run with a larger nBoot value.",sep=""))
      randIntVar <- 0
      resVar <- (SSW+SSB)/(nBoot*nImp-1)
    }
    est[i] <- mean(ests[,,i])
    var[i] <- (1+1/nBoot)*randIntVar + resVar/(nBoot*nImp)
    df[i] <- (var[i]^2)/((((nBoot+1)/(nBoot*nImp))^2*MSB^2 / (nBoot-1)) + MSW^2/(nBoot*nImp^2*(nImp-1)))
    #prevent df from going below 3
    df[i] <- max(3,df[i])
    ci[i,] <- c(est[i]-stats::qt(0.975,df[i])*var[i]^0.5, est[i]+stats::qt(0.975,df[i])*var[i]^0.5)
  }

  if (quiet==FALSE) {
    resTable <- array(0, dim=c(nParms,5))
    resTable[,1] <- est
    resTable[,2] <- var^0.5
    resTable[,3] <- ci[,1]
    resTable[,4] <- ci[,2]
    resTable[,5] <- 2*stats::pt(abs(est/var^0.5), df=df,lower.tail = FALSE)

    colnames(resTable) <- c("Estimate", "Std. error", "95% CI lower", "95% CI upper", "p")
    rownames(resTable) <- names(firstResult)
    print(resTable)
  }

  list(ests=est, var=var, ci=ci, df=df)
}

