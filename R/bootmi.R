#' Bootstrap then impute a partially observed dataset
#'
#' @param obsdata The data frame to be imputed.
#' @param impfun A function which will will impute the missing values.
#' @param B The number of bootstrap samples to take.
#' @param M The number of times to impute each bootstrap sample.
#' @param ... Other parameters that are to be passed through to \code{impfun}.
#' @return A list of imputed datasets
#' @export
impute <- function(obsdata, impfun, B=200, M=2, ...) {
  n <- dim(obsdata)[1]
  imps <- vector("list", B*M)
  count <- 1
  for (b in 1:B) {
    #take bootstrap sample
    bsIndices <- sample(1:n, replace=TRUE)
    #impute M times
    for (m in 1:M) {
      imps[[count]] <- impfun(obsdata[bsIndices,], ...)
      count <- count + 1
    }
  }
  attributes(imps) <- list(B=B, M=M)

  #return list of imputations
  imps
}

#' Analyse bootstrapped and imputed estimates
#'
#' @param imps The list of imputed datasets returned by \code{impute}
#' @param analysisfun A function which when applied to a single dataset returns
#' the estimate of the parameter(s) of interest.
#' @param ... Other parameters that are to be passed through to \code{analysisfun}.
#' @return A vector containing the point estimate(s), variance estimates, and
#' degrees of freedom.
#' @export
bootmi_analyse <- function(imps, analysisfun, ...) {
  B <- attributes(imps)$B
  M <- attributes(imps)$M
  ests <- array(0, dim=c(B,M))
  count <- 1
  for (b in 1:B) {
    for (m in 1:M) {
      ests[b,m] <- analysisfun(imps[[count]],...)
      count <- count + 1
    }
  }

  #fit one way model
  SSW <- sum((ests-rowMeans(ests))^2)
  SSB <- M*sum((rowMeans(ests)-mean(ests))^2)
  MSW <- SSW/(B*(M-1))
  MSB <- SSB/(B-1)
  resVar <- MSW
  randIntVar <- (MSB-MSW)/M
  if (randIntVar<0) {
    randIntVar <- 0
    resVar <- (SSW+SSB)/(B*M-1)
  }
  pointEstimate <- mean(ests)
  varEstimate <- (1+1/B)*randIntVar + resVar/(B*M)
  df <- (varEstimate^2)/((((B+1)/(B*M))^2*MSB^2 / (B-1)) + MSW^2/(B*M^2*(M-1)))

  print(paste("Boot MI point estimate: ", pointEstimate, sep=""))
  print(paste("Between bootstrap variance: ", randIntVar, sep=""))
  print(paste("Within bootstrap / between imputation variance: ", resVar, sep=""))
  print(paste("Degrees of freedom: ", df, sep=""))

  result <- c(pointEstimate, varEstimate, randIntVar, resVar, df)
  names(result) <- c("Point estimate", "Variance", "Between bootstrap var.",
                    "Between imputation var.", "DF")
  result
}

