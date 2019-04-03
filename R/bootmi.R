#' Bootstrap then impute a partially observed dataset.
#'
#' @param obsdata The data frame to be imputed.
#' @param impfun A function which will will impute the missing values.
#' @param B The number of bootstrap samples to take.
#' @param M The number of times to impute each bootstrap sample.
#' @param ... Other parameters that are to be passed through to \code{impfun}.
#' @return A list of imputed datasets
#' @export
impute <- function(obsdata, impfun, B=200, M=2, ...) {
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
  SSE <- sum((ests-rowMeans(ests))^2)
  SSA <- M*sum((rowMeans(ests)-mean(ests))^2)
  MSE <- SSE/(B*(M-1))
  MSA <- SSA/(B-1)
  resVar <- MSE
  randIntVar <- (MSA-MSE)/M
  if (randIntVar<0) {
    randIntVar <- 0
    resVar <- (SSE+SSA)/(B*M-1)
  }


  pointEstimate <- mean(ests)
  print(paste("Boot MI point estimate: ", pointEstimate, sep=""))
  print(paste("Between bootstrap variance: ", randIntVar, sep=""))
  print(paste("Within bootstrap / between imputation variance: ", resVar, sep=""))
  varEstimate <- (1+1/B)*randIntVar + resVar/(B*M)

  # msw <- resVar
  # msb <- randIntVar*M + msw
  # vhDf <- ((msb*(bsSamples+1)+msw*((bsSamples-1)/bsM-bsSamples-1))^2) /
  #   (((msb*(bsSamples+1))^2/(bsSamples-1)) + (msw*(((bsSamples-1)/bsM-bsSamples-1)^2))/(bsSamples*(bsM-1)))
  # marCI[sim,6,] <- c(marEstimates[sim,6]-qt(0.975,vhDf)*marVarEstimates[sim,6]^0.5,
  #                    marEstimates[sim,6]+qt(0.975,vhDf)*marVarEstimates[sim,6]^0.5)
  result <- c(pointEstimate, varEstimate)
  result
}

