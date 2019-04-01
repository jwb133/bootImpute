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
  imps <- vector("list", B*M+1)
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
  imps[[count]] <- c(B,M)
  names(imps[[count]]) <- c("B", "M")

  #return list of imputations
  imps
}

bootmi_analyse <- function(imps, analysisfun, ...) {
  B <- imps[[length(imps)]]['B']
  M <- imps[[length(imps)]]['M']
  ests <- array(0, dim=c(B,M))
  count <- 1
  for (b in 1:B) {
    for (m in 1:M) {
      ests[b,m] <- analysisfun(imps[[count]],...)
      count <- count + 1
    }
  }

  #fit one way model
}

