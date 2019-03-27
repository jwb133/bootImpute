bootmi_impute <- function(obsdata, impfun, B=200, M=2) {
  imps <- vector("list", B*M)
  count <- 1
  for (b in 1:B) {
    #take bootstrap sample
    bsIndices <- sample(1:n, replace=TRUE)
    #impute M times
    for (m in 1:M) {
      imps[[count]] <- impfun(obsdata[bsIndices,])
      count <- count + 1
    }
  }
  #return list of imputations
  imps
}

bootmi_analyse <- function(imps, analysisfun) {

}

