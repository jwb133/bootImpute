#this example imputes using bootMice then analyses
if (requireNamespace("mice", quietly = TRUE)) {
  library(mice)

  set.seed(564764)

  #bootstrap twice and impute each twice
  #in practice you should bootstrap many more times, e.g. at least 200
  imps <- bootMice(ex_linquad, nBoot=2, nImp=2)

  #analyse estimates
  #write a wapper to analyse an imputed dataset
  analyseImp <- function(inputData) {
    coef(lm(y~z+x+xsq,data=inputData))
  }
  ests <- bootImputeAnalyse(imps, analyseImp)
}
