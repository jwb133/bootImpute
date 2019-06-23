#this example imputes using bootMice then analyses
if (requireNamespace("mice", quietly = TRUE)) {
  library(mice)

  set.seed(564764)

  imps <- bootMice(ex_linquad, nBoot=10, nImp=2)

  #analyse estimates
  #write a wapper to analyse an imputed dataset
  analyseImp <- function(inputData) {
    coef(lm(y~z+x+xsq,data=inputData))
  }
  ests <- bootImputeAnalyse(imps, analyseImp)
}
