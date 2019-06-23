if (requireNamespace("mice", quietly = TRUE)) {
  library(mice)

  set.seed(564764)

  head(ex_linquad)

  #bootstrap 10 times and impute each twice
  imps <- bootMice(ex_linquad, nBoot=10, nImp=2)
}
