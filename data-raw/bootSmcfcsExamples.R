if (requireNamespace("smcfcs", quietly = TRUE)) {
  library(smcfcs)
  set.seed(564764)
  head(ex_linquad)
  #bootstrap 10 times and impute each twice
  imps <- bootSmcfcs(ex_linquad, nBoot=10, nImp=2,
                     smtype="lm", smformula="y~z+x+xsq",
                     method=c("","","norm","x^2",""))
}
