if (requireNamespace("smcfcs", quietly = TRUE)) {
  library(smcfcs)
  set.seed(564764)
  head(ex_linquad)
  #bootstrap twice and impute each twice
  #in practice you should bootstrap many more times, e.g. at least 200
  imps <- bootSmcfcs(ex_linquad, nBoot=2, nImp=2,
                     smtype="lm", smformula="y~z+x+xsq",
                     method=c("","","norm","x^2",""))
}
