#this example shows how you can use bootImpute to impute using the mice
#package. If you do want to impute using MICE you can instead use the
#bootMice function, which essentially contains the code below
if (requireNamespace("mice", quietly = TRUE)) {
  library(mice)

  set.seed(564764)

  #write a wrapper function to call mice with one imputation and return
  #the imputed dataset
  impOnce <- function(inputData) {
    oneImp <- mice::mice(inputData, m=1)
    mice::complete(oneImp)
  }

  #bootstrap 10 times and impute each twice
  imps <- bootImpute(ex_linquad, impOnce, nBoot=10, nImp=2)
}
