#this example shows how you can use bootImpute to impute using the mice
#package. If you do want to impute using MICE you can instead use the
#bootMice function, which essentially contains the code below
library(mice)

#write a wrapper function to call mice with one imputation and return
#the imputed dataset
impOnce <- function(inputData) {
  oneImp <- mice::mice(inputData, m=1)
  mice::complete(oneImp)
}

#bootstrap twice and impute each twice
#in practice you should bootstrap many more times, e.g. at least 200
imps <- bootImpute(ex_linquad, impOnce, nBoot=2, nImp=2, seed=564764)
