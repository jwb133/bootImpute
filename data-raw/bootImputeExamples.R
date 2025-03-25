#this example shows how you can use bootImpute to impute using the mice
#package. If you do want to impute using MICE you can instead use the
#bootMice function, which essentially contains the code below
library(mice)

#write a wrapper function to call mice generating M imputations
impM <- function(inputData,M) {
  miceImps <- mice::mice(inputData, m=M)
  imps <- vector("list", M)
  for (i in 1:M) {
    imps[[i]] <- mice::complete(miceImps,i)
  }
  imps
}

#bootstrap twice and impute each twice
#in practice you should bootstrap many more times, e.g. at least 200
#note you have to tell bootImpute how many imputations per bootstrap in
#nImp=2 and also pass through whatever your imp function argument is called
#for specifying number of imputations, which here is M=2.
imps <- bootImpute(linquad, impM, nBoot=2, nImp=2, M=2, seed=564764)
