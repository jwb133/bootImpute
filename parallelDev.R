
#write a wrapper function to call mice with one imputation and return
#the imputed dataset
impOnce <- function(inputData) {
  oneImp <- mice::mice(inputData, m=1)
  mice::complete(oneImp)
}

testdata <- data.frame(x=rnorm(100), y=rnorm(100))
testdata$y[1:50] <- NA

library(bootImpute)
bootImpute(testdata, impOnce, nBoot=200, nImp=2, nCores=2)


nCores <- 2
cl <- parallel::makeCluster(nCores, setup_strategy = "sequential")
parallel::clusterSetRNGStream(cl, 123)
obsdata <- testdata
impfun <- impOnce
nBootPerCore <- 100
nImp <- 2
parallel::clusterExport(cl, c("obsdata", "impfun", "nBootPerCore", "nImp"), envir=environment())
#    parallel::clusterExport(cl, impfun, envir=environment())
parImps <- parallel::parLapply(cl, X=1:nCores, fun = function(no){
  bootImpute::bootImpute(obsdata, impfun, nBootPerCore, nImp, nCores=1)
})
stopCluster(cl)


bootImpute(testdata, impOnce, 4, 2, nCores=2)

library(parallel)

testFun <- function(obsdata) {
  cl <- makeCluster(2, setup_strategy = "sequential")
  clusterSetRNGStream(cl, 123)
  clusterExport(cl, "obsdata", envir=environment())
  stopCluster(cl)
}
testFun(testdata)

obsdata <- testdata
clusterExport(cl, "testdata")
clusterExport(cl, "impOnce")
clusterEvalQ(cl, library(bootImpute))
test <- parLapply(cl, X=1:2, fun = function(no){
  bootImpute(testdata, impOnce, 5, 2)
})
imps <- lapply(test, unlist, use.names=FALSE)
stopCluster(cl)
