context("parallel bootImpute testing")

test_that("Impute and analyse functions run when they should", {
  skip_on_cran()
  expect_error({
    set.seed(1234)
    impOnce <- function(inputData) {
      oneImp <- mice::mice(inputData, m=1)
      mice::complete(oneImp)
    }

    testdata <- data.frame(x=rnorm(100), y=rnorm(100))
    testdata$y[1:50] <- NA

    res <- bootImpute(testdata, impOnce, nBoot=6, nImp=2, nCores=2)
  }, NA)
})
