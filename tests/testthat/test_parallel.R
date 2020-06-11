context("parallel bootImpute testing")

test_that("Test bootImpute using multiple cores", {
  skip_on_cran()
  expect_equal({
    set.seed(1234)

    n <- 100
    x <- rnorm(n)
    y <- x+rnorm(n)
    y[1:50] <- NA
    simData <- data.frame(x,y)

    myimp <- function(inputData) {
      mod <- lm(y~x, data=inputData)
      imp <- inputData
      imp$y[is.na(inputData$y)] <- coef(mod)[1]+coef(mod)[2]*inputData$x[is.na(inputData$y)]+rnorm(sum(is.na(inputData$y)))
      imp
    }

    result <- bootImpute(simData, myimp, nBoot=200, nImp=2, nCores=2)

    myanalysis <- function(data) {
      data$x2 <- data$x^2
      mod <- lm(y~x+x2, data=data)
      coef(mod)
    }

    result2 <- bootImputeAnalyse(result, myanalysis)
    result3 <- bootImputeAnalyse(result, myanalysis, nCores=2)
    identical(result2, result3)
  }, TRUE)
})

test_that("Test bootImpute using multiple cores with mice", {
  skip_on_cran()
  expect_equal({
    set.seed(1234)

    n <- 100
    x <- rnorm(n)
    y <- x+rnorm(n)
    y[1:50] <- NA
    simData <- data.frame(x,y)

    result <- bootMice(simData, nBoot=200, nImp=2, nCores=2)

    myanalysis <- function(data) {
      data$x2 <- data$x^2
      mod <- lm(y~x+x2, data=data)
      coef(mod)
    }

    result2 <- bootImputeAnalyse(result, myanalysis)
    result3 <- bootImputeAnalyse(result, myanalysis, nCores=2)
    identical(result2, result3)
  }, TRUE)
})
