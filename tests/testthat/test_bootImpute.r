context("bootImpute testing")

test_that("Impute and analyse functions run when they should", {
  expect_error({
    set.seed(1234)

    n <- 100
    x <- rnorm(n)
    y <- x+rnorm(n)
    y[1:50] <- NA
    simData <- data.frame(x,y)

    myimp <- function(inputData, M) {
      mod <- lm(y~x, data=inputData)
      imps <- vector("list", M)
      for (i in 1:M) {
        imps[[i]] <- inputData
        imps[[i]]$y[is.na(inputData$y)] <- coef(mod)[1]+coef(mod)[2]*inputData$x[is.na(inputData$y)]+rnorm(sum(is.na(inputData$y)))
      }
      imps
    }

    result <- bootImpute(simData, myimp, nBoot=200, nImp=2, M=2)

    myanalysis <- function(data) {
      data$x2 <- data$x^2
      mod <- lm(y~x+x2, data=data)
      coef(mod)
    }

    result2 <- bootImputeAnalyse(result, myanalysis)
  }, NA)
})

test_that("Should error when imp function does not return right number of imputations", {
  expect_error({
    set.seed(1234)

    n <- 100
    x <- rnorm(n)
    y <- x+rnorm(n)
    y[1:50] <- NA
    simData <- data.frame(x,y)

    myimp <- function(inputData, M) {
      mod <- lm(y~x, data=inputData)
      imps <- vector("list", M)
      for (i in 1:M) {
        imps[[i]] <- inputData
        imps[[i]]$y[is.na(inputData$y)] <- coef(mod)[1]+coef(mod)[2]*inputData$x[is.na(inputData$y)]+rnorm(sum(is.na(inputData$y)))
      }
      imps
    }

    result <- bootImpute(simData, myimp, nBoot=200, nImp=2, M=3)

    myanalysis <- function(data) {
      data$x2 <- data$x^2
      mod <- lm(y~x+x2, data=data)
      coef(mod)
    }

    result2 <- bootImputeAnalyse(result, myanalysis)
  }, NULL)
})

test_that("Should error when imp function does not return a list", {
  expect_error({
    set.seed(1234)

    n <- 100
    x <- rnorm(n)
    y <- x+rnorm(n)
    y[1:50] <- NA
    simData <- data.frame(x,y)

    myimp <- function(inputData, M) {
      mod <- lm(y~x, data=inputData)
      imp <- inputData
      imp$y[is.na(inputData$y)] <- coef(mod)[1]+coef(mod)[2]*inputData$x[is.na(inputData$y)]+rnorm(sum(is.na(inputData$y)))
      imp
    }

    result <- bootImpute(simData, myimp, nBoot=200, nImp=2, M=2)

    myanalysis <- function(data) {
      data$x2 <- data$x^2
      mod <- lm(y~x+x2, data=data)
      coef(mod)
    }

    result2 <- bootImputeAnalyse(result, myanalysis)
  }, NULL)
})

test_that("bootImpute warns when less than 200 bootstraps used", {
  expect_warning({
    set.seed(1234)

    n <- 100
    x <- rnorm(n)
    y <- x+rnorm(n)
    y[1:50] <- NA
    simData <- data.frame(x,y)

    myimp <- function(inputData, M) {
      mod <- lm(y~x, data=inputData)
      imps <- vector("list", M)
      for (i in 1:M) {
        imps[[i]] <- inputData
        imps[[i]]$y[is.na(inputData$y)] <- coef(mod)[1]+coef(mod)[2]*inputData$x[is.na(inputData$y)]+rnorm(sum(is.na(inputData$y)))
      }
      imps
    }

    result <- bootImpute(simData, myimp, nBoot=2, nImp=2, M=2)
  })
})

test_that("bootImputeAnalyse random intercept var zero warning check", {
  expect_warning({
    set.seed(1234)
    n <- 100
    x <- c(rep(1,n/2), rep(NA,n/2))
    simData <- data.frame(id=1:n, x=x)

    myimp <- function(inputData, M) {
      imps <- vector("list", M)
      for (i in 1:M) {
        imps[[i]] <- inputData
        imps[[i]]$x[is.na(inputData$x)] <- 1
      }
      imps
    }

    result <- bootImpute(simData, myimp, nBoot=200, nImp=2, M=2)

    myanalysis <- function(inputData) {
      mean(inputData$x)
    }

    result2 <- bootImputeAnalyse(result, myanalysis)
  })
})

test_that("Testing bootMice works", {
  expect_error({
    if (requireNamespace("mice", quietly = TRUE)) {
      library(mice)
      set.seed(564764)
      #bootstrap 10 times and impute each twice
      imps <- bootMice(ex_linquad, nBoot=10, nImp=2)
    }
  }, NA)
})

test_that("Testing bootSmcfcs works", {
  expect_error({
    if (requireNamespace("smcfcs", quietly = TRUE)) {
      library(mice)
      set.seed(564764)
      #bootstrap twice and impute each twice
      #in practice you should bootstrap many more times, e.g. at least 200
      imps <- bootSmcfcs(ex_linquad, nBoot=2, nImp=2,
                         smtype="lm", smformula="y~z+x+xsq",
                         method=c("","","norm","x^2",""))
    }
  }, NA)
})
