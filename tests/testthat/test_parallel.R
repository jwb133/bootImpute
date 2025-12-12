context("parallel bootImpute testing")

test_that("Test bootImputeAnalyse using multiple cores", {
  expect_equal({
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
    result3 <- bootImputeAnalyse(result, myanalysis, nCores=2)
    identical(result2, result3)
  }, TRUE)
})


test_that("Test bootImpute using multiple cores", {
  expect_warning({
    set.seed(1234)

    n <- 100
    x <- rnorm(n)
    y <- x+rnorm(n)
    y[1:50] <- NA
    simData <- data.frame(x,y)

    myimp <- function(inputData,M) {
      mod <- lm(y~x, data=inputData)
      imps <- vector("list", M)
      for (i in 1:M) {
        imps[[i]] <- inputData
        imps[[i]]$y[is.na(inputData$y)] <- coef(mod)[1]+coef(mod)[2]*inputData$x[is.na(inputData$y)]+rnorm(sum(is.na(inputData$y)))
      }
      imps
    }

    result <- bootImpute(simData, myimp, nBoot=20, nImp=2, nCores=2, seed=7234, M=2)

  }, "It is recommended to use at least 200 bootstraps.")
})

test_that("Test bootImpute runs using multiple cores with mice", {
  expect_warning({
    set.seed(1234)

    n <- 100
    x <- rnorm(n)
    y <- x+rnorm(n)
    y[1:50] <- NA
    simData <- data.frame(x,y)

    result <- bootMice(simData, nBoot=20, nImp=2, nCores=2, seed=123)
  }, "It is recommended to use at least 200 bootstraps.")
})

test_that("Test bootImpute runs using multiple cores with mice with extra arguments", {
  expect_warning({
    set.seed(1234)

    n <- 100
    x <- rnorm(n)
    y <- x+rnorm(n)
    y[1:50] <- NA
    simData <- data.frame(x,y)

    result <- bootMice(simData, nBoot=20, nImp=2, nCores=2, seed=123, maxit=1)
  }, "It is recommended to use at least 200 bootstraps.")
})


test_that("If you use nCores>1 you must set seed for bootImpute", {
  expect_error({
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
  }, "If you specify nCores>1 you must set a seed.")
})

test_that("Test bootImputeAnalyse using multiple cores and additional analysisfun arguments", {
  expect_equal({
    set.seed(1234)

    n <- 100
    x <- rnorm(n)
    y <- x+rnorm(n)
    y[1:50] <- NA
    simData <- data.frame(x,y)

    myimp <- function(inputData,M) {
      mod <- lm(y~x, data=inputData)
      imps <- vector("list", M)
      for (i in 1:M) {
        imps[[i]] <- inputData
        imps[[i]]$y[is.na(inputData$y)] <- coef(mod)[1]+coef(mod)[2]*inputData$x[is.na(inputData$y)]+rnorm(sum(is.na(inputData$y)))
      }
      imps
    }

    result <- bootImpute(simData, myimp, nBoot=200, nImp=2, M=2, nCores=2, seed=123)

    myanalysis <- function(data) {
      mod <- lm(y~x, data=data)
      coef(mod)
    }

    result2 <- bootImputeAnalyse(result, myanalysis, nCores=2)

    #now define analysis function that requires model formula to be specified
    myanalysis <- function(data, formula) {
      mod <- lm(formula=formula, data=data)
      coef(mod)
    }
    #and now call bootImputeAnalyse, passing the required argument
    result3 <- bootImputeAnalyse(result, myanalysis, nCores=2, formula="y~x")
    identical(result2, result3)
  }, TRUE)
})


test_that("Testing stratified sampling works with parallel", {
  expect_identical({
    set.seed(1234)

    n <- 100
    sex <- factor(c(rep("m",n/2), rep("f",n/2)))
    x <- rnorm(n)
    y <- x+rnorm(n)
    y[1:25] <- NA
    simData <- data.frame(sex,x,y)

    myimp <- function(inputData, M) {
      mod <- lm(y~x, data=inputData)
      imps <- vector("list", M)
      for (i in 1:M) {
        imps[[i]] <- inputData
        imps[[i]]$y[is.na(inputData$y)] <- coef(mod)[1]+coef(mod)[2]*inputData$x[is.na(inputData$y)]+rnorm(sum(is.na(inputData$y)))
      }
      imps
    }

    result <- bootImpute(simData, myimp, nBoot=10, nImp=2, M=2, seed=12612,
                         strata="sex", nCores=2)

    as.numeric(table(result[[1]]$sex))
  }, c(50,50))
})
