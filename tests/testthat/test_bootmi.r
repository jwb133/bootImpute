library(bootmi)

context("bootmi testing")

test_that("Impute and analyse functions run when they should", {
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

    result <- impute(simData, myimp, nBoot=200, nImp=2)

    myanalysis <- function(data) {
      mean(data$y)
    }

    result2 <- bootmi_analyse(result, myanalysis)
  }, NA)
})
