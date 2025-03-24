library(smcfcs)

head(ex_linquad)
#bootstrap twice and impute each twice
#in practice you should bootstrap many more times, e.g. at least 200
imps <- bootSmcfcs(ex_linquad, nBoot=2, nImp=2,
                   smtype="lm", smformula="y~z+x+I(x^2)",
                   method=c("","","norm",""), seed=564764)
