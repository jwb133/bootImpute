library(mice)

head(ex_linquad)

#bootstrap 10 times and impute each twice
imps <- bootMice(ex_linquad, nBoot=10, nImp=2, seed=564764)
