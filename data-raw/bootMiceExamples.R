library(mice)

head(linquad)

#bootstrap 10 times and impute each twice
imps <- bootMice(linquad, nBoot=10, nImp=2, seed=564764)
