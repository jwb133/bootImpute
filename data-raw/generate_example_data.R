#linear substantive model with quadratic covariate effect
set.seed(1234)
n <- 1000
x <- rnorm(n)
z <- rnorm(n)
#v is an auxiliary variable
v <- x+rnorm(n)
y <- 1+z+x+x^2+rnorm(n)

#make some x values missing
xobsxb <- (y-mean(y))/sd(y)
xobspr <- exp(xobsxb)/(1+exp(xobsxb))
x[runif(n)>xobspr] <- NA

linquad <- data.frame(y,z,x,v)

usethis::use_data(linquad, overwrite=TRUE)
