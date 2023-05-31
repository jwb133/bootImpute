bootImpute bootstraps then imputes each of the bootstrapped datasets multiple times. An analysis model can then be fitted to each imputed dataset, and inferences are made using an approach developed by von Hippel.

At present, the bootImpute package is not available via CRAN, due to an inadvertent policy violation on my part. As such, you can install bootImpute using
```{r}
install.packages("devtools")
devtools::install_github("jwb133/bootImpute")
```
