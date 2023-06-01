# 1.2.1
Updating Jonathan's contact email address and reference to published paper by von Hippel and Bartlett (2021) in Statistical Science.

# 1.2.0
bootImpute behaviour changed so that the imputation function, when passed an incomplete dataset, returns a list of imputed datasets, rather than a single imputed data frame. This change has been made to improve computational efficiency, since for certain imputation methods, e.g. imputation conditional on the MLE, the imputation model will only need to be fitted once to each bootstrapped incomplete dataset. 

# 1.1.0
Adds functionality to bootstrap and impute and analyse the resulting datasets making use of multiple cores/processors, to reduce computation times.
