Version 1.2.0 - bootImpute behaviour changed so that the imputation function, when passed an incomplete dataset, returns a list of imputed datasets, rather than a single imputed data frame. This change has been made to improve computational efficiency, since for certain imputation methods, e.g. imputation conditional on the MLE, the imputation model will only need to be fitted once to each bootstrapped incomplete dataset. 

Version 1.1.0 - adds functionality to bootstrap and impute and analyse the resulting datasets making use of multiple cores/processors, to reduce computation times.
