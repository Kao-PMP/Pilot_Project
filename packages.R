# This script installs packages if they haven't been already, and then outputs their versions.

packages.list <- c("table1", "tidyr", "boot", "caret", "cluster", "dplyr", "ggplot2", "Hmisc", "lattice", "purrr", "ranger", "risksetROC", "splines", "survival", "table1", "sfsmisc", "poLCA")

new.packages <- packages.list[!(packages.list %in% installed.packages() [,"Package"])]
if (length(new.packages)) install.packages(new.packages, repos="http://cran.us.r-project.org")

print(sessionInfo())
versions <- lapply(packages.list, packageVersion)
version_strings <- lapply(versions, paste)
both <- cbind(packages.list, version_strings)
print(both)


# output as of 2020-06-09
## R version 3.6.0 (2019-04-26)
## Platform: x86_64-redhat-linux-gnu (64-bit)
## Running under: Amazon Linux 2
## 
## Matrix products: default
## BLAS/LAPACK: /usr/lib64/R/lib/libRblas.so
## 
## locale:
##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
##  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
##  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## loaded via a namespace (and not attached):
## [1] compiler_3.6.0
##
##       packages.list version_strings
##  [1,] "table1"      "1.2"          
##  [2,] "tidyr"       "1.0.0"        
##  [3,] "boot"        "1.3.22"       
##  [4,] "caret"       "6.0.84"       
##  [5,] "cluster"     "2.0.8"        
##  [6,] "dplyr"       "0.8.3"        
##  [7,] "ggplot2"     "3.2.1"        
##  [8,] "Hmisc"       "4.3.0"        
##  [9,] "lattice"     "0.20.38"      
## [10,] "purrr"       "0.3.3"        
## [11,] "ranger"      "0.12.1"       
## [12,] "splines"     "3.6.0"        
## [13,] "risksetROC"  "1.0.4"
## [14,] "survival"    "3.1.7"        
## [15,] "table1"      "1.2"          
