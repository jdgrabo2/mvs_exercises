# Packages----

library(tidyverse)

# 1.1----
x1 <-
  c(3, 4, 2, 6, 8, 2, 5)
x2 <-
  c(5, 5.5, 4, 7, 10, 5, 7.5)

## Mean of x1 and x2:

mean_x1 <-
  mean(x1)

mean_x2 <-
  mean(x2)

## Variances of s11 and s22 and covariance s12:

var_s11 <-
  var(x1)

var_s22 <-
  var(x2)

covar_s12 <-
  cov(x1, x2)

# 1.2----

age_yrs <-
  c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11)

price_dollars <-
  c(18.95, 19.00, 17.95, 15.54, 14.00, 12.95, 8.94, 7.49, 6.00, 3.9)


