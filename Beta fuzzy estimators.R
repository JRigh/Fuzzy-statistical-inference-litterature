#----------------------
# Beta fuzzy estimators
# in R 
# Julian, May 2023
#----------------------

#---------------
# first approach
#---------------

# 1. Define a function that yields as output a scalar representing
# the membership grade of a fuzzy triangular number(p,q,r) - Berkachy, p.16 - given the following
# arguments: p,q,r and x: a real
fuzzytriangular <- function(p,q,r,x) {
  if(p < x & x < q) {return((x - p) / (q - p))}
  if(q < x & x < r) {return((r - x) / (r - q))}
  else return(0)
}

# example with x from a continuous U[0,1] distribution
set.seed(2023)
x = runif(1, 0, 1)
fuzzy_example <- fuzzytriangular(p=0.2, q=1.3, r=2, x=x)
x # [1] [1] 0.4666139
fuzzy_example # [1] 0.2423763

# if x is a beta realization with shape1 = 1/theta, with theta = 0 and shape2 = 1, then
set.seed(2023)
theta = 3
x = rbeta(n = 1, shape1 = 1/theta, shape2 = 1) 
fuzzy_theta <- fuzzytriangular(p=0.2, q=1.3, r=2, x=x)
x # [1] 0.3323928
fuzzy_theta # [1] 0.1203571

# in this example, we just assume that the argument x provided as input ia a beta realization.

#----------------
# second approach
#----------------

# setup
set.seed(2023)
theta = 3; n = 60
x_sample = rbeta(n = n, shape1 = 1/theta, shape2 = 1) 

# MLE
theta_hat = - (1/n) *sum(log(x_sample))

# asymptotic CI method 1
beta = seq(0.01:1, by = 0.01)
asy1CIl= theta_hat - qnorm(1-beta/2)*(theta_hat/n)
asy1CIu = theta_hat + qnorm(1-beta/2)*(theta_hat/n)

plot(x = c(asy1CIl, sort(asy1CIu, decreasing = FALSE)), 
     y = c(seq(0.01, 1, length = 100), seq(1, 0.01, length = 100)), type = 'l',
     main = 'Triangular Confidence Interval for the fuzzy parameter theta bar')

# exact CI 
beta = seq(0.01:1, by = 0.01)
theta_hat = 2.2; n = 60
exactCIl= 2*n*theta_hat / qchisq(1-beta/2, df = 2*n)
exactCIu = 2*n*theta_hat / qchisq(beta/2, df = 2*n)

plot(x = c(exactCIl, sort(exactCIu, decreasing = FALSE)), 
     y = c(seq(0.01, 1, length = 100), seq(1, 0.01, length = 100)), type = 'l',
     main = 'Triangular Confidence Interval for the fuzzy parameter theta bar 2')

#----
# end
#----