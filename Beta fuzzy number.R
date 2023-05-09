#------------------
# Beta fuzzy number
# in R (03.05.2023)
#------------------

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
