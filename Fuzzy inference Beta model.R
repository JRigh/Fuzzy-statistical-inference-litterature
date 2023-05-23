#---------------------------
# Fuzzy inference Beta model
# in R 
# Julian, May 2023
#--------------------------

#---------------
# first approach
#---------------

# 1. Define a function that yields as output a scalar representing
# the membership grade of a fuzzy triangular number(p,q,r) - Berkachy, p.16 - given the following
# arguments: p,q,r and x: a real
fuzzytriangular = function(p,q,r,x) {
  if(p < x & x < q) {return((x - p) / (q - p))}
  if(q < x & x < r) {return((r - x) / (r - q))}
  else return(0)
}

# p=1, q=2, r = 3, x=2.2
fuzzytriangular(0,2,3,2.2)

# if x is a beta realization with shape1 = 1/theta, with theta = 0 and shape2 = 1, then
set.seed(2023)
theta = 3
x = rbeta(n = 1, shape1 = 1/theta, shape2 = 1) 
x

# In this example, we just assume that the argument x provided as input ia a beta realization. 
# The membership grade is then
fuzzy_theta <- fuzzytriangular(p=0, q=2, r=3, x=x)
fuzzy_theta

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
asy1CIl= theta_hat - qnorm(1-beta/2)*(theta_hat/n) # lower bounds
asy1CIu = theta_hat + qnorm(1-beta/2)*(theta_hat/n) # upper bounds

plot(x = c(asy1CIl, sort(asy1CIu, decreasing = FALSE)), y = c(seq(0.01, 1, length = 100), seq(1, 0.01, length = 100)), type = 'l',
     main = expression(paste('First triangular fuzzy confidence interval for the fuzzy parameter ', bar(theta))))


# asymptotic CI method 2
beta = seq(0.01:1, by = 0.01)
asy2CIl = theta_hat / (1 + qnorm(1-beta/2)/n) # lower bounds
asy2CIu = theta_hat / (1 - qnorm(1-beta/2)/n) # upper bounds

plot(x = c(asy2CIl, sort(asy2CIu, decreasing = FALSE)), y = c(seq(0.01, 1, length = 100), seq(1, 0.01, length = 100)), type = 'l',
     main = expression(paste('Second triangular fuzzy confidence interval for the fuzzy parameter ', bar(theta))))

# exact CI 
beta = seq(0.01:1, by = 0.01)
theta_hat = 2.2; n = 60
exactCIl= 2*n*theta_hat / qchisq(1-beta/2, df = 2*n)
exactCIu = 2*n*theta_hat / qchisq(beta/2, df = 2*n)

plot(x = c(exactCIl, sort(exactCIu, decreasing = FALSE)), y = c(seq(0.01, 1, length = 100), seq(1, 0.01, length = 100)), type = 'l',
     main = expression(paste('Third triangular fuzzy confidence interval for the fuzzy parameter ', bar(theta))))

#----
# end
#----