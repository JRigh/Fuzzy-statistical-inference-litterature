#-----------------------
# fuzzy numbers and r.v.
#-----------------------

library(FuzzyNumbers)

# generate fuzzy numbers
A1 <- FuzzyNumber(1, 2, 4, 7,
                  left=function(x) x,
                  right=function(x) 1-x)
A1

plot(A1)

f <- splinefun(c(-4,-3.5,-3,-2.2,-2), c(0,0.4,0.7,0.9,1), method='monoH.FC')
g <- splinefun(c(-1,0,10), c(1,0.5,0), method='monoH.FC')

B <- FuzzyNumber(10,20,20,30,
               left=convertSide(f, -4, -2),
               right=convertSide(g, -1, 10)
)
plot(B, xlab=expression(x), ylab=expression(alpha))

# definition by alpha-cuts
A1 <- FuzzyNumber(1, 2, 4, 7,
                  left=function(x) x,
                  right=function(x) 1-x
)
A2 <- FuzzyNumber(1, 3, 4, 7,
                  lower=function(alpha) pbeta(alpha, 5, 9), # CDF of a beta distr.
                  upper=function(alpha) pexp(1/alpha-1) # transformed CDF of an exp. distr.
)
plot(A1, col='blue')
plot(A2, col='red', lty=2, add=TRUE)
legend('topright', c(expression(mu[A1]), expression(mu[A2])),
       col=c('blue', 'red'), lty=c(1,2))