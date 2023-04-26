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

# Berkachy
library(FuzzySTs)

mat <- matrix(c(1,2,3,7,6,5), ncol = 2) 
is.alphacuts(mat)

X <- TrapezoidalFuzzyNumber(1,2,3,4) 
alpha.X <- alphacut(X, seq(0,1,0.01)) 
nbreakpoints(alpha.X)

GFN <- GaussianFuzzyNumber(mean = 0, sigma = 1, alphacuts = TRUE, plot=TRUE) 
is.alphacuts(GFN)

GBFN <- GaussianBellFuzzyNumber(left.mean = -1, left.sigma = 1, right.mean = 2, right.sigma = 1, alphacuts = TRUE, plot=TRUE) 
is.alphacuts(GBFN)

X <- TrapezoidalFuzzyNumber(5,6,7,8) 
Y <- TrapezoidalFuzzyNumber(1,2,3,4) 
Fuzzy.Difference(X,Y)

X <- TrapezoidalFuzzyNumber(1,2,3,4) 
Fuzzy.Square(X, plot=TRUE)

data <- array(c(1,1,2,2,3,3,5,5,6,6,7,7),dim=c(2,3,2)) 
is.fuzzification(data)

data <- matrix(c(1,1,2,2,3,3,4,4),ncol=4) 
is.trfuzzification(data)

data <- matrix(c(1,1,2,2,3,3,4,4),ncol=4) 
data.tr <- tr.gfuzz(data)
is.fuzzification(data.tr)

# Fuzzification of the first sub-item of a data set - No decomposition is required 
data <- matrix(c(1,2,3,2,2,1,1,3,1,2),ncol=1) 
MF111 <- TrapezoidalFuzzyNumber(0,1,1,2) 
MF112 <- TrapezoidalFuzzyNumber(1,2,2,3) 
MF113 <- TrapezoidalFuzzyNumber(2,3,3,3) 
PA11 <- c(1,2,3) 
data.fuzzified <- FUZZ(data,mi=1,si=1,PA=PA11) 
is.trfuzzification(data.fuzzified)

# Fuzzification of the first sub-item of a data set - No decomposition 
# is required
data <- matrix(c(1,2,3,2,2,1,1,3,1,2),ncol=1) 
MF111 <- TrapezoidalFuzzyNumber(0,1,1,2) 
MF112 <- TrapezoidalFuzzyNumber(1,2,2,3) 
MF113 <- TrapezoidalFuzzyNumber(2,3,3,3) 
PA11 <- c(1,2,3) 
data.fuzzified <- GFUZZ(data,mi=1,si=1,PA=PA11) 
is.fuzzification(data.fuzzified)

X <- TrapezoidalFuzzyNumber(1,2,3,4) 
Y <- TrapezoidalFuzzyNumber(4,5,6,7) 
distance(X, Y, type = "DSGD.G") 
distance(X, Y, type = "GSGD")

# Simple example 
mat <- matrix(c(1,2,2,3,3,4,4,5), ncol =4) 
Fuzzy.sample.mean(mat)

# Simple example 
mat <- matrix(c(1,2,2,3,3,4,4,5), ncol =4) 
w <- c(1,3) 
Weighted.fuzzy.mean(mat, w)

# Simple example 
mat <- matrix(c(1,2,2,3,3,4,4,5), ncol =4) 
Moment(mat, k=4, dist.type = "GSGD")

# Simple example 
mat <- matrix(c(1,2,0.25,1.8,2,2.6,0.5,3,3,2.6,3.8,4,4,4.2,3.9,5), ncol =4) 
Skewness(mat, dist.type = "GSGD")

# Simple example 
mat <- matrix(c(1,2,0.25,1.8,2,2.6,0.5,3,3,2.6,3.8,4,4,4.2,3.9,5), ncol =4) 
Kurtosis(mat, dist.type = "GSGD")

data <- matrix(c(1,2,3,2,2,1,1,3,1,2),ncol=1) 
MF111 <- TrapezoidalFuzzyNumber(0,1,1,2) 
MF112 <- TrapezoidalFuzzyNumber(1,2,2,3) 
MF113 <- TrapezoidalFuzzyNumber(2,3,3,3) 
PA11 <- c(1,2,3) 
data.fuzzified <- FUZZ(data,mi=1,si=1,PA=PA11) 
Fuzzy.variance(data.fuzzified, method = "approximation5", plot=TRUE) 
Fuzzy.variance(data.fuzzified, method = "exact", plot=TRUE) 
Fuzzy.variance(data.fuzzified, method = "distance") 
data.fuzzified2 <- GFUZZ(data,mi=1,si=1,PA=PA11) 
Fuzzy.variance(data.fuzzified2, method = "exact", plot=TRUE) 
Fuzzy.variance(data.fuzzified2, method = "distance")


data <- matrix(c(1,2,3,2,2,1,1,3,1,2),ncol=1) 
MF111 <- TrapezoidalFuzzyNumber(0,1,1,2) 
MF112 <- TrapezoidalFuzzyNumber(1,2,2,3) 
MF113 <- TrapezoidalFuzzyNumber(2,3,3,3) 
PA11 <- c(1,2,3) 
data.fuzzified <- FUZZ(data,mi=1,si=1,PA=PA11) 
emp.dist <- boot.ml(data.fuzzified, algorithm = "algo1", distribution = "normal", sig = 0.05, sigma = 0.62) 
eta.boot <- quantile(emp.dist, probs = 95/100)

data <- matrix(c(1,2,3,2,2,1,1,3,1,2),ncol=1) 
MF111 <- TrapezoidalFuzzyNumber(0,1,1,2) 
MF112 <- TrapezoidalFuzzyNumber(1,2,2,3) 
MF113 <- TrapezoidalFuzzyNumber(2,3,3,3) 
PA11 <- c(1,2,3) 
data.fuzzified <- FUZZ(data,mi=1,si=1,PA=PA11) 
Fmean <- Fuzzy.sample.mean(data.fuzzified) 
fci.ml(data.fuzzified, t = Fmean, distribution = "normal", sig= 0.05, coef.boot = 1.8225, sigma = 0.62)


H0 <- alphacut(TriangularFuzzyNumber(2.9,3,3.1), seq(0,1, 0.01)) 
H1 <- alphacut(TriangularFuzzyNumber(3,3,5), seq(0,1,0.01)) 
t <- alphacut(TriangularFuzzyNumber(0.8,1.80,2.80), seq(0,1,0.01)) 
res <- Fuzzy.decisions(type = 0, H0, H1, t = t, s.d = 0.79, n = 10, sig = 0.05, distribution = "normal", distance.type = "GSGD")


H0 <- TriangularFuzzyNumber(2.9,3,3.1) 
H1 <- TriangularFuzzyNumber(3,3,5) 
res <- Fuzzy.CI.test(type = 0, H0, H1, t = TriangularFuzzyNumber(0.8,1.80,2.80), s.d = 0.79, n = 10, sig = 0.05, distribution = "normal", distance.type="GSGD")


data <- matrix(c(1,2,3,2,2,1,1,3,1,2),ncol=1) 
MF111 <- TrapezoidalFuzzyNumber(0,1,1,2) 
MF112 <- TrapezoidalFuzzyNumber(1,2,2,3) 
MF113 <- TrapezoidalFuzzyNumber(2,3,3,3) 
PA11 <- c(1,2,3) 
data.fuzzified <- FUZZ(data,mi=1,si=1,PA=PA11) 
Fmean <- Fuzzy.sample.mean(data.fuzzified) 
H0 <- TriangularFuzzyNumber(2.2,2.5,3) 
H1 <- TriangularFuzzyNumber(2.5,2.5,5) 
emp.dist <- boot.ml(data.fuzzified, algorithm = "algo1", distribution = "normal", sig= 0.05, sigma = 0.7888) eta.boot <- quantile(emp.dist, probs = 95/100) (res <- Fuzzy.CI.ML.test(data.fuzzified, H0, H1, t = Fmean, sigma=0.7888, sig=0.05, coef.boot=eta.boot, distribution="normal", distance.type="GSGD")) 
res$decision


H0 <- TriangularFuzzyNumber(2.2,2.5,3) 
H1 <- TriangularFuzzyNumber(2.5,2.5,5) 
Fuzzy.p.value(type=1, H0, H1, t=TriangularFuzzyNumber(0.8,1.8,2.8), s.d=0.7888, n=10, sig=0.05, distribution="normal", distance.type="GSGD")

b_j <- c(1/3,1/3,1/3) 
b_jk <- matrix(c(0.5,0.125,0.2,0.5,0.125,0.2,0,0.125,0.2,0,0.125,0.2,0,0.125, 0.2,0,0.125,0,0,0.125,0,0,0.125,0),nrow=MI) 
SI <- c(2,8,5) 
adjusted.weight.MI(data, 17, 1, b_j, b_jk, SI)