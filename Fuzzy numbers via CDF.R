#------------------------------------------------------------------------#
# Construction of fuzzy numbers via Cumulative Distribution Function CDF #
#------------------------------------------------------------------------#

set.seed(2025)
data = runif(n = 20, min = 1, max = 5)
data

sorted = sort(data, decreasing = FALSE) 
sorted

# define a function to compute the eCDF
ecdf_data = function(data) { 
  n = length(data) 
  sorted = sort(data, decreasing = FALSE) 
  
  ecdf = rep(0, n) 
  for (i in 1:n) { 
    ecdf[i] = sum(sorted<= data[i]) / n
  } 
  return(ecdf) 
} 

ecdf = ecdf_data(data) 
ecdf


# compute eCDF using inbuilt function ecdf() and plot
data.ecdf = ecdf(data)
data.ecdf

plot(data.ecdf)

# Plot function  
curve(punif(x, min = 0, max = 1), -1, 2, ylim = c(0, 1.2),  
      ylab = "f(x)")  


# Uniform CDF
uniform_cdf <- function(x, a, b) {
  if (a >= b) {
    stop("Invalid range: 'a' must be less than 'b'")
  }
  if (x < a) {
    return(0)
  } else if (x > b) {
    return(1)
  } else {
    return((x - a) / (b - a))
  }
}

uniform_cdf(0.3, a = 0, b = 1)

