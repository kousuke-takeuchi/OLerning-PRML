library(ggplot2)
library(RFinanceYJ)

# Trigonometric polynomial
phi <- function(d, i) {
  if (i == 1) {
    return(1)
  } else if (i %% 2 == 0) {
    return(sin(d / (i/2)))
  } else {
    return(cos(d / (i/2)))
  }
}

# Design matrix
base_func <- function(x, b) {
  n <- length(x)
  lphi <- matrix(vector(mode="numeric", length=n*b), nrow=n, ncol=b)
  for (i in 1:n) {
    for (j in 1:b) {
      lphi[i,j] <- phi(x[i], j)
    }
  }
  return(lphi)
}

# return regression's parameter
ridge_regression <- function(data, lamb, b) {
  p <- base_func(1:length(data), b)
  p_t <- t(p)
  return(solve(p_t%*%p + lamb*diag(b)) %*% p_t %*% data)
}

# return vector: f(1)..f(n)
# f(x) = theta * design_matrix
ridge <- function(theta, n) {
  b <- length(theta)
  p <- matrix(vector(mode="numeric", length=n*b), nrow=b, ncol=n)
              
  for (i in 1:b) {
    for (j in 1:n) {
      p[i,j] <- phi(j, i)
    }
  }
  
  vec <- t(theta) %*% p
  
  return(t(vec))
}

## main routine
# get stock data
toyota <- quoteStockTsData('7203.t',  since='2014-01-01')

# ridge regression
lamb <- 0.0000001
b <- 100
theta <- ridge_regression(toyota$close, lamb, b)
toyota$ridge <- ridge(theta, length(toyota$close))

# plot and save image
p <- ggplot() + geom_point(data=toyota, aes(x=date, y=close))
p <- p + geom_line(data=toyota, aes(x=date, y=ridge))
ggsave(file = "./ridge.png", plot = p)