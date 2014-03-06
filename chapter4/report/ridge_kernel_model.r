library(ggplot2)
library(RFinanceYJ)

# repeat matrix (for norm calcurate)
repmat <- function(mat, m, n) {
  rmat <- mat
  for (i in 1:m) {
    if (i == m) break
    rmat <- rbind(rmat, mat)
  }
  crmat <- rmat
  for (i in 1:n) {
    if (i == n) break
    crmat <- cbind(crmat, rmat)
  }
  crmat
}

# complex conjugation
Conj_t <- function(x) {
  Conj(t(x))
}

# gauss kernel
gauss_kernel <- function(X, x) {
  h <- 2
  hh <- 2*h^2
  n <- length(x)
  N <- length(X)
  exp(-(repmat(X^2, 1, n) + repmat(Conj_t(x^2), N, 1) - 2 * X %*% Conj_t(x)) / hh)
}

# ridge reglession
ridge_reglession <- function(phi, y, b) {
  lambda <- 0.1
  solve(Conj_t(phi) %*% phi + lambdaG(lambda, b)) %*% (Conj_t(phi) %*% y)
}

# lambda * G
lambdaG <- function(lambda, b) {
  if (b == 0) {
    0
  } else {
    lambda * diag(b)
  }
}

T_electrick <- quoteStockTsData('7203.t', since='2013-01-01')

y <- T_electrick$close

n <- length(y)
b <- 1
N <- n * b

# linear reglession model
x <- matrix(seq(0, n, length=n),n,1)

# redge reglession model
X <- matrix(seq(0, n, length=N),N,1)

k <- gauss_kernel(x, x)
K <- gauss_kernel(X, x)

t <- ridge_reglession(k, y, n)
f <- K %*% t

T_electrick$ridge <- f

gp <- ggplot(data=T_electrick, aes(x=date, y=close))
df_p <- gp +
  geom_point(aes(date, close, group=1), color='#66cc00') +
  geom_line(aes(date, ridge, group=2), color='#ff6633', size=0.8) 
print(df_p)
ggsave('./ridge_reglession_kernel_model_lambda01.png')
