library(ggplot2)

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

Conj_t <- function(m) {
  Conj(t(m))
}

ones <- function(m, n) {
  matrix(1, m, n)
}

# gauss kernel
gauss_kernel <- function(X, x) {
  h <- 14.5
  hh <- 2*h^2
  n <- length(x)
  N <- length(X)
  exp(-(repmat(X^2, 1, n) + repmat(Conj_t(x^2), N, 1) - 2 * X %*% Conj_t(x)) / hh)
}

f <- function(x, w) {
  a <- -(w[1, 2] - w[2, 2]) / (w[1, 3] - w[2, 3])
  b <- -(w[1, 1] - w[2, 1]) / (w[1, 3] - w[2, 3])
  a * x + b
}

n <- 200
N <- n
lambda <- 0.1
b <- ones(n, 1)

wt.bottom <- 30
wt.top <- 120
ht.bottom <- 60
ht.top <- 230

X <- matrix(seq(wt.bottom, wt.top, len=N), N, 1)

male <- read.csv('~/d_programming/R/male_100.csv')
female <- read.csv('~/d_programming/R/female_100.csv')

x <- matrix(c(male$weight,
              female$weight,
              male$height,
              female$height),
            n, 2)
x <- Conj_t(cbind(b, x))

x.b <- matrix(x[1,], n, 1)
x.wt <- matrix(x[2,], n, 1)
x.ht <- matrix(x[3,], n, 1)

y.male <- matrix(c(ones(1, n/2), -(ones(1, n/2))), n/2, 2)
y.female <- matrix(c(-(ones(1, n/2)), ones(1, n/2)), n/2, 2)
y <- rbind(y.male, y.female)

k.b <- Conj_t(gauss_kernel(x.b, x.b) %*% b)
k.wt <- Conj_t(gauss_kernel(x.wt, x.wt) %*% b)
k.ht <- Conj_t(gauss_kernel(x.ht, x.ht) %*% b)

k <- rbind(k.b, k.wt, k.ht)

t <- Conj_t(solve(Conj_t(k) %*% k + lambda * diag(n)) %*% Conj_t(k)) %*% y
F <- f(X, Conj_t(t))

df <- data.frame(male, F, X, female)

# ggplot
p <- ggplot(data=df) + 
  geom_point(aes(x=weight, y=height), col='blue') + 
  geom_point(aes(x=weight.1, y=height.1) ,col='red') +
  geom_line(aes(x=X, y=F), xlim=c(30,100))
print(p)
ggsave('./gauss_l2_classify.png')