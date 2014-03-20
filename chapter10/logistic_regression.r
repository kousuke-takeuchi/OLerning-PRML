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

sum2 <- function(x) {
  n <- length(x[,1])
  s <- matrix(0, n, 1)
  for (i in 1:n) {
    s[i] <- sum(x[i,])
  }
  s
}

n <- 90
c <- 3
y <- matrix(1:3, n, 1)

x <- matrix(rnorm(n), n, 1) + matrix(seq(-3, 3, len=c), n, 1)

h <- 1
hh <- 2*h^2
E <- 0.1

t0 <- matrix(rnorm(n*c), n, c)
for (j in 100) {
  for (o in 1:1000 * n) {
    i <- ceiling(runif(1)*n)
    yi <- y[i]
    ki <- exp(-((x-x[i])^2/hh))
    ci <- exp(Conj(t(ki)) %*% t0)
    t <- t0 - E * (ki %*% ci) / (1 + sum(ci))
    t[,yi] <- t[,yi] + E * ki
    if (norm(t-t0) < 0.000001) break
    t0 <- t
  }
}

N <- 100
X <- matrix(seq(-5, 5, len=N), N, 1)
K <- exp(-(repmat(X^2, 1, n) + repmat(Conj(t(x^2)), N, 1) - 2 * X %*% Conj(t(x))) / hh)

C <- exp(K %*% t)
C <- C / repmat(sum2(C), 1, c)

df <- data.frame(x=X, y=C)
gp <- ggplot(df) +
  geom_line(aes(x, y.1), col='red') +
  geom_line(aes(x, y.2), col='blue') +
  geom_line(aes(x, y.3), col='green')
print(gp)
ggsave('./logistic_regression.png')