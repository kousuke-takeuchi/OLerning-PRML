
library(ggplot2)

x <- sample(1:100, 50)
y <- sample(1:100, 50)
z <- sample(1:100, 50)

data <- data.frame(X=x, Y=y, Z=z)
embed_mat <- eigen(cor(data))$vectors

pca <- vector(mode="numeric", len=50)
for (i in 1:50) {
  for (j in 1:3) {
    pca[i] = pca[i] + embed_mat[j, 1] * data$X[i] + embed_mat[j, 2] * data$Y[i] + embed_mat[j, 3] * data$Z[i]
  }
}

pca <- data.frame(X=1:50, Y=pca)
p <- ggplot(pca, aes(x=X, y=Y)) + geom_point()