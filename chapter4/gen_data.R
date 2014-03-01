library(ggplot2)

y <- vector(mode="integer", length=100)
y[1:100] <- 50
y[sample(1:100, 10)] <- 50 + sample(-5:5, 10)
x <- 1:100

p <- qplot(x, y, geom=c("line", "point"))
ggsave(file = "./plot.png", plot = p)