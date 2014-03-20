# get data from json file
library(rjson)
companies.file <- file.path("~/Documents/OLearning-PRML/chapter13", "news.json")
companies.json <- fromJSON(paste(readLines(companies.file), collapse=""))

# from json data to data.frame
options(stringsAsFactors = FALSE)
companies.vec <- transform(companies.json$companies)
companies.data <- t(matrix(companies.vec, ncol=8))
companies.data <- data.frame(companies.data)
names(companies.data) <- c("Name", "Treat", "Moral", "Env", "Band", "Young", "Long", "Compliance", "Affair")
comp_names <- companies.data$Name
nc <- ncol(companies.data)
companies.data <- companies.data[, 2:nc]
companies.data <- sapply(companies.data, as.numeric)
row.names(companies.data) <- comp_names

# Principal Component Analysis
# 1. One way: calculate along formula
#embed_mat <- eigen(cov(companies.data))$vectors  # calculate covariance mat ~> eigenvectors
#pca <- embed_mat %*% t(companies.data)            # calculate principal component
# 2. Another way: use PCA Library
pca.bdc <- prcomp(companies.data, scale = TRUE)
pca.importance <- summary(pca.bdc)$importance[3, ]        # Cumulative contribution ratio
idx <- which.max(pca.importance > 0.95)                   # index of first index being true
new_var <- pca.bdc$rotation[, 1:idx]                      # new variable with pca: [var1, var2, ...] ~> [pc1, pc2, ...]

# translate datas to new pca variables
companies.pca <- companies.data %*% new_var
row.names(companies.pca) <- rownames(companies.data)

# clustering with pca data
k <- 4
fit <- kmeans(companies.pca, k)

# plot cluster
library(cluster) 
clusplot(companies.pca, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

### Varification
### check cluster with original data
varify.fit <- kmeans(companies.data, k)
#clusplot(companies.data, varify.fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)