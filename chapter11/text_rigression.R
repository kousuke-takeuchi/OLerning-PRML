# search word: python, r
# on github.com

# json file format is.. ->
# {
#   url : "https://github.com/...",
#   star : 100..
# }

# Usage :
# if you want to get first star of json_data,
# type it -> json_data$github_star[[1]]$star
library(rjson)
json_file = file.path("~/Documents/OLearning-PRML/chapter11", "github_star.json")
json_data = fromJSON(paste(readLines(json_file), collapse=""))

# get README.md!
markdown_urls <- sapply(json_data$github_stars, function(data) paste(data$url, "master/README.md", sep="/"))
library(RCurl)
#markdowns <- sapply(markdown_urls, function(url) getURL(url))

# make corpus of markdowns
documents <- data.frame(Text = markdowns)
row.names(documents) <- 1:nrow(documents)
library(tm)
corpus <- Corpus(DataframeSource(documents))
control <- list(stopwords=TRUE, removePunctuation=TRUE, removeNumbers=TRUE, tolower=TRUE, minDocFreq=2)
md.dtm <- DocumentTermMatrix(corpus, control)

# x : word matrix, y : threshold stars -> 1 (Good!) or 0 (Bad..)
x <- as.matrix(md.dtm)
stars <- sapply(json_data$github_stars, function(data) data$star)
threshold <- median(stars)
y <- sapply(stars, function(star) ifelse(star > threshold, 1, 0))

# logistic regression
library(glmnet)
library(boot)
regularized.fit <- glmnet(x, y, family='binomial')
p <- inv.logit(predict(regularized.fit, newx = x, s = 0.001)) # predicted labels probability
error <- sum(sapply(p-y, function(e) abs(e))) # stochastic error rate


# Verification
new_json_file <- file.path("~/Documents/OLearning-PRML/chapter11", "new_github_star.json")
new_json_data <- fromJSON(paste(readLines(new_json_file), collapse=""))

new_markdown_urls <- sapply(new_json_data$github_stars, function(data) paste(data$url, "master/README.md", sep="/"))
#new_markdowns <- sapply(new_markdown_urls, function(url) getURL(url))

new_documents <- data.frame(Text = new_markdowns)
row.names(new_documents) <- 1:nrow(new_documents)
new_corpus <- Corpus(DataframeSource(new_documents))
newmd.dtm <- DocumentTermMatrix(new_corpus, control)

new_x <- as.matrix(newmd.dtm) # got data!!

# predict good or bad
indices <- sample(1:20, 4)
trainning.x <- x[indices, ]
trainning.y <- y[indices]
lambda = 0.1
glm.fit <- glmnet(trainning.x, trainning.y, family='binomial')
predicted_y <- ifelse(predict(glm.fit, new_x, s = lambda) > 0, 1, 0)