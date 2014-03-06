# Load libraries
library('tm')
library('ggplot2')

# Set the global paths
spam.path <- file.path("data", "spam")
easyham.path <- file.path("data", "easy_ham")
hardham.path <- file.path("data", "hard_ham")

# Return a single element vector of just the email body
# This is a very simple approach, as we are only using 
# words as features
get.msg <- function(path)
{
  try({
    con <- file(path, open = "rt", encoding = "latin1")
    text <- readLines(con)
    # The message always begins after the first full line break
    msg <- text[seq(which(text == "")[1] + 1, length(text), 1)]
    close(con)
    return(paste(msg, collapse = "\n"))
  })
}

# Create a TermDocumentMatrix (TDM) from the corpus of SPAM email.
# The TDM control can be modified, and the sparsity level can be 
# altered.  This TDM is used to create the feature set used to do 
# train our classifier.
get.tdm <- function(doc.vec)
{
  doc.corpus <- Corpus(VectorSource(doc.vec))
  control <- list(stopwords = TRUE, removePunctuation = TRUE, removeNumbers = TRUE, minDocFreq = 2)
  doc.dtm <- TermDocumentMatrix(doc.corpus, control)
  return(doc.dtm)
}

# Create a data frame that provides the feature set from the training SPAM data
get.df <- function(doc.tdm) {
  spam.matrix <- as.matrix(spam.tdm)
  spam.counts <- rowSums(spam.matrix)
  spam.df <- data.frame(cbind(names(spam.counts), as.numeric(spam.counts)), stringsAsFactors = FALSE)
  names(spam.df) <- c("term", "frequency")
  spam.df$frequency <- as.numeric(spam.df$frequency)
  spam.occurrence <- sapply(1:nrow(spam.matrix), function(i) { length(which(spam.matrix[i, ] > 0)) / ncol(spam.matrix) })
  return(spam.df$frequency / sum(spam.df$frequency))
  
  # Add the term density and occurrence rate
  return(transform(spam.df, density = spam.density, occurrence = spam.occurrence))
}

get.all <- function(path) {
  spam.docs <- dir(path)
  spam.docs <- spam.docs[which(spam.docs != "cmds")]
  all.spam <- sapply(spam.docs,
                     function(p) get.msg(file.path(path, p)))
  
  # Create a DocumentTermMatrix from that vector
  spam.tdm <- get.tdm(all.spam)
  return(get.df(spam.tdm))
}

# Get all the SPAM-y email into a single vector
spam.df <- get.all(spam.path)
# Now do the same for the EASY HAM email
easyham.df <- get.all(easyham.path)


# Find counts of just terms 'html' and 'table' in all SPAM and EASYHAM docs, and create figure
spam.docs <- dir(spam.path)
html.spam <- sapply(spam.docs, function(p) count.word(file.path(spam.path, p), "html"))
table.spam <- sapply(spam.docs, function(p) count.word(file.path(spam.path, p), "table"))
spam.init <- cbind(html.spam, table.spam, "SPAM")

easyham.docs <- dir(easyham.path)
html.easyham <- sapply(easyham.docs,  function(p) count.word(file.path(easyham.path, p), "html"))
table.easyham <- sapply(easyham.docs, function(p) count.word(file.path(easyham.path, p), "table"))
easyham.init <- cbind(html.easyham, table.easyham, "EASYHAM")

init.df <- data.frame(rbind(spam.init, easyham.init),  stringsAsFactors = FALSE)

