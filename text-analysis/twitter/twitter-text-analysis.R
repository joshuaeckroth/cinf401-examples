
library(ggplot2)



twitter <- read.csv("/bigdata/data/superbowl/20140203_110522_e97baf5d-42b8-4d91-8b61-017afdbd4b89.csv", header=FALSE, sep="|", quote="")
colnames(twitter) <- c("username", "timestamp", "tweet", "retweetcount", "lon", "lat", "country", "name", "address", "type", "placeURL")

library(tm)
library(SnowballC) # required for stemming

corpus_small <- VCorpus(VectorSource(twitter[1:1000,]$tweet))
#corpus <- VCorpus(VectorSource(twitter$tweet))

tdm_small <- TermDocumentMatrix(corpus_small, control = list(removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, tolower = TRUE))
dtm_small <- DocumentTermMatrix(corpus_small, control = list(removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, tolower = TRUE))

#tdm <- TermDocumentMatrix(corpus, control = list(removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, tolower = TRUE))
#dtm <- DocumentTermMatrix(corpus, control = list(removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, tolower = TRUE))

library(wordcloud)

total_term_counts_small <- rowSums(as.matrix(tdm_small))
wordcloud(labels(total_term_counts_small), total_term_counts_small)


dtm_twitter_small <- DocumentTermMatrix(corpus_small, control = list(weighting = function(x) { weightTfIdf(x, normalize = TRUE) }, removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, tolower = TRUE, removeNumbers = TRUE))



#doc_twitter_small_dist <- dist(as.matrix(dtm_twitter_small))
#fit2d_twitter_small <- cmdscale(doc_twitter_small_dist, k=2)
#ggplot(data.frame(x=fit2d_twitter_small[,1], y=fit2d_twitter_small[,2])) + geom_point(aes(x=x, y=y))

library(RWeka)


library(XML)

library(tm.plugin.webmining)

readStackExchangePostBody <- function(tree) { extractHTMLStrip(xmlAttrs(xmlRoot(tree))[["Body"]]) }

readStackExchangePost <- readXML(spec = list(content = list("function", readStackExchangePostBody), id = list("attribute", "//@Id")), doc = PlainTextDocument())

PostSource <- function(x) {
  XMLSource(x, function(tree) {
    nodes <- XML::xmlChildren(XML::xmlRoot(tree))
    nodes[names(nodes) == "row"]
  }, readStackExchangePost)
}

corpus_se <- VCorpus(PostSource("/bigdata/data/stackexchange/unzipped/meta.chess.stackexchange.com/Posts.xml"))

is_non_empty <- function(doc)
{
  nchar(as.character(doc$content)) > 0
}

corpus_se <- Filter(is_non_empty, corpus_se)

tdm_se <- TermDocumentMatrix(corpus_se, control = list(weighting = function(x) { weightTfIdf(x, normalize = TRUE) }, removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, tolower = TRUE, removeNumbers = TRUE))

total_term_counts_se <- rowSums(as.matrix(tdm_se))
wordcloud(labels(total_term_counts_se), total_term_counts_se)

# with tfidf

dtm_se <- DocumentTermMatrix(corpus_se, control = list(weighting = function(x) { weightTfIdf(x, normalize = TRUE) }, removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, tolower = TRUE, removeNumbers = TRUE))

normalize <- function(x) {
  x / (sqrt(sum(x*x)))
}

dtm_se_norms <- apply(dtm_se, 1, normalize)

doc_cosine_similarity <- function(doctm1, doctm2) {
  sum(doctm1 * doctm2)
}

find_closest_doc <- function(corpus, dtm, idx) {
  other_idxs <- Filter(function(i) { i != idx }, 1:length(corpus))
  sims <- na.omit(sapply(other_idxs, function(i) { doc_cosine_similarity(dtm[idx,], dtm[i,]) }))
  print(paste("Max similarity:", max(sims)))
  closest <- which.max(sims)
  corpus[closest]
}

print_closest_doc <- function(corpus, dtm, idx) {
  print(corpus[idx]$content)
  print("----")
  print(find_closest_doc(corpus, dtm, idx)$content)
}

search <- function(corpus, query) {
  query_corpus <- VCorpus(VectorSource(c(query)))
  combined_corpus <- c(corpus, query_corpus)
  tdm <- TermDocumentMatrix(combined_corpus, control = list(weighting = function(x) { weightTfIdf(x, normalize = TRUE) }, removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, tolower = TRUE, removeNumbers = TRUE))
  tdm_norms <- apply(tdm, 2, normalize)
  query_idx <- ncol(tdm_norms)
  sims <- sapply(1:length(corpus), function(i) { tdm_norms[,i] %*% tdm_norms[,query_idx] })
  docs_sims <- data.frame(doc=names(corpus), score=sims)
  docs_sims[order(docs_sims$score, decreasing=TRUE),]
}

# then: corpus[["147"]] or whatever (document name)
