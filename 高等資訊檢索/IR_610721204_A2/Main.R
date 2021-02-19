#===============================================================

#Import Packages
library(tm)
library(readtext) #input the whole folder data
library(purrr) #for elbow map k
library(cluster) #K-Medoid
library(slam) #row/col sums
library(ggplot2)
library(wordcloud)

#library(MLmetrics) #Accuracy
#library(mlbench) #feature selection

#===============================================================

#Import Data 
x<-readtext("./data/txt/*")

#Pre-processing
source <- VectorSource(x$text)
corpus <- Corpus(source)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords('english'))

#Create document term matrix (dtm)
dtm <- DocumentTermMatrix(corpus)
rowTotals <-  row_sums(dtm)
dtm <- dtm[which(rowTotals > 0),]
dtm <- weightTfIdf(dtm) #TF-IDF weighted
dtm <- as.matrix(dtm)
dim(dtm)

#===============================================================

#====== Functions ======

#cosine similarity
cosine_dist <- function(dtm){
  csim <- dtm / sqrt(rowSums(dtm * dtm))
  csim <- csim %*% t(csim)
  csim[is.na(csim)] <- 0 # NA TO ZERO
  cdist <- as.dist(1 - csim)
  return(cdist)
}

#K-means elbow plot
elbow <- function(data, num_k){
  # Use map_dbl to run many models with varying value of k (centers)
  tot_withinss <- map_dbl(num_k,  function(num_k){
    model <- kmeans(data, centers = num_k)
    model$tot.withinss
  })
  
  # Generate a data frame containing both k and tot_withinss
  elbow_df <- data.frame(k = num_k, tot_withinss = tot_withinss)
  
  # Plot the elbow
  elbow_plot <- ggplot(elbow_df, aes(x = k, y = tot_withinss)) + geom_line() + scale_x_continuous(breaks = num_k) + geom_point()
  
  return(elbow_plot)
}

#===============================================================

cdist <- cosine_dist(dtm)
num_k = seq(from = 2, to = 20, by = 2)
set.seed(1002)
elbow(cdist, num_k)

#===============================================================

#====== K-means ======
set.seed(1002)

model_km <- kmeans(cdist, centers = 10)
model_km_factor <- factor(model_km$cluster)
summary(model_km_factor)[order(summary(model_km_factor),decreasing=TRUE)]

#===============================================================


#====== Just for report ======
library(gridExtra)

grid.table(round(dtm[1:5,1:5],3))

wordcloud(corpus, max.words = 20, random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"), scale = c(5, 0.5), 
          vfont = c("serif", "plain"))


df <- data.frame(summary(model_km_factor)[order(summary(model_km_factor),decreasing=TRUE)])
df <- t(df)
row.names(a) <- "num_doc"
grid.table(a)
