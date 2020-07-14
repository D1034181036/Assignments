#===============================================================

#Import Packages
library(tm)
library(e1071)
library(caret)
library(gridExtra)#plot
library(wordcloud)#plot

#New / for accuraty & deature selection
library(mlbench)
library(MLmetrics)

#===============================================================

#Import Data 
x_train <- read.table('data/R8/r8-train-all-terms.txt', header=FALSE, sep='\t')
x_test <- read.table('data/R8/r8-test-all-terms.txt', header=FALSE, sep='\t')

#Mark the Train/Test Data
x_train$train_test <- c("train")
x_test$train_test <- c("test")
x <- rbind(x_train, x_test)

#Subset to 3 document classes
x <- x[which(x$V1 %in% c('trade','crude', 'money-fx')),]
x <- droplevels(x)

#===============================================================

#Pre-processing
source <- VectorSource(x$V2)
corpus <- Corpus(source)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords('english'))

#Create document term matrix (dtm)
dtm <- DocumentTermMatrix(corpus)
dtm <- weightTfIdf(dtm) #TF-IDF weighted
dtm <- as.matrix(dtm)
dim(dtm)

#===============================================================

#feature selection !!
control <- rfeControl(functions=rfFuncs, method="cv", number=2)
results <- rfe(dtm[,1:9243], x$V1, sizes=c(5,10,100,500,1000), rfeControl=control)

# summarize the results
print(results)
# list the chosen features
predictors(results)

new_columns <- predictors(results)
dtm = dtm[,new_columns[1:100]]
dim(dtm)

#===============================================================

#Create Train/Test Data
train <- dtm[which(x$train_test == "train"),]
train_y <- x$V1[which(x$train_test == "train")]

test <- dtm[which(x$train_test == "test"),]
test_y <- x$V1[which(x$train_test == "test")]

#===============================================================

#Model_Building(Naive Bayes Method)
R8_NB <- naiveBayes(train, train_y)

#Predict on test data
NB_Test_predictions <- predict(R8_NB, test)
NB_table <- table(test_y, NB_Test_predictions)
NB_accuracy <- Accuracy(test_y,NB_Test_predictions)

#===============================================================

#Model_Building(K Nearest Neighbor Method)
R8_KNN <- train(train, train_y, method = "knn")

#Predict on test data
KNN_Test_predictions <- predict(R8_KNN, test)
KNN_table <- table(test_y,KNN_Test_predictions)
KNN_accuracy <- Accuracy(test_y,KNN_Test_predictions)

#===============================================================

#Model_Building(Support Vector Machine Method)
#R8_SVM <- train(train, train_y, method = "svmRadial")
R8_SVM <- train(train, train_y, method = "svmLinear")

#Predict on test data
SVM_Test_predictions <- predict(R8_SVM,test)
SVM_table <- table(test_y,SVM_Test_predictions)
SVM_accuracy <- Accuracy(test_y,SVM_Test_predictions)

#===============================================================

#Model_Building(CART Method)
R8_rpart <- train(train, train_y, method = "rpart")

#Predict on test data
rpart_Test_predictions <- predict(R8_rpart,test)
rpart_table <- table(test_y,rpart_Test_predictions)
rpart_accuracy <- Accuracy(test_y,rpart_Test_predictions)

#===============================================================

#Compare Model
Model = c("KNN","SVM","CART","Naive Bayes")
Accuracy = round(c(KNN_accuracy,SVM_accuracy,rpart_accuracy,NB_accuracy),3)
result <- data.frame(Model, Accuracy)
result

#===============================================================

#Plot
grid.table(t(result))
grid.table(KNN_table)
grid.table(SVM_table)
grid.table(rpart_table)
grid.table(NB_table)

ggplot(result, aes(x = Model, y = Accuracy)) + geom_bar(stat="identity")

wordcloud(corpus, max.words = 20, random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"), scale = c(5, 0.5), 
          vfont = c("serif", "plain"))

#===============================================================