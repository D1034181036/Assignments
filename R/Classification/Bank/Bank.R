#################################

#Import Packages
library(rpart)
library(e1071)
library(randomForest)
library(caret)
library(ada)
library(rpart.plot)
library(rattle)
library(gridExtra)
library(ggplot2)

#################################

#Import Data 
bank <- read.csv("data/bank-additional.csv", header=TRUE, sep=";")
bank$duration <- NULL

#Create Train/Test Data
set.seed(100)
train_index <- sample(1:nrow(bank), size = nrow(bank)*0.8)
train_data <- bank[train_index,]
test_data <- bank[-train_index,]

#################################

#Custom Performance function
Performance <- function(predict,label){
  prop_table <- prop.table(table(predict,label))
  accuracy <- prop_table[1,1] + prop_table[2,2]
  precision <- prop_table[2,2]/(prop_table[2,2]+prop_table[2,1])
  recall <- prop_table[2,2]/(prop_table[2,2]+prop_table[1,2])
  f1 <- 2*precision*recall/(precision+recall)
  performance_vector <- c(accuracy,precision,recall,f1)
  names(performance_vector) <- c("Accuracy","Precision","Recall","F1-Score")
  return(performance_vector)
}

#################################

#Model_Building(CART Method)
bank_rpart <- rpart(y ~ ., data = train_data)

#Training_Data
rpart_Train_predictions <- predict(bank_rpart, train_data, type = "class")
rpart_Train_Performance <- Performance(rpart_Train_predictions,train_data$y)
rpart_Train_Performance

#Testing_Data
rpart_Test_predictions <- predict(bank_rpart, test_data, type = "class")
rpart_Test_Performance <- Performance(rpart_Test_predictions,test_data$y)
rpart_Test_Performance

#################################

#Model_Building(SVM Method)
bank_svm = svm(formula = y ~ ., data = train_data)

#Training_Data
svm_Train_predictions <- predict(bank_svm, train_data)
svm_Train_Performance <- Performance(svm_Train_predictions,train_data$y)
svm_Train_Performance

#Testing_Data
svm_Test_predictions <- predict(bank_svm, test_data)
svm_Test_Performance <- Performance(svm_Test_predictions,test_data$y)
svm_Test_Performance

#################################

#Model_Building(Naive Bayes Method)
bank_NB <- naiveBayes(y ~ ., data = train_data)

#Training_Data
NB_Train_predictions <- predict(bank_NB, train_data)
NB_Train_Performance <- Performance(NB_Train_predictions,train_data$y)
NB_Train_Performance

#Testing_Data
NB_Test_predictions <- predict(bank_NB, test_data)
NB_Test_Performance <- Performance(NB_Test_predictions,test_data$y)
NB_Test_Performance

#################################

#Model_Building(Random Forest Method)
bank_rf <- randomForest(y ~ ., data=train_data, importance=TRUE, ntree=100)

#Training_Data
RF_Train_predictions <- predict(bank_rf, train_data)
RF_Train_Performance <- Performance(RF_Train_predictions,train_data$y)
RF_Train_Performance

#Testing_Data
RF_Test_predictions <- predict(bank_rf, test_data)
RF_Test_Performance <- Performance(RF_Test_predictions,test_data$y)
RF_Test_Performance

#################################

#Model_Building(K Nearest Neighbor Method)
bank_knn <- train(y ~ ., data = train_data, method = "knn", trControl = trainControl(method = "cv", number = 5))

#Training_Data
knn_Train_predictions <- predict(bank_knn, train_data)
knn_Train_Performance <- Performance(knn_Train_predictions,train_data$y)
knn_Train_Performance

#Testing_Data
knn_Test_predictions <- predict(bank_knn, test_data)
knn_Test_Performance <- Performance(knn_Test_predictions,test_data$y)
knn_Test_Performance

#################################

#Model_Building(Ada Boost Method)
bank_ada<-ada(y ~ .,data = train_data,loss="exponential",type="discrete",iter=100)

#Training_Data
ada_Train_predictions <- predict(bank_ada, train_data)
ada_Train_Performance <- Performance(ada_Train_predictions,train_data$y)
ada_Train_Performance

#Testing_Data
ada_Test_predictions <- predict(bank_ada, test_data)
ada_Test_Performance <- Performance(ada_Test_predictions,test_data$y)
ada_Test_Performance

#################################
#################################

#Create Method dataframe
df_train <- data.frame(rpart_Train_Performance,
                       svm_Train_Performance,
                       NB_Train_Performance,
                       RF_Train_Performance,
                       knn_Train_Performance,
                       ada_Train_Performance)

df_train <- t(round(df_train,3))
rownames(df_train) <- c("CART","SVM","Naive_Bayes","Random_Forest","KNN","ADA")
df_train

df_test <- data.frame(rpart_Test_Performance,
                      svm_Test_Performance,
                      NB_Test_Performance,
                      RF_Test_Performance,
                      knn_Test_Performance,
                      ada_Test_Performance)

df_test <- t(round(df_test,3))
rownames(df_test) <- c("CART","SVM","Naive_Bayes","Random_Forest","KNN","ADA")
df_test

#################################

#Plot the dataframe
grid.table(df_train)
grid.table(df_test)

#plot RPART(Seed=200)
fancyRpartPlot(bank_rpart)

#plot RF
varImpPlot(bank_rf, type=1)

#plot RF
plot(bank_rf)

#################################

bank <- read.csv("data/bank-additional.csv", header=TRUE, sep=";")
summary(bank)

#duration time(label=yes)
min(bank$duration[bank$y == "yes"])
mean(bank$duration[bank$y == "yes"])

#age count
hist(bank$age, col = "light blue", freq = FALSE)

#job_y
ggplot(bank, aes(job,y)) + geom_count(color='red') + theme_classic() + ggtitle('job and respond freq') + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#education_y
ggplot(bank, aes(education,y)) + geom_count(color='red') + theme_classic() + ggtitle('education and respond freq') + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#poutcome_y
ggplot(bank, aes(x = poutcome , fill = y)) + geom_bar(stat='count', position='dodge')

#job_education
ggplot(bank, aes(job,education)) + geom_count(color='red') + theme_classic() + ggtitle('job and education frequency') + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#################################
