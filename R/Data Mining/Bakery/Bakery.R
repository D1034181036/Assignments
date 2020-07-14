#===============================================================

#載入套件
library(arules)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(gridExtra)
library(e1071)

#===============================================================

#載入資料
#data <- read.transactions(file="data/F.csv", header=TRUE, format="single", sep=",", cols=c("Transaction","Item"))
x <- read.csv("data/BreadBasket_DMS.csv") %>%
  mutate(Date=as.Date(Date),Time=hms(Time))
x <- x[x$Item!="NONE",]
data <- as(split(x[,"Item"], x[,"Transaction"]), "transactions")

summary(x)
summary(data)

#===============================================================

x1 <- x %>% 
  group_by(Item) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count)) %>%
  slice(1:10) %>% # keep the top 10 items
  ggplot(aes(x=reorder(Item,Count),y=Count,fill=Item))+
  geom_bar(stat="identity")+
  coord_flip()+
  theme(legend.position="none")+
  ggtitle("Most popular line items")
x1

x2 <- x %>% 
  mutate(Day = wday(Date,label=T)) %>% 
  group_by(Day) %>% 
  summarise(Count= n()) %>% 
  ggplot(aes(x=Day,y=Count,fill=Day))+
  theme_fivethirtyeight()+
  geom_bar(stat="identity")+
  ggtitle("Total line items sold per weekday")+
  theme(legend.position="none")
x2

x3 <- x %>% 
  mutate(wday=wday(Date,label=T)) %>% 
  group_by(wday,Transaction) %>% 
  summarise(n_distinct(Transaction)) %>% 
  summarise(Count=n()) %>% 
  ggplot(aes(x=wday,y=Count,fill=wday))+
  theme_fivethirtyeight()+
  geom_bar(stat="identity")+
  ggtitle("Unique transactions per weekday")+
  theme(legend.position="none")
x3

x4 <- x %>%
  mutate(Hour = as.factor(hour(x$Time))) %>% 
  group_by(Hour) %>% 
  summarise(Count=n()) %>% 
  ggplot(aes(x=Hour,y=Count,fill=Hour))+
  theme_fivethirtyeight()+
  geom_bar(stat="identity")+
  ggtitle("Line items sold per Hour")+
  theme(legend.position="none")
x4

x5 <- x %>% 
  mutate(Hour = as.factor(hour(Time)))%>% 
  group_by(Hour,Transaction) %>% 
  summarise(n_distinct(Transaction)) %>% 
  summarise(Count=n()) %>% 
  ggplot(aes(x=Hour,y=Count,fill=Hour))+
  theme_fivethirtyeight()+
  geom_bar(stat="identity")+
  ggtitle("Unique transactions per Hour")+
  theme(legend.position="none")
x5

#===============================================================

#找出frequent_itemsets
frequent_itemsets <- apriori(data, parameter = list(support = 0.01,minlen = 2,target = "frequent itemsets"))
inspect(sort(frequent_itemsets, by = 'support')[1:20])

#畫出Top 10 frequent_itemsets
frequent_itemsets_df <- DATAFRAME(sort(frequent_itemsets, by = 'support'))
frequent_itemsets_df %>% 
  top_n(10,wt = count) %>%
  mutate(items = reorder(items, count)) %>%
  ggplot(aes(items, count, fill = items)) +
  geom_col(show.legend = FALSE) +
  labs(y = "Count",x = NULL) +
  theme(text=element_text(size=12))+
  coord_flip()


#===============================================================

#找出Top 10 association rules by lift
rules <- apriori(data, parameter = list(support = 0.01, confidence = 0.5, minlen = 2,target = "rules"))
inspect(sort(rules, by="lift"))
#grid.table(inspect(sort(rules, by="lift")))

rules2 <- apriori(data, parameter = list(support = 0.0005, confidence = 0.5, minlen = 2,target = "rules"))
inspect(sort(rules2, by="lift")[1:10])
#grid.table(inspect(sort(rules2, by="lift")[1:10]))

#===============================================================

# Custom Performance function
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

#data frame
df <- as.data.frame(as(data, "matrix"))

#Create Train/Test Data
set.seed(100)
train.index <- sample(x=1:nrow(df), size=nrow(df)*0.8)
train_X <- df[train.index, ]
test_y <- df[-train.index, ]

#Model_Building
NB_model <- naiveBayes(Bread ~ ., data = train_X)

#Testing_Data
pred_test_y <- predict(NB_model,test_y)
NB_Test_Performance <- Performance(pred_test_y,test_y$Bread)
NB_Test_Performance

#Plot
df_test <- data.frame(NB_Test_Performance)
df_test <- t(round(df_test,3))
rownames(df_test) <- "Naive_Bayes"
grid.table(df_test)
