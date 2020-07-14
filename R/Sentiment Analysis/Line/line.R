#===============================================================

#Packages
library(jiebaR)
library(tmcn) #轉繁/簡
library(wordcloud)
library(wordcloud2)

library(tidytext)
library(tidyverse)
library(reshape2)


#===============================================================

#Functions
line_corpus<-function(input,ID=NA){
  input<-strsplit(input, "\t")
  text<-vector()
  if(!is.na(ID)){ID<-paste(ID,collapse="|")}
  for(i in 1:length(input)){
    if(!is.na(input[[i]][3])){
      if(!is.na(ID)){
        if(grepl(ID,input[[i]][2])){
          text <- append(text,input[[i]][3])
        }
      }else{
        text <- append(text,input[[i]][3])
      }
    }
  }
  return(text)
}

line_clear <- function(text){
  text<-gsub('[[:digit:]]+', "", text)
  text<-gsub("[A-z]", "", text)
  text<-gsub("[照片]", "", text)
  text<-gsub("[影片]", "", text)
  text<-gsub("[貼圖]", "", text)
  text<-gsub("[語音訊息]", "", text)
  text<-gsub("群組語音通話已開始", "", text)
  text<-gsub("群組通話已開始。", "", text)
  text<-gsub("群組通話已結束", "", text)
  return(text)
}

clear_empty<-function(input){
  text<-vector()
  for(i in 1:length(input)){
    if(input[i]!=""){
      text <- append(text,input[i])
    }
  }
  return(text)
}

get_senti_zh<-function(){
  p <- read_file("ntusd-positive.txt")
  n <- read_file("ntusd-negative.txt")
  positive <- strsplit(p, "\r\n")[[1]]
  negative <- strsplit(n, "\r\n")[[1]]
  positive <- data.frame(word = positive, sentiments = "positive")
  negative <- data.frame(word = negative, sentiemtns = "negative")
  colnames(negative) = c("word","sentiment")
  colnames(positive) = c("word","sentiment")
  LIWC_ch <- rbind(positive, negative)
  return(LIWC_ch)
}

#===============================================================

#worker
worker<-worker(stop_word = "stop.txt")
new_user_word(worker, c("盧彥霖","蘇柏全","陳克威"), c("n","n","n"))

#Line input
x<-readLines('3.txt',encoding="UTF-8")

#corpus
#corp<-line_corpus(x,c("柏全","霖"))
corp<-line_corpus(x)
corp<-line_clear(corp)
corp<-clear_empty(corp)

#term
term<-segment(corp, worker)

#dataframe
df<-as.data.frame(table(term))
df<-df[order(df$Freq,decreasing = T),]

#wordcloud
wordcloud2(df[1:50,])

#===============================================================

#情感分析(中文)

LIWC_ch <- get_senti_zh()

colnames(df) <- c("word","count")

df_zh_word_count <- df %>%
  select(word,count) %>% 
  group_by(word) %>% 
  summarise(count = sum(count))  %>%
  filter(count>0)

df_zh_sent <- inner_join(df_zh_word_count, LIWC_ch, by = "word")
df_zh_sent <- filter(df_zh_sent,word!='知道')

table(df_zh_sent$sentiment)


df_zh_sent %>% 
  group_by(sentiment) %>%
  top_n(10,wt = count) %>%
  ungroup() %>% 
  mutate(word = reorder(word, count)) %>%
  ggplot(aes(word, count, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  theme(text=element_text(size=14))+
  coord_flip()


df_zh_sent %>% 
  group_by(word,sentiment) %>%
  summarise(count=sum(count)) %>%
  acast(word ~ sentiment, value.var = "count", fill = 0) %>%
  comparison.cloud(colors = c("blue", "red"),
                   random.order=FALSE,
                   title.size=1.5,
                   max.words = 100)

#######


