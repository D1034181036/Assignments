library(rtweet)
library(tidytext)
library(tidyverse)
library(wordcloud)
library(wordcloud2)
library(reshape2)
library(jiebaR)
library(tmcn) #轉繁/簡

#===============================================================

# 找推文
rt <- search_tweets2(c("#HongKong","#反送中"), n = 18000, include_rts = FALSE)

rt %>%
  ts_plot("3hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(x = NULL, y = NULL)

#語言推文統計
lang<-as.data.frame(table(rt$lang))
colnames(lang) <- c("lang","count")
lang<-lang[order(lang$count,decreasing = T)[1:10],]
lang$lang <- factor(lang$lang, levels = lang$lang[order(lang$count,decreasing = T)])
lang$lang<-lang$lang[order(lang$count,decreasing = T)]
ggplot(lang, aes(x=lang,y = count)) +geom_bar(stat = "identity")


rt_en<-rt[rt$lang=="en",]
rt_zh<-rt[rt$lang=="zh",]


#===============================================================

###英文處理(斷詞/停詞)

tweets_en <- rt_en %>% select(user_id, text) %>% unnest_tokens(word,text)
tweets_en$word<-gsub('[^A-z]', NA, tweets_en$word) #將非英文值改成NA
tweets_en <- na.omit(tweets_en) #去除NA攔

tweets_en %>% group_by(word) %>% tally(sort=TRUE) %>% slice(1:15) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n)) + 
  geom_bar(stat = "identity",fill = "#FF6666") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + xlab("")


#去除停詞
stop_en <- c("https", "t.co", "rt", "amp")
stop_en <- stop_words %>% select(-lexicon) %>% bind_rows(data.frame(word = stop_en))
tweets_en <- tweets_en %>% anti_join(stop_en, by="word")

tweets_en %>% group_by(word) %>% tally(sort=TRUE) %>% slice(1:15) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n)) + 
  geom_bar(stat = "identity",fill = "#FF6666") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + xlab("")


#===============================================================

#情感分析(英文)
bing <- get_sentiments("bing")
tweets_en_sent <- tweets_en %>% inner_join(bing,by="word")
table(tweets_en_sent$sentiment)

#常出現的情感詞
tweets_en_sent %>% group_by(word) %>% tally(sort=TRUE) %>% slice(1:10) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n)) + 
  geom_bar(stat = "identity",fill = "light blue") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + xlab("")


#Data Frame
df_en_sent<-as.data.frame(table(tweets_en_sent$word,tweets_en_sent$sentiment))
df_en_sent<-df_en_sent[order(df_en_sent$Freq,decreasing = T),]
colnames(df_en_sent) <- c("word","sentiment","count")



#常出現的情感詞(分類)
df_en_sent %>% 
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

df_en_sent %>% 
  group_by(word,sentiment) %>%
  summarise(count=sum(count)) %>%
  acast(word ~ sentiment, value.var = "count", fill = 0) %>%
  comparison.cloud(colors = c("red", "blue"),
                   random.order=FALSE,
                   title.size=1.5,
                   max.words = 50)



#===============================================================

###中文處理(斷詞/停詞)

#Functions
line_clear <- function(text){
  text<-gsub('[[:digit:]]+', "", text)
  text<-gsub("[A-z]", "", text)
  return(text)
}

#Functions
get_senti_zh<-function(){
  p <- read_file("dict/ntusd-positive.txt")
  n <- read_file("dict/ntusd-negative.txt")
  positive <- strsplit(p, "\r\n")[[1]]
  negative <- strsplit(n, "\r\n")[[1]]
  positive <- data.frame(word = positive, sentiments = "positive")
  negative <- data.frame(word = negative, sentiemtns = "negative")
  colnames(negative) = c("word","sentiment")
  colnames(positive) = c("word","sentiment")
  LIWC_ch <- rbind(positive, negative)
  return(LIWC_ch)
}

#worker
worker<-worker(stop_word = "dict/stop_full.txt")
new_user_word(worker, c("蔡英文","蔡總統"), c("n","n"))

#中文轉繁體
rt_zh_trad = iconv(rt_zh$text,"UTF-8","UTF-8")
rt_zh_trad = toTrad(rt_zh_trad)

#corpus
tweets_zh<-line_clear(rt_zh_trad)

#term
tweets_zh<-segment(tweets_zh, worker)

#dataframe
df_zh<-as.data.frame(table(tweets_zh))
df_zh<-df_zh[order(df_zh$Freq,decreasing = T),]

#wordcloud
wordcloud2(df_zh[1:50,])

#histogram
hist_zh<-as.data.frame(tweets_zh)

hist_zh %>% group_by(tweets_zh) %>% tally(sort=TRUE) %>% slice(1:15) %>% 
  ggplot(aes(x = reorder(tweets_zh, n, function(n) -n), y = n)) + 
  geom_bar(stat = "identity",fill = "#FF6666") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + xlab("")

#===============================================================

#情感分析(中文)
LIWC_ch <- get_senti_zh()

#統計sentiment次數
count_sent_zh<-as.data.frame(tweets_zh)
colnames(count_sent_zh) <- c("word")
df_zh_sent <- inner_join(count_sent_zh, LIWC_ch, by = "word")
table(df_zh_sent$sentiment)

colnames(df_zh) <- c("word","count")

df_zh_word_count <- df_zh %>%
  select(word,count) %>% 
  group_by(word) %>% 
  summarise(count = sum(count))  %>%
  filter(count>0)

df_zh_sent <- inner_join(df_zh_word_count, LIWC_ch, by = "word")

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

