##url <- paste0("https://en.wikipedia.org/wiki/","Electronic_health_record")

library(rvest)
term1<- read_html("https://www.himss.org/library/ehr")
word1<-html_nodes(term1,".main-container")%>% 
  html_text()%>% 
  gsub("\n", "", .) %>% 
  trimws()
term2<- read_html("https://www.healthit.gov/faq/what-electronic-health-record-ehr")
word2<-html_nodes(term2,".region__content-main")%>% 
  html_text()%>% 
  gsub("\n", "", .) %>% 
  trimws()
term3<- read_html("https://searchhealthit.techtarget.com/definition/electronic-health-record-EHR")
word3<-html_nodes(term3,".main-content")%>% 
  html_text()%>% 
  gsub("\n", "", .) %>% 
  trimws()

term4<- read_html("https://twitter.com/search?q=%23EHR&src=typd&lang=en")
word4<-html_nodes(term4,".stream-container")%>% 
  html_text()%>% 
  gsub("\n", "", .) %>% 
  trimws()



word<-paste(word1,word2,word3,word4)




library(wordcloud)
library(tm)
library(SnowballC)
docs <- Corpus(VectorSource(word))
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, removeWords, stopwords("english"))
myStopWords <- c("may", "now", "also", "many", "use", "used", "typically","given",
                 "like", "will", "can", "often", "see", "one", "pdf", "issn", "journal","isbn","abér",
                 "copy","tweet","retweets","retweeted","reply","retweet","replies","hours","liked","link","ago",
                 "follow",
                 tolower(month.name))
docs <- tm_map(docs, stemDocument)
docs <- tm_map(docs, removeWords, myStopWords)
docs <- tm_map(docs, content_transformer(gsub), pattern = "–", replacement = "")
docs <- tm_map(docs, content_transformer(gsub), pattern = "•", replacement = "")
docs <- tm_map(docs, content_transformer(gsub), pattern = "’", replacement = "")
docs <- tm_map(docs, content_transformer(gsub), pattern = "”", replacement = "")
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=round(sqrt(v),0))
set.seed(5)
cloud<-wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                 max.words=15, random.order=FALSE, rot.per=0.35, 
                 colors=brewer.pal(8, "Dark2"))
###
install.packages("stringr")
library(stringr)
d$word=str_replace_all(d$word,"[^[:graph:]]", " ") 

install.packages("syuzhet")
library(syuzhet)

sentRes <- get_sentiment(paste0(d$word))
head(sentRes)

dWSe <- data.frame(d,sentRes=sentRes, color=rep("black",length(sentRes)), 
                   stringsAsFactors = FALSE)
dWSe[dWSe$sentRes<0,4] <- "red"
dWSe[dWSe$sentRes>0,4] <- "green"


NegativeWords <- dWSe[dWSe$sentRes<0,]
PositiveWords <- dWSe[dWSe$sentRes>0,]
NoneNeutralWords <- dWSe[dWSe$sentRes!=0,]
NeutralWords <- dWSe[dWSe$sentRes==0,]



set.seed(5)
wordcloud(words = NoneNeutralWords$word, freq = NoneNeutralWords$freq, min.freq = 1,
          max.words=70, random.order=FALSE, rot.per=0.35, 
          colors=NoneNeutralWords$color,
          ordered.colors=TRUE
)


weighted.mean(NoneNeutralWords$sentRes,NoneNeutralWords$freq)


dWSe <- data.frame(d,sentRes=sentRes, color=rep("#00000000",length(sentRes)), 
                   stringsAsFactors = FALSE)
for (i in 1:length(dWSe$word)) {
  curRate=dWSe$sentRes[i]
  if(curRate<0){
    dWSe$color[i]=rgb(red = 179/255,green = 0,blue = 12/255,alpha = -curRate)
  }
  if(curRate>0){
    dWSe$color[i]=rgb(red = 13/255,green = 89/255,blue = 1/255,alpha = curRate)
  }
}

NoneNeutralWords <- dWSe[dWSe$sentRes!=0,]

set.seed(5)
wordcloud(words = NoneNeutralWords$word, freq = NoneNeutralWords$freq, min.freq = 1,
          max.words=70, random.order=FALSE, rot.per=0.35, 
          colors=NoneNeutralWords$color,
          ordered.colors=TRUE
)

