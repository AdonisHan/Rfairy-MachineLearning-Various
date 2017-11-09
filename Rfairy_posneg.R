############################################################
### 수업내용 : data crawling
### 작성자 : Adonis Han.
### 작성일자 : 2017.08.03 (목)
############################################################

library(XML)
acsi.url<-"http://theacsi.org/index.php?option=com_content&view=article&id=147&catid=&Itemid=212&i=Department+and+Discount+Stores"


acsi.df<-readHTMLTable(acsi.url, header=T, which=1, stringsAsFactors=F)


setwd("C:/R2go/Bootcamp/data")
getwd()
acsi.df<-acsi.df[,c(1,18)]
acsi.df<-acsi.df[1:12,]

str(data)

table(data$com)

# acsi.df<-acsi.df[c(-4,-5,-7,-11),]

acsi.df2 = acsi.df[c(1,2,8,9,6,14,7,16), ]
acsi.df2 = acsi.df2[,24]
View(acsi.df2)


colnames(acsi.df)=c("company", "score")
acsi.df$score<-as.numeric(acsi.df$score)

acsi.df$code<-c("C1","C2","C3","C4","C5","C6","C7","C8")

pos = readLines("pos.txt")
neg = readLines("neg.txt")
data = read.csv2("reviews.csv")

# data<-read.csv(file.choose())
# pos<-readLines(file.choose())
# neg<-readLines(file.choose())



pos.final<-c(pos, 'upgrade')
neg.final<-c(neg, 'wait')

#소비자 리뷰, 긍정부정어 가지고 점수를 긍정부정어- 함수. 
{
score.sentiment = function(sentences, pos.words, neg.words, .progress='none') 
  require(plyr)
  require(stringr)
  
  scores = lapply(sentences, function(sentence, pos.words, neg.words) {
    
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    sentence = tolower(sentence)
    # 리뷰가 있고 긍정부정을 정제작업.
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches)
    ## 긍정어 개수에 따라 스코어를 매기는 것. 
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}




result<-score.sentiment(data[,2], pos.final, neg.final)
result$score

result<-score.sentiment(data[,2], pos.final, neg.final, .progress="text")
result$score


result$company<-data[,1]

#plyr, company별 평균 점수
result1<-ddply(result, .(company), summarise, avg=mean(score))
hist(result$score)

library(ggplot2)

all.scores<-result
ggplot(data=result)+
  geom_bar(mapping=aes(x=score, fill=company), binwidth=1)+
  facet_grid(company~.)+theme_bw()