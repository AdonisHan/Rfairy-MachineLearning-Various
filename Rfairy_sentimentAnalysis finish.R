      rm(list=ls())
      #' @ title sentimental practice
      #' @ 06 sentimentAnalysis
      #' @ author Adonis Han
      #' @ version 1.0
      
      
      # 01 Setting ------------------------------------------------------------
      ## library
      
      library(tm)
      library(glmnet)
      library(tidytext)
      library(rvest)
      library(dplyr)
      library(topicmodels)
      library(tidytext)
      library(tm)
      library(rvest)
      library(KoNLP)
      library(slam)
      library(glmnet)
      library(wordcloud)
      ## source
      source("MS_crawling.R.R", encoding = 'utf-8')
      ## var 
      
      ## date
      date <- format(Sys.time(), "%y%m%d")
      date
      
      dir.create(paste0("", date))
      2
      path <- paste0("", date)
      
      set.seed(1)
       
      
      # 02. Load data -----------------------------------------------------------
      textData <- c()
      for(i in 1:100){
        textData <- rbind(textData, MS_crawling(i, 94074))
        textData <- rbind(textData, MS_crawling(i, 105357))
        textData <- rbind(textData, MS_crawling(i, 103389))
      }
      
      textData$Review <- as.character(textData$Review)
      write.csv(textData, paste0(path, "/movie.csv"), row.names = F)
      
      # encoding 때문에 한 번 저장 후 load
      text <- read.csv(paste0(path, "/movie.csv"), stringsAsFactors = F)
      
      ## filter
      text$rate2 <- ifelse(text$Rate > 5, 1, -0)
      minL <- min(table(text$rate2)) #평점 5점이 긍정적인것도 부정적인 최소값으로.
      negative <- text[text$rate2 ==0 , ]
      positive <- text[text$rate2 ==1 , ]
      newText <- rbind(negative[sample(nrow(negative), minL), ], positive[sample(nrow(positive), minL), ]) %>% tbl_df
                      
      
      docCorp <- Corpus(VectorSource(newText$Review))
      dtm <- DocumentTermMatrix(docCorp, control = list(tokenize = extractNoun, removePunctuation = T, removeNumbers = T))
      
      # or 
      dtm <- DocumentTermMatrix(docCorp, control=list(tokenize = extractNoun, 
                                                      removePunctuation = T,
                                                      removeNumbers = T,
                                                      wordLenghts = c(2, Inf),
                                                      stopwords = c("ㅠㅠ", "많이", "너무", "정말", "진짜", "있는", "그리고", "하고",
                                                                    "그냥", "같은", "이런", "하는", "솔직히", "많은", "합니다", "보세요")))
      # pre_processing, 빈도수 0인 문서 제거  (전처리로 인해서 단어가 없는 문서)
      
      # pre processing, 빈도수 0인 문서 제거 (전처리로 인해서 단어가 없는 문서)
      
      set.seed(1)
      # resRidge  <- cv.glmnet(as.matrix(dtm), newText$rate2, famil = "binomial", alpha = 0,
      #                        nfolds = 5, type.measure = "class")
      # coefRidge <- coef(resRidge, s = "lambda.min")[,1]sort(coefRi)
      # coefRidge <- ceofRidge[-1]
      # coefRidge <- sort(coefRidge, decreasing = T)
      # coefRidge <- data.frame(ridge = coefRidge, words = names(coefRidge)) %>%
      #               tbl_df
      
      resRidge <- cv.glmnet(as.matrix(dtm), newText$rate2, family="binomial", alpha=0,
                            nfolds=5, type.measure="class") # Ridge: alpha = 0
      coefRidge <- coef(resRidge, s="lambda.min")[,1]
      coefRidge <- coefRidge[-1]
      coefRidge <- sort(coefRidge, decreasing = T)
      coefRidge <- data.frame(ridge = coefRidge, words = names(coefRidge)) %>% tbl_df
      
      