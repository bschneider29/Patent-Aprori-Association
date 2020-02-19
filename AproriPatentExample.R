patentsdata <- read.csv("patents_fraud.csv")
install.packages("sqldf")
install.packages("gdata")
install.packages("ggplot2")
install.packages("ggmap")
library(sqldf)
library(gdata)
library(ggplot2)
library(ggmap)

install.packages("arules")
install.packages("arulesViz")
install.packages("datasets")
library(arules)
library(arulesViz)
library(datasets)

install.packages("tm")  
install.packages("SnowballC") 
library("tm")
library("SnowballC")
install.packages("wordcloud") 
library("wordcloud")

################# Normalization ##############
(any(is.na(patentsdata$patents.patent_type)))
(any(is.na(patentsdata$patents.patent_num_claims)))

glm.fit <- glm(patentsdata$patents.patent_type ~ patentsdata$patents.patent_num_claims, data = patentsdata, family = binomial)
summary(glm.fit)

trainingdf<-data.frame(patentsdata$patents.patent_type,patentsdata$patents.patent_num_claims) #Split Train

predicted <- predict(glm.fit, trainingdf, type="response")

predicted[1:20]


docs <- Corpus(VectorSource(patentsdata$patents.patent_abstract))
docs <- tm_map(docs, content_transformer(tolower))

docs <- tm_map(docs, removeNumbers)

docs <- tm_map(docs, removeWords, c(stopwords("english"),"may","one","information","user","method","device","can","include","includes","first","associated","based","least","transactions")) 

docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 

docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

################# Normalization ##############

################ Visualizations/Word Frequencies ############
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

findFreqTerms(dtm, lowfreq = 4)

findAssocs(dtm, terms = "fraud", corlimit = 0.1)

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

median(patentsdata$patents.patent_num_claims)
max(patentsdata$patents.patent_num_claims)
mean(patentsdata$patents.patent_num_claims)

################ Visualizations/Word Frequencies ############

################# Aprori's ##############
claims<-patentsdata$patents.patent_num_claims
patentsdata$patents.patent_num_claims <- cut(claims, breaks =c(1,20,200,600,Inf), labels=c("Low","Medium","High","Extreme"))

aproridf <-patentsdata[,5:8]
aproridf$patents.patent_firstnamed_assignee_id<-NULL

apriori$patents.patent_num_claims

rules<-apriori(data=aproridf, parameter=list(supp=0.001,conf = 0.15,minlen=2), 
               appearance = list(default="rhs",lhs="patents.patent_type=utility"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
rules
detach(package:tm, unload=TRUE)
inspect(rules[1:2])

rules<-apriori(data=aproridf, parameter=list(supp=0.001,conf = 0.15,minlen=2), 
               appearance = list(default="rhs",lhs="patents.patent_type=statutory invention registration"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
rules
inspect(rules[1:2])

rules<-apriori(data=aproridf, parameter=list(supp=0.001,conf = 0.15,minlen=2), 
               appearance = list(default="lhs",rhs="patents.patent_num_claims=Extreme"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
rules
inspect(rules[1:2])
################# Aprori's ##############

#Quick View
sqldf("select * from aproridf where [patents.patent_num_claims] = 'High' ")
