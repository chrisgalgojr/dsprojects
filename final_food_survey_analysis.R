---
###food survey analysis
###text analysis in R
###february 25, 2022
---
#install packages----

install.packages("tidyverse")
install.packages("quanteda")

install.packages("readxl")
install.packages("tidytext")
install.packages("widyr")
install.packages("igraph")
install.packages("ggraph")
install.packages("dplyr")
install.packages("tm")
install.packages("writexl")

##load packages

library("readxl")
library("quanteda")


library("tidytext")
library("widyr")
library("igraph")
library("ggraph")
library("dplyr")
library("tm")
library(ggplot2)
library(tidyverse)
library(writexl)



##read data----
data<-read_excel("food_survey.xlsx")

##Subsetting
#healthy_food<-c(data$healthy_food)
#nhealthy_food<-c(data$unhealthy_food)
#future_food<-c(data$future_food)
#facilitate_food_future<-c(data$facilitate_food__future)
#inhibity_food_future<-c(data$inhibit_food_future)
#information<-c(data$information)
#consumption_covid<-c(data$consumption_covid)

##text analysis in r -EDA
##review words

#corpus1<-SimpleCorpus(VectorSource(healthy_food))
#corpus2<-SimpleCorpus(VectorSource(unhealthy_food))
#corpus3<-SimpleCorpus(VectorSource(future_food))
#corpus4<-SimpleCorpus(VectorSource(facilitate_food_future))
#corpus5<-SimpleCorpus(VectorSource(inhibity_food_future))
#corpus6<-SimpleCorpus(VectorSource(information))
#corpus7<-SimpleCorpus(VectorSource(consumption_covid))#

corp=corpus(data, text_field = 'healthy_food') #create the corpus
corp


dtm=dfm(corp, stem=T, remove = stopwords('en'), remove_punct=T)
dtm

#sparse = of all the cells contain the value 0

dtm <- dfm_trim(dtm, min_termfreq = 5)
dtm

textplot_wordcloud(dtm, max_words = 50)                          ## top 50 (most frequent) words
textplot_wordcloud(dtm, max_words = 50, color = c('blue','red')) ## change colors
textstat_frequency(dtm, n = 10)

male <- docvars(dtm)$sex == 'Male' 
male_dtm <- dtm[male,]
textplot_wordcloud(male_dtm, max_words = 25)
textstat_frequency(male_dtm, n = 10)


female <- docvars(dtm)$sex == 'Female' 
female_dtm <- dtm[female,]
textplot_wordcloud(female_dtm, max_words = 25)
textstat_frequency(female_dtm, n = 10)


# 1. Stripping any extra white space:
corpus1 <- tm_map(corpus1, stripWhitespace)
corpus2 <- tm_map(corpus2, stripWhitespace)
corpus3 <- tm_map(corpus3, stripWhitespace)
corpus4 <- tm_map(corpus4, stripWhitespace)
corpus5 <- tm_map(corpus5, stripWhitespace)
corpus6 <- tm_map(corpus6, stripWhitespace)
corpus7 <- tm_map(corpus7, stripWhitespace)

# 2. Transforming everything to lowercase
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus2 <- tm_map(corpus2, content_transformer(tolower))
corpus3 <- tm_map(corpus3, content_transformer(tolower))
corpus4 <- tm_map(corpus4, content_transformer(tolower))
corpus5 <- tm_map(corpus5, content_transformer(tolower))
corpus6 <- tm_map(corpus6, content_transformer(tolower))
corpus7 <- tm_map(corpus7, content_transformer(tolower))

# 3. Removing numbers 
corpus1 <- tm_map(corpus1, removeNumbers)
corpus2 <- tm_map(corpus3, removeNumbers)
corpus3 <- tm_map(corpus3, removeNumbers)
corpus4 <- tm_map(corpus4, removeNumbers)
corpus5 <- tm_map(corpus5, removeNumbers)
corpus6 <- tm_map(corpus6, removeNumbers)
corpus7 <- tm_map(corpus7, removeNumbers)

# 4. Removing punctuation
corpus1 <- tm_map(corpus1, removePunctuation)
corpus2 <- tm_map(corpus2, removePunctuation)
corpus3 <- tm_map(corpus3, removePunctuation)
corpus4 <- tm_map(corpus4, removePunctuation)
corpus5 <- tm_map(corpus5, removePunctuation)
corpus6 <- tm_map(corpus6, removePunctuation)
corpus7 <- tm_map(corpus7, removePunctuation)

# 5. Removing stop words
corpus1 <- tm_map(corpus1, removeWords, stopwords("english"))
corpus2 <- tm_map(corpus2, removeWords, stopwords("english"))
corpus3 <- tm_map(corpus3, removeWords, stopwords("english"))
corpus4 <- tm_map(corpus4, removeWords, stopwords("english"))
corpus5 <- tm_map(corpus5, removeWords, stopwords("english"))
corpus6 <- tm_map(corpus6, removeWords, stopwords("english"))
corpus7 <- tm_map(corpus7, removeWords, stopwords("english"))

#stopwords("english")
#corpus1[[1]]$content

## method by Brian Ward 2019 https://towardsdatascience.com/a-light-introduction-to-text-analysis-in-r-ea291a9865a8
#DTM
DTM1 <- DocumentTermMatrix(corpus1)
DTM2 <- DocumentTermMatrix(corpus2)
DTM3 <- DocumentTermMatrix(corpus3)
DTM4 <- DocumentTermMatrix(corpus4)
DTM5 <- DocumentTermMatrix(corpus5)
DTM6 <- DocumentTermMatrix(corpus6)
DTM7 <- DocumentTermMatrix(corpus7)



##Data frame for DTM
sums1 <- as.data.frame(colSums(as.matrix(DTM1)))
sums1<-rownames_to_column(sums1)
colnames(sums1) <- c("term", "count")
sums1 <- arrange(sums1, desc(count))
#sums1 <- subset(sums1, count >=10)
write_xlsx(sums1,"iCloud Drive\\Desktop\\dsprojects\\healthyfood.xlsx")



class(sums1)

sums2 <- as.data.frame(colSums(as.matrix(DTM2)))
sums2<-rownames_to_column(sums2)
colnames(sums2) <- c("term", "count")
sums2 <- arrange(sums2, desc(count))
sums2 <- subset(sums2, count >=10)

sums3 <- as.data.frame(colSums(as.matrix(DTM3)))
sums3<-rownames_to_column(sums3)
colnames(sums3) <- c("term", "count")
sums3 <- arrange(sums3, desc(count))
sums3 <- subset(sums3, count >=10)

sums3 <- as.data.frame(colSums(as.matrix(DTM3)))
sums3 <-rownames_to_column(sums3)
colnames(sums3) <- c("term", "count")
sums3 <- arrange(sums3, desc(count))
sums3 <- subset(sums3, count >=10)


sums4 <- as.data.frame(colSums(as.matrix(DTM4)))
sums4<-rownames_to_column(sums4)
colnames(sums4) <- c("term", "count")
sums4 <- arrange(sums4, desc(count))
sums4 <- subset(sums4, count >=10)


sums5 <- as.data.frame(colSums(as.matrix(DTM5)))
sums5<-rownames_to_column(sums5)
colnames(sums5) <- c("term", "count")
sums5 <- arrange(sums5, desc(count))
sums5 <- subset(sums5, count >=10)


sums6 <- as.data.frame(colSums(as.matrix(DTM6)))
sums6<-rownames_to_column(sums6)
colnames(sums6) <- c("term", "count")
sums6 <- arrange(sums6, desc(count))
sums6 <- subset(sums6, count >=10)

sums7 <- as.data.frame(colSums(as.matrix(DTM7)))
sums7<-rownames_to_column(sums7)
colnames(sums7) <- c("term", "count")
sums7 <- arrange(sums7, desc(count))
sums7 <- subset(sums7, count >=10)



##Plotting healthy food
ggplot(sums1,aes(x=factor(term), y=count))+
  geom_col()+
  coord_flip()

ggplot(sums2,aes(x=factor(term), y=count))+
  geom_col()+
  coord_flip()
 
ggplot(sums3,aes(x=factor(term), y=count))+
  geom_col()+
  coord_flip()

ggplot(sums4,aes(x=factor(term), y=count))+
  geom_col()+
  coord_flip()

ggplot(sums5,aes(x=factor(term), y=count))+
  geom_col()+
  coord_flip()

ggplot(sums6,aes(x=factor(term), y=count))+
  geom_col()+
  coord_flip()

ggplot(sums7,aes(x=factor(term), y=count))+
  geom_col()+
  coord_flip()









