---
###food survey analysis
###text analysis in R
###february 25, 2022
---
#install packages----

install.packages("tidyverse")
install.packages("quanteda")
install.packages("dplyr")

##load packages----

library(quanteda)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(readxl)

##read data----
data<-read_excel("food_survey.xlsx")

#Text analysis----
#Creating corpus for variables----

corp1=corpus(data, text_field = 'healthy_food') 
corp2=corpus(data, text_field = 'unhealthy_food')
corp3=corpus(data, text_field = 'future_food') 
corp4=corpus(data, text_field = 'facilitate_food_future')
corp5=corpus(data, text_field = 'inhibit_food_future') 
corp6=corpus(data, text_field = 'info_source')
corp7=corpus(data, text_field = 'consumption_covid') 

#Creating DTM
dtm1=dfm(corp1, stem=T, remove = stopwords('en'), remove_punct=T)
dtm2=dfm(corp2, stem=T, remove = stopwords('en'), remove_punct=T)
dtm3=dfm(corp3, stem=T, remove = stopwords('en'), remove_punct=T)
dtm4=dfm(corp4, stem=T, remove = stopwords('en'), remove_punct=T)
dtm5=dfm(corp5, stem=T, remove = stopwords('en'), remove_punct=T)
dtm6=dfm(corp6, stem=T, remove = stopwords('en'), remove_punct=T)
dtm7=dfm(corp7, stem=T, remove = stopwords('en'), remove_punct=T)

#Selecting the top 50 most frequent words
#dtm1 <- dfm_trim(dtm1, min_termfreq = 5)
#textplot_wordcloud(dtm, max_words = 50) 

textplot_wordcloud(dtm1, max_words = 50, color = c('blue','red')) 
textplot_wordcloud(dtm2, max_words = 50, color = c('blue','red')) 
textplot_wordcloud(dtm3, max_words = 50, color = c('blue','red'))
textplot_wordcloud(dtm4, max_words = 50, color = c('blue','red')) 
textplot_wordcloud(dtm5, max_words = 50, color = c('blue','red')) 
textplot_wordcloud(dtm6, max_words = 50, color = c('blue','red')) 
textplot_wordcloud(dtm7, max_words = 50, color = c('blue','red')) 

##
textstat_frequency(dtm1, n = 30)
textstat_frequency(dtm2, n = 30)
textstat_frequency(dtm3, n = 30)
textstat_frequency(dtm4, n = 30)
textstat_frequency(dtm5, n = 30)
textstat_frequency(dtm6, n = 30)
textstat_frequency(dtm7, n = 30)

#Keyword-in-context
k <- kwic(tokens(corp4), 'plant', window = 5)
head(k, 10)    ## only view first 10 results

##Plotting DTM
#Healthyfood
dataframe1 <- as.data.frame(colSums(as.matrix(dtm1)))
dataframe1 <-rownames_to_column(dataframe1)
colnames(dataframe1) <- c("Feature", "Frequency")
dataframe1 <- arrange(dataframe1, desc(Frequency))
dataframe1 <- dataframe1[-c(3,6,11),]
#healthyfood <- subset(dataframe1, Frequency >=5)
healthyfood  <- dataframe1[1:20, ]
ggplot(healthyfood,aes(x=reorder(Feature,Frequency), y=Frequency))+
  geom_col() + 
  coord_flip()+
  xlab("Feature") + ylab("Frequency")

#Unhealthyfood
dataframe2 <- as.data.frame(colSums(as.matrix(dtm2)))
dataframe2 <-rownames_to_column(dataframe2)
colnames(dataframe2) <- c("Feature", "Frequency")
dataframe2 <- arrange(dataframe2, desc(Frequency))
dataframe2 <- dataframe2[-c(2,10,17),]
unhealthyfood  <- dataframe2[1:20, ]
#unhealthyfood <- subset(dataframe2, Frequency >=5)
ggplot(unhealthyfood,aes(x=reorder(Feature,Frequency), y=Frequency))+
  geom_col() + 
  coord_flip()+
  xlab("Feature") + ylab("Frequency")

#Futurefood
dataframe3 <- as.data.frame(colSums(as.matrix(dtm3)))
dataframe3 <-rownames_to_column(dataframe3)
colnames(dataframe3) <- c("Feature", "Frequency")
dataframe3 <- arrange(dataframe3, desc(Frequency))
dataframe3 <- dataframe3[-c(9,15,16),]
futurefood <- dataframe3[1:20, ]
#futurefood <- subset(dataframe3, Frequency >=5)
ggplot(futurefood,aes(x=reorder(Feature,Frequency), y=Frequency))+
  geom_col() + 
  coord_flip()+
  xlab("Feature") + ylab("Frequency")

#Facilitatefoodfuture
dataframe4 <- as.data.frame(colSums(as.matrix(dtm4)))
dataframe4 <-rownames_to_column(dataframe4)
colnames(dataframe4) <- c("Feature", "Frequency")
dataframe4 <- arrange(dataframe4, desc(Frequency))
dataframe4 <- dataframe4[-c(9,15,16),]
facilitatefuturefood <- dataframe4[1:20, ]
#facilitatefuturefood <- subset(dataframe4, Frequency >=5)
ggplot(facilitatefuturefood,aes(x=reorder(Feature,Frequency), y=Frequency))+
  geom_col() + 
  coord_flip()+
  xlab("Feature") + ylab("Frequency")

#Inhibitfoodfuture - we select words that address the context 
dataframe5 <- as.data.frame(colSums(as.matrix(dtm5)))
dataframe5 <-rownames_to_column(dataframe5)
colnames(dataframe5) <- c("Feature", "Frequency")
dataframe5 <- arrange(dataframe5, desc(Frequency))
#dataframe5 <- dataframe5[-c(9,15,16),]
inhibitfuturefood <- subset(dataframe5, Frequency >=5)
ggplot(inhibitfuturefood,aes(x=reorder(Feature,Frequency), y=Frequency))+
  geom_col() + 
  coord_flip()+
  xlab("Feature") + ylab("Frequency")

#info_source - we select words that address the context 
dataframe6 <- as.data.frame(colSums(as.matrix(dtm6)))
dataframe6 <-rownames_to_column(dataframe6)
colnames(dataframe6) <- c("Feature", "Frequency")
dataframe6 <- arrange(dataframe6, desc(Frequency))
dataframe6 <- dataframe6[-c(3,4,5,14,19,20,29,32),]
ggplot(info_source,aes(x=reorder(Feature,Frequency), y=Frequency))+
  geom_col() + 
  coord_flip()+
  xlab("Feature") + ylab("Frequency")

dataframe6 <- as.data.frame(colSums(as.matrix(dtm6)))
dataframe6 <-rownames_to_column(dataframe6)
colnames(dataframe6) <- c("Feature", "Frequency")
dataframe6 <-arrange(dataframe6, desc(Frequency)) 
dataframe6 <- dataframe6[-c(3,4,5,14,19,20,29,32),]
info_source <-dataframe6[1:20, ]
ggplot(info_source,aes(x=reorder(Feature,Frequency), y=Frequency))+
  geom_col() + 
  coord_flip()+
  xlab("Feature") + ylab("Frequency")




male <- docvars(dtm1)$sex == 'Male' 
male_dtm <- dtm1[male,]
#textplot_wordcloud(male_dtm, max_words = 25)
textstat_frequency(male_dtm, n = 20)


female <- docvars(dtm1)$sex == 'Female' 
female_dtm <- dtm1[female,]
#textplot_wordcloud(female_dtm, max_words = 25)
textstat_frequency(female_dtm, n = 20)



