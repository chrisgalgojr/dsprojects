---
###food survey analysis
###text analysis in R
###february 25, 2022
---
#install packages----

install.packages("tidyverse")
install.packages("quanteda")
install.packages("dplyr")
install.packages("tm")
install.packages("writexl")
install.packages("forcats")

##load packages----

library("readxl")
library("quanteda")
library(ggplot2)
library(tidyverse)
library(writexl)

##read data----
data<-read_excel("food_survey.xlsx")

#Text analysis
#Creating corpus
corp1=corpus(data, text_field = 'healthy_food') #create the corpus_healthyfood
corp2=corpus(data, text_field = 'unhealthy_food') #create the corpus_unhealthyfood
corp3=corpus(data, text_field = 'future_food') #create the corpus_futurefood
corp4=corpus(data, text_field = 'facilitate_food_future') #create the corpus_fff
corp5=corpus(data, text_field = 'inhibit_food_future') #create the corpus_inhibitff
corp6=corpus(data, text_field = 'info_source') #create the corpus_info_source
corp7=corpus(data, text_field = 'consumption_covid') #create the corpus_consumption_covid

#Creating DTM
dtm1=dfm(corp1, stem=T, remove = stopwords('en'), remove_punct=T)
dtm2=dfm(corp2, stem=T, remove = stopwords('en'), remove_punct=T)
dtm3=dfm(corp3, stem=T, remove = stopwords('en'), remove_punct=T)
dtm4=dfm(corp4, stem=T, remove = stopwords('en'), remove_punct=T)
dtm5=dfm(corp5, stem=T, remove = stopwords('en'), remove_punct=T)
dtm6=dfm(corp6, stem=T, remove = stopwords('en'), remove_punct=T)
dtm7=dfm(corp7, stem=T, remove = stopwords('en'), remove_punct=T) #sparse = of all the cells contain the value 0

#dtm1 <- dfm_trim(dtm1, min_termfreq = 5)
#textplot_wordcloud(dtm, max_words = 50)                          ## top 50 (most frequent) words
textplot_wordcloud(dtm1, max_words = 50, color = c('blue','red')) ## change colors
textplot_wordcloud(dtm2, max_words = 50, color = c('blue','red')) ## change colors
textplot_wordcloud(dtm3, max_words = 50, color = c('blue','red')) ## change colors
textplot_wordcloud(dtm4, max_words = 50, color = c('blue','red')) ## change colors
textplot_wordcloud(dtm5, max_words = 50, color = c('blue','red')) ## change colors
textplot_wordcloud(dtm6, max_words = 50, color = c('blue','red')) ## change colors
textplot_wordcloud(dtm7, max_words = 50, color = c('blue','red')) ## change colors

##
textstat_frequency(dtm1, n = 30)
textstat_frequency(dtm2, n = 20)
textstat_frequency(dtm3, n = 20)
textstat_frequency(dtm4, n = 10)
textstat_frequency(dtm5, n = 10)
textstat_frequency(dtm6, n = 10)
textstat_frequency(dtm7, n = 10)

##Plotting DTM
dataframe1 <- as.data.frame(colSums(as.matrix(dtm1)))
dataframe1 <-rownames_to_column(dataframe1)
colnames(dataframe1) <- c("Feature", "Frequency")
dataframe1 <- arrange(dataframe1, desc(Frequency))
dataframe1 <- dataframe1[-c(3,6,11),]
healthyfood <- subset(dataframe1, Frequency >=5)
ggplot(healthyfood,aes(x=reorder(Feature,Frequency), y=Frequency))+
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



