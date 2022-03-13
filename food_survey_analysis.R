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

##load packages

library("readxl")
library("quanteda")

library(ggplot2)
library(tidyverse)
library(writexl)



##read data----
data<-read_excel("food_survey.xlsx")

corp=corpus(data, text_field = 'healthy_food') #create the corpus
corp

dtm=dfm(corp, stem=T, remove = stopwords('en'), remove_punct=T)
dtm #sparse = of all the cells contain the value 0

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









