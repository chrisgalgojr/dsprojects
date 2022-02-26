---
###food survey analysis
###text analysis in R
###february 25, 2022
---
#install packages----

install.packages("readxl")
install.packages("tidytext")
install.packages("widyr")
install.packages("igraph")
install.packages("ggraph")
install.packages("dplyr")
install.packages("tm")

##load packages

library("readxl")
library("tidytext")
library("widyr")
library("igraph")
library("ggraph")
library("dplyr")
library("tm")
library(ggplot2)
library(tidyverse)


##read data----
data<-read_excel("food_survey.xlsx")

#summary statistics----
summary(data)
head(data)

healthy_food<-c(data$healthy_food)
unhealthy_food<-c(data$unhealthy_food)
future_food<-c(data$future_food)
facilitate_food_future<-c(data$facilitate_food__future)
inhibity_food_future<-c(data$inhibit_food_future)
information<-c(data$information)
consumption_covid<-c(data$consumption_covid)

##text analysis in r -EDA
##review words

corpus1<-SimpleCorpus(VectorSource(healthy_food))
View(corpus1)

# 1. Stripping any extra white space:
corpus1 <- tm_map(corpus1, stripWhitespace)
# 2. Transforming everything to lowercase
corpus1 <- tm_map(corpus1, content_transformer(tolower))
# 3. Removing numbers 
corpus1 <- tm_map(corpus1, removeNumbers)
# 4. Removing punctuation
corpus1 <- tm_map(corpus1, removePunctuation)
# 5. Removing stop words
corpus1 <- tm_map(corpus1, removeWords, stopwords("english"))

stopwords("english")

corpus1[[1]]$content
## method by Brian Ward 2019 https://towardsdatascience.com/a-light-introduction-to-text-analysis-in-r-ea291a9865a8
#DTM
DTM <- DocumentTermMatrix(corpus1)

##Data frame for DTM
sums <- as.data.frame(colSums(as.matrix(DTM)))
class(sums)
sums<-rownames_to_column(sums)
colnames(sums) <- c("term", "count")
sums <- arrange(sums, desc(count))
sums <- subset(sums, count >=10)

##Plotting healthy food
ggplot(sums,aes(x=factor(term), y=count))+
  geom_col()+
  coord_flip()


 



