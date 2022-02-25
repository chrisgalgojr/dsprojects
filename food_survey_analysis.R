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

##load packages

library("readxl")
library("tidytext")
library("widyr")
library("igraph")
library("ggraph")
library("dplyr")


##read data----
data<-read_excel("food_survey.xlsx")
data

#summary statistics----
summary(data)
head(data)

##text analysis in r -EDA
##review words

tidy_data <- data$healthy_food %>% 
  unnest_tokens(word,data$healthy_food)


