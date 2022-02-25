##Tutorial for Prinicipal component analysis ----

#Load data ----

data(iris)
head(iris)
summary(iris)

#PCA----
myPr <-prcomp(iris[, -5], scale =TRUE)
plot(iris$Sepal.Length, iris$Sepal.Width)


#PCA with select variables ----
#prcomp (-Sepal.ength + Petal Width, data=iris)

#When doing a PCA it is important that we use a scale and center for the relationship to stay the same
#center a variable - (subtract) mean
#scaled divide by the standard deviation

