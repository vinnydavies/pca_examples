library(ggfortify)
library(readr)
library(dplyr)

##### Iris Example #####

# Load data
data(iris)
head(iris, 3)

# Data standardisation
iris$Sepal.Length = scale(iris$Sepal.Length)
iris$Sepal.Width = scale(iris$Sepal.Width)
iris$Petal.Length = scale(iris$Petal.Length)
iris$Petal.Width = scale(iris$Petal.Width)

# 2-Dimension PCA Projection
pca <- prcomp(iris[,1:4], rank=2)

# Visualisation
autoplot(pca, data = iris, colour = 'Species')

# Explained Variance
summary(pca)$importance[3,1:2]

### end ###

##### Digits Example #####

# Load data

mnist_raw <- read_csv("https://pjreddie.com/media/files/mnist_train.csv", col_names = FALSE)
mnist_raw <- data.frame(mnist_raw)

# Data standardisation
y <- factor(as.character(mnist_raw[,1]))
for(i in 2:785){
  if(sum(mnist_raw[,i])>0){
    mnist_raw[,i] <- scale(mnist_raw[,i])
  }
}

# 2-Dimension PCA Projection
pca <- prcomp(mnist_raw[,-1], rank=2)

# Visualisation
autoplot(pca, data = mnist_raw, colour = 'X1')

# Explained Variance
summary(pca)$importance[3,1:2]

### end ###
