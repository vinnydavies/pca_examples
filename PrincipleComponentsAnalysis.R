# Load the relevant packages

library(readr)
library(dplyr)

##### 1. Iris Example #####

# 1.1 Load data
data(iris)
head(iris, 20)

# 1.2. Data standardisation
iris$Sepal.Length = scale(iris$Sepal.Length)
iris$Sepal.Width = scale(iris$Sepal.Width)
iris$Petal.Length = scale(iris$Petal.Length)
iris$Petal.Width = scale(iris$Petal.Width)

# 1.3. 2-Dimension PCA Projection
pca <- prcomp(iris[,1:4], rank=2)

# 1.4. Visualisation
base_colours <- c('red', 'green', 'blue')
colours <- rep('red', nrow(iris))
colours[which(iris$Species == 'versicolor')] <- 'green'
colours[which(iris$Species == 'virginica')] <- 'blue'

components <- cbind(pca$x[,1],-pca$x[,2]) # sign of rotation matrix is arbitrary, switched to match python implementation
colnames(components) <- c('Principal Component 1', 'Principal Component 2')
plot(components, col=colours, main='2 Component PCA',pch=20, 
     cex.lab=1.5, cex=2.5, cex.axis=1.5)
legend(2.3, -1.8, legend=c('setosa', 'versicolor', 'virginica'),
       col=base_colours,pch=20, cex=1.5)

# 1.5. Explained Variance
summary(pca)$importance[3,1:2]

### end ###

##### 2. Digits Example #####

# 2.1. Load data

mnist_raw <- read_csv("https://pjreddie.com/media/files/mnist_train.csv", col_names = FALSE)
mnist_raw <- data.frame(mnist_raw)

# 2.2 Data standardisation
for(i in 2:785){
  if(sum(mnist_raw[,i])>0){
    mnist_raw[,i] <- scale(mnist_raw[,i])
  }
}

# 2.3. 2-Dimension PCA Projection
pca <- prcomp(mnist_raw[,-1], rank=2)

# 2.4. Visualisation
base_colours <- palette(rainbow(10))
colours <- c()
for(i in 1:nrow(mnist_raw)){
  colours[i] <- base_colours[mnist_raw$X1[i]+1]
}

components_raw <- cbind(-pca$x[,1],pca$x[,2]) # sign of rotation matrix is arbitrary, switched to match python implemnetation
colnames(components_raw) <- c('Principal Component 1', 'Principal Component 2')
plot(components_raw, col=colours, main='2 Component PCA',pch=20, xlim=c(-10,35), ylim=c(-20,30), cex.lab=1.5, cex=1.5, cex.axis=1.5)
legend(32.5, 4, legend=as.character(0:9),
       col=base_colours,pch=20, cex=1.5)

components <- components_raw[which(mnist_raw$X1 == '0' | mnist_raw$X1 == '7'),]
#colnames(components) <- c('Principal Component 1', 'Principal Component 2')
plot(components, col=colours[which(mnist_raw$X1 == '0' | mnist_raw$X1 == '7')], main='2 Component PCA',pch=20, xlim=c(-10,35), ylim=c(-20,30), cex.lab=1.5, cex=1.5, cex.axis=1.5)
legend(32.5, -6, legend=c('0','7'),
       col=base_colours[c(1,8)],pch=20, cex=1.5)

components <- components_raw[which(mnist_raw$X1 == '2' | mnist_raw$X1 == '3'),]
#colnames(components) <- c('Principal Component 1', 'Principal Component 2')
plot(components, col=colours[which(mnist_raw$X1 == '2' | mnist_raw$X1 == '3')], main='2 Component PCA',pch=20, xlim=c(-10,35), ylim=c(-20,30), cex.lab=1.5, cex=1.5, cex.axis=1.5)
legend(32.5, -6, legend=c('2','3'),
       col=base_colours[c(3,4)],pch=20, cex=1.5)


# 2.5. Explained Variance
summary(pca)$importance[3,1:2]

### end ###
