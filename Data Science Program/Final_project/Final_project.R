options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
library(hopach)
library(ggfortify)


seeds <- read.delim("seeds_dataset.txt")

names(seeds) <- c("Area","Perimeter","Compactness","Kernel_length","Kernel_width","Asymmetry","kernel_groove","Class")

seeds$Class <- replace(seeds$Class, seeds$Class==1.00, "Kama") %>% replace( seeds$Class==2.00, "Rosa") %>% replace(seeds$Class==3.00, "Canadian")


seeds <- na.omit(seeds)

seeds_x <- data.frame(seeds[,1:7])

seeds_y <- data.frame(seeds[,8])

df_means=t(apply(seeds_x,2,mean))
df_sds=t(apply(seeds_x,2,sd))

df=sweep(sweep(seeds_x,2,df_means,"-"),2,df_sds,"/")
x_scaled <- df



test_index <- createDataPartition(seeds_x$Perimeter, times = 1, p = 0.2, list = FALSE)
test_x <- x_scaled[test_index,]
test_y <- seeds_y[test_index]
train_x <- x_scaled[-test_index,]
train_y <- seeds_y[-test_index]

train_df <- data.frame(train_x )
train_df["y"] <- train_y

# Logistic regression
#fit <- train(y ~ ., method = "glm", data = train_df)
#glm_preds <- predict(fit,test_x)

# LDA
fit <- train(y ~ ., method = "lda", data = train_df)
lda_preds <- predict(fit,test_x)
levels(lda_preds) <- c(3,1,2)


# QDA
fit <- train(y ~ ., method = "qda", data = train_df)
qda_preds <- predict(fit,test_x)
levels(qda_preds) <- c(3,1,2)


# LOESS
fit <- train(y ~ ., method = 'gamLoess', data = train_df)
loess_preds <- predict(fit,test_x)
levels(loess_preds) <- c(3,1,2)


# Random Forest
fit <- train(y ~ ., method = 'rf', data = train_df,metric = "Accuracy",tuneGrid = expand.grid(.mtry=c(3,5,7,9)))
rf_preds <- predict(fit,test_x)
levels(rf_preds) <- c(3,1,2)


# K nearest neighbours
fit <- train(y ~ ., data = train_df, method = "knn", tuneLength = seq(3,21,2))
knn_preds <- predict(fit,test_x)
levels(knn_preds) <- c(3,1,2)


# K-means
fit <- kmeans(train_x , 3, iter.max = 10)

predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}

kmeans_preds <- predict_kmeans(test_x,fit)
#levels(kmeans_preds) <- c(1,2,3)

ensemble <- cbind(lda =as.numeric(as.character(lda_preds)), qda =  as.numeric(as.character(qda_preds)), rf = as.numeric(as.character(rf_preds)), loess = as.numeric(as.character(loess_preds)), knn = as.numeric(as.character(knn_preds)))


ensemble_preds  <- apply(ensemble[,-1], 1, function(idx) {
  which(tabulate(idx) == max(tabulate(idx)))
})
sapply(ensemble_preds, paste, sep="", collapse = "")

ensemble_preds <- replace(ensemble_preds, ensemble_preds==1, "Kama") %>% replace( ensemble_preds==2, "Rosa") %>% replace(ensemble_preds==3, "Canadian")

mean(ensemble_preds == test_y)
