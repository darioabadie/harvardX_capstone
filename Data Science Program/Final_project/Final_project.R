
set.seed(7)

# Libraries
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
library(hopach)
library(ggfortify)
library(ggplot2)
library(gridExtra)
library(grid)

## Data load

seeds <- read.delim("seeds_dataset.txt") 

head(seeds) %>% # Sample of the dataset
  print.data.frame()

summary(seeds) # Summary to check if there are missing values.

## Data processing

seeds <- na.omit(seeds) # Omit rows with NA values

names(seeds) <- c("Area","Perimeter","Compactness","Kernel_length","Kernel_width","Asymmetry","kernel_groove","Class") # Add the names of each column according to their meaning

y <- seeds$Class # We save the original values since they will be useful later

seeds$Class <- replace(seeds$Class, seeds$Class==1.00, "Kama") %>% replace( seeds$Class==2.00, "Rosa") %>% replace(seeds$Class==3.00, "Canadian") # Replace the class (1, 2 and 3) with their names too (Kama, Rosa and Canadian)

## Data exploration 

# We generate histograms for each feature in order to visualize how these predictors behave for each class. 
D1 <-    ggplot(seeds, aes(x=Area, colour=Class, fill=Class)) +
  geom_density(alpha=.3) +
  xlab("Area (mm2)") +  
  ylab("Density")+
  theme(legend.position="none")

D2 <- ggplot(seeds, aes(x=Perimeter, colour=Class, fill=Class)) +
  geom_density(alpha=.3) +
  xlab("Perimeter(mm)") +  
  ylab("Density")

D3 <- ggplot(seeds, aes(x=Compactness, colour=Class, fill=Class)) +
  geom_density(alpha=.3) +
  xlab("Compactness") +  
  ylab("Density")+
  theme(legend.position="none")

D4 <- ggplot(seeds, aes(x=Kernel_length, colour=Class, fill=Class)) +
  geom_density(alpha=.3) +
  xlab("Kernel_length") +  
  ylab("Density")+
  theme(legend.position="none")

D5 <- ggplot(seeds, aes(x=Kernel_width, colour=Class, fill=Class)) +
  geom_density(alpha=.3) +
  xlab("Kernel_width") +  
  ylab("Density")+
  theme(legend.position="none")

D6 <- ggplot(seeds, aes(x=Asymmetry, colour=Class, fill=Class)) +
  geom_density(alpha=.3) +
  xlab("Asymmetry") +  
  ylab("Density")+
  theme(legend.position="none")

D7 <- ggplot(seeds, aes(x=kernel_groove, colour=Class, fill=Class)) +
  geom_density(alpha=.3) +
  xlab("kernel_groove") +  
  ylab("Density")+
  theme(legend.position="none")

grid.arrange(D1 + ggtitle(""), # Plot all density visualizations
             D2  + ggtitle(""),
             D3 + ggtitle(""),
             D4 + ggtitle(""),
             D5  + ggtitle(""),
             D6 + ggtitle(""),
             D7 + ggtitle(""),
             nrow = 4,
             top = textGrob("Seeds Density Plot", 
                            gp=gpar(fontsize=15))
)

# Additionally we create *Box plots* for each feature to complement the analysis.

grid.arrange(
  ggplot(seeds, aes(Class, Area)) + geom_boxplot() + ggtitle(""),
  ggplot(seeds, aes(Class, Perimeter)) + geom_boxplot() + ggtitle(""),
  ggplot(seeds, aes(Class, Compactness)) + geom_boxplot() + ggtitle(""),
  ggplot(seeds, aes(Class, Kernel_length)) + geom_boxplot() + ggtitle(""),
  ggplot(seeds, aes(Class, Kernel_width)) + geom_boxplot() + ggtitle(""),
  ggplot(seeds, aes(Class, Asymmetry)) + geom_boxplot() + ggtitle(""),
  ggplot(seeds, aes(Class, kernel_groove)) + geom_boxplot() + ggtitle(""),
  
  top = textGrob("Wheat seeds Box Plot", 
                 gp=gpar(fontsize=15)))

## Classification system

# Normalization x = (X-mu)/sd

seeds_x <- data.frame(seeds[,1:7]) # Separate predictors from target variable
seeds_y <- data.frame(seeds[,8])

df_means=t(apply(seeds_x,2,mean)) # Obtain the mean value for each feature
df_sds=t(apply(seeds_x,2,sd)) # Obtain the standard deviation for each feature
df=sweep(sweep(seeds_x,2,df_means,"-"),2,df_sds,"/") # Substract the mean to each value and then divide by the standard deviation.

x_scaled <- df

# Training and testing sets

test_index <- createDataPartition(seeds_x$Perimeter, times = 1, p = 0.2, list = FALSE) # Create partition 80% Training - 20% Testing

test_x <- x_scaled[test_index,] # Testing set
test_y <- y[test_index]

train_x <- x_scaled[-test_index,] # Training set
train_y <- seeds_y[-test_index]
train_df <- data.frame(train_x )
train_df["y"] <- train_y

# Classification algorithms

# LDA
fit <- train(y ~ ., method = "lda", data = train_df) # Creation of model
lda_preds <- predict(fit,test_x) # Predictions using the model
levels(lda_preds) <- c(3,1,2) # Change levels according to data pre processing.
lda_accuracy <- mean(lda_preds == test_y) # LDA accuracy
results <- data_frame(Algorithm = "LDA", Accuracy = lda_accuracy ) # Table with results

# QDA
fit <- train(y ~ ., method = "qda", data = train_df)
qda_preds <- predict(fit,test_x)
levels(qda_preds) <- c(3,1,2)
qda_accuracy <- mean(qda_preds == test_y)
results <- bind_rows(results, data_frame(Algorithm = "QDA", Accuracy = qda_accuracy ))


# LOESS
fit <- train(y ~ ., method = 'gamLoess', data = train_df)
loess_preds <- predict(fit,test_x)
levels(loess_preds) <- c(3,1,2)
loess_accuracy <- mean(loess_preds == test_y)
results <- bind_rows(results, data_frame(Algorithm = "LOESS", Accuracy = loess_accuracy ))

# Random Forest
fit <- train(y ~ ., method = 'rf', data = train_df,metric = "Accuracy",tuneGrid = expand.grid(.mtry=c(3,5,7,9)))
rf_preds <- predict(fit,test_x)
levels(rf_preds) <- c(3,1,2)
rf_accuracy <- mean(rf_preds == test_y)
results <- bind_rows(results, data_frame(Algorithm = "RF", Accuracy = rf_accuracy ))


# K nearest neighbours
fit <- train(y ~ ., data = train_df, method = "knn", tuneLength = seq(3,21,2))
knn_preds <- predict(fit,test_x)
levels(knn_preds) <- c(3,1,2)
knn_accuracy <- mean(knn_preds == test_y)
results <- bind_rows(results, data_frame(Algorithm = "Knn", Accuracy = knn_accuracy ))

# Ensemble

# We create a dataframe containing the results of each algorithm. 
# It is necessary to convert every prediction from factor to numeric.
ensemble <- cbind(lda =as.numeric(as.character(lda_preds)), qda =  as.numeric(as.character(qda_preds)), rf = as.numeric(as.character(rf_preds)), loess = as.numeric(as.character(loess_preds)), knn = as.numeric(as.character(knn_preds)))

# Now we calculate the majority vote for each sample

ensemble_preds  <- apply(ensemble[,-1], 1, function(idx) {
  which(tabulate(idx) == max(tabulate(idx)))
})
sapply(ensemble_preds, paste, sep="", collapse = "")

# Accuracy is calculated
ensemple_accuracy <- mean(na.omit(as.numeric(as.character(ensemble_preds)) == test_y))
results <- bind_rows(results, data_frame(Algorithm = "Ensemble", 
                                         Accuracy = ensemple_accuracy )) # Save results

## Results

results %>% knitr::kable()

