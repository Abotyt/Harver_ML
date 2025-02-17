#Section 6
#Pre-processing
library(dslabs)
mnist <- read_mnist()

names(mnist)
dim(mnist$train$images)

class(mnist$train$labels)
table(mnist$train$labels)

# sample 10k rows from training set, 1k rows from test set
set.seed(1990)
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$test$images), 1000)
x_test <- mnist$test$images[index,]
y_test <- factor(mnist$test$labels[index])

library(matrixStats)
library(ggplot2)
sds <- colSds(x)
qplot(sds, bins = 256, color = I("black"))

library(caret)
nzv <- nearZeroVar(x)
image(matrix(1:784 %in% nzv, 28, 28))

col_index <- setdiff(1:ncol(x), nzv)
length(col_index)
#------------------------------------------------------
#KNN
#set column names
colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(x)

#test with subset
n <- 1000
b <- 2
index <- sample(nrow(x), n)
control <- trainControl(method = "cv", number = b, p = .9)
train_knn <- train(x[index ,col_index], y[index],
                   method = "knn",
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)

#cross validation with full dataset
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(x[,col_index], y,
                   method = "knn", 
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)

#fit with final hyper parameter
fit_knn <- knn3(x[ ,col_index], y,  k = 3)

y_hat_knn <- predict(fit_knn,
                     x_test[, col_index],
                     type="class")
cm <- confusionMatrix(y_hat_knn, factor(y_test))
cm$overall["Accuracy"]

cm$byClass[,1:2]

#random forest
library(randomForest)
control <- trainControl(method="cv", number = 5)
grid <- data.frame(mtry = c(1, 5, 10, 25, 50, 100))
train_rf <-  train(x[, col_index], y,
                   method = "rf",
                   nTree = 150,
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 5000)

fit_rf <- randomForest(x[, col_index], y,
                       minNode = train_rf$bestTune$mtry)

y_hat_rf <- predict(fit_rf, x_test[ ,col_index])
cm <- confusionMatrix(y_hat_rf, y_test)
cm$overall["Accuracy"]

#variable importance
imp <- importance(fit_rf)
imp

mat <- rep(0, ncol(x))
mat[col_index] <- imp
image(matrix(mat, 28, 28))


p_max <- predict(fit_rf, x_test[,col_index], type = "prob") 
p_max <- p_max / rowSums(p_max)
p_max <- apply(p_max, 1, max)

ind  <- which(y_hat_rf != y_test)
ind <- ind[order(p_max[ind], decreasing = TRUE)]

rafalib::mypar(1,4)
for(i in ind[1:4]){
  image(matrix(x_test[i,], 28, 28)[, 28:1], 
        main = paste0("Pr(",y_hat_rf[i],")=",round(p_max[i], 2),
                      " but is a ",y_test[i]),
        xaxt="n", yaxt="n")
}

#Ensemble
p_rf <- predict(fit_rf, x_test[,col_index], type = "prob")
class(p_rf)
p_rf <- p_rf / rowSums(p_rf)
p_knn <- predict(fit_knn, x_test[,col_index])
p <- (p_rf + p_knn)/2
y_pred <- factor(apply(p, 1, which.max)-1)
y_pred
confusionMatrix(y_pred, y_test)$overall["Accuracy"]

#----------------------------------------

#Q1
models <- c("glm", "lda", "naive_bayes", "knn", "gamLoess", "qda", "rf")

library(caret)
library(dslabs)
library(tidyverse)
set.seed(1)
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 
names(fits) <- models

names(fits[1])
#Q2
y_hats <- sapply(fits, function(model){ 
  predict(model, mnist_27$test) 
}) 
class(y_hats)
dim(y_hats)

#Q3

acc <- colMeans(y_hats == mnist_27$test$y)
acc
class(acc)
mean(acc)

#Q4

votes <- rowCounts(y_hats == "7")
y_hat <- ifelse(votes > 3, "7", "2")
accuracy_ensemble <- mean(y_hat == mnist_27$test$y)
accuracy_ensemble

#Q5
for (idx in 1:length(acc)) {
  if(acc[idx] > accuracy_ensemble) {print(acc[idx])}
}

#Q6

train_acc <- sapply(models, function(model){ 
  print(model)
  fit<-train(y ~ ., method = model, data = mnist_27$train)
  max(fit$results$Accuracy)
}) 
mean(train_acc)

#acc_hat <- sapply(fits, function(fit) max(fit$results$Accuracy))
#acc_hat
#mean(acc_hat)


#Q7

selected_accuracy <- fits[fits>=0.8]
selected_model <- names(selected_accuracy)
dim(y_hats[,selected_model])
votes <- rowMeans(y_hats[,selected_model] == "7")
prediction <- ifelse(votes > 0.5, "7", "2")
accuracy_ensemble <- mean(prediction == mnist_27$test$y)
accuracy_ensemble

#Alternative
#ind <- train_acc >= 0.8
#votes <- rowMeans(y_hats[,ind] == "7")
#y_hat <- ifelse(votes>=0.5, 7, 2)
#mean(y_hat == mnist_27$test$y)