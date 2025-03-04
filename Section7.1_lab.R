#Breast Cancer
options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)

x <- brca$x
y <- brca$y

#analysis
#size data sets
dim(x)
length(y)

#proportion of malignant
mean(y=='M')

#find highest column mean
which(colMeans(x) == max(colMeans(x)))

#find lowest standard deviation
which(apply(x, 2, sd) == min(apply(x, 2, sd)))

#scaling each columns
#centered
col_mean <- colMeans(x) 
subtract_means <- sweep(x, 2, col_mean)

#scaled
sd <- apply(x, 2, sd)
scaled_x <-  sweep(subtract_means, 2, sd, '/')
length(scaled_x[,1])
sd(scaled_x[,1])
median(scaled_x[,1])
#PCA Analysis
# propotion of variance of first component
pca <- prcomp(scaled_x)

plot(pca$sdev)

var_explained <- cumsum(pca$sdev^2/sum(pca$sdev)^2)
var_explained

var_explained[1]/var_explained[length(var_explained)]

#number of components that are required to explain at least 90%
#components that explain more than 90%
component90 = var_explained[var_explained>var_explained[length(var_explained)] * 0.9]
which(var_explained == component90[1])

#Alternative
summary(pca)

#Plotting PCs
library(ggrepel)

pcs <- data.frame(pca$x, type=y) 

pcs %>%  ggplot(aes(PC1, PC2, color=type)) + 
  geom_point() 

#boxplot PCs
library(reshape2) 
#transform the dataset from wide to long(columns:type, variables, and values)
data_melt <- melt(data=pcs, 
                  id.vars='type', 
                  measure.vars=c(colnames(pcs[1:10]))
                  )
colnames(data_melt)
#plot boxplot color by type
ggplot(data_melt) + 
  geom_boxplot(aes(x=variable, y=value, color=type)) 

#alternative use gather to reshape the dataset
#data.frame(type = brca$y, pca$x[,1:10]) %>%
#  gather(key = "PC", value = "value", -type) %>%
#  ggplot(aes(PC, value, fill = type)) +
#  geom_boxplot()

#Model training
#split data
set.seed(1) 
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- scaled_x[test_index,]
test_y <- brca$y[test_index]
train_x <- scaled_x[-test_index,]
train_y <- brca$y[-test_index]

#proportion of category targets
mean(train_y=='B')
mean(test_y=='B')

