#Recommendation Systems
#Library
library(dslabs)
library(tidyverse)
data("movielens")

#setup training and testing data sets
movielens %>%
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

library(caret)
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]

test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#Develop model
#first model- mean

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

mu_hat <- mean(train_set$rating)
mu_hat

naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

#second model- any data
predictions <- rep(2.5, nrow(test_set))
RMSE(test_set$rating, predictions)

#third model- avg of a movie
#model(it will takes a longtime to train):
#fit <- lm(rating ~ as.factor(userId), data = movielens)
#estimate model
mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
model_1_rmse

#fourth model- avg rating of a user
#plot the rating of users
train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

#model(it will takes a longtime to train):
#lm(rating ~ as.factor(movieId) + as.factor(userId))

#estimate model
user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
model_2_rmse

#Regularization
library(dslabs)
test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>% 
  select(title,  residual) %>% slice(1:10) %>% pull(title) 

#best movie and worse movie
movie_titles <- movielens %>% 
  dplyr::select(movieId, title) %>%
  distinct()

movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  dplyr::select(title, b_i) %>% 
  slice(1:10) %>%  
  pull(title)

movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  dplyr::select(title, b_i) %>% 
  slice(1:10) %>%  
  pull(title)

#frequency of a movie is rated
train_set %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10) %>% 
  pull(n)

train_set %>% dplyr::count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>%
  slice(1:10) %>% 
  pull(n)


#Q1
#simulate a school dataset
#num of student in each school
set.seed(1986)
n <- round(2^rnorm(1000, 8, 1))

#true quality
set.seed(1)
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

#top ten
schools %>% 
  top_n(10, quality) %>% 
  arrange(desc(quality))

#simulate test result
scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

#Q1 
#find top 10 according to score
schools %>% 
  top_n(10, score) %>% 
  arrange(desc(score))

#Q2
#find median size overall 
schools %>%
  summarise(median(size))
#find median size of the top 10
schools %>% 
  top_n(10, score) %>% 
  arrange(desc(score)) %>%
  summarise(median(size))

#Q3
#find median size of bottom 10
schools %>% 
  top_n(-10, score) %>% 
  arrange(score) %>%
  summarise(median(size))

#Q4
#plot schools size vs avg score of schools, and highlight the top 10 
top_10_rating <- schools %>%
  top_n(10, quality)%>% 
  arrange(desc(quality))

ggplot(data=schools, aes(x=size, y=score)) +
  geom_point() +
  geom_point(data=top_10_rating, aes(x=size, y=score), colour='red')

#Q5
#apply regularisation
#find mu
overall <- mean(sapply(scores, mean))

schools$score
head(scores)
#find difference between each score and mu
y_mu <- sapply(scores, function(x) {x-overall})
#find sum of all scores in each school
sum_ymu <- sapply(y_mu, sum)
#each school total score divided by each school size and + 25(arpha)
b_i <- sum_ymu/(schools$size+25)
# regularised b_i to find the top 10
schools %>% 
  mutate(b_i = b_i) %>%
  top_n(10, b_i) %>%
  arrange(desc(b_i))
#predict with the regularised b_i
top_10_rating <- schools %>% 
  mutate(predict = overall+b_i) %>%
  top_n(10, predict) %>%
  arrange(desc(predict))


#alternative
#alpha <- 25
#score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
#schools %>% mutate(score_reg = score_reg) %>%
#  top_n(10, score_reg) %>% arrange(desc(score_reg))

#Q6
#find the best alpha that minimise the RMSE
alpha <- seq(10, 250)
alpha
score_reg <-  sapply(alpha, function(alpha){                 
  sapply(scores, function(x) {
    overall + sum(x-overall)/(length(x)+alpha)
  })
})

RMSE <-  rep(NA, length(alpha))

for(idx in seq(length(alpha))) {
  RMSE[idx] <- sqrt(sum((schools$quality-score_reg[,idx])^2)/1000)

}

length(RMSE)
length(alpha)

plot(alpha, RMSE)

alpha[which(RMSE==min(RMSE))]

#Q7
#use the alpha that we found to identify the top school and the regularised avg of the 10th school
alpha <- alpha[which(RMSE==min(RMSE))]
score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

#Q8
alpha <- seq(10, 250)
score_reg <-  sapply(alpha, function(alpha){                 
  sapply(scores, function(x) {
    sum(x)/(length(x)+alpha)
  })
})
score_reg
RMSE <-  rep(NA, length(alpha))

for(idx in seq(length(alpha))) {
  RMSE[idx] <- sqrt(sum((schools$quality-score_reg[,idx])^2)/1000)
  
}

length(RMSE)
length(alpha)

plot(alpha, RMSE)

alpha[which(RMSE==min(RMSE))]

#Matrix Factorization
train_small <- movielens %>% 
  group_by(movieId) %>%
  filter(n() >= 50 | movieId == 3252) %>% ungroup() %>%
  group_by(userId) %>%
  filter(n() >= 50) %>% ungroup()

y <- train_small %>% 
  dplyr::select(userId, movieId, rating) %>%
  pivot_wider(names_from = "movieId", values_from = "rating") %>%
  as.matrix()

rownames(y)<- y[,1]
y <- y[,-1]

movie_titles <- movielens %>% 
  dplyr::select(movieId, title) %>%
  distinct()

colnames(y) <- with(movie_titles, title[match(colnames(y), movieId)]) 

y <- sweep(y, 1, rowMeans(y, na.rm=TRUE))
y <- sweep(y, 2, colMeans(y, na.rm=TRUE))

#PCA
y[is.na(y)] <- 0
y <- sweep(y, 1, rowMeans(y))
pca <- prcomp(y)

dim(pca$rotation)

dim(pca$x)

plot(pca$sdev)

var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_explained)

library(ggrepel)
pcs <- data.frame(pca$rotation, name = colnames(y))
pcs %>%  ggplot(aes(PC1, PC2)) + geom_point() + 
  geom_text_repel(aes(PC1, PC2, label=name),
                  data = filter(pcs, 
                                PC1 < -0.1 | PC1 > 0.1 | PC2 < -0.075 | PC2 > 0.1))

pcs %>% select(name, PC1) %>% arrange(PC1) %>% slice(1:10)

pcs %>% select(name, PC1) %>% arrange(desc(PC1)) %>% slice(1:10)

pcs %>% select(name, PC2) %>% arrange(PC2) %>% slice(1:10)

pcs %>% select(name, PC2) %>% arrange(desc(PC2)) %>% slice(1:10)

#Student Performance:
#simulate the dataset
set.seed(1987, sample.kind="Rounding")
#if using R 3.6 or later, use `set.seed(1987, sample.kind="Rounding")` instead
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))
head(y)
#Q1
my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)

#Q2
my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

#Q3
s <- svd(y)
names(s)

y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))

ss_y <- apply(y^2, 2, sum)
ss_yv <- apply((y%*%s$v)^2, 2, sum)
sum(ss_y)
sum(ss_yv)

#Q4
length(ss_y)
plot(ss_y)
plot(ss_yv)

#Q5
sqrt_yv <- sqrt(ss_yv)
sqrt_yv

plot(s$d, sqrt_yv)

#Q6
sum(s$d[1:3]^2)/sum(s$d^2)

#Q7
identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))

#Q8

student_avg <- rowMeans(y, na.rm=TRUE)
UD <- sweep(s$u, 2, s$d, FUN = "*")

plot(student_avg,UD[,1])

#Q9

my_image(s$v)

#Q10
plot(s$u[,1], ylim = c(-0.3,0.3))
plot((s$v)[,1], ylim = c(-0.3,0.3))

dim(diag(s$d))

U1d11tV1 <- (s$u[,1,drop=FALSE]*s$d[1]) %*% t(s$v[, 1, drop=FALSE])


my_image(U1d11tV1)
my_image(y)

#Q11
resid <- y - with(s,(u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

plot(s$u[,2], ylim = c(-0.3,0.3))
plot((s$v)[,2], ylim = c(-0.3,0.3))

U2d22tV2 <- (s$u[,2,drop=FALSE]*s$d[2]) %*% t(s$v[, 2, drop=FALSE])

my_image(U2d22tV2)
my_image(resid)

#Q12
resid <- y - with(s,sweep(u[, 1:2], 2, d[1:2], FUN="*") %*% t(v[, 1:2]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

plot(s$u[,3], ylim = c(-0.3,0.3))
plot((s$v)[,3], ylim = c(-0.3,0.3))

U3d33tV3 <- (s$u[,3,drop=FALSE]*s$d[3]) %*% t(s$v[, 3, drop=FALSE])

my_image(U3d33tV3)
my_image(resid)

#Q13
resid <- y - with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

#Q14
three_cols <- U1d11tV1+U2d22tV2+U3d33tV3
my_image(y, zlim = range(y))
my_image(three_cols, zlim = range(y))
my_image(resid, zlim = range(y))

