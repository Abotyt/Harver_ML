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

}

length(RMSE)
length(alpha)

plot(alpha, RMSE)

alpha[which(RMSE==min(RMSE))]

