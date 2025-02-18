#Recommendation Systems

library(dslabs)
library(tidyverse)
data("movielens")

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
