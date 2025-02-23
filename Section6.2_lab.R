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

#Q1
names(train_set)

head(movielens)
# find the text pattern in title column
movielens%>%
  filter(title=='The Shawshank Redemption')
movielens%>%
  filter(title=='Toy Story')

#count and plot the number of ratings against year
num_rating<- movielens %>% 
  group_by(movieId, year) %>%
  count(movieId)

boxplot(sqrt(n)~year,
        data = num_rating,
        outline = FALSE,
        medcol="red")
# hight light 2 and 2.5 to find the max number of ratings
abline(h=2, col = "blue")
abline(h=2.5, col = "blue")


#alternative
#movielens %>% group_by(movieId) %>%
#  summarize(n = n(), year = as.character(first(year))) %>%
#  qplot(year, n, data = ., geom = "boxplot") +
#  coord_trans(y = "sqrt") +
#  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Q2
#filter 1993 later, group id and year,and 
#calculate the years (number of years from published to 2018) and rating avg
new_movies <- movielens %>%
  filter(year>=1993) %>%
  group_by(movieId,year) %>%
  summarise(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating))

new_movies

#find the avg of number of rating between published day to 2018, and find top 25
max_count <- new_movies %>%
  mutate(avg_num_rate = n/years) %>%
  slice_max(avg_num_rate, n=25)

max_count

# identify the movies, Forrest Gump and The Shawshank Redemption
max_count%>%
  filter(title=='Forrest Gump')

max_count %>%
  filter(title=='Shawshank Redemption, The') 

#alternative
#movielens %>% 
#  filter(year >= 1993) %>%
#  group_by(movieId) %>%
#  summarize(n = n(), years = 2018 - first(year),
#            title = title[1],
#            rating = mean(rating)) %>%
#  mutate(rate = n/years) %>%
#  top_n(25, rate) %>%
#  arrange(desc(rate))  
#-------------------------  
#Q3
#plot the avg of number of ratings against rating of the movie
ggplot(max_count, aes(avg_num_rate, rating)) +
  geom_point() +
  geom_smooth()

#alternative
#movielens %>% 
#  filter(year >= 1993) %>%
#  group_by(movieId) %>%
#  summarize(n = n(), years = 2018 - first(year),
#            title = title[1],
#            rating = mean(rating)) %>%
#  mutate(rate = n/years) %>%
#  ggplot(aes(rate, rating)) +
#  geom_point() +
#  geom_smooth()

#------------------------------

#Q5
#transfer the timestamp(sec) to datetime datatype
movielens <- mutate(movielens, date = as_datetime(timestamp))
movielens

#Q6
#find the week of each year
movielens$rate_week <- week(movielens$date)
movielens$rate_year <- year(movielens$date)

#round week and graph the avg weekly against date
movielens %>%
  mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(
    avg_weekly = mean(rating),
    date = date[1]
  ) %>%
  ggplot(aes(x=date, y=avg_weekly)) +
  geom_point() +
  geom_smooth()


#alternative
#movielens %>% mutate(date = round_date(date, unit = "week")) %>%
#  group_by(date) %>%
#  summarize(rating = mean(rating)) %>%
#  ggplot(aes(date, rating)) +
#  geom_point() +
#  geom_smooth()

#------------------------------------
#Q8
names(movielens)
#find avg rating, standard error, filter out the genres that is less than 1000
movie_genres <- movielens %>% 
  group_by(genres) %>%
  summarise(genres=genres[1], n=n(), avg=mean(rating), se=sd(rating)/sqrt(n())) %>%
  filter(n>1000) %>%
  mutate(genres = reorder(genres, avg))
movie_genres

#plot the genres against avg
ggplot(movie_genres, aes(x=genres, y=avg)) +
  geom_point()+
  geom_errorbar(aes(ymin=avg-2*se, ymax=avg+2*se)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#alternative
#movielens %>% group_by(genres) %>%
#  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
#  filter(n >= 1000) %>% 
#  mutate(genres = reorder(genres, avg)) %>%
#  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
#  geom_point() +
#  geom_errorbar() + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#----------------------------

