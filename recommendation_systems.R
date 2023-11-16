# HarvardX: PH125.8x
# Data Science: Machine Learning
# R code from course videos

# Model Fitting and Recommendation Systems

## Recommendation Systems

### Recommendation Systems

# https://bits.blogs.nytimes.com/2009/09/21/netflix-awards-1-million-prize-and-starts-a-new-contest/
# http://blog.echen.me/2011/10/24/winning-the-netflix-prize-a-summary/
# https://www.netflixprize.com/assets/GrandPrize2009_BPC_BellKor.pdf

library(dslabs)
library(tidyverse)

data("movielens")

movielens %>% as_tibble()

movielens %>% 
     summarize(n_users = n_distinct(userId),
               n_movies = n_distinct(movieId))

keep <- movielens %>%
     dplyr::count(movieId) %>%
     top_n(5) %>%
     pull(movieId)

tab <- movielens %>%
     filter(userId %in% c(13:20)) %>% 
     filter(movieId %in% keep) %>% 
     dplyr::select(userId, title, rating) %>% 
     pivot_wider(names_from="title", values_from="rating")
tab %>% knitr::kable()

users <- sample(unique(movielens$userId), 100)
rafalib::mypar()
movielens %>% filter(userId %in% users) %>% 
dplyr::select(userId, movieId, rating) %>%
mutate(rating = 1) %>%
pivot_wider(names_from = movieId, values_from = rating) %>% 
(\(mat) mat[, sample(ncol(mat), 100)])()%>%
as.matrix() %>% 
t() %>%
image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")

library(gridExtra)
p1 <- movielens %>% 
     dplyr::count(movieId) %>% 
     ggplot(aes(n)) + 
     geom_histogram(bins = 30, color = "black") + 
     scale_x_log10() + 
     ggtitle("Movies")

p2 <- movielens %>% 
     dplyr::count(userId) %>% 
     ggplot(aes(n)) + 
     geom_histogram(bins = 30, color = "black") + 
     scale_x_log10() + 
     ggtitle("Users")
grid.arrange(p1, p2, ncol = 2)

library(caret)
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]

#To make sure we don't include users and movies in the test set that do not appear in the training set, we removed these using the semi_join function, using this simple code.
test_set <- test_set %>% 
     semi_join(train_set, by = "movieId") %>%
     semi_join(train_set, by = "userId")

RMSE <- function(true_ratings, predicted_ratings){
     sqrt(mean((true_ratings - predicted_ratings)^2))
}

### Building the Recommendation System
# A first model
mu_hat <- mean(train_set$rating)
mu_hat

naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

predictions <- rep(2.5, nrow(test_set))
RMSE(test_set$rating, predictions)

rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)

# Modeling movie effects

# fit <- lm(rating ~ as.factor(userId), data = movielens)
mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
     group_by(movieId) %>% 
     summarize(b_i = mean(rating - mu))


movie_avgs %>% qplot(b_i, geom ="histogram", bins = 30, data = ., color = I("black"))

predicted_ratings <- mu + test_set %>% 
     left_join(movie_avgs, by='movieId') %>%
     pull(b_i)

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))
rmse_results %>% knitr::kable()

# Modeling user effects

train_set %>% 
     group_by(userId) %>% 
     summarize(b_u = mean(rating)) %>% 
     filter(n()>=100) %>%
     ggplot(aes(b_u)) + 
     geom_histogram(bins = 30, color = "black")

# lm(rating ~ as.factor(movieId) + as.factor(userId))
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

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()