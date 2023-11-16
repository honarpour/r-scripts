library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female"))

y_hat <- ifelse(x == "online", "Male", "Female") %>% 
  factor(levels = levels(y))

# accuracy
mean(y_hat==y)

# confusion matrix
table(y_hat, y)

library(caret)
sensitivity(y_hat, y)
specificity(y_hat, y)

# prevalence
mean(y == "Female") 

data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(76)
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

library(caret)
library(dplyr)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

plot(iris,pch=21,bg=iris$Species)

set.seed(2)
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

petalLengthRange <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
petalWidthRange <- seq(range(train[,4])[1],range(train[,4])[2],by=0.1)
cutoffs <- expand.grid(petalLengthRange,petalWidthRange)

id <- sapply(seq(nrow(cutoffs)),function(i){
  y_hat <- ifelse(train[,3]>cutoffs[i,1] | train[,4]>cutoffs[i,2],'virginica','versicolor')
  mean(y_hat==train$Species)
}) %>% which.max

optimalCutoff <- cutoffs[id,] %>% as.numeric
y_hat <- ifelse(test[,3]>optimalCutoff[1] & test[,4]>optimalCutoff[2],'virginica','versicolor')
mean(y_hat==test$Species)

predictions <- func(train[,3])
rangedValues <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
cutoffs <-rangedValues[which(predictions==max(predictions))]

y_hat <- ifelse(test[,3]>cutoffs[1],'virginica','versicolor')
mean(y_hat==test$Species) 
