options(digits = 3)

library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)

data(brca)

head(brca, 3)

length(brca$y)

ncol(brca$x)

sum(brca$y=="M")/length(brca$y)

which.max(colMeans(brca$x))

which.min(colSds(brca$x))

x_mean <- sweep(brca$x,2,colMeans(brca$x))
x_scaled <- sweep(x_mean,2,colSds(x_mean),FUN="/")

colSds(x_scaled)
colMedians(x_scaled)

pca<-prcomp(x_scaled)
summary(pca)

data.frame(pca$x[,1:2], tumorType=brca$y) %>% 
  ggplot(aes(PC1,PC2, fill = tumorType))+
  geom_point(cex=3, pch=21) +
  coord_fixed(ratio = 1)

data.frame(tumorType = brca$y, pca$x[,1:10]) %>%
  gather(key = "PC", value = "value", -tumorType) %>%
  ggplot(aes(PC, value, fill = tumorType)) +
  geom_boxplot()

set.seed(1) 
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_scaled[test_index,]
test_y <- brca$y[test_index]
train_x <- x_scaled[-test_index,]
train_y <- brca$y[-test_index]

mean(test_y=="B")
mean(train_y=="B")

train_glm <- train(train_x, train_y,method = "glm")
pglm <- predict(train_glm, test_x)
mean(pglm == test_y)

set.seed(5)
train_loess <- train(train_x, train_y,method = "gamLoess")
ploess <- predict(train_loess, test_x)
mean(ploess == test_y)

set.seed(7)
train_knn <- train(train_x, train_y, method = "knn", tuneGrid = data.frame(k=seq(3,21,2)))
train_knn$bestTune
y_hat_knn <- predict(train_knn,test_x)
mean(y_hat_knn == test_y)

set.seed(9)
train_rf <- train(train_x, train_y, method = "rf", tuneGrid = data.frame(mtry=c(3, 5, 7, 9)), importance = TRUE)
train_rf$bestTune
y_hat_rf <- predict(train_knn,test_x)
mean(y_hat_rf== test_y)
varImp(train_rf)

ensemble <- cbind(glm = pglm == "B", loess = ploess == "B", rf = y_hat_rf == "B", knn = y_hat_knn == "B")
ensemble_preds <- ifelse(rowMeans(ensemble) > 0.5, "B", "M")
mean(ensemble_preds == test_y)

models <- c("Logistic regression", "Loess", "K nearest neighbors", "Random forest", "Ensemble")
accuracy <- c(mean(pglm == test_y),
              mean(ploess == test_y),
              mean(y_hat_knn == test_y),
              mean(y_hat_rf == test_y),
              mean(ensemble_preds == test_y))
data.frame(Model = models, Accuracy = accuracy)

