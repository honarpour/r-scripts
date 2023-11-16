# HarvardX: PH125.8x
# Data Science: Machine Learning
# R code from course videos

### Matrix Factorization
library(dslabs)
library(tidyverse)
library(caret)
data("movielens")

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

y <- sweep(y, 1, rowMeans(y, na.rm=TRUE)) #summary stats which needs to swept away, rowMeans removed
y <- sweep(y, 2, colMeans(y, na.rm=TRUE)) #summary stats which needs to swept away, colMeans removed

#Now y is a matrix of residuals
library(gridExtra)

m_1 <- "Godfather, The"
m_2 <- "Godfather: Part II, The"
p1 <- qplot(y[ ,m_1], y[,m_2], xlab = m_1, ylab = m_2)

m_1 <- "Godfather, The"
m_3 <- "Goodfellas"
p2 <- qplot(y[ ,m_1], y[,m_3], xlab = m_1, ylab = m_3)

m_4 <- "You've Got Mail" 
m_5 <- "Sleepless in Seattle" 
p3 <- qplot(y[ ,m_4], y[,m_5], xlab = m_4, ylab = m_5)

grid.arrange(p1, p2 ,p3, ncol = 3)

cor(y[, c(m_1, m_2, m_3, m_4, m_5)], use="pairwise.complete") %>% 
     knitr::kable()

#factor analysis

set.seed(1988)
options(digits = 2)
Q <- matrix(c(1 , 1, 1, -1, -1), ncol=1)
rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5)
P <- matrix(rep(c(2,0,-2), c(3,5,4)), ncol=1)
rownames(P) <- 1:nrow(P)

X <- jitter(P%*%t(Q))
X <- round(X, 1)
X %>% knitr::kable(align = "c")

cor(X)

t(Q) %>% knitr::kable(aling="c")

P

set.seed(1988)
m_6 <- "Scent of a Woman"
Q <- cbind(c(1 , 1, 1, -1, -1, -1), 
           c(1 , 1, -1, -1, -1, 1))
rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5, m_6)
P <- cbind(rep(c(2,0,-2), c(3,5,4)), 
           c(-1,1,1,0,0,1,1,1,0,-1,-1,-1))/2
rownames(P) <- 1:nrow(X)

X <- jitter(P%*%t(Q), factor=1)
X %>% knitr::kable(align = "c")

cor(X)

t(Q) %>% knitr::kable(aling="c")

P

six_movies <- c(m_1, m_2, m_3, m_4, m_5, m_6)
tmp <- y[,six_movies]
cor(tmp, use="pairwise.complete")