library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

set.seed(42, sample.kind = 'Rounding') # if R version >= 3.6

test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE)
train_set <- titanic_clean[-test_index,]
test_set <- titanic_clean[test_index,]

nrow(train_set)
nrow(test_set)

mean(train_set$Survived == 1)

set.seed(3, sample.kind = 'Rounding') # if R version >= 3.6
guess <- sample(c(0,1), nrow(test_set), replace = TRUE)
y_hat <- guess %>% factor(levels = levels(test_set$Survived))
confusionMatrix(y_hat, test_set$Survived)$overall["Accuracy"]
# mean(test_set$Survived == guess)

train_set %>%
  group_by(Sex, Pclass) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Survived > 0.5)

confusionMatrix(data = factor(sex_model), reference = factor(test_set$Survived))
confusionMatrix(data = factor(class_model), reference = factor(test_set$Survived))
# confusionMatrix(data = factor(sex_class_model), reference = factor(test_set$Survived))

library(MASS)
fit_lda <- train(Survived ~ Fare, data = train_set, method = 'lda')
Survived_hat <- predict(fit_lda, test_set)
mean(test_set$Survived == Survived_hat)
