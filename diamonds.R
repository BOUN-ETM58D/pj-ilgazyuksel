library(tidyverse)
library(ggplot2)
library(caret)
library(caTools)

data("diamonds")
glimpse(diamonds)

model <- lm(price ~ ., diamonds)
model
p <- predict(model, diamonds)
p
error <- p - diamonds$price
sqrt(mean(error^2))

set.seed(42)
rows <- sample(nrow(diamonds))
head(rows)
diamonds <- diamonds[rows, ]
split <- round(nrow(diamonds) * .80)
split
train <- diamonds[1:split,]
test <- diamonds[(split + 1):nrow(diamonds),]

dim(diamonds)
dim(train)
dim(test)

model <- lm(price ~ ., train)
model
p <- predict(model, test)
error <- p - test$price
sqrt(mean(error^2))

model <- train(
  price ~ ., diamonds,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 5,
    verboseIter = TRUE
  )
)

model
predict(model)
error <- predict(model) - diamonds$price
sqrt(mean(error^2))




library(rpart)
library(rpart.plot)
summary(model)
head(diamonds)

diamond_model <- rpart(price ~ ., data = train)
rpart.plot(diamond_model)
