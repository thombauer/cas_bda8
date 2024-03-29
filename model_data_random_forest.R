library(data.table)
library(ggplot2)
library(caret)

source("/Users/thombauer/Desktop/helper.R")

data <- fread("/Users/thombauer/Desktop/classification.csv")
data$success <- as.factor(data$success)

set.seed(42)
train.index <- createDataPartition(data$success, p = 0.75, list = FALSE)
train <- data[train.index, ]
test <- data[-train.index, ]

# install.packages("randomForest")
library(randomForest)

model <- randomForest(success ~ age + interest, 
                      data = train,
                      ntree = 500, 
                      nodesize = 5)
print(model)

pred <- predict(model, test)
print(confusionMatrix(pred, test$success)$overall["Accuracy"])

g <- plot_classifier(model, train, success ~ age + interest, proba = TRUE)
print(g)

g <- plot_classifier(model, test, success ~ age + interest, proba = TRUE)
print(g)