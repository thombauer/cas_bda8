library(data.table)
library(ggplot2)
library(caret)

source("/Users/thombauer/Desktop/helper.R")

data <- fread("/Users/thombauer/Desktop/classification.csv")
data$success <- as.logical(data$success)

train.index <- createDataPartition(data$success, p = 0.75, list = FALSE)
train <- data[train.index, ]
test <- data[-train.index, ]

model <- glm(success ~ age + interest, family = binomial(), data = train)
print(model)

g <- plot_classifier(model, train, success ~ age + interest, proba = TRUE)
print(g)

g <- plot_classifier(model, test, success ~ age + interest, proba = TRUE)
print(g)

predictions <- predict(model, test, type = "response")
print(confusionMatrix(predictions > 0.5, test$success))

# Bestimmtheitsmass berechnen
print(summary(model))
