library(data.table)
library(ggplot2)
library(caret)

cars <- fread("/Users/thombauer/Desktop/autos_prepared.csv")

cars.scaled <- scale(cars[, c("yearOfRegistration", "price")])

model <- kmeans(cars.scaled, 3)
print(model$centers)

# install.packages("DMwR")
library(DMwR)
centers <- data.table(unscale(model$centers, cars.scaled))

g <- qplot(cars$yearOfRegistration, cars$price, 
           color = as.factor(model$cluster),
           xlab = "Erstzulassung", 
           ylab = "Wert in €") +
  geom_point(aes(x = yearOfRegistration, y = price), color = "black", data = centers) +
  labs(color='Cluster #')

print(g)

####elbow

xs <- 2:10
ys <- sapply(xs, function(x) {
  model <- kmeans(cars.scaled, x)
  return(model$tot.withinss)
})

qplot(xs, ys, geom = "line")

####parametrisierung
cars.scaled <- scale(cars[, c("yearOfRegistration", "price")])

model <- kmeans(cars.scaled, 3, iter.max = 100, nstart = 4, algorithm = "MacQueen")
print(model$centers)

centers <- data.table(unscale(model$centers, cars.scaled))

g <- qplot(cars$yearOfRegistration, cars$price, 
           color = as.factor(model$cluster),
           xlab = "Erstzulassung", 
           ylab = "Wert in €") +
  geom_point(aes(x = yearOfRegistration, y = price), color = "black", data = centers) +
  labs(color='Cluster #')

print(g)
