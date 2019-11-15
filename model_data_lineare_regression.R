# Benoetigte Libraries laden
library(ggplot2)
library(scales)
library(stats)
library(data.table)
library(caret)
library(miscTools)


# Daten einlesen 
df <- fread("/Users/thombauer/Desktop/autos_prepared.csv")
df <- df %>% 
  dplyr::filter(., grepl('benzin|diesel', fuelType)) %>%
  mutate(fuelType=ifelse(fuelType=="diesel",1,0)) %>%
  mutate(fuelType=as.integer(fuelType))

table(df$fuelType)

# Train / Test
set.seed(42)
train.index <- createDataPartition(df$fuelType, list = FALSE, p = 0.75)
train <- df[train.index, ]
test <- df[-train.index, ]

g <- qplot(price, kilometer, data = train) + 
  geom_smooth(method = "lm", se = FALSE, data = train) + 
  geom_point(data = test, color = I("red"))
print(g)

model <- lm(price ~ kilometer + powerPS + yearOfRegistration +fuelType, data = train)
print(model)

test$VerkaufspreisPredicted <- predict(model, test)
print(test)

# Bestimmtheitsmass berechnen
print(summary(model))
print(summary(model)$r.squared)


