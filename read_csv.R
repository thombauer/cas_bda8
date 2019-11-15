getwd()
library(data.table)
library(dplyr)
library(funModeling)
df <- fread("/Users/thombauer/Desktop/autos_prepared.csv")
head(df)

table(df$model,df$fuelType)
table <- as.data.frame(table(df$model,df$fuelType))
