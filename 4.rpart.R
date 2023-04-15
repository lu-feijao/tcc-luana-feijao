# -------------------- Rpart -------------------- #

# Pacotes & Diretório ----
library(rpart)
library(tidyverse)
library(car)


# Banco de dados

# n = 10
df10 <- read.csv('df10.csv')
df10 <- df10[,-1]
# n = 12
df12 <- read.csv('df12.csv')
df12 <- df12[,-1]
# n = 15
df15 <- read.csv('df15.csv')
df15 <- df15[,-1]
# n = 17
df17 <- read.csv('df17.csv')
df17 <- df17[,-1]
# n = 20
df20 <- read.csv('df20.csv')
df20 <- df20[,-1]





# Modelo ----
train <- df10


model <- rpart(campeao1 ~ vitoria+empate+derrota+GP+GC+SG, data=train)
summary(model)
