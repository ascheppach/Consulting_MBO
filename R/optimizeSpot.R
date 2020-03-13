library(ParamHelpers)
library(checkmate)
library(smoof)
library(mlr)
library(mlrMBO)
library(ggplot2)
library(tidyr)
library(DiceKriging)
library(SPOT)
library(randomForest)
library("irace")
source("fixed/eiParam.R")
source("fixed/eiParamAda.R")

setwd("~/Documents/GitHub/Consulting_MBO")

data <- read.csv("fixed/kapton_argon.csv")
data$X <- NULL

# data transformation
n <- length(data$ratio)
for (i in 1:n) {
  data$ratio[i] <- data$ratio[i]*(-1)
}


set.seed(345)


                         ############## SPOT ##################
##### ES-Optimizer

# define objective function / simulation
model <- randomForest(data = data, x = data[,1:3], y = data$ratio)
fun = function(x) {
  predict(model, newdata = x)
}

# define optimizer / algo
fun2 = function(x) {
  optimES(fun = fun, lower = c(10,500,0), upper = c(5555,20210,1000), 
          control = list(funEvals = 50, mue = x))$ybest
}

fun3 = function(xmat){
  apply(xmat,1,fun2)
}

# define metamodel and run spot
res <- spot(fun = fun3, lower = c(5), upper = c(15)
            control = list(funevals = 100))



############## SPOT ##################
##### Genoud-Optimizer
# define objective function / simulation
model <- randomForest(data = data, x = data[,1:3], y = data$ratio)
fun = function(x) {
  predict(model, newdata = x)
}

# define optimizer / algo
fun2 = function(x) {
  optimGenoud(fun = fun, lower = c(10,500,0), upper = c(5555,20210,1000), 
              control = list(funEvals = 50, populationSize = x))$ybest
}

fun3 = function(xmat){
  apply(xmat,1,fun2)
}

# define metamodel and run spot
res <- spot(fun = fun3, lower = c(20), upper = c(40), 
            control = list(funevals = 100))

res$ybest
res$xbest



