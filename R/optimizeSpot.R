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
fun2 = function(xmat) {
  xmat = as.data.frame(t(xmat))
  colnames(xmat) <- c("nu", "mue")
  optimES(fun = fun, lower = c(10,500,0), upper = c(5555,20210,1000), 
              control = list(funEvals = 50, nu = xmat$nu, mue = xmat$mue))$ybest
}

fun3 = function(xmat) {
  apply(xmat, 1, fun2)
}

# define metamodel and run spot
res <- spot(fun = fun3, lower = c(3,5), upper = c(5,15),
            control = list(funevals = 100))



res <- spot(fun = fun, lower = c(10,500,0), upper = c(5555,20210,1000))



##### Genoud-Optimizer -> muss nicht optimiert werden, weil nur populationsize als parameter
##### logischerweise ist er umso höher, je höher populationsize ist
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




####### Mit neuer Fun ########
model = train(makeLearner("regr.km", nugget.estim = TRUE, control = list(trace = FALSE)), makeRegrTask(data = data, target = "ratio"))
fun = function(x) {
  df = as.data.frame(t(x))
  colnames(df) <- c("power", "time", "pressure")
  return(getPredictionResponse(predict(model, newdata = df)))
}



fun = function(x) {
  df = as.data.frame(x)
  return(getPredictionResponse(predict(model, newdata = df)))
}


# alternativ mit objfun
ps = makeParamSet(
  makeIntegerParam("power", lower = 10, upper = 5555),
  makeIntegerParam("time", lower = 500, upper = 20210),
  makeIntegerParam("pressure", lower = 0, upper = 1000)
)


objfun = makeSingleObjectiveFunction(
  name = "Kapton",
  fn = fun,
  par.set = ps,
  has.simple.signature = FALSE,
  minimize = FALSE
)

objfun(t(c(1004, 10624	,660)))


objfun2 = function(df){
  df = as.data.frame(t(x))
  colnames(df) <- c("power", "time", "pressure")
  return(getPredictionResponse(predict(objfun, newdata = df)))
}


objfun2(da1)

da1 <- as.data.frame(c(1004, 10624	,660))

da1 = as.data.frame(t(da1))
colnames(da1) <- c("power", "time", "pressure")


