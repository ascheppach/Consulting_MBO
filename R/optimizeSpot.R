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
# define and fix (-> for all optimzers the same) objective function / simulation 
model = train(makeLearner("regr.randomForest", nodesize = 2), makeRegrTask(data = data, target = "ratio"))
fun = function(x) {
  df = as.data.frame(t(x))
  colnames(df) <- c("power", "time", "pressure")
  return(getPredictionResponse(predict(model, newdata = df)))
}

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



##### 1. ES-Optimizer

##automatisiert
# objfun has to be defined
# entweder ich mache nur für minimize und schreibe, mann muss es selber definieren, indem negative, oder ich mache es automatisiert, also innerhalb der funktion, wenn minimization=False dann zuerst ratio negativ machen und am ende wieder positiv

da1 <- optimizeES(objfun = objfun, n= 50, m=100)

# lower
ps1 <- ps[[1]][1]
ps1[[1]][["lower"]]

ps2 <- ps[[1]][2]
ps2[[1]][["lower"]]

ps3 <- ps[[1]][3]
ps3[[1]][["lower"]]

ps4 <- ps[[1]][4]
ps4[[1]][["lower"]]

ps5 <- ps[[5]][1]
ps5[[1]][["lower"]]

# upper
ps1 <- ps[[1]][1]
ps1[[1]][["upper"]]

ps2 <- ps[[1]][2]
ps2[[1]][["upper"]]

ps3 <- ps[[1]][3]
ps3[[1]][["upper"]]

ps4 <- ps[[1]][4]
ps4[[1]][["upper"]]

ps5 <- ps[[5]][1]
ps5[[1]][["upper"]]






optimizeES = function(objfun, n, m) { #pass a defined objfun, n is iterations of optimizer, m is iteration of tuning
  
  # apply a wrapper function to the objfun; therefore fun3 receives matrix in the right structure
  fun2 = function(xmat1) {
    apply(xmat1, 1, objfun)
  }
  
  # define optimizer / algo
  fun3 = function(xmat) {
    xmat = as.data.frame(t(xmat))
    colnames(xmat) <- c("nu", "mue")
    optimES(fun = fun2, lower = c(10,500,0), upper = c(5555,20210,1000), 
            control = list(funEvals = n, nu = xmat$nu, mue = xmat$mue))$ybest
  }
  
  fun4 = function(xmat2) {
    apply(xmat2, 1, fun3)
  }
  
  # define metamodel and run spot
  res <- spot(fun = fun4, lower = c(3,5), upper = c(5,15),
              control = list(funevals = m))
  
  return(as.list(res$x))
}




# define optimizer / algo
fun3 = function(xmat) {
  xmat = as.data.frame(t(xmat))
  colnames(xmat) <- c("nu", "mue")
  optimES(fun = fun2, lower = c(10,500,0), upper = c(5555,20210,1000), 
              control = list(funEvals = 50, nu = xmat$nu, mue = xmat$mue))$ybest
}

fun4 = function(xmat2) {
  apply(xmat2, 1, fun3)
}

# define metamodel and run spot
res <- spot(fun = fun4, lower = c(3,5), upper = c(5,15),
            control = list(funevals = 100))





##### 2. Genoud-Optimizer -> muss nicht optimiert werden, weil nur populationsize als parameter
##### logischerweise ist er umso höher, je höher populationsize ist
# define objective function / simulation

# define optimizer / algo
fun3 = function(x) {
  optimGenoud(fun = fun2, lower = c(10,500,0), upper = c(5555,20210,1000), 
              control = list(funEvals = 50, populationSize = x))$ybest
}

fun4 = function(xmat){
  apply(xmat,1,fun3)
}

# define metamodel and run spot
res <- spot(fun = fun4, lower = c(20), upper = c(40), 
            control = list(funevals = 100))

res$ybest
res$xbest





