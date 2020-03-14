library(ParamHelpers)
?library(checkmate)
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

set.seed(345)


                       ############## SPOT ##################
# define and fix (-> for all optimzers the same) objective function / simulation 
model = train(makeLearner("regr.randomForest", nodesize = 2), makeRegrTask(data = data, target = "ratio"))

ps = makeParamSet(
  makeIntegerParam("power", lower = 10, upper = 5555),
  makeIntegerParam("time", lower = 500, upper = 20210),
  makeIntegerParam("pressure", lower = 0, upper = 1000)
)


##### 1. ES-Optimizer

optimizeES = function(objfun, ps, n, m, minimize = FALSE) { #pass a defined objfun, n is iterations of optimizer, m is iteration of tuning
  
  names = model[["features"]]
  
  nn = length(names)
  
  if (minimize == FALSE) {
    p = (-1)
  }
  if (minimize == TRUE) {
    p = (1)
  }
  
  fun = function(x) {
    df = as.data.frame(t(x))
    colnames(df) = names[1:nn]
    return(getPredictionResponse(predict(model, newdata = df))*p)
  }
  
  objfun = makeSingleObjectiveFunction(
    name = "Kapton",
    fn = fun,
    par.set = ps,
    has.simple.signature = FALSE,
    minimize = FALSE
  )
  
  # apply a wrapper function to the objfun; therefore fun3 receives matrix in the right structure
  fun2 = function(xmat1) {
    apply(xmat1, 1, objfun)
  }
  
  # define optimizer / algo
  fun3 = function(xmat) {
    xmat = as.data.frame(t(xmat))
    colnames(xmat) <- c("nu", "mue")
    optimES(fun = fun2, lower = getLower(ps), upper = getUpper(ps), 
            control = list(funEvals = n, nu = xmat$nu, mue = xmat$mue))$ybest
  }
  
  fun4 = function(xmat2) {
    apply(xmat2, 1, fun3)
  }
  
  # define metamodel and run spot
  res <- spot(fun = fun4, lower = c(3,5), upper = c(5,15),
              control = list(funevals = m))
  
  results <- list(besty = res$ybest*p, besthypparam = res$xbest, y = res$y*p, hypparam = res$x)
}

### example run
# run optimization and tuning
run <- optimizeES(objfun, ps, 50, 100)

# show hyperparameters
run[["hypparam"]]

# show besty
run[["besty"]]

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





