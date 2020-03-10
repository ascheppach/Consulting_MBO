library(ParamHelpers)
library(checkmate)
library(smoof)
library(mlr)
library(mlrMBO)
library(ggplot2)
library(tidyr)
library(DiceKriging)
library(SPOT)
library("irace")
source("fixed/eiParam.R")
source("fixed/eiParamAda.R")

setwd("~/Documents/GitHub/Consulting_MBO")

data <- read.csv("fixed/kapton_argon.csv")
data$X <- NULL

n <- length(data$ratio)
for (i in 1:n) {
  data$ratio[i] <- data$ratio[i]*(-1)
}


set.seed(345)


### Objfun ###
#model = train(makeLearner("regr.randomForest"), makeRegrTask(data = data, target = "ratio"))
#model = train(makeLearner("regr.km", nugget.estim = TRUE), makeRegrTask(data = data, target = "ratio"))

#RF (funktioniert)
model1 <- randomForest(data = data, x = data[,1:3], y = data$ratio)
#original = 5.5, hier 4.48
predict(model1, newdata = c(1749, 10378, 975))
#original = 4.7, hier 4.1
#predict(model1, newdata = c(1739, 10379,	990))
#prediction als funktion
fun = function(x) {
  predict(model1, newdata = x)
}

fun(c(1739,10379,990))

#GP
#model <- km(design = data[, 1:3], response = data$ratio, covtype = "powexp", nugget.estim = TRUE)
#predict.km(model, newdata = data[46, 1:3], type = "SK", nugget.estim = TRUE)

#model <- randomForest(data = data, ratio ~ data[,1:3])
#fun(data[46, 1:3])

#fun = function(x) {
#  df = as.data.frame(x)
#  return(getPredictionResponse(predict(model, newdata = df)))
#}

#ps = makeParamSet(
#  makeIntegerParam("power", lower = 10, upper = 5555),
#  makeIntegerParam("time", lower = 500, upper = 20210),
#  makeIntegerParam("pressure", lower = 0, upper = 1000)
#)

#objfun = makeSingleObjectiveFunction(
#  name = "Kapton",
#  fn = fun,
#  par.set = ps,
#  has.simple.signature = FALSE,
#  minimize = FALSE
#)

# gruber methode irace
#df = as.data.frame(configuration)
#y = getPredictionResponse(predict(model, newdata = df))



### Define Starting Point ###
#tmax <- 10
#temp <- 10

#x0 <- c(10, 10)
#maxit <- 250

### Algorithm ###
#y1 <- optim(x0, fn = objfun)

#y1 <- optim(x0, fun, method = "SANN",
#            control = list(maxit = maxit, temp = temp, tmax = tmax))
#y1$value

Alternativ
res <- spot(fun = fun, lower = c(10,500,0), upper = c(5555,20210,1000), control = list(funEvals=50, optimizer=optimLHD))
res$ybest
res$xbest
res$count




############## Nachbau Versuch ##################
# algorithm to be tuned
alg.func = "spotAlgStartSann"
# number of runs of algorithm (SANN)
auto.loop.nevals = 100
# hyperparameter sampling design ()
init.design.func = "spotCreateDesignLhs"
init.design.size = 10
init.design.repeats = 2
# metamodel
seq.predictionModel.func = "spotPredictRandomForest"


spot("demo07RandomForestSann.conf")





