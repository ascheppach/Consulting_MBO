
source(file = "R/plotHyperparams.R")

library(data.table)
library(mlrMBO)
library(lhs)

# algorithm design: the hyperparameters to benchmark
# be careful: you should not benchmark more than 6 configurations the same time as the plot would get messy

data_synthesis <- read.csv("provided/synthesis.csv", colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA))
objectiveFunction = train(makeLearner("regr.randomForest"), makeRegrTask(data = data_synthesis, target = "target"))

problemName = "synthesis"

parameterSet = makeParamSet(
  makeNumericParam("f", lower = 0, upper = 0.25),
  makeNumericParam("k", lower = 0, upper = 0.1),
  makeNumericParam("du", lower = 0, upper = 2e-5),
  makeNumericParam("dv", lower = 0, upper = 2e-5),
  makeIntegerParam("x", lower = 0, upper = 200),
  makeIntegerParam("y", lower = 0, upper = 200))

minimize = FALSE

param = CJ(iters = 50,
           crit = list(makeMBOInfillCritEI()),
           funDesign = list(maximinLHS),
           surrogate = list(makeLearner("regr.randomForest", predict.type = "se")),
           amount = c(10,
                      25,
                      40,
                      55),
           sorted = FALSE)

repls = 2
names = c("EiRf10","EiRf25","EiRf40","EiRf55")

plotHyperparams(objectiveFunction, problemName, parameterSet, minimize, param, repls, names)
