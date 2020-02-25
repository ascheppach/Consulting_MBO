library(ParamHelpers)
library(checkmate)
library(smoof)
library(mlr)
library(mlrMBO)
library(ggplot2)
library(tidyr)
library(parallel)
library(data.table)
library(irace)
library(cmaesr)
library(lhs)
library(batchtools)

source("R/benchmarkOptimizations.R")

# data
data_kapton <- read.csv("provided/kapton_argon.csv", colClasses=c("NULL",NA,NA,NA,NA))

# model from all data points
model = train(makeLearner("regr.randomForest"), makeRegrTask(data = data_kapton, target = "ratio"))



ps = makeParamSet(
  makeIntegerParam("power", lower = 10, upper = 5555),
  makeIntegerParam("time", lower = 500, upper = 20210),
  makeIntegerParam("pressure", lower = 0, upper = 1000)
)

n = 50

repls = 10

minimize = FALSE

paramsMBO = CJ(iters = n,
           crit = list(makeMBOInfillCritEI()),
           funDesign = list(maximinLHS),
           surrogate = list(makeLearner("regr.randomForest", predict.type = "se")),
           amount = c(10),
           sorted = FALSE)

sigma = 1000

lambda = 400



benchmarkOptimizations(model, ps, n, repls, paramsMBO, minimize)











source("R/optimizeRandom.R")
source("R/optimizeRacing.R")
source("R/optimizeCMAESR.R")
source("R/optimizeMBO.R")

optimizeRandom(model, ps, 50, minimize = FALSE)

optimizeRacing(model, ps, 50, minimize = FALSE)

optimizeCMAES(model, ps, 50, 1000, 40, minimize = FALSE)

optimizeMBO(model, ps, 50, param, minimize = FALSE, repls = 10)
