#!/usr/bin/env Rscript

library(mlr)
library(mlrMBO)
library(smoof)

# data
data_synthesis <- read.csv("provided/synthesis.csv")
data_synthesis$X <- NULL

# model from all data points
model = train(makeLearner("regr.randomForest"), makeRegrTask(data = data_synthesis, target = "target"))

# Bayesian Optimization

fun = function(x) {
    df = as.data.frame(x)
    return(getPredictionResponse(predict(model, newdata = df)))
}

ps = makeParamSet(
    makeNumericParam("f", lower = 0, upper = 0.25),
    makeNumericParam("k", lower = 0, upper = 0.1),
    makeNumericParam("du", lower = 0, upper = 2e-5),
    makeNumericParam("dv", lower = 0, upper = 2e-5),
    makeIntegerParam("x", lower = 0, upper = 200),
    makeIntegerParam("y", lower = 0, upper = 200)
)

objfun = makeSingleObjectiveFunction(
     name = "Synthesis",
     fn = fun,
     par.set = ps,
     has.simple.signature = FALSE,
     minimize = FALSE
)

# sample 10 points for the initial surrogate model
initial.data = data_synthesis[sample(1:nrow(data_synthesis), 10), ]
cat(paste("Best training fitness: ", max(initial.data$target), "\n", sep = ""))

ctrl = makeMBOControl(y.name = "target")
ctrl = setMBOControlInfill(ctrl, opt = "focussearch", opt.focussearch.maxit = 20, opt.focussearch.points = 1000, crit = makeMBOInfillCritEI())
ctrl = setMBOControlTermination(ctrl, iters = 50)

res = mbo(objfun, design = initial.data, control = ctrl, show.info = TRUE)
cat("Best configuration:\n")
cat(paste(paste(lapply(names(res$x), function(n) { paste(n, res$x[n], sep = ": ") }), collapse = ", "), "; fitness: ", res$y, "\n", sep = ""))
