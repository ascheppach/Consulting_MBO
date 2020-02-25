#!/usr/bin/env Rscript

library(mlr)
library(mlrMBO)
library(smoof)

# data
data_kapton <- read.csv("provided/kapton_argon.csv", colClasses=c("NULL",NA,NA,NA,NA))

# model from all data points
model = train(makeLearner("regr.randomForest"), makeRegrTask(data = data_kapton, target = "ratio"))

# Bayesian Optimization

fun = function(x) {
    df = as.data.frame(x)
    df$gas = factor(df$gas, levels = levels(data$gas))
    return(getPredictionResponse(predict(model, newdata = df)))
}

ps = makeParamSet(
  makeIntegerParam("power", lower = 10, upper = 5555),
  makeIntegerParam("time", lower = 500, upper = 20210),
  makeDiscreteParam("gas", values = c("Nitrogen", "Air", "Argon")),
  makeIntegerParam("pressure", lower = 0, upper = 1000)
)

objfun = makeSingleObjectiveFunction(
     name = "Kapton",
     fn = fun,
     par.set = ps,
     has.simple.signature = FALSE,
     minimize = FALSE
)

# sample 9 points for the initial surrogate model, stratified across gases
samples.argon = sample(rownames(data_kapton[data_kapton$gas == "Argon", ]), 3)
samples.nitro = sample(rownames(data_kapton[data_kapton$gas == "Nitrogen", ]), 3)
samples.air = sample(rownames(data_kapton[data_kapton$gas == "Air", ]), 3)
initial.data = data_kapton[c(samples.argon, samples.nitro, samples.air), ]
cat(paste("Best training ratio: ", max(initial.data$ratio), "\n", sep = ""))

ctrl = makeMBOControl(y.name = "ratio")
ctrl = setMBOControlInfill(ctrl, opt = "focussearch", opt.focussearch.maxit = 20, opt.focussearch.points = 5, crit = makeMBOInfillCritEI())
ctrl = setMBOControlTermination(ctrl, iters = 50)

res = mbo(objfun, design = initial.data, control = ctrl, show.info = TRUE)
cat("Best configuration:\n")
cat(paste(paste(lapply(names(res$x), function(n) { paste(n, res$x[n], sep = ": ") }), collapse = ", "), "; ratio: ", res$y, "\n", sep = ""))
