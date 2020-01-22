library("batchtools")

# to use this framework make sure you have the following files
# KmArgonNugget.rds - to save some time, we fixed a learner (krigging ("km.regr") defaults + nugget.estim = TRUE)
# initialDesign.rds - fixed initial design which will be used for surrogate/infill criteria benchmark
# eiParam.R - expected improvement with exploration parameter
# eiParamAda.R - expected improvement with adaptive exploration parameter
# reduceResultsBoxplot.R - reduce results function
# plot.R - boxplot function

# create registry
# @param file.dir - name of the directory
# @param packages - r packages which are needed for the computation
reg = makeExperimentRegistry(file.dir = "differentModelConfigurations", make.default = TRUE,
                             seed = 1, packages = c("mlr","mlrMBO","smoof","snow","lhs","data.table"))


# define the objective function and the initial design for the mbo process
# @param data - will be defined in addProblem() further down
kaptonObj = function(data, ...) {
  
  fun = function(x) {
    df = as.data.frame(x)
    return(getPredictionResponse(predict(data, newdata = df)))
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
  
  # fixed initial design which will be used for surrogate/infill criteria benchmark
  initialDesign = readRDS("fixed/initialDesign.rds")
  
  return(list(objfun,initialDesign))
}

# @param name - name of the problem
# @param data - fixed learner for the objective function
# @param fun - objective function function
# you can add more objective functions or data sets for the benchmark by adding more addProblem() functions
addProblem(name = "kaptonArgon", 
           data = readRDS("fixed/KmArgonNugget.rds"),
           fun = kaptonObj, reg = reg)


# set the control parameters for the optimization
# @param instance - provides arguments forwarded from addProblem()
# @param instance[[1]] - provides the objective function
# @param instance[[2]] - provides a fixed initial design for the surrogate/infill criteria benchmark
# @param iters - will be defined in the algorithm design further down
# @param crit - will be defined in the algoritm desgin further down

# the infill criterias benchmark
mbofunCrits = function(instance, iters, crit, ...) {
  ctrl = makeMBOControl(y.name = "ratio")
  ctrl = setMBOControlInfill(ctrl, opt = "focussearch", opt.focussearch.maxit = 20,
                             opt.focussearch.points = 5, crit)
  ctrl = setMBOControlTermination(ctrl, iters)
  res = mbo(fun = instance[[1]], design = instance[[2]], control = ctrl, show.info = TRUE)
}

addAlgorithm(name = "infillCriteria", fun = mbofunCrits, reg = reg)


# the initial Design benchmark
mbofunInitial = function(instance, iters, funDesign, ...) {
  ctrl = makeMBOControl(y.name = "ratio")
  ctrl = setMBOControlInfill(ctrl, opt = "focussearch", opt.focussearch.maxit = 20,
                             opt.focussearch.points = 5, makeMBOInfillCritEI())
  ctrl = setMBOControlTermination(ctrl, iters)
  design = generateDesign(n = 9, par.set = getParamSet(instance[[1]]), fun = funDesign)
  res = mbo(fun = instance[[1]], design, control = ctrl, show.info = TRUE)
}

addAlgorithm(name = "initialDesign", fun = mbofunInitial, reg = reg)


# the surrogate benchmark
mbofunSurrogate = function(instance, iters, surrogate, ...) {
  ctrl = makeMBOControl(y.name = "ratio")
  ctrl = setMBOControlInfill(ctrl, opt = "focussearch", opt.focussearch.maxit = 20,
                             opt.focussearch.points = 5, makeMBOInfillCritEI())
  ctrl = setMBOControlTermination(ctrl, iters)
  res = mbo(fun = instance[[1]], design = instance[[2]], learner = surrogate, control = ctrl, show.info = TRUE)
}

addAlgorithm(name = "surrogate", fun = mbofunSurrogate, reg = reg)


# the amount of initial data benchmark
mbofunAmount = function(instance, iters, amount, ...) {
  ctrl = makeMBOControl(y.name = "ratio")
  ctrl = setMBOControlInfill(ctrl, opt = "focussearch", opt.focussearch.maxit = 20,
                             opt.focussearch.points = 5, makeMBOInfillCritEI())
  ctrl = setMBOControlTermination(ctrl, iters)
  design = generateDesign(n = amount, par.set = getParamSet(instance[[1]]), fun = lhs::maximinLHS)
  res = mbo(fun = instance[[1]], design, control = ctrl, show.info = TRUE)
}

addAlgorithm(name = "amountInitialData", fun = mbofunAmount, reg = reg)




# initialize functions makeMBOInfillCritEIcontrolExploration() and makeMBOInfillCritAdaEIctrlExploration()
source("fixed/eiParam.R")
source("fixed/eiParamAda.R")


# @param iterations - change it to investigate the effect of additional iteraions
iterations = 50

# algorithm design: the hyperparameters to benchmark
# each benchmark has its own algorithm design aka CJ
# be careful: you should not benchmark more than 6 configurations the same time
# the plot would get messy
algoDesign = list(
  infillCriteria = CJ(iters = c(iterations), 
                      crit = list(makeMBOInfillCritEI(),
                                  makeMBOInfillCritAEI(),
                                  makeMBOInfillCritEIcontrolExploration(controlExploration = 0.01),
                                  makeMBOInfillCritAdaEIctrlExploration(controlExplorationStart = 0.01,
                                                                        controlExplorationEnd = 0.001),
                                  makeMBOInfillCritCB()
                                 ),
                      sorted = FALSE),
  
  initialDesign = CJ(iters = c(iterations), 
                     funDesign = list(
                        maximinLHS,
                        randomLHS,
                        geneticLHS,
                        improvedLHS,
                        optimumLHS
                                     ),
                     sorted = FALSE),
  
  surrogate = CJ(iters = c(iterations), 
                 crit = list(
                   makeLearner("regr.km", predict.type = "se", covtype = "powexp", control = list(trace = FALSE)),
                   makeLearner("regr.km", predict.type = "se", covtype = "gauss", control = list(trace = FALSE)),
                   makeLearner("regr.km", predict.type = "se", covtype = "matern5_2", control = list(trace = FALSE)),
                   makeLearner("regr.randomForest", predict.type = "se", nodesize = 1),
                   makeLearner("regr.randomForest", predict.type = "se", nodesize = 5),
                   makeLearner("regr.randomForest", predict.type = "se", mtry = 4)
                            ),
                 sorted = FALSE),
  
  amountInitialData = CJ(iters = c(iterations),
                         amount = c(10,
                                    15,
                                    20,
                                    25,
                                    30),
                         sorted = FALSE)
)


# add Experiments to registry
addExperiments(prob.designs = NULL, algo.designs = algoDesign, repls = 5, reg = reg)

# summarize jobs 
summarizeExperiments(by = c("problem","algorithm"))

# use multiple CPU cores for the computation - default: use all cores
reg$cluster.functions = makeClusterFunctionsSocket()

# submit the jobs
submitJobs()

# check if the computation is finished or not
waitForJobs()

# check if the computation caused any error
getStatus()

# initialize function reduce() - not working at the moment
source("reduceResultsBoxplot.R")
# store the results - be careful it might be a large object!
results = reduceResultsDataTable(fun = reduce)

# initialize function plotBoxplots() - not working at the moment
# have a look to see the boxplot computet for a amount of initial design benchmark
# plotAmountInitialDesign.jpeg
source("plotBoxplots.R")
plotBoxplots(results)


# additional stuff - you will need it sometimes
########
######## test some jobs before submitting
testJob(id = 175)
########

########
######## find jobs which caused an error and submit them again
failed <- findErrors()
submitJobs(failed)
########

########
######## load registry if you started a clean session
loadRegistry(file.dir = "differentModelConfigurations",work.dir = NULL,
             conf.file = findConfFile(), make.default = TRUE, writeable = FALSE)
#######
