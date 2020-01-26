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
reg = makeExperimentRegistry(file.dir = "tuneRandomRF2", make.default = TRUE,
                             seed = 1, packages = c("mlr","mlrMBO","smoof","snow","lhs","data.table"))



############## randomTune #############

### 1. Define Parameterspace of Hyperparameters
psTune = makeParamSet(
  
  makeDiscreteParam("surrogate", values = c("regr.km","regr.randomForest")), #1
  
  makeDiscreteParam("kernel", values = c("powexp","gauss","matern5_2", "matern3_2"), #2
                    requires = quote(surrogate == "regr.km")),
  
  makeIntegerParam("nodesize", lower = 2, upper = 7,
                   requires = quote(surrogate == "regr.randomForest")), #3
  
  makeIntegerParam("mtry", lower = 1, upper = 3,
                   requires = quote(surrogate == "regr.randomForest")), #4 
  
  makeDiscreteParam("infillCrit", values = c("makeMBOInfillCritEI()",
                                             "makeMBOInfillCritCB()",
                                             "makeMBOInfillCritAEI()",
                                             "makeMBOInfillCritAdaCB()",
                                             "makeMBOInfillCritEIcontrolExploration()", 
                                             "makeMBOInfillCritAdaEIctrlExploration()")), #5
  
  makeNumericParam("controlExploration", lower = 0.008, upper = 0.015,
                   requires = quote(infillCrit == "makeMBOInfillCritEIcontrolExploration()")), #6
  
  makeNumericParam("startControlExploration", lower = 0.008, upper = 0.03,
                   requires = quote(infillCrit == "makeMBOInfillCritAdaEIctrlExploration()")), #7
  
  makeNumericParam("endControlExploration", lower = 0.0008, upper = 0.002,
                   requires = quote(infillCrit == "makeMBOInfillCritAdaEIctrlExploration()")), #8
  
  makeIntegerParam("amountInitialDesign", lower = 9, upper = 30), #9
  
  makeDiscreteParam("initialDesign", values = c("maximinLHS", #10
                                                "randomLHS",
                                                "geneticLHS",
                                                "improvedLHS",
                                                "optimumLHS",
                                                #"randomData",
                                                "radomPs"))
)







desRandom = generateRandomDesign(200,psTune)
desRandom$surrogate = as.character(desRandom$surrogate)
desRandom$kernel = as.character(desRandom$kernel)
desRandom$infillCrit = as.character(desRandom$infillCrit)
desRandom$initialDesign = as.character(desRandom$initialDesign)


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
  
  return(objfun)
}

# @param name - name of the problem
# @param data - fixed learner for the objective function
# @param fun - objective function function
# you can add more objective functions or data sets for the benchmark by adding more addProblem() functions
addProblem(name = "kaptonArgon", 
           data = readRDS("fixed/RfArgon.rds"),
           fun = kaptonObj, reg = reg)


# set the control parameters for the optimization
# @param instance - provides arguments forwarded from addProblem()
# @param iters - will be defined in the algorithm design further down
# @param crit - will be defined in the algoritm desgin further down

# the infill criterias benchmark
tuneRandom = function(instance, iters, surrogate, kernel, nodesize, mtry, 
                       infillCrit, controlExploration, startControlExploration,
                       endControlExploration, amountInitialDesign, 
                       initialDesign, ...) {
  
  # initialize functions makeMBOInfillCritEIcontrolExploration() and makeMBOInfillCritAdaEIctrlExploration()
  source("fixed/eiParam.R")
  source("fixed/eiParamAda.R")
  
  ctrl = makeMBOControl(y.name = "ratio")
  ctrl = setMBOControlTermination(ctrl, iters = iters)
  
  if (surrogate == "regr.km") {
    lrn = makeLearner("regr.km", predict.type = "se", nugget.estim = TRUE,
                      covtype = kernel, control = list(trace = FALSE)) 
  }
  
  if (surrogate =="regr.randomForest") {
    lrn = makeLearner("regr.randomForest", predict.type = "se",
                      nodesize = nodesize, mtry = mtry)
  }
  
  if (infillCrit == "makeMBOInfillCritEI()") {
    ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())
  }
  
  if (infillCrit == "makeMBOInfillCritCB()") {
    ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritCB())
  }
  
  if (infillCrit == "makeMBOInfillCritAdaCB()") {
    ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritAdaCB())
  }
  
  if (infillCrit == "makeMBOInfillCritAEI()") {
    ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritAEI())
  }
  
  if (infillCrit == "makeMBOInfillCritEIcontrolExploration()") {
    ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritEIcontrolExploration(
      controlExploration = controlExploration))
  }
  
  if (infillCrit == "makeMBOInfillCritAdaEIctrlExploration()") {
    ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritAdaEIctrlExploration(
      controlExplorationStart = startControlExploration,
      controlExplorationEnd = endControlExploration))
  }
  
  if (initialDesign == "maximinLHS") {
    des = generateDesign(n = amountInitialDesign,
                         par.set = getParamSet(instance), fun = lhs::maximinLHS)
  }
  
  if (initialDesign == "randomLHS") {
    des = generateDesign(n = amountInitialDesign,
                         par.set = getParamSet(instance), fun = lhs::randomLHS)
  }
  
  if (initialDesign == "geneticLHS") {
    des = generateDesign(n = amountInitialDesign, 
                         par.set = getParamSet(instance), fun = lhs::geneticLHS)
  }
  
  if (initialDesign == "optimumLHS") {
    des = generateDesign(n = amountInitialDesign,
                         par.set = getParamSet(instance), fun = lhs::optimumLHS)
  }
  
  if (initialDesign == "improvedLHS") {
    des = generateDesign(n = amountInitialDesign, 
                         par.set = getParamSet(instance), fun = lhs::improvedLHS)
  }
  
  if (initialDesign == "radomPs") {
    des = generateRandomDesign(amountInitialDesign, getParamSet(instance))
  }
  
  #if (initialDesign == "randomData") {
  #  des = data_kapton[sample(1:nrow(data_kapton), amountInitialDesign), 1:3]
  #}
  
 res = mbo(instance, design = des , learner = lrn, control = ctrl) 
  
}

addAlgorithm(name = "tuneRandom", fun = tuneRandom, reg = reg)











# @param iterations - change it to investigate the effect of additional iteraions
iterations = 50

# algorithm design: the hyperparameters to benchmark
# each benchmark has its own algorithm design
# use CJ for cross join and data.table for normal join
# be careful: you should not benchmark more than 6 configurations the same time
# the plot would get messy
algoDesign = list(
  tuneRandom = data.table(iters = iterations, 
                          surrogate = desRandom$surrogate,
                          kernel = desRandom$kernel,
                          nodesize = desRandom$nodesize,
                          mtry = desRandom$mtry,
                          infillCrit = desRandom$infillCrit,
                          controlExploration = desRandom$controlExploration,
                          startControlExploration = desRandom$startControlExploration,
                          endControlExploration = desRandom$endControlExploration,
                          amountInitialDesign = desRandom$amountInitialDesign,
                          initialDesign = desRandom$initialDesign
  )
)


# add Experiments to registry
addExperiments(prob.designs = NULL, algo.designs = algoDesign, repls = 10, reg = reg)

# summarize jobs 
summarizeExperiments(by = c("problem","algorithm"))

# use multiple CPU cores for the computation - default: use all cores
reg$cluster.functions = makeClusterFunctionsSocket(ncpus = 8)

# submit the jobs
submitJobs()

# check if the computation is finished or not
waitForJobs()

# check if the computation caused any error
getStatus()

# initialize function reduce() - not working at the moment
source("3_1tuneBtReduce.R")
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
expired <- findExpired()
submitJobs(failed)
submitJobs(expired)
########

########
######## load registry if you started a clean session
loadRegistry(file.dir = "differentModelConfigurations",work.dir = NULL,
             conf.file = findConfFile(), make.default = TRUE, writeable = FALSE)
#######
