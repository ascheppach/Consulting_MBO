library(ParamHelpers)
library(checkmate)
library(smoof)
library(mlr)
library(mlrMBO)
library(ggplot2)
library(tidyr)
library("irace")
source("fixed/eiParam.R")
source("fixed/eiParamAda.R")


## execute
data <- read.csv("fixed/kapton_argon.csv")
data$X <- NULL

parameters.table <- '
surrogate                                    "surrogate"                            c ("regr.km","regr.randomForest")             
kernel                                        "kernel"                              c ("powexp","gauss","matern5_2", "matern3_2")                                                                                                                                                                     | surrogate == "regr.km"
nodesize                                      "nodesize"                            i (2,7)                                                                                                                                                                                                           | surrogate == "regr.randomForest"
mtry                                            "mtry"                              i (1,3)                                                                                                                                                                                                           | surrogate == "regr.randomForest"
infillCrit                                     "infillCrit"                         c ("makeMBOInfillCritEI","makeMBOInfillCritEIcontrolExploration","makeMBOInfillCritAdaEIctrlExploration", "makeMBOInfillCritCB", "makeMBOInfillCritAEI","makeMBOInfillCritAdaCB")
controlExploration                           "controlExploration"                   r (0.008,0.015)                                                                                                                                                                                                   | infillCrit == "makeMBOInfillCritEIcontrolExploration" 
startControlExploration                      "startControlExploration"              r (0.008,0.03)                                                                                                                                                                                                    | infillCrit == "makeMBOInfillCritAdaEIctrlExploration" 
endControlExploration                         "endControlExploration"               r (0.0008,0.002)                                                                                                                                                                                                  | infillCrit == "makeMBOInfillCritAdaEIctrlExploration" 
amountInitialDesign                          "amountInitialDesign"                  i (9,30)
initialDesign                                   "initialDesign"                     c ("maximinLHS","randomLHS","geneticLHS", "improvedLHS", "optimumLHS")
'  
parameters <- readParameters(text= parameters.table)

##, "radomParam" raus aus paramspace


######## target runner #######
############ Instance ############
target.runner <- function(experiment, scenario) {
  
  debugLevel    <- scenario$debugLevel
  configuration.id  <- experiment$id.configuration
  instance.id   <- experiment$id.instance
  seed          <- experiment$seed
  configuration <- experiment$configuration
  instance      <- experiment$instance
  instance <- data
  
  model = train(makeLearner("regr.randomForest"), makeRegrTask(data = instance, target = "ratio"))
  
  fun = function(x) {
    df = as.data.frame(x)
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
  
  ctrl = makeMBOControl(y.name = "ratio")
  
  ctrl = setMBOControlTermination(ctrl, iters = 50)
  
  if (as.character(configuration[["surrogate"]]) == "regr.km") {
    lrn = makeLearner("regr.km", predict.type = "se", nugget.estim = TRUE,
                      control = list(trace = FALSE),
                      covtype = as.character(configuration[["kernel"]]))
  }
  
  if (as.character(configuration[["surrogate"]]) == "regr.randomForest") {
    lrn = makeLearner("regr.randomForest", predict.type = "se",
                      nodesize = as.integer(configuration[["nodesize"]]),
                      mtry = as.integer(configuration[["mtry"]]))
  }
  
  if (as.factor(configuration[["infillCrit"]]) == "makeMBOInfillCritEI") {
    ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())
  }
  
  if (as.factor(configuration[["infillCrit"]]) == "makeMBOInfillCritEIcontrolExploration") {
    ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritEIcontrolExploration(
      controlExploration = as.numeric(configuration[["controlExploration"]])))
  }
  
  if (as.factor(configuration[["infillCrit"]]) == "makeMBOInfillCritAdaEIctrlExploration") {
    ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritAdaEIctrlExploration(
      controlExplorationStart = as.numeric(configuration[["startControlExploration"]]), 
      controlExplorationEnd = as.numeric(configuration[["endControlExploration"]])))
  }
  
  if (as.factor(configuration[["infillCrit"]]) == "makeMBOInfillCritCB") {
    ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritCB())
  }
  
  if (as.factor(configuration[["infillCrit"]]) == "makeMBOInfillCritAEI") {
    ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritAEI())
  }
  
  if (as.factor(configuration[["infillCrit"]]) == "makeMBOInfillCritAdaCB") {
    ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritAdaCB())
  }
  
  if (as.factor(configuration[["initialDesign"]]) == "maximinLHS") {
    des = generateDesign(n = as.integer(configuration[["amountInitialDesign"]]),
                         par.set = getParamSet(instance), fun = lhs::maximinLHS)
  }
  
  if (as.factor(configuration[["initialDesign"]]) == "randomLHS") {
    des = generateDesign(n = as.integer(configuration[["amountInitialDesign"]]),
                         par.set = getParamSet(objfun), fun = lhs::randomLHS)
  }
  
  if (as.factor(configuration[["initialDesign"]]) == "geneticLHS") {
    des = generateDesign(n = as.integer(configuration[["amountInitialDesign"]]),
                         par.set = getParamSet(objfun), fun = lhs::geneticLHS)
  }
  
  if (as.factor(configuration[["initialDesign"]]) == "improvedLHS") {
    des = generateDesign(n = as.integer(configuration[["amountInitialDesign"]]),
                         par.set = getParamSet(objfun), fun = lhs::improvedLHS)
  }
  
  if (as.factor(configuration[["initialDesign"]]) == "optimumLHS") {
    des = generateDesign(n = as.integer(configuration[["amountInitialDesign"]]),
                         par.set = getParamSet(objfun), fun = lhs::optimumLHS)
  }
  
  #if (as.factor(configuration[["initialDesign"]]) == "radomParam") {
  #  des = generateDesign(n = as.integer(configuration[["amountInitialDesign"]]),
  #                       par.set = getParamSet(objfun), fun = lhs::radomParam)
  #}
  
  res <- mbo(objfun, design = des , learner = lrn, control = ctrl) 
  
  result <- list(cost = -1*(res$y), call = toString(experiment))
  
  return(result)
}


########### Run-Function / Irace Options / Configuration Scenario ########
#
scenario                <- defaultScenario()
scenario$seed           <- 132348834
scenario$targetRunner   <- "target.runner" 
scenario$maxExperiments <- 25000 
scenario$instances      <- data
scenario$nbConfigurations <- 20

checkIraceScenario(scenario, parameters)

printScenario(scenario)

irace(scenario = scenario, parameters = parameters)






