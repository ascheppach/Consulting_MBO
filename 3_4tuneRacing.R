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
KmArgonNugget <- readRDS("fixed/KmArgonNugget.rds")


parameters.table <- '
Surrogate                              "Surrogate"                                c ("regr.km","regr.randomForest")             
Kernel                                  "Kernel"                                  c ("powexp","gauss","matern5_2", "matern3_2")                                                                   | Surrogate == "regr.km"
Nodesize                                  "Nodesize"                              i (5,10)                                                                                                        | Surrogate == "regr.randomForest"
InfillCrit                             "InfillCrit"                               c ("makeMBOInfillCritEI","makeMBOInfillCritEIcontrolExploration","makeMBOInfillCritAdaEIctrlExploration")
CPEI                                     "CPEI"                                   r (0.008,0.015)                                                                                                 | InfillCrit == "makeMBOInfillCritEIcontrolExploration" 
adaStartCPEI                           "adaStartCPEI"                             r (0.008,0.03)                                                                                                  | InfillCrit == "makeMBOInfillCritAdaEIctrlExploration" 
adaEndCPEI                              "adaEndCPEI"                              r (0.0008,0.002)                                                                                                | InfillCrit == "makeMBOInfillCritAdaEIctrlExploration" 
'  
parameters <- readParameters(text = parameters.table)


######## target runner ###### + ############ Instance ############
target.runner <- function(experiment, scenario) {
  
  debugLevel    <- scenario$debugLevel
  configuration.id  <- experiment$id.configuration
  instance.id   <- experiment$id.instance
  seed          <- experiment$seed
  configuration <- experiment$configuration
  instance      <- experiment$instance
  instance <- data
  
  fun = function(x) {
    df = as.data.frame(x)
    return(getPredictionResponse(predict(KmArgonNugget, newdata = df)))
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
  
  des = generateDesign(n = 9, par.set = getParamSet(objfun), fun = lhs::maximinLHS)
  ctrl = makeMBOControl(y.name = "ratio")
  ctrl = setMBOControlTermination(ctrl, iters = 50)
  
  if (as.character(configuration[["Surrogate"]]) == "regr.km") {
    lrn = makeLearner("regr.km", predict.type = "se", covtype = as.character(configuration[["Kernel"]]))
  }
  if (as.character(configuration[["Surrogate"]]) == "regr.randomForest") {
    lrn = makeLearner("regr.randomForest", predict.type = "se", nodesize = as.integer(configuration[["Nodesize"]]))
  }
  
  if (as.factor(configuration[["InfillCrit"]]) == "makeMBOInfillCritEI") {
    ctrl = setMBOControlInfill(ctrl, opt = "focussearch", opt.focussearch.maxit = 20, opt.focussearch.points = 5, crit = makeMBOInfillCritEI())
  }
  
  if (as.factor(configuration[["InfillCrit"]]) == "makeMBOInfillCritEIcontrolExploration") {
    ctrl = setMBOControlInfill(ctrl, opt = "focussearch", opt.focussearch.maxit = 20, opt.focussearch.points = 5, crit = makeMBOInfillCritEIcontrolExploration(controlExploration = as.numeric(configuration[["CPEI"]])))
  }
  
  if (as.factor(configuration[["InfillCrit"]]) == "makeMBOInfillCritAdaEIctrlExploration") {
    ctrl = setMBOControlInfill(ctrl, opt = "focussearch", opt.focussearch.maxit = 20, opt.focussearch.points = 5, crit = makeMBOInfillCritAdaEIctrlExploration(controlExplorationStart = as.numeric(configuration[["adaStartCPEI"]]), controlExplorationEnd = as.numeric(configuration[["adaEndCPEI"]])))
  }
  
  res <- mbo(objfun, design = des , learner = lrn, control = ctrl) 
  
  result <- list(cost = -1*(res$y), call = toString(experiment))
  
  return(result)
}


########### Run-Function / Irace Options / Configuration Scenario ########
#
scenario                <- defaultScenario()
scenario$seed           <- 132348834
scenario$targetRunner   <- "target.runner" 
scenario$maxExperiments <- 100 
scenario$instances      <- data
scenario$nbConfigurations <- 20

checkIraceScenario(scenario, parameters)

printScenario(scenario)

irace(scenario = scenario, parameters = parameters)

