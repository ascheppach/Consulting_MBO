library(ParamHelpers)
library(checkmate)
library(smoof)
library(mlr)
library(mlrMBO)
library(ggplot2)
library(tidyr)
source("fixed/eiParam.R")
source("fixed/eiParamAda.R")


## execute
KmArgonNugget <- readRDS("fixed/KmArgonNugget.rds")

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
ctrl = makeMBOControl(y.name = "ratio")
ctrl = setMBOControlTermination(ctrl, iters = 50)


############## gridTune #############

### 1. Define Parameterspace of Hyperparameters
psTune = makeParamSet(
  
  makeDiscreteParam("Surrogate", values = c("regr.km","regr.randomForest")), #1
  
  makeDiscreteParam("Kernel", values = c("powexp","gauss","matern5_2", "matern3_2"), #2
                    requires = quote(Surrogate == "regr.km")),
  
  makeIntegerParam("nodesize", lower = 2, upper = 7,
                   requires = quote(Surrogate == "regr.randomForest")), #3
  
  makeIntegerParam("mtry", lower = 1, upper = 3,
                   requires = quote(Surrogate == "regr.randomForest")), #4 
  
  makeDiscreteParam("InfillCrit", values = c("makeMBOInfillCritEI()",
                                             "makeMBOInfillCritCB()",
                                             "makeMBOInfillCritAEI()",
                                             "makeMBOInfillCritAdaCB()",
                                             "makeMBOInfillCritEIcontrolExploration()", 
                                             "makeMBOInfillCritAdaEIctrlExploration()")), #5
  
  makeNumericParam("ControlExploration", lower = 0.008, upper = 0.015,
                   requires = quote(InfillCrit == "makeMBOInfillCritEIcontrolExploration()")), #6
  
  makeNumericParam("startControlExploration", lower = 0.008, upper = 0.03,
                   requires = quote(InfillCrit == "makeMBOInfillCritAdaEIctrlExploration()")), #7
  
  makeNumericParam("endControlExploration", lower = 0.0008, upper = 0.002,
                   requires = quote(InfillCrit == "makeMBOInfillCritAdaEIctrlExploration()")), #8
  
  makeIntegerParam("amountInitialDesign", lower = 9, upper = 30), #9
  
  makeDiscreteParam("initialDesign", values = c("maximinLHS", #10
                                                "randomLHS",
                                                "geneticLHS",
                                                "improvedLHS",
                                                "optimumLHS",
                                                "randomData",
                                                "radomPs"))
)

### 2. Define Number of Iterations/Experiments and choose the Experiments/Hyperparameters with Grid-Design
experimentsGrid = generateGridDesign(psTune, 1)  

# some data transformation to get the right structure
experiments <- list(experimentsGrid[1,]) 
for (i in 2:210) {
  experiments <- rbind(experiments, list(experimentsGrid[i,]))
}

### 3. Execute tuneRandom Algorithm

n <- length(experiments)
#list with saved experiment results
TuneResult <- list()

# Execute the entire mbo-process n times (n=number of experiments/iterations) and save the resuls in a list
tuneGrid = function(experiments) {
  
  if (experiments[[1]] == "regr.km") {
    lrn = makeLearner("regr.km", predict.type = "se", nugget.estim = TRUE,
                      covtype = as.character(experiments[[2]]), control = list(trace = FALSE)) 
  }
  
  if (experiments[[1]] =="regr.randomForest") {
    lrn = makeLearner("regr.randomForest", predict.type = "se",
                      nodesize = experiments[[3]], mtry = experiments[[4]])
  }
  
  if (experiments[[5]] == "makeMBOInfillCritEI()") {
    ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())
  }
  
  if (experiments[[5]] == "makeMBOInfillCritCB()") {
    ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritCB())
  }
  
  if (experiments[[5]] == "makeMBOInfillCritAdaCB()") {
    ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritAdaCB())
  }
  
  if (experiments[[5]] == "makeMBOInfillCritAEI()") {
    ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritAEI())
  }
  
  if (experiments[[5]] == "makeMBOInfillCritEIcontrolExploration()") {
    ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritEIcontrolExploration(
      controlExploration = experiments[[6]]))
  }
  
  if (experiments[[5]] == "makeMBOInfillCritAdaEIctrlExploration()") {
    ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritAdaEIctrlExploration(
      controlExplorationStart = experiments[[7]], controlExplorationEnd = experiments[[8]]))
  }
  
  if (experiments[[10]] == "maximinLHS") {
    des = generateDesign(n = experiments[[9]], par.set = getParamSet(objfun), fun = lhs::maximinLHS)
  }
  
  if (experiments[[10]] == "randomLHS") {
    des = generateDesign(n = experiments[[9]], par.set = getParamSet(objfun), fun = lhs::randomLHS)
  }
  
  if (experiments[[10]] == "geneticLHS") {
    des = generateDesign(n = experiments[[9]], par.set = getParamSet(objfun), fun = lhs::geneticLHS)
  }
  
  if (experiments[[10]] == "optimumLHS") {
    des = generateDesign(n = experiments[[9]], par.set = getParamSet(objfun), fun = lhs::optimumLHS)
  }
  
  if (experiments[[10]] == "improvedLHS") {
    des = generateDesign(n = experiments[[9]], par.set = getParamSet(objfun), fun = lhs::improvedLHS)
  }
  
  if (experiments[[10]] == "radomPs") {
    des = generateRandomDesign(experiments[[9]], ps)
  }
  
  if (experiments[[10]] == "randomData") {
    des = data_kapton[sample(1:nrow(data_kapton), experiments[[9]]), 1:3]
  }
  
  tuneResult <- mbo(objfun, design = des , learner = lrn, control = ctrl) 
}

tuneResult <- lapply(experiments, tuneGrid)



### 4. Order the Result and show the best configurations 

# brauche keinen vektor, sondern eine ganze matrix mit den zugeöhrigen konfigurationen
# danach nach ratio sortieren und ich bin fertig

ratio <- array(0, c(length(TuneResult),1))
for (i in 1:length(TuneResult)) {
  ratio[i,1] <- TuneResult[[i]]$y
}

configurations <- array(0, c(length(TuneResult),length(psTune$pars)))
configurations <- as.data.frame(Experiments[[1]]) 
for (i in 2:length(TuneResult)) {
  configurations <- rbind(configurations, as.data.frame(Experiments[[i]]))
}

Results <- cbind(ratio, configurations)

# order results 
orderedResults <- Results[order(Results$ratio, decreasing = TRUE),]

# show me best 5 configurations (eliteConfigurations)

eliteConfigurations <- orderedResults[1:5,]









