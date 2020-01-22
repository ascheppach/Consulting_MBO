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

funn = function(x) {
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
  fn = funn,
  par.set = ps,
  has.simple.signature = FALSE,
  minimize = FALSE
)
ctrl = makeMBOControl(y.name = "ratio")
ctrl = setMBOControlTermination(ctrl, iters = 50)
############## randomTune #############

### 1. Define Parameterspace of Hyperparameters
psTune = makeParamSet(
  makeDiscreteParam("Surrogate", values = c("regr.km","regr.randomForest")),
  makeDiscreteParam("Kernel", values = c("powexp","gauss","matern5_2", "matern3_2"), requires = quote(Surrogate == "regr.km")),
  makeIntegerParam("ntree", lower = 200, upper = 500, requires = quote(Surrogate == "regr.randomForest")), 
  makeDiscreteParam("InfillCrit", values = c("makeMBOInfillCritEI()","makeMBOInfillCritEIcontrolExploration()","makeMBOInfillCritAdaEIctrlExploration()")),  
  makeNumericParam("ControlExploration", lower = 0.008, upper = 0.015, requires = quote(InfillCrit == "makeMBOInfillCritEIcontrolExploration()")),
  makeNumericParam("startControlExploration", lower = 0.008, upper = 0.03, requires = quote(InfillCrit == "makeMBOInfillCritAdaEIctrlExploration()")),
  makeNumericParam("endControlExploration", lower = 0.0008, upper = 0.002, requires = quote(InfillCrit == "makeMBOInfillCritAdaEIctrlExploration()"))
)

### 2. Define Number of Iterations/Experiments and choose the Experiments/Hyperparameters with Random-Design
Experiments = sampleValues(psTune,10)

### 3. Execute tuneRandom Algorithm

n <- length(Experiments)
#list with saved experiment results
TuneResult <- list()

# Execute the entire mbo-process n times (n=number of experiments/iterations) and save the resuls in a list

for (i in 1:n) {

  if (Experiments[[i]][[1]] == "regr.km") {
     lrn = makeLearner("regr.km", predict.type = "se", nugget.estim = TRUE, covtype = Experiments[[i]][[2]]) 
  }
  
  if (Experiments[[i]][[1]] =="regr.randomForest") {
    lrn = makeLearner("regr.randomForest", predict.type = "se", ntree = Experiments[[i]][[3]])
  }

  if (Experiments[[i]][[4]] == "makeMBOInfillCritEI") {
      ctrl = setMBOControlInfill(ctrl, opt = "focussearch", opt.focussearch.maxit = 20, opt.focussearch.points = 5, crit = makeMBOInfillCritEI())
  }

  if (Experiments[[i]][[4]] == "makeMBOInfillCritEIcontrolExploration") {
     ctrl = setMBOControlInfill(ctrl, opt = "focussearch", opt.focussearch.maxit = 20, opt.focussearch.points = 5, crit = makeMBOInfillCritEIcontrolExploration(controlExploration = Experiments[[i]][[5]]))
  }

  if (Experiments[[i]][[4]] == "makeMBOInfillCritAdaEIctrlExploration") {
     ctrl = setMBOControlInfill(ctrl, opt = "focussearch", opt.focussearch.maxit = 20, opt.focussearch.points = 5, crit = makeMBOInfillCritAdaEIctrlExploration(controlExplorationStart = Experiments[[i]][[6]], controlExplorationEnd = Experiments[[i]][[7]]))
  }
  
  des = generateDesign(n = 9, par.set = getParamSet(objfun), fun = lhs::maximinLHS)
  
  TuneResult[[i]] <- mbo(objfun, design = des , learner = lrn, control = ctrl) 
}  


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


