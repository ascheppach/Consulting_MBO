library(ParamHelpers)
library(checkmate)
library(smoof)
library(mlr)
library(mlrMBO)
library(ggplot2)
library(tidyr)

source("fixed/eiParam.R")
source("fixed/eiParamAda.R")

data = readRDS("fixed/KmArgonNugget.rds")

model = train(makeLearner("regr.km", nugget.estim = TRUE, control = list(trace = FALSE)),
              makeRegrTask(data = data, target = "ratio"))

funn = function(x) {
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
  
  makeDiscreteParam("Kernel", values = c("powexp","gauss","matern5_2", "matern3_2"),
                    requires = quote(Surrogate == "regr.km")),
  
  makeIntegerParam("ntree", lower = 200, upper = 500,
                   requires = quote(Surrogate == "regr.randomForest")), 
  
  makeDiscreteParam("InfillCrit", values = c("makeMBOInfillCritEI()",
                                             "makeMBOInfillCritEIcontrolExploration()",
                                             "makeMBOInfillCritAdaEIctrlExploration()")),
  
  makeNumericParam("ControlExploration", lower = 0.008, upper = 0.015,
                   requires = quote(InfillCrit == "makeMBOInfillCritEIcontrolExploration()")),
  
  makeNumericParam("startControlExploration", lower = 0.008, upper = 0.03,
                   requires = quote(InfillCrit == "makeMBOInfillCritAdaEIctrlExploration()")),
  
  makeNumericParam("endControlExploration", lower = 0.0008, upper = 0.002,
                   requires = quote(InfillCrit == "makeMBOInfillCritAdaEIctrlExploration()"))
)

### 2. Define Number of Iterations/Experiments and choose the Experiments/Hyperparameters 
# with Random-Design

Experiments = sampleValues(psTune,10)

### 3. Execute tuneRandom Algorithm

n <- length(Experiments)


# Execute the entire mbo-process n times (n=number of experiments/iterations) and save the 
# resuls in a list named tuneResult

tuneRandom = function(Experiments) {
  
  if (Experiments[[1]] == "regr.km") {
    lrn = makeLearner("regr.km", predict.type = "se", nugget.estim = TRUE,
                      covtype = Experiments[[2]], control = list(trace = FALSE)) 
  }
  
  if (Experiments[[1]] =="regr.randomForest") {
    lrn = makeLearner("regr.randomForest", predict.type = "se", ntree = Experiments[[3]])
  }
  
  if (Experiments[[4]] == "makeMBOInfillCritEI()") {
    ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())
  }
  
  if (Experiments[[4]] == "makeMBOInfillCritEIcontrolExploration()") {
    ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritEIcontrolExploration(
      controlExploration = Experiments[[5]]))
  }
  
  if (Experiments[[4]] == "makeMBOInfillCritAdaEIctrlExploration()") {
    ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritAdaEIctrlExploration(
      controlExplorationStart = Experiments[[6]], controlExplorationEnd = Experiments[[7]]))
  }
  
  des = generateDesign(n = 9, par.set = getParamSet(objfun), fun = lhs::maximinLHS)
  
  tuneResult <- mbo(objfun, design = des , learner = lrn, control = ctrl) 
}


tuneResult <- lapply(Experiments, tuneRandom)


### 4. Order the Result and show the best configurations 
ratio <- array(0, c(length(tuneResult),1))

for (i in 1:length(tuneResult)) {
  ratio[i,1] <- tuneResult[[i]]$y
}

configurations <- array(0, c(length(tuneResult),length(psTune$pars)))

configurations <- as.data.frame(Experiments[[1]]) 

for (i in 2:length(tuneResult)) {
  configurations <- rbind(configurations, as.data.frame(Experiments[[i]]))
}

results <- cbind(ratio, configurations)

# order results 
orderedResults <- results[order(results$ratio, decreasing = TRUE),]

# show me best 5 configurations (eliteConfigurations)

eliteConfigurations <- orderedResults[1:5,]

