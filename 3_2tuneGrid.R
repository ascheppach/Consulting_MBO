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


## Funktionen ausführen, damit in Environment gespeichert
#EICP
makeMBOInfillCritEIcontrolExploration = function(se.threshold = 1e-6, controlExploration = 0.01) {
  assertNumber(se.threshold, lower = 1e-20)
  force(se.threshold)
  makeMBOInfillCrit(
    fun = function(points, models, control, par.set, designs, iter, progress, attributes = FALSE) {
      model = models[[1L]] # surrogate
      design = designs[[1]] # evaluated points
      maximize.mult = if (control$minimize) 1 else -1 # min oder max setup
      assertString(control$y.name) # assertions
      y = maximize.mult * design[, control$y.name] # y wert der desgin punkte
      assertNumeric(y, any.missing = FALSE) # assertions
      p = predict(model, newdata = points)$data # predicted mu_hat und sigma_hat by the surrogate model on the n points
      p.mu = maximize.mult * p$response # predicted mu of the surrogate
      p.se = p$se # predicted se of the surrogate
      y.min = min(y) # aktuelle minimum der bisherigen evaluierten punkte bestimmen
      d = y.min - p.mu - (controlExploration) # gap between actual y_min and the predicted mean of the new point, exploration parameter shrinks the gap -- geklaut: https://github.com/yanyachen/rBayesianOptimization/blob/ff949bc0a1c7dd3a8b79e704f74775c99638d56d/R/Utility.R#L27
      xcr = (d / p.se) #
      xcr.prob = pnorm(xcr) # distribution
      xcr.dens = dnorm(xcr) # density
      ei = d * xcr.prob + p.se * xcr.dens # ei berechnen
      res = ifelse(p.se < se.threshold, 0, -ei)
      if (attributes) {
        res = setAttribute(res, "crit.components", data.frame(se = p$se, mean = p$response,
                                                              controlExploration = controlExploration)) # wird in zeile 19&20 bestimmt. unser param beeinflusst dieses ergebniss nicht
      }
      return(res)
    },
    name = "Expected improvement with exploration parameter",
    id = "eicp",
    components = c("se", "mean", "controlExploration"),
    params = list(se.threshold = se.threshold, controlExploration = controlExploration),
    opt.direction = "maximize",
    requires.se = TRUE
  )
}

#adaEI
makeMBOInfillCritAdaEIctrlExploration = function(se.threshold = 1e-6, controlExplorationStart = 0.01, controlExplorationEnd = 0.001) {
  assertNumber(se.threshold, lower = 1e-20)
  force(se.threshold)
  makeMBOInfillCrit(
    fun = function(points, models, control, par.set, designs, iter, progress, attributes = FALSE) {
      model = models[[1L]] # surrogate model der bisherigen iterationen
      design = designs[[1]] # already evaluated points
      maximize.mult = if (control$minimize) 1 else -1 # min oder max setup
      assertString(control$y.name) # assertions
      y = maximize.mult * design[, control$y.name] # y wert der desgin punkte
      assertNumeric(y, any.missing = FALSE) # assertions
      p = predict(model, newdata = points)$data # predicted mu_hat und sigma_hat by the surrogate model on the n points
      p.mu = maximize.mult * p$response # predicted mu of the surrogate
      p.se = p$se # predicted se of the surrogate
      y.min = min(y) # aktuelle minimum der bisherigen evaluierten punkte bestimmen
      controlExploration = (1-progress) * controlExplorationStart + progress * controlExplorationEnd # geklaut: https://github.com/mlr-org/mlrMBO/blob/master/R/infill_crits.R
      d = y.min - p.mu - (controlExploration) # gap between actual y_min and the predicted mean of the new point -- geklaut: https://github.com/yanyachen/rBayesianOptimization/blob/ff949bc0a1c7dd3a8b79e704f74775c99638d56d/R/Utility.R#L27
      xcr = (d / p.se)
      xcr.prob = pnorm(xcr) # prob
      xcr.dens = dnorm(xcr) # density
      ei = d * xcr.prob + p.se * xcr.dens # ei berechnen
      res = ifelse(p.se < se.threshold, 0, -ei)
      if (attributes) {
        res = setAttribute(res, "crit.components", data.frame(se = p$se, mean = p$response))
      }
      return(res)
    },
    name = "Expected improvement with adaptive exploration parameter",
    id = "eiacp",
    components = c("se", "mean"),
    params = list(se.threshold = se.threshold),
    opt.direction = "maximize",
    requires.se = TRUE
  )
}

############## gridTune #############

### 1. Define Parameterspace of Hyperparameters
psTune = makeParamSet(
  makeDiscreteParam("Surrogate", values = c("regr.km","regr.randomForest")),
  makeDiscreteParam("Kernel", values = c("powexp","gauss","matern5_2", "matern3_2"), requires = quote(Surrogate == "regr.km")),
  makeIntegerParam("ntree", lower = 200, upper = 400, requires = quote(Surrogate == "regr.randomForest")), ## mit noch kleiner also 2 versuchen, weil beim interpolieren dann noch genauer und besser weil ja nur initial.design als punkte!
  makeDiscreteParam("InfillCrit", values = c("makeMBOInfillCritEI()","makeMBOInfillCritEIcontrolExploration()","makeMBOInfillCritAdaEIctrlExploration()")),  
  makeNumericParam("ControlExploration", lower = 0.008, upper = 0.015, requires = quote(InfillCrit == "makeMBOInfillCritEIcontrolExploration()")),
  makeNumericParam("startControlExploration", lower = 0.008, upper = 0.03, requires = quote(InfillCrit == "makeMBOInfillCritAdaEIctrlExploration()")),
  makeNumericParam("endControlExploration", lower = 0.0008, upper = 0.002, requires = quote(InfillCrit == "makeMBOInfillCritAdaEIctrlExploration()"))
)

### 2. Define Number of Iterations/Experiments and choose the Experiments/Hyperparameters with Grid-Design
Experiments_Grid = generateGridDesign(psTune, 3)  

# some data transformation to get the right structure
Experiments <- list(Experiments_Grid[1,]) 
for (i in 2:7) {
  Experiments <- rbind(Experiments, list(Experiments_Grid[i,]))
}

### 3. Execute tuneRandom Algorithm

n <- length(Experiments)
#list with saved experiment results
TuneResult <- list()

# Execute the entire mbo-process n times (n=number of experiments/iterations) and save the resuls in a list
for (i in 1:n) {
  
  if (Experiments[[i]][[1]] == "regr.km") {
    lrn = makeLearner("regr.km", predict.type = "se", covtype = as.character(Experiments[[i]][[2]])) 
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









