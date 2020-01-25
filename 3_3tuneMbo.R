library(mlr)
library(mlrMBO)
source("fixed/eiParam.R")
source("fixed/eiParamAda.R")
##################

# we try to tune the hyperparameters of the BO with mbo
# we have 2 mbo levels
# 1st level mbo is the BO-Code provided by Lars (to find max ratio)
# 2nd level mbo is the parameter tuning for the 1st level mbo
# sadly we have a problem:
# if the 1st level runs more than 20 iterations "R Session Aborted"
# we have no idea why this happens

data_kapton <- read.csv("fixed/kapton_argon.csv", colClasses=c("NULL",NA,NA,NA,NA))

# model from all data points
model = train(makeLearner("regr.randomForest"), makeRegrTask(data = data_kapton, target = "ratio"))



funTuning = function(x) {
  
  df2 = as.list(x)
  
  funMBO = function(x) {
    df = as.data.frame(x)
    return(getPredictionResponse(predict(model, newdata = df)))
  }
  
  psMBO = makeParamSet(
    makeIntegerParam("power", lower = 10, upper = 5555),
    makeIntegerParam("time", lower = 500, upper = 20210),
    makeIntegerParam("pressure", lower = 0, upper = 1000)
  )
  
  objfun = makeSingleObjectiveFunction(
    name = "Kapton",
    fn = funMBO,
    par.set = psMBO,
    has.simple.signature = FALSE,
    minimize = FALSE
  )
  
  ctrl = makeMBOControl(y.name = "ratio")
  
  if (df2$learner == "regr.km") {
    lrn = makeLearner("regr.km", predict.type = "se", nugget.estim = TRUE,
                      covtype = df2$kmKernel, control = list(trace = FALSE))
  }
  
  if (df2$learner == "regr.randomForest") {
    lrn = makeLearner("regr.randomForest", predict.type = "se")
  }
  
  if (df2$infillCrit != ("makeMBOInfillCritEIcontrolExploration()" || "makeMBOInfillCritAdaEIctrlExploration()")) {
    ctrl = setMBOControlInfill(ctrl, crit = df2$infillCrit)
  }
  
  if (df2$infillCrit == "makeMBOInfillCritEIcontrolExploration()") {
    ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritEIcontrolExploration(
      controlExploration = df2$controlExploration))
  }
  
  if (df2$infillCrit == "makeMBOInfillCritAdaEIctrlExploration()") {
    ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritAdaEIctrlExploration(
      controlExplorationStart = df2$startControlExploration,
      controlExplorationEnd = df2$endControlExploration))
  }
  
  if (df2$infillCrit == "makeMBOInfillCritAEI()") {
    ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritAEI())
  }
  
  ctrl = setMBOControlTermination(ctrl, iters = 50)
  
  res = mbo(objfun,
            learner = lrn, 
            control = ctrl, show.info = TRUE)
  
  return(res$y)
  # 1st level end
}


# 2nd level start
psTune = makeParamSet(
  makeDiscreteParam("learner", values = c("regr.km","regr.randomForest")),
  
  makeDiscreteParam("kmKernel", values = c("powexp","gauss","matern5_2", "matern3_2"),
                    requires = quote(learner == "regr.km")),
  
  makeDiscreteParam("infillCrit", values = c("makeMBOInfillCritEI()",
                                             
                                             "makeMBOInfillCritAEI()")) #"makeMBOInfillCritEIcontrolExploration()", "makeMBOInfillCritAdaEIctrlExploration()",
  
  #makeNumericParam("controlExploration", lower = 0.008, upper = 0.015,
  #                 requires = quote(infillCrit == "makeMBOInfillCritEIcontrolExploration()")),
  
  #makeNumericParam("startControlExploration", lower = 0.008, upper = 0.03,
  #                 requires = quote(infillCrit == "makeMBOInfillCritAdaEIctrlExploration()")),
  
  #makeNumericParam("endControlExploration", lower = 0.0008, upper = 0.002,
  #                 requires = quote(infillCrit == "makeMBOInfillCritAdaEIctrlExploration()"))
)

objfun = makeSingleObjectiveFunction(
  name = "HPmbo",
  fn = funTuning,
  par.set = psTune,
  has.simple.signature = FALSE,
  minimize = FALSE
)

ctrl = makeMBOControl(y.name = "bestY")
ctrl = setMBOControlInfill(ctrl)
ctrl = setMBOControlTermination(ctrl, iters = 200)

res = mbo(objfun, control = ctrl, show.info = TRUE)

optimization_path <- as.data.frame(res$opt.path)
# 2nd level end
