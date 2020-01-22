library(mlr)
library(mlrMBO)
source("done/eiParam.R")
source("done/eiParamAda.R")
##################

# we try to tune the hyperparameters of the BO with mbo
# we have 2 mbo levels
# 1st level mbo is the BO to find max ratio
# 2nd level mbo is the parameter tuning for the 1st level mbo

funTuning = function(x) {
  df = as.list(x)
  
  # 1st level start
  KmArgonNugget <- readRDS("fixed/KmArgonNugget.rds")

  funMBO = function(x) {
    df = as.data.frame(x)
    return(getPredictionResponse(predict(KmArgonNugget, newdata = df)))
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
  
  if (df$learner == "regr.km") {
    lrn = makeLearner("regr.km", predict.type = "se", nugget.estim = TRUE, covtype = df$kmKernel, control = list(trace = TRUE))
  }
  
  if (df$learner == "regr.randomForest") {
    lrn = makeLearner("regr.randomForest", predict.type = "se", ntree = df$ntree)
  }
    
  if (df$infillCrit == "makeMBOInfillCritEI()") {
    ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())
  }
    
  if (df$infillCrit == "makeMBOInfillCritEIcontrolExploration()") {
    ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritEIcontrolExploration(controlExploration = df$controlExploration))
  }

  if (df$infillCrit == "makeMBOInfillCritAdaEIctrlExploration()") {
    ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritAdaEIctrlExploration(controlExplorationStart = df$startControlExploration,
                                                                                  controlExplorationEnd = df$endControlExploration))
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
  
  makeIntegerParam("ntree", lower = 200, upper = 500,
                   requires = quote(learner == "regr.randomForest")), 
  
  makeDiscreteParam("infillCrit", values = c("makeMBOInfillCritEI()",
                                             "makeMBOInfillCritEIcontrolExploration()",
                                             "makeMBOInfillCritAdaEIctrlExploration()")),
  
  makeNumericParam("controlExploration", lower = 0.008, upper = 0.015,
                   requires = quote(infillCrit == "makeMBOInfillCritEIcontrolExploration()")),
  
  makeNumericParam("startControlExploration", lower = 0.008, upper = 0.03,
                   requires = quote(infillCrit == "makeMBOInfillCritAdaEIctrlExploration()")),
  
  makeNumericParam("endControlExploration", lower = 0.0008, upper = 0.002,
                   requires = quote(infillCrit == "makeMBOInfillCritAdaEIctrlExploration()"))
)

objfun = makeSingleObjectiveFunction(
  name = "HPmbo",
  fn = funTuning,
  par.set = psTune,
  has.simple.signature = FALSE,
  minimize = FALSE
)

ctrl = makeMBOControl(y.name = "best y")
ctrl = setMBOControlInfill(ctrl)
ctrl = setMBOControlTermination(ctrl, iters = 50)

res = mbo(objfun, control = ctrl, show.info = TRUE)

optimization_path <- as.data.frame(res$opt.path)
# 2nd level end