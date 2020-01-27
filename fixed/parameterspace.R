## parameterspace

ps = makeParamSet(
  
  makeDiscreteParam("surrogate", values = c("regr.km","regr.randomForest")),
  
  makeDiscreteParam("kernel", values = c("powexp","gauss","matern5_2", "matern3_2"),
                    requires = quote(surrogate == "regr.km")),
  
  makeIntegerParam("nodesize", lower = 2, upper = 7,
                   requires = quote(surrogate == "regr.randomForest")),
  
  makeIntegerParam("mtry", lower = 1, upper = 3,
                   requires = quote(surrogate == "regr.randomForest")), 
  
  makeDiscreteParam("infillCrit", values = c("makeMBOInfillCritEI",
                                             "makeMBOInfillCritEIcontrolExploration",
                                             "makeMBOInfillCritAdaEIctrlExploration",
                                             "makeMBOInfillCritCB",
                                             "makeMBOInfillCritAEI",
                                             "makeMBOInfillCritAdaCB")),
  
  makeNumericParam("controlExploration", lower = 0.008, upper = 0.015,
                   requires = quote(infillCrit == "makeMBOInfillCritEIcontrolExploration")),
  
  makeNumericParam("startControlExploration", lower = 0.008, upper = 0.03,
                   requires = quote(infillCrit == "makeMBOInfillCritAdaEIctrlExploration")),
  
  makeNumericParam("endControlExploration", lower = 0.0008, upper = 0.002,
                   requires = quote(infillCrit == "makeMBOInfillCritAdaEIctrlExploration")),
  
  makeIntegerParam("amountInitialDesign", lower = 9, upper = 30),
  
  makeDiscreteParam("initialDesign", values = c("maximinLHS",
                                               "randomLHS",
                                               "geneticLHS",
                                               "improvedLHS",
                                               "optimumLHS",
                                               "randomData",
                                               "radomParam"))  #generateRandomDesign(10,ps)
)