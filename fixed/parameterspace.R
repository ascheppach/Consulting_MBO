## parameterspace

ps = makeParamSet(
  
  makeDiscreteParam("Surrogate", values = c("regr.km","regr.randomForest")),
  
  makeDiscreteParam("Kernel", values = c("powexp","gauss","matern5_2", "matern3_2"),
                    requires = quote(Surrogate == "regr.km")),
  
  makeIntegerParam("nodesize", lower = 2, upper = 7,
                   requires = quote(Surrogate == "regr.randomForest")), 
  
  makeDiscreteParam("InfillCrit", values = c("makeMBOInfillCritEI()",
                                             "makeMBOInfillCritEIcontrolExploration()",
                                             "makeMBOInfillCritAdaEIctrlExploration()",
                                             "makeMBOInfillCritCB()",
                                             "makeMBOInfillCritAEI()",
                                             "makeMBOInfillCritAdaCB()")),
  
  makeNumericParam("ControlExploration", lower = 0.008, upper = 0.015,
                   requires = quote(InfillCrit == "makeMBOInfillCritEIcontrolExploration()")),
  
  makeNumericParam("startControlExploration", lower = 0.008, upper = 0.03,
                   requires = quote(InfillCrit == "makeMBOInfillCritAdaEIctrlExploration()")),
  
  makeNumericParam("endControlExploration", lower = 0.0008, upper = 0.002,
                   requires = quote(InfillCrit == "makeMBOInfillCritAdaEIctrlExploration()")),
  
  makeIntegerParam("amountInitialDesign", lower = 9, upper = 30),
  
  makeDiscreteParam("initialDesign", values = c("maximinLHS",
                                               "randomLHS",
                                               "geneticLHS",
                                               "improvedLHS",
                                               "optimumLHS"))
)
