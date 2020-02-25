optimizeRacing = function(model, ps, n = 1000, minimize = FALSE) {
  
  if (minimize == FALSE) {
    p = (-1)
  }
  if (minimize == TRUE) {
    p = (1)
  }
  
  y.name = model[["task.desc"]][["target"]]
  parameters = convertParamSetToIrace(ps)
  
  target.runner <- function(experiment, scenario) {
    
    debugLevel    <- scenario$debugLevel
    configuration.id  <- experiment$id.configuration
    instance.id   <- experiment$id.instance
    seed          <- experiment$seed
    configuration <- experiment$configuration
    instance      <- experiment$instance
    instance <- data
    
    df = as.data.frame(configuration)
    
    y = getPredictionResponse(predict(model, newdata = df))
    
    result = list(cost = -1*(y), call = toString(experiment))
    
    return(result)
  }
  
  scenario                <- defaultScenario()
  scenario$seed           <- 132348834
  scenario$targetRunner   <- "target.runner" 
  scenario$maxExperiments <- n
  scenario$instances      <- 1
  scenario$nbConfigurations <- 2
  scenario$minNbSurvival <- 1
  irace(scenario = scenario, parameters = parameters)
  
  load("irace.Rdata")
  n = length(ps[["pars"]])
  bestparams = iraceResults[["state"]][["eliteConfigurations"]][2:(n+1)]
  bestparams = as.data.frame(bestparams)
  besty = getPredictionResponse(predict(model, newdata = bestparams))
  besty = as.data.frame(besty)
  colnames(besty) = y.name
  cbind(besty, bestparams)
}
