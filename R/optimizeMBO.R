source(file = "R/createProbDesign.R")
source(file = "R/createParamDesign.R")
source(file = "R/computeConfigs.R")
source(file = "R/reduceOptimizeMBO.R")

optimizeMBO = function(objectiveFunction, parameterSet, n = 50, 
                           param, minimize = FALSE, repls = 1) {
  n = length(objectiveFunction[["features"]])
  y.name = objectiveFunction[["task.desc"]][["target"]]
  probDesign = createProbDesign(objectiveFunction, problemName = "x", parameterSet = parameterSet, minimize = minimize)
  paramDesign = createParamDesign(param, y.name)
  computeConfigs(probDesign, paramDesign, repls)
  results = reduceResultsDataTable(fun = reduceOptimizeMBO)
}