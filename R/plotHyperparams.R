source(file = "R/createProbDesign.R")
source(file = "R/createParamDesign.R")
source(file = "R/computeConfigs.R")
source(file = "R/reduceDifferentConfigs.R")
source(file = "R/plotDifferentConfigs.R")

plotHyperparams = function(objectiveFunction, problemName, parameterSet, minimize,
                           param, repls, names) {
  y.name = objectiveFunction[["task.desc"]][["target"]]
  iters = param$iters[1]
  probDesign = createProbDesign(objectiveFunction, problemName, parameterSet, minimize)
  paramDesign = createParamDesign(param, y.name)
  computeConfigs(probDesign, paramDesign, repls)
  results = reduceResultsDataTable(fun = reduceDifferentConfigs)
  plotDifferentConfigs(results, repls, names, iters, y.name, problemName)
}