createProbDesign = function(objectiveFunction, problemName, parameterSet, minimize) {
  probDesign = list(
    data = data.table(
      
      model = list(objectiveFunction),
      
      data.name = problemName,
      
      ps = list(parameterSet),
      
      min = minimize
    )
  )
}