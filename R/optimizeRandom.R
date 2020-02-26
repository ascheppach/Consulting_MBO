optimizeRandom = function(model, ps, n, minimize = FALSE) {
  experiments = sampleValues(ps, n, trafo = FALSE)
  minimize = minimize
  tuneRandom = function(experiments) {
    df = as.data.frame(experiments)
    response = getPredictionResponse(predict(model, newdata = df))
    return(response)
  }
  
  tuneResult = lapply(experiments, tuneRandom)
  tuneResult = cbind(as.data.frame(do.call(rbind,tuneResult)), as.data.frame(do.call(rbind,experiments)))
  
  colnames(tuneResult)[1] = model[["task.desc"]][["target"]]
  
  if (minimize == FALSE) {
    return(tuneResult[which.max(tuneResult[,1]),]) 
  }
  if (minimize == TRUE) {
    return(tuneResult[which.min(tuneResult[,1]),]) 
  }
}