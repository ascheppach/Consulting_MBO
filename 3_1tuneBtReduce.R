### reduce best y tuneRandom
tuneBtReduce = function() {
  reduce = function(res) {
    y = as.data.frame(res$y)
  }

  results = reduceResultsDataTable(fun = reduce)

  for (i in 1:495) {
    results[i,2] <- as.numeric(results[[2]][[i]])
  }
 
  params = getJobPars()
  params$prob.pars = NULL
  paramss = matrix(NA, 500, 12)
  paramss = as.data.frame(paramss)

  for (i in 1:500) {
    paramss[i,1] = as.integer(params$job.id[i])
    paramss[i,2] = params[[4]][[i]]$iters
    paramss[i,3] = params[[4]][[i]]$surrogate
    paramss[i,4] = params[[4]][[i]]$kernel
    paramss[i,5] = params[[4]][[i]]$nodesize
    paramss[i,6] = params[[4]][[i]]$mtry
    paramss[i,7] = params[[4]][[i]]$infillCrit
    paramss[i,8] = params[[4]][[i]]$controlExploration
    paramss[i,9] = params[[4]][[i]]$startControlExploration
    paramss[i,10] = params[[4]][[i]]$endControlExploration
    paramss[i,11] = params[[4]][[i]]$amountInitialDesign
    paramss[i,12] = params[[4]][[i]]$initialDesign
  }

  x = ijoin(paramss, results, by = c("V1" = "job.id"))

  x$result = as.numeric(x$result)

  colnames(x) <- c("job.id","iters","surrogate","kernel","nodesize","mtry","infillCrit",
                   "controlExploration","startControlExploration","endControlExploration",
                   "amountInitialDesign","initialDesign","best ratio")
  return(x)
}
