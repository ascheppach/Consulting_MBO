reduceDifferentConfigs = function(optimizationPath) {
  # compute the best y found so far. iteration 0 = initial data
  # add a column to the data frame optimization path to save the best y
  optimizationPath["best_y"] <- NA
  # compute the best y over the initial data
  # optimizationPath$best_y[nrow(initial.data)] <- max(optimizationPath$ratio[1:nrow(initial.data)])
  n <- length(optimizationPath$best_y)
  for (i in 1:n) {
    optimizationPath$best_y[i] <- max(optimizationPath$target[i], optimizationPath$best_y[i - 1])
  }
  
  index1 = sum(optimizationPath$dob >= 0) 
  index2 = sum(optimizationPath$dob == 0)
  
  y = optimizationPath$best_y[index2:index1]
  x = optimizationPath$dob[index2:index1]
  return(cbind(data.frame(y),data.frame(x)))
}