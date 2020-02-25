source("R/optimizeRandom.R")
source("R/optimizeRacing.R")
source("R/optimizeCMAESR.R")
source("R/optimizeMBO.R")


benchmarkOptimizations = function(model, ps, n, repls, paramsMBO, minimize) {
  x = 1:repls
  random = lapply(x, optimizeRandom, model = model, ps = ps,  minimize = minimize) ###### add all Hyperparameters !!!!
  cmaes = lapply(x, optimizeCMAES, model = model, ps = ps, n = n)    ######addd minimize to the 3 following functions !!!
  racing = lapply(x, optimizeRacing, model = model, ps = ps, n = n)                                 ####### automatise "ratio" in all of the optimizations!!
  mbo = optimizeMBO(objectiveFunction = model, parameterSet = ps, n = n, param = paramsMBO, repls = repls)

  
  cmaes1 = NA
  for (i in 1:repls) {
    cmaes1[i] = cmaes[[i]]$ratio
  }
  cmaes1 = as.data.frame(cmaes1)
  cmaes1[,2] = "cmaes"
  colnames(cmaes1) = c("ratio","method")
  
  random1 = NA
  for (i in 1:repls) {
    random1[i] = random[[i]]$ratio
  }
  random1 = as.data.frame(random1)
  random1[,2] = "random"
  colnames(random1) = c("ratio","method")
  
  racing1 = NA
  for (i in 1:repls) {
    racing1[i] = racing[[i]]$ratio
  }
  racing1 = as.data.frame(racing1)
  racing1[,2] = "racing"
  colnames(racing1) = c("ratio","method")
  
  mbo = mbo$result
  mbo1 = NA
  for (i in 1:repls) {
    mbo1[i] = as.vector(mbo[[i]])
  }
  mbo1 = as.data.frame(mbo1)
  mbo1[,2] = "mbo"
  colnames(mbo1) = c("ratio","method")
  
  y = as.data.frame(rbind(mbo1,cmaes1,racing1,random1))
  
  ggplot(y, aes(factor(method), ratio)) + geom_boxplot()
}