computeConfigs = function(probDesign, paramDesign, repls) {
  # create registry
  # @param file.dir - name of the directory
  # @param packages - r packages which are needed for the computation
  reg = makeExperimentRegistry(file.dir = NA, make.default = TRUE,
                               seed = 1, packages = c("mlr","mlrMBO","smoof","snow","lhs","data.table","ggplot2","dplyr"))
  
  
  # define the objective function and the initial design for the mbo process
  # @param data - will be defined in addProblem() further down
  kaptonObj = function(model, data.name, ps, min, ...) {
    
    fun = function(x) {
      df = as.data.frame(x)
      return(getPredictionResponse(predict(model, newdata = df)))
    }
    
    objfun = makeSingleObjectiveFunction(
      name = data.name,
      fn = fun,
      par.set = ps,
      has.simple.signature = FALSE,
      minimize = min)
    
    return(objfun)
  }
  
  
  # @param name - name of the problem
  # @param data - fixed learner for the objective function
  # @param fun - objective function function
  # you can add more objective functions or data sets for the benchmark by adding more addProblem() functions
  addProblem(name = "data",
             fun = kaptonObj, reg = reg)
  
  
  # set the control parameters for the optimization
  # @param instance - provides the objective function
  # @param iters - will be defined in the algorithm design further down
  # @param crit - will be defined in the algoritm desgin further down
  
  # the infill criterias benchmark
  mbofunCrits = function(instance, iters, crit, funDesign, surrogate, amount, y.name, ...) {
    
    ctrl = makeMBOControl(y.name = y.name)
    
    ctrl = setMBOControlInfill(ctrl, crit)
    
    ctrl = setMBOControlTermination(ctrl, iters)
    
    design = generateDesign(n = amount, par.set = getParamSet(instance), fun = funDesign)
    
    res = mbo(fun = instance, design = design, learner = surrogate, control = ctrl, show.info = TRUE)
    
    optimizationPath = as.data.frame(res$opt.path)
  }
  
  
  addAlgorithm(name = "param", fun = mbofunCrits, reg = reg)
  
  # add Experiments to registry
  addExperiments(prob.designs = probDesign, algo.designs = paramDesign, repls = repls, reg = reg)
  
  # use multiple CPU cores for the computation - default: use all cores
  reg$cluster.functions = makeClusterFunctionsSocket(fs.latency = 65)
  
  # submit jobs
  submitJobs()
  
  # wait until computation is finished (ie TRUE in console)
  waitForJobs()
}