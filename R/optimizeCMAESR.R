optimizeCMAES = function(model, ps, n = 50, sigma = 1000, lambda = 400, minimize = FALSE) {
  
  names = model[["features"]]
  
  nn = length(names)
  
  integer = NA
  
  for (i in 1:nn) {
    if (ps$pars[[i]]$type == "numeric") {
      integer[i] = FALSE
    }
    if (ps$pars[[i]]$type == "integer") {
      integer[i] = TRUE
      ps$pars[[i]]$type = "numeric"
    }
  }
  
  if (minimize == FALSE) {
    p = (-1)
  }
  if (minimize == TRUE) {
    p = (1)
  }
  
  
  fun = function(x) {
    df = t(x)
    
    for (i in 1:nn) {
      if (integer[i] == TRUE) {
        df[i] = round(df[i])
      }
    }
    df = as.data.frame(df)
    
    colnames(df) = names[1:nn]
    
    return(getPredictionResponse(predict(model, newdata = df))*p)
  }
  
  
  objfun = makeSingleObjectiveFunction(
    name = "default",
    fn = fun,
    par.set = ps,
    has.simple.signature = FALSE,
    minimize = FALSE
  )
  
  iters = list(stopOnMaxIters(n))
  
  res = cmaes(
    objfun, 
    monitor = NULL,
    control = list(
      sigma = sigma, # initial step size
      lambda = lambda, # number of offspring
      stop.ons = iters
    )
  )
  
  y = as.data.frame(res[["best.fitness"]])
  x = as.data.frame(t(res[["best.param"]]))
  
  for (i in 1:nn) {
    if (integer[i] == TRUE) {
      x[i] = round(x[i])
    }
  }
  
  y = y * p
  colnames(y) = model[["task.desc"]][["target"]]
  colnames(x) = names[1:nn]
  result = cbind(y,x)
  return(result)
}