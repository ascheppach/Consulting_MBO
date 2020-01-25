# control exploration EI
# makeMBOInfillCrit(fun, name, id, opt.direction = "minimize",
# components = character(0L), params = list(), requires.se = FALSE)
# fun = makeMBOInfillCritEIcontrolExploration
# default = 0.01 see: http://krasserm.github.io/2018/03/21/bayesian-optimization/


makeMBOInfillCritEIcontrolExploration = function(se.threshold = 1e-6, controlExploration = 0.01) {
  assertNumber(se.threshold, lower = 1e-20)
  force(se.threshold)
  makeMBOInfillCrit(
    fun = function(points, models, control, par.set, designs, iter, progress, attributes = FALSE) {
      model = models[[1L]] # surrogate
      design = designs[[1]] # evaluated points
      maximize.mult = if (control$minimize) 1 else -1 # min oder max setup
      assertString(control$y.name) # assertions
      y = maximize.mult * design[, control$y.name] # y wert der desgin punkte
      assertNumeric(y, any.missing = FALSE) # assertions
      p = predict(model, newdata = points)$data # predicted mu_hat und sigma_hat by the surrogate model on the n points
      p.mu = maximize.mult * p$response # predicted mu of the surrogate
      p.se = p$se # predicted se of the surrogate
      y.min = min(y) # aktuelle minimum der bisherigen evaluierten punkte bestimmen
      d = y.min - p.mu - (controlExploration) # gap between actual y_min and the predicted mean of the new point, exploration parameter shrinks the gap -- geklaut: https://github.com/yanyachen/rBayesianOptimization/blob/ff949bc0a1c7dd3a8b79e704f74775c99638d56d/R/Utility.R#L27
      xcr = (d / p.se) #
      xcr.prob = pnorm(xcr) 
      xcr.dens = dnorm(xcr)
      ei = d * xcr.prob + p.se * xcr.dens # ei berechnen
      res = ifelse(p.se < se.threshold, 0, -ei)
      #if (attributes) {
      #  res = setAttribute(res, "crit.components", data.frame(se = p$se, mean = p$response,
      #                                                        controlExploration = controlExploration))
      #}
      return(res)
    },
    name = "Expected improvement with exploration parameter",
    id = "eicp",
    components = c("se", "mean", "controlExploration"),
    params = list(se.threshold = se.threshold, controlExploration = controlExploration),
    opt.direction = "maximize",
    requires.se = TRUE
  )
}
