# not working at the moment you can skip it


loadResult(1)


reduce = function(res) {
  optPath = as.data.frame(res$opt.path)
}

results = reduceResultsDataTable(fun = reduce)
params = getJobPars()
x = ijoin(params, results, by = c("job.id" = "job.id"))
x$prob.pars <- NULL
apply(x, 1, ...)


# x[[4]][[1]][["iters"]]
# x[[4]][[1]][["crit"]][["id"]]



params = getJobPars()
params$prob.pars = NULL
as.factor(params$algorithm)



##########################################
getMBOInfillCritParams(x) # get params of the infill crit

getMBOInfillCritParam(x, par.name)

getMBOInfillCritName(x)

getMBOInfillCritId(x)

hasRequiresInfillCritStandardError(x)

getMBOInfillCritComponents(x)

##########################################




params = getJobPars()


pars = unwrap(getJobPars())
tab = ijoin(pars, results)


