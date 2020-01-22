


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

n = nrow(params)
for (i in 1:n) {
    params[[4]][[i]] = params[[4]][[i]][[2]]

}


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


#### Boxplots zu Benchmarks v
z <- length(list_lhs_frame)

for(z in 1:4) {
  colnames(list_lhs_frame[[z]]) <- c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50")
  list_lhs_frame[[z]] <- list_lhs_frame[[z]] %>% 
    gather(`0`,`1`, `2`, `3`,`4`,`5`,`6`, `7`,`8`,`9`,`10`,`11`,`12`,`13`,`14`,`15`,`16`,`17`,`18`,`19`,`20`,`21`,`22`,`23`,`24`,`25`,`26`,`27`,`28`,`29`,`30`,`31`,`32`,`33`,`34`,`35`,`36`,`37`,`38`,`39`,`40`,`41`,`42`,`43`,`44`,`45`,`46`,`47`,`48`,`49`,`50`, key = "Iteration", value = "Ratio")
  list_lhs_frame[[z]]$Iteration <- ordered(list_lhs_frame[[z]]$Iteration, levels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50"))
}

list_lhs_frame[[1]] <- cbind(list_lhs_frame[[1]], array("maxminlhs"))
list_lhs_frame[[2]] <- cbind(list_lhs_frame[[2]], array("random"))
list_lhs_frame[[3]] <- cbind(list_lhs_frame[[3]], array("randomlhs"))
list_lhs_frame[[4]] <- cbind(list_lhs_frame[[4]], array("genlhs"))

for(z in 1:4) {
  colnames(list_lhs_frame[[z]]) <- c("Iteration", "Ratio","Class")
}

lhs <- rbind(list_lhs_frame[[1]],list_lhs_frame[[2]],list_lhs_frame[[3]],list_lhs_frame[[4]])
lhs$Class <- as.factor(lhs$Class)

ggplot(amountbevorggplot, aes(x = Iteration, y = Ratio, fill = Class)) + geom_boxplot() 
