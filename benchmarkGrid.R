library("batchtools")

# to use this framework make sure you have the following files (we recommend to clone our repro)
# KmArgonNugget.rds - to save some time, we fixed a learner (krigging ("km.regr") defaults + nugget.estim = TRUE)
# as the krigging model interpolates we know that the maximum ratio is equal 5.499063
# initialDesign.rds - fixed initial design which will be used for surrogate/infill criteria benchmark
# eiParam.R - expected improvement with exploration parameter
# eiParamAda.R - expected improvement with adaptive exploration parameter
# reduceResultsBoxplot.R - reduce results function
# plot.R - boxplot function
source("fixed/eiParam.R")
source("fixed/eiParamAda.R")



reg = makeRegistry(file.dir = "try", seed = 1)
batchMap(fun = grid, n = rep(2,10))

# summarize jobs 
summarizeExperiments(by = c("problem","algorithm"))

# use multiple CPU cores for the computation - default: use all cores
reg$cluster.functions = makeClusterFunctionsSocket()

# submit the jobs
submitJobs()

# check if the computation is finished or not
waitForJobs()

# check if the computation caused any error
getStatus()

# initialize function reduce() - not working at the moment
source("reduceResultsBoxplot.R")
# store the results - be careful it might be a large object!
results = reduceResultsDataTable(fun = reduce)

# initialize function plotBoxplots() - not working at the moment
# have a look to see the boxplot computet for a amount of initial design benchmark
# plotAmountInitialDesign.jpeg
source("plotBoxplots.R")
plotBoxplots(results)


# additional stuff - you will need it sometimes
########
######## test some jobs before submitting
testJob(id = 175)
########

########
######## find jobs which caused an error and submit them again
failed <- findErrors()
submitJobs(failed)
########

########
######## load registry if you started a clean session
loadRegistry(file.dir = "differentModelConfigurations",work.dir = NULL,
             conf.file = findConfFile(), make.default = TRUE, writeable = FALSE)
########