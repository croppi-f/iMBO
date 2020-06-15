################# Data and Configurations for the tests ##############################
# source the data 
source("tests/test_data/md.R")

library(mlrMBO)
model = train(learner = makeLearner("regr.randomForest"), 
              task = makeRegrTask(data = data, target = "target")
)

# Bayesian Optimization
# getPredictionResponse are the evaliation of the (new) proposed points
fun = function(x) {
  df = as.data.frame(x)
  return(getPredictionResponse(predict(model, newdata = df)))
}

ps = makeParamSet(
  makeNumericParam("f", lower = 0, upper = 0.25),
  makeNumericParam("k", lower = 0, upper = 0.1),
  makeNumericParam("du", lower = 0, upper = 2e-5),
  makeNumericParam("dv", lower = 0, upper = 2e-5),
  makeIntegerParam("x", lower = 0, upper = 200),
  makeIntegerParam("y", lower = 0, upper = 200)
)

objfun = makeSingleObjectiveFunction(
  name = "Synthesis",
  fn = fun,
  par.set = ps,
  has.simple.signature = FALSE,
  minimize = TRUE
)

# sample 10 points for the initial surrogate model
# initial.data = data[sample(1:nrow(data), 10), ]
set.seed(1)
initial.data = generateDesign(50, attributes(objfun)$par.set)


######################### TEST ################################################
library(testthat)

test_that("inflInst results data frames have the correct nrow, ncol, colnames, rownames", {
  ctrl = makeMBOControl(y.name = "target", store.model.at = 1:6)
  ctrl = setMBOControlTermination(ctrl, iters = 5)
  ctrl = setMBOControlInfill(ctrl,
  opt = "focussearch",
  opt.focussearch.maxit = 20,
  opt.focussearch.points = 50,
  crit = makeMBOInfillCritEI()
  )
  
  set.seed(1)
  res = mbo(objfun,
             design = initial.data,
             control = ctrl,
             show.info = FALSE
  )
  pars = names(res$x)
  components = res$control$infill.crit$components
  infill = res$control$infill.crit$id
  
  ii = inflInst(res.mbo = res, iter = 4, influence = "linear", interest = "surrogate", 
                parallel = TRUE, seed = 1, maxdepth = 2, plot = FALSE)
  
  choice = ii$choice.of.af
  cnchoice = c(pars, infill, components, "dist.pp.true")
  rnchoice = c(as.character(1:(nrow(initial.data) + 4 - 1)), "true")
  
  expect_data_frame(choice,
                    any.missing = FALSE, all.missing = FALSE,
                    nrows = nrow(initial.data) + 4 - 1 + 1, # iter = 4, - 1 exclude pp iter 4, +1 true pp
                    ncols = length(cnchoice),
                    col.names = "unique",
                    row.names = "unique"
  )
  expect_names(rownames(choice), type = "unique", identical.to = rnchoice)
  expect_names(colnames(choice), type = "unique", identical.to = cnchoice)
  
  infl = ii$influence.analysis$data.frame
  cninfl = c("infl", pars, "dist.pp.true")
  rninfl = as.character(1:(nrow(initial.data) + 4 - 1))
  
  expect_data_frame(infl,
    any.missing = FALSE, all.missing = FALSE,
    nrows = nrow(initial.data) + 4 - 1, # iter = 4, - 1 exclude pp iter 4
    ncols = length(cninfl),
    col.names = "unique",
    row.names = "unique"
  )
  expect_names(rownames(infl), type = "unique", identical.to = rninfl)
  expect_names(colnames(infl), type = "unique", identical.to = cninfl)

})

test_that("df in influence analysis (params col) equal to design df (in iter i) in the exact same order", {
  ctrl = makeMBOControl(y.name = "target", store.model.at = 1:6)
  ctrl = setMBOControlTermination(ctrl, iters = 5)
  ctrl = setMBOControlInfill(ctrl,
                             opt = "focussearch",
                             opt.focussearch.maxit = 20,
                             opt.focussearch.points = 50,
                             crit = makeMBOInfillCritEI()
  )
  
  set.seed(1)
  res = mbo(objfun,
            design = initial.data,
            control = ctrl,
            show.info = FALSE
  )
  pars = names(res$x)
  opdf = as.data.frame(res$opt.path)
  opdf = opdf[which(opdf$dob < 4), pars]
  
  ii = inflInst(res.mbo = res, iter = 4, influence = "linear", interest = "surrogate", 
                parallel = TRUE, seed = 1, maxdepth = 2, plot = FALSE)
  infl = ii$influence.analysis$data.frame

  expect_equal(opdf, infl[,pars])
  
})

test_that("inflInst error if interest = surrogate, infill = se & model in iter not stored", {
  ctrl = makeMBOControl(y.name = "target", store.model.at = 1)
  ctrl = setMBOControlTermination(ctrl, iters = 2)
  ctrl = setMBOControlInfill(ctrl,
                             opt = "focussearch",
                             opt.focussearch.maxit = 20,
                             opt.focussearch.points = 50,
                             crit = makeMBOInfillCritStandardError()
  )
  
  set.seed(1)
  res = mbo(objfun,
            design = initial.data,
            control = ctrl,
            show.info = FALSE
  )
  
  expect_error(inflInst(res.mbo = res, iter = 2, interest = "surrogate"))
  
})

test_that("pp in choice.of.af has name = true, correct columns & dist = 0", {
  ctrl = makeMBOControl(y.name = "target", store.model.at = 1:2)
  ctrl = setMBOControlTermination(ctrl, iters = 2)
  ctrl = setMBOControlInfill(ctrl,
                             opt = "focussearch",
                             opt.focussearch.maxit = 20,
                             opt.focussearch.points = 50,
                             crit = makeMBOInfillCritEI()
  )
  
  set.seed(1)
  res = mbo(objfun,
            design = initial.data,
            control = ctrl,
            show.info = FALSE
  )
  opdf = as.data.frame(res$opt.path)
  pars = names(res$x)
  components = res$control$infill.crit$components
  infill = res$control$infill.crit$id
  pp = opdf[which(opdf$dob == 1), c(pars, infill, components)]
  pp$dist.pp.true = 0
  rownames(pp) = "true"
  
  ii = inflInst(res.mbo = res, iter = 1, influence = "linear", interest = "acquisition", 
                parallel = TRUE, seed = 1, plot = FALSE)
  
  choice = ii$choice.of.af
  true.choice = choice["true", ]
  
  expect_equal(pp, true.choice)
  
})

test_that("inflInst gives same result for parallel = TRUE & parallel = FALSE", {
  ctrl = makeMBOControl(y.name = "target", store.model.at = 1:6)
  ctrl = setMBOControlTermination(ctrl, iters = 5)
  ctrl = setMBOControlInfill(ctrl,
                             opt = "focussearch",
                             opt.focussearch.maxit = 20,
                             opt.focussearch.points = 50,
                             crit = makeMBOInfillCritEI()
  )
  
  set.seed(1)
  res = mbo(objfun,
            design = initial.data,
            control = ctrl,
            show.info = FALSE
  )
  
  ii = inflInst(res.mbo = res, iter = 4, influence = "linear", interest = "surrogate", 
                parallel = FALSE, seed = 1, plot = FALSE)
  
  choice = ii$choice.of.af
  infl = ii$influence.analysis$data.frame
  
  ii.p = inflInst(res.mbo = res, iter = 4, influence = "linear", interest = "surrogate", 
                parallel = TRUE, seed = 1, plot = FALSE)
  
  choice.p = ii.p$choice.of.af
  infl.p = ii.p$influence.analysis$data.frame
  
  expect_equal(choice, choice.p)
  expect_equal(infl, infl.p)
})

test_that("getParam extracts Params correctly", {
  # Test1
  obj.fun = makeBraninFunction()
  p = getParam(getParamSet(obj.fun))
  
  expect_names(p, type = "unique", identical.to = c("x1", "x2"))
  
  # Test2 Materials Design
  p2 = getParam(ps)
  
  expect_names(p2, type = "unique", identical.to = c("f", "k", "du", "dv", "x", "y"))
  
  # Test3 Kapton
  parset = makeParamSet(
    makeIntegerParam("power", lower = 10, upper = 5555),
    makeIntegerParam("time", lower = 500, upper = 20210),
    makeDiscreteParam("gas", values = c("Nitrogen", "Air", "Argon")),
    makeIntegerParam("pressure", lower = 0, upper = 1000)
  )
  p3 = getParam(parset)
  
  expect_names(p3, type = "unique", identical.to = c("power", "time", "gas", "pressure"))
  
  # Test4 Sin(x)
  obj.fun = makeSingleObjectiveFunction(
    name = "Sine",
    fn = function(x) sin(x),
    par.set = makeNumericParamSet(lower = 3, upper = 13, len = 1),
    global.opt.value = -1
  )
  p4 = getParam(getParamSet(obj.fun))
  
  expect_names(p4, type = "unique", identical.to = "x")
  
})

