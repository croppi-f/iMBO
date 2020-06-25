################# Data and Configurations for the tests ##############################
# source the data
source("tests/test_data/kapton.R")

library(mlrMBO)
set.seed(1)
model = train(makeLearner("regr.randomForest"), makeRegrTask(data = data, target = "ratio"))

# Bayesian Optimization

fun = function(x) {
  df = as.data.frame(x)
  df$gas = factor(df$gas, levels = levels(data$gas))
  return(getPredictionResponse(predict(model, newdata = df)))
}

ps = makeParamSet(
  makeIntegerParam("power", lower = 10, upper = 5555),
  makeIntegerParam("time", lower = 500, upper = 20210),
  makeDiscreteParam("gas", values = c("Nitrogen", "Air", "Argon")),
  makeIntegerParam("pressure", lower = 0, upper = 1000)
)

objfun = makeSingleObjectiveFunction(
  name = "Kapton",
  fn = fun,
  par.set = ps,
  has.simple.signature = FALSE,
  minimize = FALSE
)

# sample 9 points for the initial surrogate model, stratified across gases
set.seed(1)
samples.argon = sample(rownames(data[data$gas == "Argon", ]), 3)
samples.nitro = sample(rownames(data[data$gas == "Nitrogen", ]), 3)
samples.air = sample(rownames(data[data$gas == "Air", ]), 3)
initial.data = data[c(samples.argon, samples.nitro, samples.air), ]


ctrl = makeMBOControl(y.name = "ratio")
ctrl = setMBOControlTermination(ctrl, iters = 3)

#source modified function from mlrMBO
source("R/utils_xplxpl/proposePointsByInfillOptimization.R")
source("R/utils_xplxpl/makeMBOResult.OptState.R")
source("R/utils_xplxpl/getSupportedInfillOptFunctions.R")
source("R/utils_xplxpl/proposePointsByInfillOptimization.R")
source("R/utils_xplxpl/getInfillOptFunction.R")
source("R/utils_xplxpl/checkStuff.R")

# source new infill optimization functions "...Savepts"
source("R/utils_xplxpl/infillOptFocusSavepts.R")
source("R/utils_xplxpl/infillOptEASavepts.R")
source("R/utils_xplxpl/infillOptCMAESSavepts.R")

######################### TEST ################################################
library(testthat)


test_that("data corresponds to train set if interest = surrogate and to seen points if interest = acquisition", {
  ctrl = makeMBOControl(y.name = "ratio", store.model.at = 1:3)
  ctrl = setMBOControlTermination(ctrl, iters = 3)
  ctrl = setMBOControlInfill(ctrl,
                             opt = "focussearchSavepts",
                             opt.focussearch.maxit = 20,
                             opt.focussearch.points = 5,
                             crit = makeMBOInfillCritEI()
  )
  set.seed(1)
  res = mbo(objfun,
            design = initial.data,
            control = ctrl,
            show.info = FALSE
  )
  opdf = as.data.frame(res$opt.path)
  acq.fun = res$control$infill.crit$fun
  eff.s = FeatureEffectMBO(res.mbo = res, interest = "surrogate", feature = "time")
  
  # iter 2
  opdf.2 = opdf[1:10, 1:5]
  data = eff.s$`2`$predictor$data$get.xy()
  expect_equal(data, data.table::data.table(opdf.2))
  
  eff.af = FeatureEffectMBO(res.mbo = res, interest = "acquisition", feature = "time")
  # iter 2
  data.af = eff.af$`2`$predictor$data$get.xy()
  seen = res$seen.points[[2]]
  ei = acq.fun(
    points = seen,
    models = list(res$models[[2]]),
    control = res$control,
    par.set = res$opt.path$par.set,
    designs = list(opdf[1:10, 1:5]),
    progress = NULL,
    iter = NULL,
    attributes = FALSE
  )
  s = cbind(seen, ei)
  
  expect_equal(data.af, data.table::data.table(s))
  
})

test_that("FeatureEffectMBO result has the correct length", {
  
  # s.m.a default (iters + 1), iters = 3
  ctrl1 = makeMBOControl(y.name = "ratio")
  ctrl1 = setMBOControlTermination(ctrl1, iters = 3)
  ctrl1 = setMBOControlInfill(ctrl1,
                                opt = "focussearchSavepts",
                                opt.focussearch.maxit = 20,
                                opt.focussearch.points = 5,
                                crit = makeMBOInfillCritEI()
  )
  set.seed(1)
  res1 = mbo(objfun,
                   design = initial.data,
                   control = ctrl1,
                   show.info = FALSE
  )
  
  eff = FeatureEffectMBO(res.mbo = res1, interest = "surrogate", feature = "gas")
  expect_list(eff, len = 1, null.ok = FALSE, any.missing = FALSE, all.missing = FALSE)
  expect_named(eff, c("4"))
  
  expect_error(FeatureEffectMBO(res.mbo = res1, interest = "acquisition", feature = "gas"))
  
  # s.m.a alle sm, iters = 3 (also 4)
  ctrl2 = makeMBOControl(y.name = "ratio", store.model.at = 1:4)
  ctrl2 = setMBOControlTermination(ctrl2, iters = 3)
  ctrl2 = setMBOControlInfill(ctrl2,
                              opt = "focussearchSavepts",
                              opt.focussearch.maxit = 20,
                              opt.focussearch.points = 5,
                              crit = makeMBOInfillCritEI()
  )
  set.seed(1)
  res2 = mbo(objfun,
             design = initial.data,
             control = ctrl2,
             show.info = FALSE
  )
  eff = FeatureEffectMBO(res.mbo = res2, interest = "surrogate", feature = "gas")
  expect_list(eff, len = 4, null.ok = FALSE, any.missing = FALSE, all.missing = FALSE)
  expect_named(eff, as.character(1:4))
  
  eff.af = FeatureEffectMBO(res.mbo = res2, interest = "acquisition", feature = "gas")
  expect_list(eff.af, len = 3, null.ok = FALSE, any.missing = FALSE, all.missing = FALSE)
  expect_named(eff.af, as.character(1:3))
  
  # s.m.a 1:10L (stored every model but is bigger than iters), iters = 3 (also 4)
  ctrl3 = makeMBOControl(y.name = "ratio", store.model.at = 1:10L)
  ctrl3 = setMBOControlTermination(ctrl3, iters = 3)
  ctrl3 = setMBOControlInfill(ctrl3,
                              opt = "focussearchSavepts",
                              opt.focussearch.maxit = 20,
                              opt.focussearch.points = 5,
                              crit = makeMBOInfillCritEI()
  )
  set.seed(1)
  res3 = mbo(objfun,
             design = initial.data,
             control = ctrl3,
             show.info = FALSE
  )
  eff = FeatureEffectMBO(res.mbo = res3, interest = "surrogate", feature = "gas")
  expect_list(eff, len = 4, null.ok = FALSE, any.missing = FALSE, all.missing = FALSE)
  expect_named(eff, as.character(1:4))
  
  eff.af = FeatureEffectMBO(res.mbo = res3, interest = "acquisition", feature = "gas")
  expect_list(eff.af, len = 3, null.ok = FALSE, any.missing = FALSE, all.missing = FALSE)
  expect_named(eff.af, as.character(1:3))
  
  # s.m.a only for selected models, iters = 3
  ctrl4 = makeMBOControl(y.name = "ratio", store.model.at = 2)
  ctrl4 = setMBOControlTermination(ctrl4, iters = 3)
  ctrl4 = setMBOControlInfill(ctrl4,
                              opt = "focussearchSavepts",
                              opt.focussearch.maxit = 20,
                              opt.focussearch.points = 5,
                              crit = makeMBOInfillCritEI()
  )
  set.seed(1)
  res4 = mbo(objfun,
             design = initial.data,
             control = ctrl4,
             show.info = FALSE
  )
  
  eff = FeatureEffectMBO(res.mbo = res4, interest = "surrogate", feature = "gas")
  expect_list(eff, len = 1, null.ok = FALSE, any.missing = FALSE, all.missing = FALSE)
  expect_named(eff, "2")
  
  eff.af = FeatureEffectMBO(res.mbo = res4, interest = "acquisition", feature = "gas")
  expect_list(eff.af, len = 1, null.ok = FALSE, any.missing = FALSE, all.missing = FALSE)
  expect_named(eff.af, "2")
  
  # s.m.a 1:3, iters = 3
  ctrl5 = makeMBOControl(y.name = "ratio", store.model.at = 1:3)
  ctrl5 = setMBOControlTermination(ctrl5, iters = 3)
  ctrl5 = setMBOControlInfill(ctrl5,
                              opt = "focussearchSavepts",
                              opt.focussearch.maxit = 20,
                              opt.focussearch.points = 5,
                              crit = makeMBOInfillCritEI()
  )
  set.seed(1)
  res5 = mbo(objfun,
             design = initial.data,
             control = ctrl5,
             show.info = FALSE
  )
  
  eff = FeatureEffectMBO(res.mbo = res5, interest = "surrogate", feature = "gas")
  expect_list(eff, len = 3, null.ok = FALSE, any.missing = FALSE, all.missing = FALSE)
  expect_named(eff, as.character(1:3))
  
  eff.af = FeatureEffectMBO(res.mbo = res5, interest = "acquisition", feature = "gas")
  expect_list(eff.af, len = 3, null.ok = FALSE, any.missing = FALSE, all.missing = FALSE)
  expect_named(eff.af, as.character(1:3))
  
})

test_that("FeatureEffect error if acquisition not applicable for given stored model",{
  ctrl1 = makeMBOControl(y.name = "ratio")
  ctrl1 = setMBOControlTermination(ctrl1, iters = 3)
  ctrl1 = setMBOControlInfill(ctrl1,
                              opt = "focussearchSavepts",
                              opt.focussearch.maxit = 20,
                              opt.focussearch.points = 5,
                              crit = makeMBOInfillCritEI()
  )
  set.seed(1)
  res1 = mbo(objfun,
             design = initial.data,
             control = ctrl1,
             show.info = FALSE
  )
  
  expect_error(FeatureEffectMBO(res.mbo = res1, interest = "acquisition", feature = "gas"))
  
})

test_that("FeatureEffectMBO error if seen points  not stored and interest = acquisition", {
  ctrl = makeMBOControl(y.name = "ratio")
  ctrl = setMBOControlTermination(ctrl, iters = 1)
  ctrl = setMBOControlInfill(ctrl,
                              opt = "focussearch",
                              opt.focussearch.maxit = 20,
                              opt.focussearch.points = 5,
                              crit = makeMBOInfillCritEI()
  )
  set.seed(1)
  res = mbo(objfun,
             design = initial.data,
             control = ctrl,
             show.info = FALSE
  )
  
  expect_error(FeatureEffectMBO(res.mbo = res, interest = "acquisition", feature = "gas"))
})

test_that("FeatureEffectMBO have same results for parallel TRUE and FALSE", {
  ctrl = makeMBOControl(y.name = "ratio", store.model.at = 1:3)
  ctrl = setMBOControlTermination(ctrl, iters = 3)
  ctrl = setMBOControlInfill(ctrl,
                             opt = "focussearchSavepts",
                             opt.focussearch.maxit = 20,
                             opt.focussearch.points = 5,
                             crit = makeMBOInfillCritEI()
  )
  set.seed(1)
  res = mbo(objfun,
            design = initial.data,
            control = ctrl,
            show.info = FALSE
  )
  # surrogate
  eff = FeatureEffectMBO(res.mbo = res, interest = "surrogate", feature = "time", parallel = FALSE)
  eff1 = eff[[1]]$results
  eff2 = eff[[2]]$results
  eff3 = eff[[3]]$results
  
  eff.par = FeatureEffectMBO(res.mbo = res, interest = "surrogate", feature = "time", parallel = TRUE)
  eff1.par = eff.par[[1]]$results
  eff2.par = eff.par[[2]]$results
  eff3.par = eff.par[[3]]$results
  
  expect_equal(eff1, eff1.par)
  expect_equal(eff2, eff2.par)
  expect_equal(eff3, eff3.par)
  
  # acuisition function
  eff = FeatureEffectMBO(res.mbo = res, interest = "acquisition", feature = "time", parallel = FALSE)
  eff1 = eff[[1]]$results
  eff2 = eff[[2]]$results
  eff3 = eff[[3]]$results
  
  eff.par = FeatureEffectMBO(res.mbo = res, interest = "acquisition", feature = "time", parallel = TRUE)
  eff1.par = eff.par[[1]]$results
  eff2.par = eff.par[[2]]$results
  eff3.par = eff.par[[3]]$results
  
  expect_equal(eff1, eff1.par)
  expect_equal(eff2, eff2.par)
  expect_equal(eff3, eff3.par)
})

test_that("getFeatureEffectMBO and getFeatureEffectAfMBO works correctly", {
  
  ctrl = makeMBOControl(y.name = "ratio", store.model.at = 1:3)
  ctrl = setMBOControlTermination(ctrl, iters = 3)
  ctrl = setMBOControlInfill(ctrl,
                              opt = "focussearchSavepts",
                              opt.focussearch.maxit = 20,
                              opt.focussearch.points = 5,
                              crit = makeMBOInfillCritEI()
  )
  set.seed(1)
  res = mbo(objfun,
             design = initial.data,
             control = ctrl,
             show.info = FALSE
  )
  opdf = as.data.frame(res$opt.path)
  
  #Test1 (surrogate model)
  #iter 1, surrogate, time
  eff = getFeatureEffectMBO(
    model.p = res$models$`1`, data.p = initial.data, y.p = "ratio",
    class.p = NULL, predict.fun.p = NULL, type.p = NULL, batch.size.p = 1000,
    feature.fe = "time", method.fe = "pdp", grid.size.fe = 20, center.at.fe = NULL
  )
  
  res.eff = eff$results
  
  #with the iml Package stepwise
  pred = iml::Predictor$new(model = res$models$`1`, data = initial.data, y = "ratio",
                            class = NULL, predict.function = NULL, type = NULL)
  eff.iml = iml::FeatureEffect$new(predictor = pred, feature = "time", method = "pdp", 
                                   grid.size = 20, center.at = NULL)
  res.eff.iml = eff.iml$results
  
  expect_equal(res.eff, res.eff.iml)
  expect_equal(eff$center.at, eff.iml$center.at)
  expect_equal(eff$feature.name, eff.iml$feature.name)
  expect_equal(eff$feature.type, eff.iml$feature.type)
  expect_equal(eff$grid.size, eff.iml$grid.size)
  expect_equal(eff$method, eff.iml$method)
  expect_equal(eff$n.features, eff.iml$n.features)
  
  #Test2 (Acuisition Function)
  #infos needed
  #iter 2, time & power
  pars = names(res$x)
  y.name = res$control$y.name
  infill = res$control$infill.crit$id
  acq.fun = res$control$infill.crit$fun
  af.seen = acq.fun(
    points = res$seen.points[[2]],
    models = list(res$models[[2]]),
    control = res$control,
    par.set = res$opt.path$par.set,
    designs = list(opdf[nrow(initial.data) + 1, c(pars, y.name)]),
    iter = 2,
    progress = NULL,
    attributes = FALSE
  )
  seen.points = cbind(res$seen.points[[2]], ei = af.seen)

  # here starts the comparison
  eff = getFeatureEffectAfMBO(
    model.p = res$models$`2`, data.p = seen.points, y.p = "ei",batch.size.p = 1000, 
    res.mbo.p = res, design.p = opdf[1:10, c(pars, y.name)], iter.p = 2, 
    feature.fe = c("time", "power"), method.fe = "pdp", grid.size.fe = 20,
    center.at.fe = NULL
  )
  
  res.eff = eff$results
  
  pred = PredictorAf$new(model = res$models$`2`, data = seen.points, y = "ei", 
                         batch.size = 1000, res.mbo = res, design = opdf[1:10, c(pars, y.name)], iter = 2
  )
  eff.iml = iml::FeatureEffect$new(predictor = pred, feature = c("time", "power"), method = "pdp", 
                                      grid.size = 20, center.at = NULL)
  res.eff.iml = eff.iml$results
  
  expect_equal(res.eff, res.eff.iml)
  expect_equal(eff$center.at, eff.iml$center.at)
  expect_equal(eff$feature.name, eff.iml$feature.name)
  expect_equal(eff$feature.type, eff.iml$feature.type)
  expect_equal(eff$grid.size, eff.iml$grid.size)
  expect_equal(eff$method, eff.iml$method)
  expect_equal(eff$n.features, eff.iml$n.features)
})

test_that("FeatureEffectMBO computes ICE for acquisition correctly", {
  # Test 1
  # Expected Improvement
  ctrl = makeMBOControl(y.name = "ratio", store.model.at = 1:3)
  ctrl = setMBOControlTermination(ctrl, iters = 3)
  ctrl = setMBOControlInfill(ctrl,
                             opt = "focussearchSavepts",
                             opt.focussearch.maxit = 20,
                             opt.focussearch.points = 5,
                             crit = makeMBOInfillCritEI()
  )
  set.seed(1)
  res = mbo(objfun,
            design = initial.data,
            control = ctrl,
            show.info = FALSE
  )
  # extract the acq.fun
  acq.fun = res$control$infill.crit$fun
  opdf = as.data.frame(res$opt.path)
  # for simplicity we store only the first seen point
  sp1 = res$seen.points[[1]][1,]
  sp2 = res$seen.points[[2]][1,]
  sp3 = res$seen.points[[3]][1,]
  
  eff = FeatureEffectMBO(res.mbo = res, interest = "acquisition", feature = "power", 
                           method = "ice", grid.size = 2)
  # store the results for the first, which corresponds to the first 3 sp in iter
  res1 = eff[[1]]$results[which(eff[[1]]$results$.id <= 1),]
  res2 = eff[[2]]$results[which(eff[[2]]$results$.id <= 1),]
  res3 = eff[[3]]$results[which(eff[[3]]$results$.id <= 1),]
  
  #for each iter we set the value of power like grid values
  # iter 1
  sp1 = rbind(sp1, sp1)
  sp1[1, "power"] = res1[1, "power"]
  sp1[2, "power"] = res1[2, "power"]
  sp1$ei = acq.fun(points = sp1, models = list(res$models$`1`), control = res$control, 
                   par.set = res$opt.path$par.set, designs = list(initial.data), iter = 1, 
                   progress = NULL, attributes = FALSE)
  # iter 2
  sp2 = rbind(sp2, sp2)
  sp2[1, "power"] = res2[1, "power"]
  sp2[2, "power"] = res2[2, "power"]
  sp2$ei = acq.fun(points = sp2, models = list(res$models$`2`), control = res$control, 
                   par.set = res$opt.path$par.set, designs = list(opdf[1:10,1:5]), iter = 2, 
                   progress = NULL, attributes = FALSE)
  # iter 3
  sp3 = rbind(sp3, sp3)
  sp3[1, "power"] = res3[1, "power"]
  sp3[2, "power"] = res3[2, "power"]
  sp3$ei = acq.fun(points = sp3, models = list(res$models$`3`), control = res$control, 
                   par.set = res$opt.path$par.set, designs = list(opdf[1:11,1:5]), iter = 3, 
                   progress = NULL, attributes = FALSE)
  
  #results of the test
  expect_equal(res1$.value, sp1$ei)
  expect_equal(res2$.value, sp2$ei)
  expect_equal(res3$.value, sp3$ei)
  
  # Test 2
  # AdaCB, becuase it includes Progress which can be tricky
  ctrl = makeMBOControl(y.name = "ratio", store.model.at = 1:3)
  ctrl = setMBOControlTermination(ctrl, iters = 3)
  ctrl = setMBOControlInfill(ctrl,
                             opt = "focussearchSavepts",
                             opt.focussearch.maxit = 20,
                             opt.focussearch.points = 5,
                             crit = makeMBOInfillCritAdaCB()
  )
  set.seed(1)
  res = mbo(objfun,
            design = initial.data,
            control = ctrl,
            show.info = FALSE
  )
  # extract the acq.fun
  acq.fun = res$control$infill.crit$fun
  opdf = as.data.frame(res$opt.path)
  # for simplicity we store only the first seen point
  sp1 = res$seen.points[[1]][1,]
  sp2 = res$seen.points[[2]][1,]
  sp3 = res$seen.points[[3]][1,]
  
  eff = FeatureEffectMBO(res.mbo = res, interest = "acquisition", feature = "power", 
                           method = "ice", grid.size = 2)
  
  # store the results for the first, which corresponds to the first 3 sp in iter
  res1 = eff[[1]]$results[which(eff[[1]]$results$.id <= 1),]
  res2 = eff[[2]]$results[which(eff[[2]]$results$.id <= 1),]
  res3 = eff[[3]]$results[which(eff[[3]]$results$.id <= 1),]
  
  #for each iter we set the value of power like grid values
  # iter 1
  sp1 = rbind(sp1, sp1)
  sp1[1, "power"] = res1[1, "power"]
  sp1[2, "power"] = res1[2, "power"]
  sp1$adacb = acq.fun(points = sp1, models = list(res$models$`1`), control = res$control, 
                      par.set = res$opt.path$par.set, designs = list(initial.data), iter = 1, 
                      progress = getProgressAdaCB(res, 1), attributes = FALSE)
  # iter 2
  sp2 = rbind(sp2, sp2)
  sp2[1, "power"] = res2[1, "power"]
  sp2[2, "power"] = res2[2, "power"]
  sp2$adacb = acq.fun(points = sp2, models = list(res$models$`2`), control = res$control, 
                      par.set = res$opt.path$par.set, designs = list(opdf[1:10,1:5]), iter = 2, 
                      progress = getProgressAdaCB(res, 2), attributes = FALSE)
  # iter 3
  sp3 = rbind(sp3, sp3)
  sp3[1, "power"] = res3[1, "power"]
  sp3[2, "power"] = res3[2, "power"]
  sp3$adacb = acq.fun(points = sp3, models = list(res$models$`3`), control = res$control, 
                      par.set = res$opt.path$par.set, designs = list(opdf[1:11,1:5]), iter = 3, 
                      progress = getProgressAdaCB(res, 3), attributes = FALSE)
  
  expect_equal(res1$.value, sp1$adacb)
  expect_equal(res2$.value, sp2$adacb)
  expect_equal(res3$.value, sp3$adacb)
  
})

test_that("FeatureEffectMBO works both with chr and num features", {
  ctrl = makeMBOControl(y.name = "ratio", store.model.at = 1)
  ctrl = setMBOControlTermination(ctrl, iters = 1)
  ctrl = setMBOControlInfill(ctrl,
                             opt = "focussearchSavepts",
                             opt.focussearch.maxit = 20,
                             opt.focussearch.points = 5,
                             crit = makeMBOInfillCritEI()
  )
  set.seed(1)
  res = mbo(objfun,
            design = initial.data,
            control = ctrl,
            show.info = FALSE
  )
  
  eff.chr = FeatureEffectMBO(res.mbo = res, interest = "acquisition", feature = c("power", "time"),
                           method = "pdp", grid.size = c(10,10))
  res.chr = eff.chr[[1]]$results
  
  eff.num = FeatureEffectMBO(res.mbo = res, interest = "acquisition", feature = c(1,2),
                               method = "pdp", grid.size = c(10,10))
  res.num = eff.num[[1]]$results
  
  expect_equal(res.chr, res.num)
  
})

