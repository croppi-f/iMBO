# Test for PredictorAf
# source the data and MBO configs
source("fc_notizen/materials-design-playground-fc.R")

# run the MBO with all single infill and Savepts, the MBO are run with 1 iter
#old: source("2_acquisition_function/2.2_Prediction_of_surrogate/FeatureEffectMBO-playground-fc.R")
#old: source("_iml_tools/2.2_FeatureEffectMBO/FeatureEffectMBO-playground-fc.R")

# create the PredictorAf object (source the internal functions needed)
#old: source("2_acquisition_function/2.2_Prediction_of_surrogate/Prediction_af/PredictorAf-fc.R")
source("_iml_tools/2.2_FeatureEffectMBO/PredictorAf/PredictorAf-fc.R")

library(testthat)
library(mlrMBO)

## Tests
test_that("Equivalence for Confidence Bound", {
  #create the PredictorAfObject in iter 1
  p.cb = PredictorAf$new(model = res.mbo.cb$models[[1]],
                         data = res.mbo.cb$seen.points[[1]], 
                         res.mbo = res.mbo.cb,
                         iter = 1,
                         design = opdf.cb[1:20,c(names(res.mbo.cb$x), res.mbo.cb$control$y.name)] 
  )
  #create the PredictorAfObject in iter 2
  p2.cb = PredictorAf$new(model = res.mbo.cb$models[[2]],
                          data = res.mbo.cb$seen.points[[2]], 
                          res.mbo = res.mbo.cb,
                          iter = 2, 
                          design = opdf.cb[1:21,c(names(res.mbo.cb$x), res.mbo.cb$control$y.name)] 
  )
  # iter 1
  expect_equivalent(
    p.cb$predict(pp.true.cb[1,1:6]),
    opdf.cb[21, res.mbo.cb$control$infill.crit$id]
  )
  # iter 2
  expect_equivalent(
    p2.cb$predict(pp.true.cb[2,1:6]),
    opdf.cb[22, res.mbo.cb$control$infill.crit$id]
  )
})

test_that("Equivalence for Expected Improvement", {
  #create the PredictorAfObject in iter 1
  p.ei = PredictorAf$new(model = res.mbo.ei$models[[1]],
                         data = res.mbo.ei$seen.points[[1]], 
                         res.mbo = res.mbo.ei,
                         iter = 1, 
                         design = opdf.ei[1:20,c(names(res.mbo.ei$x), res.mbo.ei$control$y.name)] 
  )
  #create the PredictorAfObject in iter 2
  p2.ei = PredictorAf$new(model = res.mbo.ei$models[[2]],
                          data = res.mbo.ei$seen.points[[2]], 
                          res.mbo = res.mbo.ei,
                          iter = 2,
                          design = opdf.ei[1:21,c(names(res.mbo.ei$x), res.mbo.ei$control$y.name)]
  )
  # iter 1
  expect_equivalent(
    p.ei$predict(pp.true.ei[1,1:6]),
    opdf.ei[21, res.mbo.ei$control$infill.crit$id]
  )
  # iter 2
  expect_equivalent(
    p2.ei$predict(pp.true.ei[2,1:6]),
    opdf.ei[22, res.mbo.ei$control$infill.crit$id]
  )
  
})

test_that("Equivalence for Mean Response", {
  #create the PredictorAfObject in iter 1
  p.mr = PredictorAf$new(model = res.mbo.mr$models[[1]],
                         data = res.mbo.mr$seen.points[[1]], 
                         res.mbo = res.mbo.mr,
                         iter = 1, 
                         design = opdf.mr[1:20,c(names(res.mbo.mr$x), res.mbo.mr$control$y.name)] 
  )
  #create the PredictorAfObject in iter 2
  p2.mr = PredictorAf$new(model = res.mbo.mr$models[[2]], 
                          data = res.mbo.mr$seen.points[[2]], 
                          res.mbo = res.mbo.mr,
                          iter = 2,
                          design = opdf.mr[1:21,c(names(res.mbo.mr$x), res.mbo.mr$control$y.name)] 
  )
  #iter 1
  expect_equivalent(
    p.mr$predict(pp.true.mr[1,1:6]),
    opdf.mr[21, res.mbo.mr$control$infill.crit$id]
  )
  # iter 2
  expect_equivalent(
    p2.mr$predict(pp.true.mr[2,1:6]),
    opdf.mr[22, res.mbo.mr$control$infill.crit$id]
  )
})

test_that("Equivalence for Standard Error", {
  #create the PredictorAfObject in iter 1
  p.se = PredictorAf$new(model = res.mbo.se$models[[1]], 
                         data = res.mbo.se$seen.points[[1]], 
                         res.mbo = res.mbo.se,
                         iter = 1,
                         design = opdf.se[1:20,c(names(res.mbo.se$x), res.mbo.se$control$y.name)] 
  )
  #create the PredictorAfObject in iter 2
  p2.se = PredictorAf$new(model = res.mbo.se$models[[2]],
                          data = res.mbo.se$seen.points[[2]], 
                          res.mbo = res.mbo.se,
                          iter = 2,
                          design = opdf.se[1:21,c(names(res.mbo.se$x), res.mbo.se$control$y.name)] 
  )
  # iter 1
  expect_equivalent(
    p.se$predict(pp.true.se[1,1:6]),
    opdf.se[21, res.mbo.se$control$infill.crit$id]
  )
  # iter 2
  expect_equivalent(
    p2.se$predict(pp.true.se[2,1:6]),
    opdf.se[22, res.mbo.se$control$infill.crit$id]
  )
})

test_that("Equivalence for Augmented EI", {
  #create the PredictorAfObject in iter 1
  p.aei = PredictorAf$new(model = res.mbo.aei$models[[1]], 
                          data = res.mbo.aei$seen.points[[1]], 
                          res.mbo = res.mbo.aei,
                          iter = 1, 
                          design = opdf.aei[1:20,c(names(res.mbo.aei$x), res.mbo.aei$control$y.name)] 
  )
  #create the PredictorAfObject in iter 2
  p2.aei = PredictorAf$new(model = res.mbo.aei$models[[2]], 
                           data = res.mbo.aei$seen.points[[2]], 
                           res.mbo = res.mbo.aei,
                           iter = 2,
                           design = opdf.aei[1:21,c(names(res.mbo.aei$x), res.mbo.aei$control$y.name)] 
  )
  # iter 1
  expect_equivalent(
    p.aei$predict(pp.true.aei[1,1:6]),
    opdf.aei[21, res.mbo.aei$control$infill.crit$id]
  )
  # iter 2
  expect_equivalent(
    p2.aei$predict(pp.true.aei[2,1:6]),
    opdf.aei[22, res.mbo.aei$control$infill.crit$id]
  )
})

test_that("Equivalence for Expected Quantile Improvement", {
  #create the PredictorAfObject in iter 1
  p.eqi = PredictorAf$new(model = res.mbo.eqi$models[[1]], 
                          data = res.mbo.eqi$seen.points[[1]], 
                          res.mbo = res.mbo.eqi,
                          iter = 1, 
                          design = opdf.eqi[1:20,c(names(res.mbo.eqi$x), res.mbo.eqi$control$y.name)] 
  )
  #create the PredictorAfObject in iter 2
  p2.eqi = PredictorAf$new(model = res.mbo.eqi$models[[2]],
                           data = res.mbo.eqi$seen.points[[2]],
                           res.mbo = res.mbo.eqi,
                           iter = 2, 
                           design = opdf.eqi[1:21,c(names(res.mbo.eqi$x), res.mbo.eqi$control$y.name)] 
  )
  # iter 1
  expect_equivalent(
    p.eqi$predict(pp.true.eqi[1,1:6]),
    opdf.eqi[21, res.mbo.eqi$control$infill.crit$id]
  )
  # iter 2
  expect_equivalent(
    p2.eqi$predict(pp.true.eqi[2,1:6]),
    opdf.eqi[22, res.mbo.eqi$control$infill.crit$id]
  )
  
  
})

test_that("Equivalence for Adaptive CB & term.cond. iters", {
  # create the PredictorAfObject in iter 1
  p.adacb.iter = PredictorAf$new(model = res.mbo.adacb.iter$models[[1]], 
                                 data = res.mbo.adacb.iter$seen.points[[1]], 
                                 res.mbo = res.mbo.adacb.iter,
                                 iter = 1, 
                                 design = opdf.adacb.iter[1:20,c(names(res.mbo.adacb.iter$x), res.mbo.adacb.iter$control$y.name)]
  )
  # create the PredictorAfObject in iter 2
  p2.adacb.iter = PredictorAf$new(model = res.mbo.adacb.iter$models[[2]], 
                                  data = res.mbo.eqi$seen.points[[2]], 
                                  res.mbo = res.mbo.adacb.iter,
                                  iter = 2,
                                  design = opdf.adacb.iter[1:21,c(names(res.mbo.adacb.iter$x), res.mbo.adacb.iter$control$y.name)]
  )
  # Term.condition iters, iter 1
  expect_equivalent(
    p.adacb.iter$predict(pp.true.adacb.iter[1,1:6]),
    opdf.adacb.iter[21, res.mbo.adacb.iter$control$infill.crit$id]
  )
  # Term.condition iters, iter 2
  expect_equivalent(
    p2.adacb.iter$predict(pp.true.adacb.iter[2,1:6]),
    opdf.adacb.iter[22, res.mbo.adacb.iter$control$infill.crit$id]
  )
  
})
## FIXME: 
test_that("Equivalence for AdaCB over multiple iterations and term.cond. time budget", {
  # create the PredictorAfObject in iter 1
  p.adacb.tb = PredictorAf$new(model = res.mbo.adacb.tb$models[[1]], 
                                 data = res.mbo.adacb.tb$seen.points[[1]], 
                                 res.mbo = res.mbo.adacb.tb,
                                 iter = 1, 
                                 design = opdf.adacb.tb[1:20,c(names(res.mbo.adacb.tb$x), res.mbo.adacb.tb$control$y.name)]
  )
  # iter 3
  p3.adacb.tb = PredictorAf$new(model = res.mbo.adacb.tb$models[[3]], 
                               data = res.mbo.adacb.tb$seen.points[[3]], 
                               res.mbo = res.mbo.adacb.tb,
                               iter = 3, 
                               design = opdf.adacb.tb[1:22,c(names(res.mbo.adacb.tb$x), res.mbo.adacb.tb$control$y.name)]
  )
  
  # iter 7
  p7.adacb.tb = PredictorAf$new(model = res.mbo.adacb.tb$models[[7]], 
                                data = res.mbo.adacb.tb$seen.points[[7]], 
                                res.mbo = res.mbo.adacb.tb,
                                iter = 7, 
                                design = opdf.adacb.tb[1:26,c(names(res.mbo.adacb.tb$x), res.mbo.adacb.tb$control$y.name)]
  )
  
  expect_equivalent(
    p.adacb.tb$predict(pp.true.adacb.tb[1,1:6]),
    opdf.adacb.tb[21, res.mbo.adacb.tb$control$infill.crit$id]
  )
  expect_equivalent(
    p3.adacb.tb$predict(pp.true.adacb.tb[3,1:6]),
    opdf.adacb.tb[23, res.mbo.adacb.tb$control$infill.crit$id]
  )
  expect_equivalent(
    p7.adacb.tb$predict(pp.true.adacb.tb[7,1:6]),
    opdf.adacb.tb[27, res.mbo.adacb.tb$control$infill.crit$id]
  )
  
})

test_that("Data corresponds to seen points and designs to training set within a specific iteration (iter = 1)", {
  #create the PredictorAfObject in iter 1
  p.ei = PredictorAf$new(model = res.mbo.ei$models[[1]],
                         data = res.mbo.ei$seen.points[[1]], 
                         res.mbo = res.mbo.ei,
                         iter = 1, 
                         design = opdf.ei[1:20,c(names(res.mbo.ei$x), res.mbo.ei$control$y.name)] 
  )
  expect_equal(
    p.ei$data$X, data.table::data.table(res.mbo.ei$seen.points[[1]])
  )
  expect_equal(
    p.ei$design[[1]], opdf.ei[1:20,c(names(res.mbo.ei$x), res.mbo.ei$control$y.name)]
  )
})
#NOTE: PredictorAf works also if y not specified since PDP, ALE, ICE do not need y to be measured, 
#      though internally FeatureEffectMBO calculates the target values of the seen points, otherwise
#      an error occurs
test_that("PredictorAf works also if y not specified in data", {
  #create the PredictorAfObject in iter 1
  # without y 
  p.ei = PredictorAf$new(model = res.mbo.ei$models[[1]],
                         data = res.mbo.ei$seen.points[[1]], 
                         res.mbo = res.mbo.ei,
                         iter = 1, 
                         design = opdf.ei[1:20,c(names(res.mbo.ei$x), res.mbo.ei$control$y.name)])
  
  # with y specified, both in data & y
  p2.ei = PredictorAf$new(model = res.mbo.ei$models[[1]],
                         data = cbind(res.mbo.ei$seen.points[[1]], 
                                      target = objfun(res.mbo.ei$seen.points[[1]])
                                      ),
                         y = "ei",
                         res.mbo = res.mbo.ei,
                         iter = 1, 
                         design = opdf.ei[1:20,c(names(res.mbo.ei$x), res.mbo.ei$control$y.name)])
  # with y specified only in data
  p3.ei = PredictorAf$new(model = res.mbo.ei$models[[1]],
                          data = cbind(res.mbo.ei$seen.points[[1]], 
                                       target = objfun(res.mbo.ei$seen.points[[1]])
                          ),
                          res.mbo = res.mbo.ei,
                          iter = 1, 
                          design = opdf.ei[1:20,c(names(res.mbo.ei$x), res.mbo.ei$control$y.name)])
  
  expect_equivalent(
    p.ei$predict(pp.true.ei[1,1:6]),
    opdf.ei[21, res.mbo.ei$control$infill.crit$id]
  )
  expect_equivalent(
    p2.ei$predict(pp.true.ei[1,1:6]),
    opdf.ei[21, res.mbo.ei$control$infill.crit$id]
  )
  expect_equivalent(
    p3.ei$predict(pp.true.ei[1,1:6]),
    opdf.ei[21, res.mbo.ei$control$infill.crit$id]
  )
})

test_that("extracts data (seen.points) correctly from the mbo object", {
  #create the PredictorAfObject in iter 1
  p.ei = PredictorAf$new(model = res.mbo.ei$models[[1]],
                         res.mbo = res.mbo.ei,
                         iter = 1,
                         design = opdf.ei[1:20,1:7]
  )
  #create the PredictorAf Object in iter 2
  p2.ei = PredictorAf$new(model = res.mbo.ei$models[[2]],
                    res.mbo = res.mbo.ei,
                    iter = 2,
                    design = opdf.ei[1:21,1:7]
  )
  expect_equal(p.ei$data$X, data.table::data.table(res.mbo.ei$seen.points[[1]]))
  expect_equal(p2.ei$data$X, data.table::data.table(res.mbo.ei$seen.points[[2]]))
  
})

test_that("PredictorAf gives error if data or design contain NA", {
  # data contains NA
  dat = rbind(
    opdf.ei[1:21, c(names(res.mbo.ei$x), res.mbo.ei$control$y.name)],
    NA
  )
  expect_error(
    p.ei = PredictorAf$new(
      model = res.mbo.ei$models[[1]],
      data = dat,
      res.mbo = res.mbo.ei,
      iter = 1,
      design = opdf.ei[1:20, c(names(res.mbo.ei$x), res.mbo.ei$control$y.name)]
    )
  )
  
  # design contains NA
  des = rbind(
    opdf.ei[1:19, c(names(res.mbo.ei$x), res.mbo.ei$control$y.name)],
    NA
  )
  expect_error(
    p.ei = PredictorAf$new(
      model = res.mbo.ei$models[[1]],
      data = opdf.ei[1:21, c(names(res.mbo.ei$x), res.mbo.ei$control$y.name)],
      res.mbo = res.mbo.ei,
      iter = 1,
      design = des
    )
  )
})

test_that("design does not include any observation in data", {
  
})

test_that("PredictorAf gives error if design is not correctly specified",{
  # missing y value, to less columns
  expect_error(p.ei = PredictorAf$new(
    model = res.mbo.ei$models[[1]],
    data = res.mbo.ei$seen.points[[1]],
    res.mbo = res.mbo.ei,
    iter = 1,
    design = opdf.ei[1:20, names(res.mbo.ei$x)]
  ))
  
  # wrong numbers of rows
  expect_error(p.ei = PredictorAf$new(
    model = res.mbo.ei$models[[2]],
    data = res.mbo.ei$seen.points[[2]],
    res.mbo = res.mbo.ei,
    iter = 2,
    design = opdf.ei[1:20, c(names(res.mbo.ei$x), res.mbo.ei$control$y.name)]
  ))
  
})
