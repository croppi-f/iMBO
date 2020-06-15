################# Data and Configurations for the tests ##############################
library(mlrMBO)
# model from all data points
data.kapton = read.csv(text = "power,time,gas,pressure,ratio
1004,10624,Argon,660,2.65884223
1008,10500,Argon,720,3.424371829
1013,10475,Argon,720,3.182223683
1095,19029,Argon,940,2.776420163
1105,12381,Air,860,0.12
1108,9828,Nitrogen,820,0.953560013
1206,19145,Argon,920,3.245754845
1211,19279,Argon,470,3.228924409
123,14994,Nitrogen,220,1.234061595
1249,19711,Argon,460,3.249050132
1280,17180,Argon,980,2.177099828
1285,1922,Argon,990,3.132160852
1286,15717,Argon,530,2.891256344
1286,18227,Air,920,0.12
1286,18626,Argon,930,1.67188718
1287,9365,Argon,920,2.486961786
1288,19449,Argon,920,2.964636773
1288,19515,Argon,920,2.446689168
1293,9319,Argon,940,3.307882968
1300,9324,Argon,1000,3.370876358
1304,9321,Argon,1000,3.582150476
1307,9322,Argon,1000,5.159847662
1315,9331,Argon,980,3.92767588
1316,10972,Nitrogen,200,1.2159602
1329,9605,Argon,990,3.766115716
1333,6623,Argon,970,3.45655082
1341,10625,Argon,700,3.264845323
1343,9585,Argon,980,4.276949681
1355,9322,Argon,1000,3.062587782
1403,16863,Argon,990,2.68061448
1408,2841,Argon,880,3.193382374
1450,15621,Argon,510,3.05041702
1457,2629,Argon,890,3.080188754
1468,7549,Nitrogen,20,2.722335204
1474,6454,Nitrogen,20,1.861073482
1491,19728,Argon,400,2.731740027
1511,16433,Air,500,0.12
1518,17231,Argon,890,3.308118401
1521,17553,Argon,820,3.490871423
1527,18520,Argon,310,1.791615889
1537,6043,Air,70,0.921751931
1542,18514,Argon,310,3.274632664
1546,18375,Argon,320,1.818586293
1560,9314,Argon,1000,3.230997225
1574,11017,Argon,1000,2.932014539
1575,6634,Nitrogen,20,2.596027828
1596,18198,Argon,310,2.909999609
1610,1822,Argon,730,3.243461912
1665,18552,Argon,310,2.289480755
1703,10481,Argon,960,3.007009955
1717,10382,Argon,970,3.16631637
1720,1831,Argon,80,1.682780666
1735,10293,Nitrogen,970,1.175244034
1736,10319,Nitrogen,970,1.051970301
1738,10331,Argon,810,3.301696296
1739,10379,Argon,990,4.724743249
1740,10379,Argon,970,3.700694716
1757,10378,Argon,970,5.499063387
1760,11229,Argon,970,3.207143574
1769,3920,Air,1000,0.12
1776,10375,Argon,970,4.08690544
1780,5333,Argon,960,2.857800965
1789,1792,Argon,500,3.177328601
1805,17740,Argon,970,2.912210568
1806,8198,Nitrogen,810,1.040370964
1809,10259,Argon,980,4.16942864
1842,18881,Argon,830,2.975606279
1843,16852,Air,810,0.12
1844,16618,Air,760,0.12
1875,16877,Argon,990,3.051428698
1875,18881,Argon,820,4.0168837
1878,7879,Argon,840,3.118894583
1883,16859,Air,990,0.12
1888,16925,Argon,990,3.638606351
1890,16900,Argon,990,3.983891201
1891,10378,Argon,970,4.067806774
1894,17003,Argon,990,3.017604271
1908,18880,Nitrogen,860,1.036697397
1915,10379,Argon,990,3.893852932
1930,2421,Argon,800,3.080778976
1948,18880,Nitrogen,860,1.255781892
1956,18873,Argon,720,2.901374775
1962,1814,Nitrogen,520,2.121105968
1975,4268,Argon,990,3.827582704
1976,1827,Argon,70,2.093948947
1984,18880,Argon,850,3.855773748
2008,18880,Argon,850,4.276358767
2009,4227,Argon,1000,3.680987162
2021,18886,Argon,840,3.81399825
2023,3171,Argon,800,3.439505258
2035,1491,Argon,490,3.72323823
2065,18880,Argon,900,3.571770687
2068,15492,Nitrogen,310,1.236027609
2069,18445,Argon,830,2.985552758
2073,1840,Argon,490,2.595710184
210,11475,Air,970,0.12
2128,18880,Argon,820,3.870920293
2131,18800,Argon,820,4.051220319
2141,1099,Argon,80,3.073853026
2143,18880,Argon,810,3.960298908
2145,18880,Argon,810,4.349114421
2148,18880,Argon,290,1.914237706
2150,18880,Argon,810,4.16268586
2153,3502,Argon,800,2.96307017
2155,18832,Air,790,0.12
2166,1815,Argon,140,3.191922596
2168,1825,Argon,490,4.024224557
2170,1822,Argon,490,5.203238087
2170,1830,Argon,470,3.827207228
2172,1835,Argon,380,3.433754797
2222,1747,Argon,490,3.453268783
2243,18925,Argon,770,3.081672687
2251,18903,Argon,770,3.12589218
2274,1983,Argon,490,3.215536504
2309,16179,Argon,990,2.858442071
2358,1821,Argon,240,2.977786277
2394,1822,Argon,160,1.62537662
242,4974,Nitrogen,440,0.85456231
2430,18879,Argon,770,2.461660064
2450,19833,Argon,770,2.386308297
2451,10379,Argon,970,4.599581634
2456,1822,Argon,180,2.012994937
2535,1822,Argon,80,3.726127597
2637,16266,Argon,500,2.373853521
2673,1818,Argon,690,3.812412746
2678,4279,Argon,920,3.613608221
2687,2822,Argon,930,2.982047036
2696,1824,Argon,680,3.59011885
2697,8206,Air,540,0.12
2765,514,Argon,490,3.01456403
2824,10377,Argon,960,2.902882653
2838,18878,Argon,880,2.127514126
2905,10380,Argon,970,3.444947127
2962,12485,Nitrogen,1000,1.486708819
2982,17867,Nitrogen,530,1.530874491
3018,10379,Air,960,0.12
3063,4467,Argon,570,2.613552957
3066,10379,Air,960,0.12
3099,18787,Argon,910,2.271008185
3155,10378,Air,950,0.12
3156,10357,Argon,950,2.691402923
3164,14268,Nitrogen,30,2.065513817
320,2241,Argon,850,1.414816905
3214,1823,Argon,720,3.310161931
3263,2601,Argon,1000,3.067360272
3266,2523,Argon,980,2.626501432
3302,2414,Air,860,0.12
3303,3421,Argon,860,2.317012514
3314,2120,Argon,840,2.779790313
3445,1184,Argon,690,3.384759098
3516,813,Argon,40,2.128768105
3519,1129,Argon,40,2.989507712
3527,1824,Argon,40,1.538457125
3584,1131,Air,20,0.57836145
3613,10766,Argon,600,2.093101526
3617,16380,Nitrogen,740,1.321003072
3706,10137,Nitrogen,370,1.522617069
3728,9350,Argon,870,2.089539008
3777,9767,Argon,900,2.132647453
3797,18821,Air,800,0.12
3805,19668,Argon,920,1.959362197
3810,10110,Argon,910,3.537409645
3812,10118,Argon,970,2.264059348
3830,10217,Argon,910,2.996145204
3907,9823,Air,30,0.12
396,9065,Argon,460,1.365235057
3993,3615,Argon,860,2.662923334
4022,2321,Argon,900,3.341319333
4033,3055,Argon,880,3.371732725
4046,2524,Argon,880,3.334058141
4060,9988,Argon,270,1.32755134
4068,10089,Argon,910,2.894545384
4079,3055,Argon,850,2.338842087
4139,1957,Argon,900,3.134584619
429,4810,Nitrogen,630,0.869027237
4297,9256,Nitrogen,110,1.685642824
4326,3961,Argon,250,1.416376543
4431,3854,Argon,270,2.190961
4506,8046,Nitrogen,110,1.458586461
4564,18002,Air,50,0.12
4585,14485,Nitrogen,100,1.174840469
4717,1457,Argon,640,2.323310757
4834,5615,Air,170,0.12
4886,11627,Nitrogen,680,1.340241441
489,15493,Argon,460,1.236779697
4909,1710,Argon,960,2.765735333
4962,15074,Argon,360,1.323049628
4963,2336,Argon,960,3.364084792
5028,2371,Argon,960,2.58479023
5041,9305,Argon,1000,2.249307903
5069,2735,Nitrogen,480,1.419028259
5153,19318,Air,780,0.12
5239,14517,Nitrogen,230,1.19774686
5309,2566,Argon,960,2.705688601
5321,16727,Air,510,0.12
5332,6770,Air,630,0.12
5394,10380,Argon,970,2.287450434
5480,12267,Nitrogen,180,1.149918539
616,7988,Air,60,0.12
628,10332,Air,200,0.12
698,18907,Argon,820,1.978090509
731,10628,Argon,710,1.748881858
745,2168,Argon,790,2.152875412
783,10474,Argon,690,1.989581105
826,1826,Argon,480,3.148752413
830,10446,Argon,730,2.743036246
83,8031,Air,610,0.12
933,7024,Nitrogen,940,0.917709244
952,18662,Argon,290,1.815097367
981,18639,Argon,290,1.256006035")

set.seed(1)
model = train(makeLearner("regr.randomForest"), makeRegrTask(data = data.kapton, target = "ratio"))

# Bayesian Optimization

fun = function(x) {
  df = as.data.frame(x)
  df$gas = factor(df$gas, levels = levels(data.kapton$gas))
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
samples.argon = sample(rownames(data.kapton[data.kapton$gas == "Argon", ]), 3)
samples.nitro = sample(rownames(data.kapton[data.kapton$gas == "Nitrogen", ]), 3)
samples.air = sample(rownames(data.kapton[data.kapton$gas == "Air", ]), 3)
initial.data = data.kapton[c(samples.argon, samples.nitro, samples.air), ]


ctrl = makeMBOControl(y.name = "ratio")
ctrl = setMBOControlTermination(ctrl, iters = 3)

#source modified function from mlrMBO
source("R/utils_xplxpl/proposePointsByInfillOptimization-jr.R")
source("R/utils_xplxpl/makeMBOResult.OptState-jr.R")
source("R/utils_xplxpl/getSupportedInfillOptFunctions-jr.R")
source("R/utils_xplxpl/proposePointsByInfillOptimization-jr.R")
source("R/utils_xplxpl/getInfillOptFunction-jr.R")
source("R/utils_xplxpl/checkStuff-jr.R")

# source new infill optimization functions "...Savepts"
source("R/utils_xplxpl/infillOptFocusSavepts-jr.R")
source("R/utils_xplxpl/infillOptEASavepts-jr.R")
source("R/utils_xplxpl/infillOptCMAESSavepts-jr.R")

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

