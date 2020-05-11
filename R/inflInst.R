library(magrittr) #put magitrr in Import to import the Pipe operator
library(checkmate)
library(rpart.plot)
library(parallel)

#' @title Captures the influence of training instances within a selected iteration of a MBO process
#' 
#' @description The function captures the influence of the training (designs) instances on the proposed point
#' in a specific iteration of the MBO process. For that, we use the initial data and the proposed points 
#' in iters 1,...,i-1 as design & run teh MBO process for 1 iter. We then train a ML model at the end 
#' to distinguish between infl and non infl instances.
#' The function basically answers the questions: if training instance x is missing, 
#' 1) which point would be propsed in iter i?
#' 2) how much would the prediction or the infill criteria value of the original proposed point change?
#' - So far, it works for Single-Objective and Single-Point Proposal only. Also, it does not work for custom Infillcrits.
#' - Please use it only in combination with the `mlrMBO` package.
#' - built on mlrMBO version 1.1.4. If latest versions of iml are installed it might generate errors. 
#' 
#' @param res.mbo [\code{\link{MBOSingleObjResult}}]\cr
#' Results of the mbo process
#' @param iter [\code{numeric(1)}]\cr
#' iteration in which we want to measure influential instances
#' @param influence [\code{character(1)}]\cr
#' how should influence be measured? Default is \code{normal}, where simple difference is taken.
#' With \code{absolute} the absolute value of the difference is taken.
#' @param interest [\code{character(1)}]\cr
#' If \code{surrogate} (default) the change in the prediction (mean) of the surrogate model is measured. 
#' In such case and if infill criteria \dQuote{se}, be sure that the surrogate model has been stored (see \code{\link[mlrMBO]{makeMBOControl}} \code{store.model.at})
#' If \code{acquisition} the change in the value of the infill criteria is measured. 
#' @param parallel  [\code{logical(1)}]\cr
#' Should the inflInst be measurd in parallel? Default is FALSE. If TRUE, parLapply
#' \code[parLapply] from package parallel is used (see \code{\link[parallel]{clusterApply}}). The parallel
#' version currently uses \code{FORK} type and works only on Mac and Linux.
#' @param plot [\code{logical(1)}]\cr
#' If \code{TRUE} the resulting plot of the trained CART is visualized.
#' @param ... 
#' optional argument to pass to the CART model. See \code{\link[rpart]{rpart.control}}].
#' @return a list containing the proposed point of the mbo run without instance 
#' i (\code{choice.of.af})and the results of the influence analysis (\code{influence.analysis}), namely the ML model and the data used to train it.
#' Use \code{\link[rpart.plot]{rpart.plot}}] to create the plot, if plot = FALSE.
#' @export
inflInst <- function(res.mbo, iter, influence =  "normal", interest = "surrogate", 
                     parallel = FALSE, plot = TRUE, seed = 1, ...
                     ) {

  # extract infos from from res.mbo
  # data frames
  opdf =  as.data.frame(res.mbo$opt.path)
  design.mbo = res.mbo$final.opt.state$opt.problem$design
  # infos to run the mbo again
  fun.mbo = res.mbo$final.opt.state$opt.problem$fun
  control.mbo = res.mbo$control
  par.set.mbo = res.mbo$opt.path$par.set
  learner.mbo = res.mbo$final.opt.state$opt.problem$learner
  more.args.mbo = res.mbo$final.opt.state$opt.problem$more.args
  # to measure the influence
  acq.fun = res.mbo$control$infill.crit$fun
  if (ParamHelpers::isNumeric(par.set.mbo))
    metric = "euclidean"
  else metric = "gower"
  stored = sort(as.integer(names(res.mbo$models)))
  
  # Tags, Names, Integers
  infill.mbo = res.mbo$control$infill.crit$id
  y.name.mbo = res.mbo$control$y.name
  pars.mbo = getParam(par.set.mbo)
  # iters.mbo are the actually done "iterations" in the process, corresponds to dob in the opdf
  iters.mbo = opdf[nrow(opdf), "dob"]
  
  
  # assertions and checks on inputs
  checkmate::assertClass(res.mbo, classes = c("MBOSingleObjResult", "MBOResult"))
  checkmate::assertNumber(iter, lower = 1, upper = iters.mbo)
  checkmate::assertChoice(influence, c("absolute", "normal"))
  checkmate::assertChoice(interest, c("surrogate", "acquisition"))
  checkmate::assertLogical(parallel, len = 1, any.missing = FALSE)
  checkmate::assertLogical(plot, len = 1, any.missing = FALSE)

  # stops if multi point proposal or multiobjective target fun
  if (control.mbo$propose.points > 1 | control.mbo$n.objectives > 1) 
    stop("inflInst not implemented for Multipoint-Proposal or Multiobjective function")
  # stops if infill is not one of the seven built-in infill in mlrMBO
  if (!(infill.mbo %in% c("mean", "se", "ei", "cb", "eqi", "aei", "adacb"))) 
    stop("inflInst only implemented for the seven built-in single obj. infill crits")
  # stops if intrest = surrogate, no stored model for the iter
  if (interest == "surrogate" & infill.mbo == "se") {
    if (iter %in% stored) {
      surr = res.mbo$models[[iter]]
    } else {
      stop("The surrogate model for selected iter was not stored. Please
           store the model, change infill criteria or use interest = acquisition")
    }
  }
  # in order to proceed we need to 
  # change the design
  new.design = opdf[1:(nrow(design.mbo) + iter - 1), pars.mbo, drop = FALSE]
  
  # change the iters of the process & store the models
  control.mbo = setMBOControlTermination(control.mbo, iters = 1) #control.mbo$iters = 1 does not work!
  control.mbo$store.model.at = 1

  if (parallel) {
    # calulate the number of cores & initiate cluster
    no.cores = parallel::detectCores() - 1
    cl = parallel::makeCluster(no.cores, type = "FORK")
    
    res.without = parallel::parLapply(cl, 1:nrow(new.design), function(to.remove.index) {
      not.removed = setdiff(1:nrow(new.design), to.remove.index)
      # we set a seed so that each mbo process starts from the same "position" & different results
      # occurs because of different designs
      set.seed(seed)
      mbo.hypo = mbo(fun = fun.mbo, 
                     design = new.design[not.removed, , drop = FALSE],
                     learner = learner.mbo,
                     control = control.mbo, 
                     more.args = more.args.mbo,
                     show.info = FALSE
      )
      opdf.hypo = as.data.frame(mbo.hypo$opt.path)
      result.mbo.hypo = list(
        # design of the mbo process without instance i
        design = opdf.hypo[which(opdf.hypo$dob == 0), c(pars.mbo, y.name.mbo)],
        # proposed points and value of the of iter i, dob > 0 is fine because only 1 iter is run
        df = opdf.hypo[which(opdf.hypo$dob > 0),c(pars.mbo, y.name.mbo, infill.mbo)],
        # surrogate model used for the initial fit
        sm = mbo.hypo$models$`1`
      )
    })
    
    #close the cluster
    parallel::stopCluster(cl)
    
  } else{
    res.without = lapply(1:nrow(new.design), function(to.remove.index) {
      not.removed = base::setdiff(1:nrow(new.design), to.remove.index)
      set.seed(seed)
      mbo.hypo = mbo(fun = fun.mbo, 
                     design = new.design[not.removed, , drop = FALSE],
                     learner = learner.mbo,
                     control = control.mbo, 
                     more.args = more.args.mbo,
                     show.info = FALSE
      )
      opdf.hypo = as.data.frame(mbo.hypo$opt.path)
      result.mbo.hypo = list(
        # design of the mbo process without instance i
        design = opdf.hypo[which(opdf.hypo$dob == 0), c(pars.mbo, y.name.mbo)],
        # proposed points, target and infill value in iter 1 
        df = opdf.hypo[which(opdf.hypo$dob == 1),c(pars.mbo, y.name.mbo, infill.mbo)],
        # surrogate model fitted
        sm = mbo.hypo$models$`1`
      )
    })
  }
  
  # reshaping the results of all the mbo processes run
  design.without = lapply(res.without, function(x) x["design"]) %>%
    unlist(recursive = FALSE, use.names = FALSE)
  df.without = lapply(res.without, function(x) x["df"]) %>% 
   unlist(recursive = FALSE, use.names = FALSE) %>%
   dplyr::bind_rows()
  sm.without = lapply(res.without, function(x) x["sm"]) %>%
   unlist(recursive = FALSE, use.names = FALSE)
  
  # 1.compare pp.without & pp.true (it also includes choice of af)
  # true proposed point, the target, af and (if interest = surrogate) the prediction of the surrogate
  df.true = opdf[which(opdf$dob == iter), c(pars.mbo, y.name.mbo, infill.mbo)]
  dimnames(df.true)[[1]] = "true"
  pp.true = df.true[,pars.mbo, drop = FALSE]
  pp.without = df.without[,pars.mbo, drop = FALSE]

  # 1.2 computing the distance between original proposed point and proposed points without instance i
  # dist.vec is the distance vector
  choice.af = rbind(pp.without, pp.true) %>%
    cluster::daisy(metric = metric) %>%
    as.matrix() %>%
    as.data.frame() %>%
    dplyr::select(true) %>%
    dplyr::rename(dist.pp.true = true)
  
  # pp.all is a df with all the pp,their infill anf target values
  # we merge pp.without and dist.vec together, and see which instance causes
  # the biggest change in th proposed point
  df.all = rbind(df.without, df.true)
  res1 = cbind(df.all, choice.af)

  # 2.evaluate the prediction of the df.true in the new model & train a CART to 
  #  distinguish infl & non infl instances
  
  # we add a column "pred.sm" in case interest = "surrogate"
  if (interest == "surrogate" & infill.mbo == "mean") {
    df.true$pred.sm = ifelse(control.mbo$minimize, df.true[,infill.mbo], -1 * df.true[,infill.mbo])
  }
  if (interest == "surrogate" & infill.mbo == "se") {
    df.true$pred.sm = predict(surr, newdata = pp.true)$data$response
  }
  if (interest == "surrogate" & !(infill.mbo %in% c("se", "mean"))) {
    df.true$pred.sm = opdf[which(opdf$dob == iter), "mean"]
  }

  pred.true = switch(interest,
    "surrogate" = df.true[, "pred.sm"],
    "acquisition" = df.true[, infill.mbo]
  )
  # computes the prediction at the true proposed point with the surrogate model or 
  # af fitted without training instances
  pred.without <- switch(interest,
    "surrogate" = sapply(
      seq_along(sm.without), function(x) {
      predict(sm.without[[x]], newdata = pp.true)$data$response
      }
    ),
    "acquisition" = sapply(seq_along(sm.without), function(x) {
      acq.fun(
        points = pp.true,
        models = list(sm.without[[x]]),
        control = control.mbo,
        par.set = par.set.mbo,
        designs = list(design.without[[x]]),
        iter = NULL, # NULL since only used for Multicrit DIB & DIB not implemented
        progress = getProgressAdaCB(res.mbo = res.mbo, iter = iter),
        attributes = FALSE
      )
    })
  )
  
  # names(pred.without) = 1:length(pred.without)
  # remove assertion once it works
  checkmate::assertNumeric(pred.without, any.missing = FALSE)

  # 2.2 compute the influence of instance i the absolute difference between infill.mbo & ic.without
  infl = switch(influence,
    "absolute" = abs(pred.true - pred.without),
    "normal" = (pred.true - pred.without)
  )
  # NOTe: instance i has a positive influence on the prediction if the ic.true < ic.without (since internally
  #       all the infill criteria are minimized), therefore if ic.true - ic.without < 0

  # 2.3. compute the distance between pp.true and the points in new design
  dist.to.nd = rbind(new.design, pp.true) %>%
    cluster::daisy(metric = metric) %>%
    as.matrix() %>%
    as.data.frame() %>%
    dplyr::select(true) %>%
    dplyr::rename(dist.pp.true = true) %>%
    dplyr::slice(1:nrow(new.design))

  # 2.4 train a ML model to distinguish infl and non infl instances
  #- for that we need to create a df with the removed instances of the design and their influence on ic.true
  infl.df = cbind(infl, new.design, dist = dist.to.nd)
  task = makeRegrTask(data = infl.df[,-ncol(infl.df)], target = "infl")
  lrn =  makeLearner("regr.rpart", ...)
  mod = train(lrn, task)
  if (plot) {
    rpart.plot::rpart.plot(mod$learner.model, roundint = FALSE)
  }

  # 3.final results
  result = list(choice.of.af = res1,
                influence.analysis = list(data.frame = infl.df, model = mod)
  )
}

################
getProgressAdaCB = function(res.mbo, iter) {
  if (res.mbo$control$infill.crit$id == "adacb") {
    
    opdf = as.data.frame(res.mbo$opt.path)
    lambda.start = res.mbo$control$infill.crit$params$cb.lambda.start
    lambda.end = res.mbo$control$infill.crit$params$cb.lambda.end
    lambda = opdf[which(opdf$dob == iter), "lambda"]
    progress = (lambda - lambda.start) / (lambda.end - lambda.start)
    
  } else {
    progress = NULL
  }
}

#################
getParam = function(par.set) {
  checkmate::assertClass(par.set, "ParamSet")
  pars = par.set$pars
  p = list()
  for (i in seq_along(pars)) {
    if (pars[[i]][["len"]] == 1) {
      p[[i]] = pars[[i]][["id"]]
    } else {
      p[[i]] = sapply(1:pars[[i]][["len"]], function(x) paste0(pars[[i]][["id"]], x))
    }
  }
  p = unlist(p)
  p
}


###########
ctrl = makeMBOControl(y.name = "target", store.model.at = 1:6)
ctrl = setMBOControlTermination(ctrl, iters = 5)
ctrl = setMBOControlInfill(ctrl,
                           opt = "focussearchSavepts",
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
opdf = as.data.frame(res$opt.path)

debug(inflInst)
undebug(inflInst)
ii = inflInst(res.mbo = res, iter = 4, influence = "normal", interest = "surrogate", 
              parallel = TRUE, seed = 1, plot = FALSE)
rpart.plot::rpart.plot(ii$influence.analysis$model$learner.model, roundint = FALSE)
