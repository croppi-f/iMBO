library(magrittr) #for the Pipe operator

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
#' how should influence be measured? Default is \code{linear}, where simple difference is taken.
#' With \code{absolute} the absolute value of the difference is taken.
#' @param interest [\code{character(1)}]\cr
#' If \code{surrogate} (default) the change in the prediction of the surrogate model is measured. 
#' In such cases and if infill criteria \dQuote{se}, be sure that the corresponding surrogate model has been stored 
#' (see \code{\link[mlrMBO]{makeMBOControl}} \code{store.model.at}). If \code{acquisition} the change 
#' in the value of AF is measured. 
#' @param parallel  [\code{logical(1)}]\cr
#' Should the inflInst be measurd in parallel? Default is FALSE. If TRUE, parLapply
#' \code[parLapply] from package parallel is used (see \code{\link[parallel]{clusterApply}}). The parallel
#' version currently uses \code{FORK} type and works only on Mac and Linux.
#' @param plot [\code{logical(1)}]\cr
#' If \code{TRUE} the resulting plot of the trained CART is visualized.
#' @param ... 
#' optional argument to pass to the CART model. See \code{\link[rpart]{rpart.control}}].
#' @return a list containing the hypothetical proposed point in the selected iteration
#' (\code{choice.of.af}) and the results of the influence analysis (\code{influence.analysis}), namely the ML model and the data used to train it.
#' Use \code{\link[rpart.plot]{rpart.plot}}] to create the plot, if plot = FALSE.
#' @export
inflInst <- function(res.mbo, iter, influence =  "linear", interest = "surrogate", 
                     parallel = FALSE, plot = TRUE, seed = 1, ...
                     ) {

  # extract infos from from res.mbo
  # data frames
  opdf =  as.data.frame(res.mbo$opt.path)
  design.mbo = res.mbo$final.opt.state$opt.problem$design # the initial design
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
  
  # Tags, Names, Values
  infill.mbo = res.mbo$control$infill.crit$id
  y.name.mbo = res.mbo$control$y.name
  pars.mbo = getParam(par.set.mbo)
  components.mbo = res.mbo$control$infill.crit$components
  # iters.mbo are the actually done "iterations" in the process, corresponds to dob in the opdf
  iters.mbo = opdf[nrow(opdf), "dob"]
  
  
  # assertions and checks on inputs
  checkmate::assertClass(res.mbo, classes = c("MBOSingleObjResult", "MBOResult"))
  checkmate::assertNumber(iter, lower = 1, upper = iters.mbo)
  checkmate::assertChoice(influence, c("absolute", "linear"))
  checkmate::assertChoice(interest, c("surrogate", "acquisition"))
  checkmate::assertLogical(parallel, len = 1, any.missing = FALSE)
  checkmate::assertLogical(plot, len = 1, any.missing = FALSE)

  # stops if multi point proposal or multiobjective target fun
  if (control.mbo$propose.points > 1 | control.mbo$n.objectives > 1) 
    stop("inflInst not implemented for Multipoint-Proposal or Multiobjective function")
  # stops if infill is not one of the seven built-in infill in mlrMBO
  if (!(infill.mbo %in% c("mean", "se", "ei", "cb", "eqi", "aei", "adacb"))) 
    stop("inflInst only implemented for the seven built-in single obj. infill crits")
  # stops if we not compute the prediction of the SM when "se" infill crit is used
  if (interest == "surrogate" & infill.mbo == "se") {
    if (iter %in% stored) {
      surr = res.mbo$models[[iter]] # the surrogate model
    } else {
      stop("The surrogate model for selected iter was not stored. Please
           store the model, change infill criteria or use interest = acquisition")
    }
  }
  # in order to proceed we need to adjust the design according to iter
  new.design = opdf[1:(nrow(design.mbo) + iter - 1), pars.mbo, drop = FALSE]
  
  # change the iters of the process & store the models
  control.mbo = setMBOControlTermination(control.mbo, iters = 1)
  control.mbo$store.model.at = 1

  if (parallel) {
    # calulate the number of cores & initiate cluster
    no.cores = parallel::detectCores() - 1
    cl = parallel::makeCluster(no.cores, type = "FORK")
    
    # FIRST, we compute the hypothetical results of the MBO without instance i (thanks to C.Molnar for the intuition in his IML book)
    # -->res.without is a nested list of length = nrow(new.design) and in each list element we have the design, the proposed point and the SM 
    res.without = parallel::parLapply(cl, 1:nrow(new.design), function(to.remove.index) {
      not.removed = setdiff(1:nrow(new.design), to.remove.index)
      # we set a seed so that each mbo process starts from the same "position" & different results
      # occurs because of different designs
      set.seed(seed)
      mbo.hypo = mlrMBO::mbo(fun = fun.mbo, 
                     design = new.design[not.removed, , drop = FALSE],
                     learner = learner.mbo,
                     control = control.mbo, 
                     more.args = more.args.mbo,
                     show.info = FALSE
      )
      opdf.hypo = as.data.frame(mbo.hypo$opt.path)
      # we extract the relevant results for the analysis and sort them in a list
      result.mbo.hypo = list(
        # design of the mbo process without instance i
        design = opdf.hypo[which(opdf.hypo$dob == 0), c(pars.mbo, y.name.mbo)],
        # proposed points and infill value (components included), dob > 0 is fine because only 1 iter is run
        # we can not store it's target value, because we can not evaluate it (only with "fake" MBO)
        df = opdf.hypo[which(opdf.hypo$dob > 0),c(pars.mbo, infill.mbo, components.mbo)],
        # surrogate model used for the initial fit
        sm = mbo.hypo$models$`1`
      )
    })
    
    #close the cluster
    parallel::stopCluster(cl)
    
  } else{#if parallel = FALSE
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
        # proposed points and infill value
        df = opdf.hypo[which(opdf.hypo$dob == 1),c(pars.mbo, infill.mbo, components.mbo)],
        # surrogate model fitted
        sm = mbo.hypo$models$`1`
      )
    })
  }
  
  # reshaping res.without --> we get 3 different objects, each one consisting of nrow(new.design) elements
  #1. a list for the designs, element [[i]] is like new.design, but without instance i
  design.without = lapply(res.without, function(x) x["design"]) %>%
    unlist(recursive = FALSE, use.names = FALSE)
  #2.a df for the proposed points, row i is the proposed point that would result if instance i is removed (infll and componenets included)
  df.without = lapply(res.without, function(x) x["df"]) %>% 
   unlist(recursive = FALSE, use.names = FALSE) %>%
   dplyr::bind_rows()
  #3. a list for the SMs, element[[i]] is the trained model without instance i
  sm.without = lapply(res.without, function(x) x["sm"]) %>%
   unlist(recursive = FALSE, use.names = FALSE)
  
  # SECOND, we conduct our analysis:
  # prepare the infos
  df.true = opdf[which(opdf$dob == iter), c(pars.mbo, infill.mbo, components.mbo)]
  dimnames(df.true)[[1]] = "true"
  pp.true = df.true[,pars.mbo, drop = FALSE]
  pp.without = df.without[,pars.mbo, drop = FALSE]
  
  # TASK 1: choice of the AF
  # 1.compare (measure the distance) pp.without & pp.true
  # -->dist2dec is the resulting distance vector
  dist2dec = rbind(pp.without, pp.true) %>%
    cluster::daisy(metric = metric) %>%
    as.matrix() %>%
    as.data.frame() %>%
    dplyr::select(true) %>%
    dplyr::rename(dist.pp.true = true)
  
  # df.all with bind the hypothetical proposed points (infill and components included) and the original one
  df.all = rbind(df.without, df.true)
  # we bind the proposed points to its corresponding distance, this is the final result of TASK 1
  res1 = cbind(df.all, dist2dec)

  # TASK 2 : influence analysis
  # 1. we add a column "pred.sm" in case interest = "surrogate", which is the prediction of pp.true
  # with the original SM
  if (interest == "surrogate" & infill.mbo == "mean") {
    df.true$pred.sm = ifelse(control.mbo$minimize, df.true[,infill.mbo], -1 * df.true[,infill.mbo])
  }
  if (interest == "surrogate" & infill.mbo == "se") {
    # surr is the surrogate model in the selected iteration
    df.true$pred.sm = predict(surr, newdata = pp.true)$data$response 
  }
  if (interest == "surrogate" & !(infill.mbo %in% c("se", "mean"))) {
    df.true$pred.sm = opdf[which(opdf$dob == iter), "mean"]
  }
  # 2. we define the true "prediction" based on the interest argument (needed to measure the influence)
  pred.true = switch(interest,
    "surrogate" = df.true[, "pred.sm"],
    "acquisition" = df.true[, infill.mbo]
  )
  # 3. we compute the prediction of pp.true in the hypoothetical BO processes, using the surrogate models
  # trained without instance i
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

  # 4. compute the influence of each design instance
  infl = switch(influence,
    "absolute" = abs(pred.true - pred.without),
    "linear" = (pred.true - pred.without)
  )
  
  # 5. compute the distance between pp.true and the points in new design
  dist2design = rbind(new.design[,pars.mbo], pp.true) %>%
    cluster::daisy(metric = metric) %>%
    as.matrix() %>%
    as.data.frame() %>%
    dplyr::select(true) %>%
    dplyr::rename(dist.pp.true = true) %>%
    dplyr::slice(1:nrow(new.design))

  # 6. train a ML model to distinguish infl and non infl instances
  #-->for that we need to create a dfwith the design instances and their influence
  # (we include the distance vector in order to have a more valuable data frame as a result)
  infl.df = cbind(infl, new.design[,pars.mbo], dist.to.des = dist2design)
  # exclude column dist.pp.true, because we do not need it in the ML model
  task = makeRegrTask(data = infl.df[,-ncol(infl.df)], target = "infl")
  lrn =  makeLearner("regr.rpart", ...) # ... additional arguments come here
  mod = train(lrn, task)
  if (plot) {
    rpart.plot::rpart.plot(mod$learner.model, roundint = FALSE)
  }

  # FINAL RESULTS OF TASK 1 AND TASK 2
  result = list(choice.of.af = res1,
                influence.analysis = list(data.frame = infl.df, model = mod)
  )
}

