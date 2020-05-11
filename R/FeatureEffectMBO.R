# WARNING: if library dplyr library is used, errors might occur in Data-iml-molnar.R 
# because base::setdiff() and base::intersect() are used, which are masked from dplyr. Please make sure
# that dplyr is not attached. TO DO: base::setdiff() and base::intersect() where needed


#' @title  Computes feature effects (ALE, PDP, ICE) of surrogate models and acquisition functions 
#' within a MBO process. 
#' 
#' @description In is an extension of \code{\link[iml]{FeatureEffect}}. For every stored surrogate model
#' of the MBO process the effect of one (first order) or two (second order) feature is computed & 
#' stored as FeatureEffect object. For the selected iters a new Predictor (interest = "surrogate") or a 
#' new PredictorAf (interest = "acquisition") is created and then the effect(s) are computed. For publich methods
#' applicable see \code{\link[iml]{FeatureEffect}}.
#' - So far, it works for Single.Objective and Single-Point Proposal only.
#' - FeatureEffectMBO only considers the iters, for which surrogate model has been stored 
#' (see \code{\link[mlrMBO]{makeMBOControl}} \code{store.model.at}).
#' - Please use it only in combination with the `mlrMBO` package.
#' - built on iml version ‘0.10.0’. Due to inherit, if latest versions of iml are installed it might generate errors. 
#' - built on mlrMBO version 1.1.4. If latest versions of iml are installed it might generate errors. 
#' @param res.mbo [\code{MBOSingleObjResult}]\cr
#' Results of the MBO process.
#' @param interest [\code{character(1)}]\cr
#' The \dQuote{model} for which the effects should be measured. Choose \code{surrogate} if the surrogate
#' model should be analysed (corresponds to the analysis of a typical machine learning model) or
#' \code{acquisition} for the acquisition function used in the process. So far, only the seven built in
#' infill criteria in mlrMBO are implemented [\code{\link[mlrMBO]{infillcrits}}].
#' @param feature [\code{character(1) | character(2) | numeric(1) | numeric(2)}]\cr
# #' @template feature(character(1) | character(2) | numeric(1) | numeric(2))
#'   The feature name or index for which to compute the effects.
#' @param grid.size [\code{numeric(1) | numeric(2)}]\cr
#' The size of the grid for evaluating the predictions. Note, that since in each iteration
#' different data (seen points) are used, the grid values between iters will differ.
#' @param method [\code{character(1)}]\cr
#' - 'ale' for accumulated local effects,
#' - 'pdp' for partial dependence plot,
#' - 'ice' for individual conditional expectation curves,
#' - 'pdp + ice' for partial dependence plot and ice curves within the same
#' plot.
#' @param center.at [\code{numeric(1)}]\cr
#' Value at which the plot should be centered. Ignored in the case of two
#' features.
#'  @param batch.size  [\code{numeric(1)}]\cr
#' The maximum number of rows to be input the model for prediction at once. This argument
#' is passed to the Predictor or PreditorAf object. Currently only respected for \code{\link[iml]{FeatureImp}},
#' \code{\link[iml]{Partial}}, \code{\link[iml]{Interaction}}.
#' @return A [\code{list}], each element contatining a \code{\link[iml]{FeatureEffect}} object.
#' @param parallel  [\code{logical(1)}]\cr
#' Should the FeatureEffect in the selected iters be measurd in parallel? Default is FALSE. If TRUE, parLapply
#' \code[parLapply] from package parallel is used (see \code{\link[parallel]{clusterApply}}). The parallel
#' version currently uses \dQuote{FORK} type and works only on Mac and Linux.

library(mlrMBO) #actually don't needed
library(iml) # Predictor, FeatureEffect
library(data.table) # data.table and setnames

FeatureEffectMBO = function(res.mbo,
                            # which model/predictor do you want to analyse? If "surrogate" the training instances used, 
                            # if "acquisition" the seen.points are used
                            interest = "surrogate",
                            # argument of the FeatureEffect object
                            feature,
                            method = "ale",
                            grid.size = 20,
                            center.at = NULL,
                            # additional arguments to create a Predictor obejct
                            batch.size = 1000,
                            parallel = FALSE
) {
  
  # extract infos from from res.mbo
  # data frames
  opdf =  as.data.frame(res.mbo$opt.path)
  design.mbo = res.mbo$final.opt.state$opt.problem$design
  # surrogate models, control object, par.set, und acq.fun
  sm = res.mbo$models
  control.mbo = res.mbo$control
  par.set.mbo = res.mbo$opt.path$par.set
  acq.fun = res.mbo$control$infill.crit$fun
  # Tags, Names, Integers
  infill.mbo = res.mbo$control$infill.crit$id
  y.name.mbo = res.mbo$control$y.name
  pars.mbo = getParam(par.set.mbo)
  # iters.mbo are the actually done "iterations" in the process, corresponds to dob in the opdf
  iters.mbo = opdf[nrow(opdf), "dob"]
  # stored are the stored models in the process, not every iter has a stored model, inly iters with
  # stored models can be analyzed
  stored = sort(as.integer(names(res.mbo$models)))
  
  # a & c
  checkmate::assertClass(res.mbo, classes = c("MBOSingleObjResult", "MBOResult"))
  checkmate::assertChoice(interest, c("surrogate", "acquisition"))
  checkmate::assertChoice(method, c("ale", "pdp", "ice", "pdp+ice"))
  checkmate::assertLogical(parallel, len = 1, any.missing = FALSE)
  if (is.character(feature)) {
    assertCharacter(feature, max.len = 2, unique = TRUE, any.missing = FALSE, all.missing = FALSE)
    assertSubset(feature, pars.mbo)
  } else {
    assertNumeric(feature, lower = 1, upper = length(pars.mbo), any.missing = FALSE, 
                  min.len = 1, max.len = 2)
  }
  
  assertInteger(stored,
    lower = 1,
    any.missing = FALSE,
    min.len = 1,
    max.len = iters.mbo + 1,
    sorted = TRUE,
    unique = TRUE #mbo automatically saves unique models, e.g. s.m.a = c(1,2,2,5)
                  #models 1,2,5 are stored. it also rm NAs automatically, e.g. s.m.a = c(1,2,NA)
  )
  # stops if multi point proposal or multiobjective target fun
  if (control.mbo$propose.points > 1 | control.mbo$n.objectives > 1) 
    stop("FeatureEffectMBO not implemented for Multipoint-Proposal or Multiobjective function")
  # stops if infill is not one of the seven built-in infill in mlrMBO
  if (!(infill.mbo %in% c("mean", "se", "ei", "cb", "eqi", "aei", "adacb"))) 
    stop("inflInst only implemented for the seven built-in single obj. infill crits")
  # if clauses to guarantee that to stop if 
  if (length(stored) == 1 & max(stored) == iters.mbo + 1 & interest == "acquisition") {
    #1.case
    stop(paste0("Only the final model (default) has been stored.There are no seen points in iter ", stored,
                " because the MBO has terminated after iter ", stored - 1, ". Please use interest = surrogate if you want to analyse the final model or run the MBO with others store.model.at")
    )
  } 
  if (length(stored) > 1 & max(stored) == iters.mbo + 1 & interest == "acquisition") {
    #2.case
    # we need to remove the last model, since there are no seen.points
    stored = stored[-length(stored)]
  }
  # in all other cases the analysis can be conducted, note that if length(stored) == 1, only 1 iter
  # can be analysed, and eventually only the surrogate model (if stored = iters.mbo + 1)
  
  # if clauses to guarantee that it works with "seen.points" if stored, but also if not stored
  if (interest != "acquisition") seen.points = NULL
  if (interest == "acquisition" & !(res.mbo$control$infill.opt %in% c("focussearchSavepts", "cmaesSavepts", "eaSavepts"))) {
    stop("Seen points have not been stored. Use interest = surrogate or run the MBO again with
         Savepts infill.opt")
  }
  if (interest == "acquisition" & res.mbo$control$infill.opt %in% c("focussearchSavepts", "cmaesSavepts", "eaSavepts")) {
    # seen points is a list
    seen.points = res.mbo$seen.points
    assertList(seen.points, any.missing = FALSE, len = iters.mbo)
    # NOTE:- works if stored seen points in res.mbo do not have AF value
    #      - we care about the seen points, for which we stored the surrogate models, others are useless
    seen.points <- lapply(stored, function(x) {
      #assertDataFrame(seen.points[[x]], all.missing = FALSE, any.missing = FALSE)
      ic = acq.fun(
        points = seen.points[[x]],
        models = list(sm[[as.character(x)]]),
        control = control.mbo,
        par.set = par.set.mbo,
        designs = list(opdf[1:(nrow(design.mbo) + x - 1), c(pars.mbo, y.name.mbo)]),
        iter = x,
        progress = getProgressAdaCB(res.mbo = res.mbo, iter = x),
        attributes = FALSE
      )
      df = cbind(seen.points[[x]], ic)
      df = data.table::data.table(df)
      data.table::setnames(df, "ic", infill.mbo)
      df = as.data.frame(df)
    })
    names(seen.points) = stored
  }
  
  #seen.points <<- seen.points
  # # If "seen" but too many obs either quit, unchanged or change method is possible 
  # if (interest == "acquisition") {
  #   seen.point.iter = sapply(seen.points, function(x) nrow(x))
  #   threshold = 15000
  #   if (any(seen.point.iter > threshold)) {
  #     quit = askYesNo("The seen points in at least one iteration are more than 15000. \n Calculation might take a while and ICE or PDP+ICE plots might be chaotic.\n Do you want to quit?")
  #     if (quit == TRUE) {
  #       stop("Too many seen points might generate chaotic plots. You quit. Blame yourself. Again.")
  #     } else{
  #       if (method %in% c("ice", "pdp+ice")) {
  #         change.method = askYesNo("Do you want to switch to PDP?")
  #         if (change.method == TRUE) {
  #           method = "pdp"
  #         } else{
  #           method = method
  #         }
  #       }
  #     }
  #   }
  # }
  
  # in order to use parLapply and interest we need to use lists. Convert the designs
  # of each iter in a list 
  designs = lapply(stored, function(x){
    opdf[1:(nrow(design.mbo) + x - 1), c(pars.mbo, y.name.mbo)]
  })
  names(designs) = stored
  
  if (parallel) {
    # calulate the number of cores & initiate cluster
    no.cores = parallel::detectCores() - 1
    cl = parallel::makeCluster(no.cores, type = "FORK")
    
    if (interest == "surrogate") {
      result = parallel::parLapply(
        cl,
        stored,
        function(x) {
          getFeatureEffectMBO(
            model.p = sm[[as.character(x)]], data.p = designs[[as.character(x)]], y.p = y.name.mbo,
            class.p = NULL, predict.fun.p = NULL, type.p = NULL, batch.size.p = batch.size,
            feature.fe = feature, method.fe = method, grid.size.fe = grid.size, center.at.fe = center.at
          )
        }
      )
      # close the cluster
      parallel::stopCluster(cl)
    }
    
    if (interest == "acquisition") {
      result = parallel::parLapply(
        cl,
        stored,
        function(x) {
          getFeatureEffectAfMBO(
            model.p = sm[[as.character(x)]], data.p = seen.points[[as.character(x)]], y.p = infill.mbo, batch.size.p = batch.size,
            res.mbo.p = res.mbo, design.p = designs[[as.character(x)]], iter.p = x,
            feature.fe = feature, method.fe = method, grid.size.fe = grid.size, center.at.fe = center.at
          )
        }
      )
      # close the cluster
      parallel::stopCluster(cl)
    }
    
  } else {
    # results, depends on the interest
    if (interest == "surrogate") {
      result <- lapply(
        stored,
        function(x) {
          getFeatureEffectMBO(
            model.p = sm[[as.character(x)]], data.p = designs[[as.character(x)]], y.p = y.name.mbo,
            class.p = NULL, predict.fun.p = NULL, type.p = NULL, batch.size.p = batch.size,
            feature.fe = feature, method.fe = method, grid.size.fe = grid.size, center.at.fe = center.at
          )
        }
      )
      names(result) = stored
    }
    if (interest == "acquisition") {
      result <- lapply(
        stored,
        function(x) {
          getFeatureEffectAfMBO(
            model.p = sm[[as.character(x)]], data.p = seen.points[[as.character(x)]], y.p = infill.mbo, batch.size.p = batch.size,
            res.mbo.p = res.mbo, design.p = designs[[as.character(x)]], iter.p = x,
            feature.fe = feature, method.fe = method, grid.size.fe = grid.size, center.at.fe = center.at
          )
        }
      )
      names(result) = stored
    }
  }
  return(result)
}
############################ getFeatureEffect ##################################

getFeatureEffectMBO = function(model.p, data.p, y.p, class.p, predict.fun.p, type.p, batch.size.p, 
                               feature.fe, method.fe, grid.size.fe, center.at.fe
) {
  # 1. create a Predictor object
  pred = iml::Predictor$new(
    model = model.p, data = data.p, y = y.p, class = class.p, predict.fun = predict.fun.p,
    type = type.p, batch.size = batch.size.p
  )
  # 2. estimate the feature effect
  effect = iml::FeatureEffect$new(
    predictor = pred, feature = feature.fe, method = method.fe, grid.size = grid.size.fe,
    center.at = center.at.fe
  )
}

############################## getFeatureEffectAf ##############################
getFeatureEffectAfMBO = function(model.p, data.p, y.p, batch.size.p, res.mbo.p, design.p, iter.p,
                                 feature.fe, method.fe, grid.size.fe, center.at.fe
) {
  # 1. create a Predictor object
  pred = PredictorAf$new(
    model = model.p, data = data.p, y = y.p, batch.size = batch.size.p,
    res.mbo = res.mbo.p, design = design.p, iter = iter.p
  )
  # 2. estimate the feature effect
  effect = iml::FeatureEffect$new(
    predictor = pred, feature = feature.fe, method = method.fe, grid.size = grid.size.fe,
    center.at = center.at.fe
  )
}

########################## getProgressAdaCB #################################
# this function is used to calculate the progress in the given iteration if 
# infill crit AdaCB is used
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


