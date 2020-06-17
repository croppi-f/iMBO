library(mlrMBO) 
library(iml) # Predictor, FeatureEffect


#' @title  Computes feature effects (ALE, PDP, ICE) of surrogate models and acquisition functions 
#' within a MBO process. 
#' 
#' @description In is an extension of \code{\link[iml]{FeatureEffect}}. For every stored surrogate model
#' of the MBO process the effect of one (first order) or two (second order) feature is computed & 
#' stored as FeatureEffect object. In each iter a new Predictor (interest = "surrogate") or a 
#' new PredictorAf (interest = "acquisition") is created and then the effect(s) are computed. See \code{\link[iml]{FeatureEffect}} to
#' for public methods and fields.
#' - So far, it works for Single-Objective and Single-Point Proposal only.
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
#' - 'ice' for individual conditional expectation curves (one feature only),
#' - 'pdp + ice' for partial dependence plot and ice curves within the same
#' plot.
#' @param center.at [\code{numeric(1)}]\cr
#' Value at which the plot should be centered. Not applicable for 'ale' and ignored in the case of two
#' features.
#'  @param batch.size  [\code{numeric(1)}]\cr
#' The maximum number of rows to be input the model for prediction at once. This argument
#' is passed to the Predictor or PreditorAf object. It is rather for internal purposes.
#'  @param parallel  [\code{logical(1)}]\cr
#' Should the FeatureEffect in the selected iters be measurd in parallel? Default is FALSE. If TRUE, parLapply
#' \code[parLapply] from package parallel is used (see \code{\link[parallel]{clusterApply}}). The parallel
#' version currently uses \dQuote{FORK} type and works only on Mac and Linux.
#' @return A [\code{list}], each element contatining a \code{\link[iml]{FeatureEffect}} object.
#' 

FeatureEffectMBO = function(res.mbo,
                            # which model/predictor do you want to analyse? If "surrogate" the training instances used, 
                            # if "acquisition" the seen.points are used
                            interest = "surrogate",
                            # argument of the FeatureEffect object
                            feature,
                            method = "ale",
                            grid.size = 20,
                            center.at = NULL,
                            # additional arguments to create a Predictor object
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
  # Tags, Names, Values
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
    checkmate::assertCharacter(feature, max.len = 2, unique = TRUE, any.missing = FALSE, all.missing = FALSE)
    checkmate::assertSubset(feature, pars.mbo)
  } else {
    checkmate::assertNumeric(feature, lower = 1, upper = length(pars.mbo), any.missing = FALSE, 
                  min.len = 1, max.len = 2)
  }
  #mbo automatically saves unique models, e.g. for store.model.at = c(1,2,2,5) -->
  #models 1,2,5 are stored. It also rm NAs automatically, e.g. s.m.a = c(1,2,NA) --> models 1 and 2 are stored
  checkmate::assertInteger(stored,
    lower = 1,
    any.missing = FALSE,
    min.len = 1,
    max.len = iters.mbo + 1,
    sorted = TRUE,
    unique = TRUE 
  )
  # stops if multi point proposal or multiobjective target fun
  if (control.mbo$propose.points > 1 | control.mbo$n.objectives > 1) 
    stop("FeatureEffectMBO not implemented for Multipoint-Proposal or Multiobjective function")
  # stops if infill is not one of the seven built-in infill in mlrMBO
  if (!(infill.mbo %in% c("mean", "se", "ei", "cb", "eqi", "aei", "adacb"))) 
    stop("FeatureEffectMBO only implemented for the seven built-in single obj. infill crits")
  # if clauses to guarantee that to stop if model (iters+1) has been stored and interest = "acquisition"
  if (length(stored) == 1 & max(stored) == iters.mbo + 1 & interest == "acquisition") {
    stop(paste0("Only the final model (default) has been stored.There are no candidate points in iter ", stored,
                " because the MBO has terminated after iter ", stored - 1, ". Please use interest = surrogate if you want to analyse the final model or run the MBO with others store.model.at")
    )
  } 
  if (length(stored) > 1 & max(stored) == iters.mbo + 1 & interest == "acquisition") {
    # we need to remove the last model, since there are no seen.points
    stored = stored[-length(stored)]
  }
  # in all other cases the analysis can be conducted, note that if length(stored) == 1, only 1 iter
  # can be analysed, and eventually only the surrogate model (if stored = iters.mbo + 1)
  
  # if clauses to guarantee that is compatible with the candidate points
  if (interest != "acquisition") seen.points = NULL
  if (interest == "acquisition" & !(res.mbo$control$infill.opt %in% c("focussearchSavepts", "cmaesSavepts", "eaSavepts"))) {
    stop("Candidate points have not been stored. Use interest = surrogate or run the MBO again with
         Savepts infill.opt")
  }
  if (interest == "acquisition" & res.mbo$control$infill.opt %in% c("focussearchSavepts", "cmaesSavepts", "eaSavepts")) {
    # seen points is a list
    seen.points = res.mbo$seen.points
    checkmate::assertList(seen.points, any.missing = FALSE, len = iters.mbo)
    # assert that seen.points do not contain NAs
    for (i in seq_along(seen.points)) {
      checkmate::assertDataFrame(seen.points[[i]], any.missing = FALSE, all.missing = FALSE)
    }
  # computing the AF value of the candidate points in each iteration  and then bind 
  # it to the observation to its AF value
    seen.points <- lapply(stored, function(x) {
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
      df = data.table::data.table(df, stringsAsFactors = TRUE)
      data.table::setnames(df, "ic", infill.mbo)
      df = as.data.frame(df)
    })
    # each list element is named with the corresponding iteration
    names(seen.points) = stored
  }
  
  # in order to use parLapply to use lists. Convert the designs
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
          # getFeatureEffectMBO is a sub function that computes the Predictor and the FeatureEffect. 
          # argumnts that ends with .p are used for Predictor and with .fe for FeatureEffect
          getFeatureEffectMBO(
            model.p = sm[[as.character(x)]], data.p = designs[[as.character(x)]], y.p = y.name.mbo,
            class.p = NULL, predict.fun.p = NULL, type.p = NULL, batch.size.p = batch.size,
            feature.fe = feature, method.fe = method, grid.size.fe = grid.size, center.at.fe = center.at
          )
        }
      )
      # each list element is named with the corresponding iteration
      names(result) = stored
      # close the cluster
      parallel::stopCluster(cl)
    }
    
    if (interest == "acquisition") {
      result = parallel::parLapply(
        cl,
        stored,
        function(x) {
          # getFeatureEffectAfMBO is a sub function that computes the PredictorAf and the FeatureEffect
          getFeatureEffectAfMBO(
            model.p = sm[[as.character(x)]], data.p = seen.points[[as.character(x)]], y.p = infill.mbo, batch.size.p = batch.size,
            res.mbo.p = res.mbo, design.p = designs[[as.character(x)]], iter.p = x,
            feature.fe = feature, method.fe = method, grid.size.fe = grid.size, center.at.fe = center.at
          )
        }
      )
      # each list element is named with the corresponding iteration
      names(result) = stored
      # close the cluster
      parallel::stopCluster(cl)
    }
    
  } else {# for parallel = FALSE
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


