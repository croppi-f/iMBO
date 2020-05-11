#utils
# measure the Progress for AdaCB infill crit
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

# correctly extract the parameters from the ParamSet object
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

# calculates the FeatureEffect for surrogate within FeatureEffectMBO
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

# calculates the FeatureEffect for acquisition within FeatureEffectMBO
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

# In order for the PredictorAf object to be created we need some iternal function of the iml 
# package. All credits of such functions are reserved to iml authors. 
# Dowloaded from the iml Repo on Github.
# iml: 
#' @importFrom data.table data.table
Data <- R6::R6Class("Data",
                    public = list(
                      X = NULL,
                      y = NULL,
                      y.names = NULL,
                      feature.types = NULL,
                      feature.names = NULL,
                      n.features = NULL,
                      n.rows = NULL,
                      prob = NULL,
                      # Removes additional columns, stops when some are missing
                      match_cols = function(newdata) {
                        colnames_new <- colnames(newdata)
                        missing_columns <- setdiff(self$feature.names, colnames_new)
                        if (length(missing_columns) > 0) {
                          stop(sprintf("Missing columns: %s", paste(missing_columns, collapse = ", ")))
                        }
                        additional_columns <- setdiff(colnames_new, self$feature.names)
                        if (length(additional_columns) > 0) {
                          warning(sprintf("Dropping additional columns: %s", paste(additional_columns, collapse = ", ")))
                        }
                        newdata[self$feature.names]
                      },
                      sample = function(n = 100, replace = TRUE, prob = NULL, get.y = FALSE) {
                        if (is.null(prob) & !is.null(self$prob)) {
                          prob <- self$prob
                        }
                        indices <- sample.int(self$n.rows,
                                              size = n,
                                              replace = replace, prob = prob
                        )
                        if (get.y) {
                          cbind(self$X[indices, ], self$y[indices, ])
                        } else {
                          self$X[indices, ]
                        }
                      },
                      get.x = function(...) {
                        self$X
                      },
                      get.xy = function(...) {
                        cbind(self$X, self$y)
                      },
                      print = function() {
                        cat("Sampling from data.frame with", nrow(self$X), "rows and", ncol(self$X), "columns.")
                      },
                      initialize = function(X, y = NULL, prob = NULL) {
                        
                        assertDataFrame(X, all.missing = FALSE)
                        assertNamed(X)
                        if (length(y) == 1 & is.character(y)) {
                          assert_true(y %in% names(X))
                          self$y <- X[, y, drop = FALSE]
                          self$y.names <- y
                        } else if (inherits(y, "data.frame")) {
                          assertDataFrame(y, all.missing = FALSE, null.ok = TRUE, nrows = nrow(X))
                          self$y <- y
                          self$y.names <- colnames(self$y)
                          if (length(intersect(colnames(self$y), colnames(X))) != 0) {
                            stop("colnames of y and X have to be different.")
                          }
                        } else if (is.vector(y) | is.factor(y)) {
                          assert_vector(y, any.missing = FALSE, null.ok = TRUE, len = nrow(X))
                          self$y <- data.frame(.y = y)
                          self$y.names <- colnames(self$y)
                        }
                        self$X <- data.table::data.table(X[, setdiff(colnames(X), self$y.names), drop = FALSE])
                        if (ncol(self$X) == 1) stop("Only 1 feature was provided. The iml package is only useful and works for multiple features.")
                        self$prob <- prob
                        self$feature.types <- get.feature.type(unlist(lapply(self$X, class)))
                        self$feature.names <- colnames(self$X)
                        self$n.features <- ncol(self$X)
                        self$n.rows <- nrow(self$X)
                        names(self$feature.types) <- self$feature.names
                      }
                    )
)

# iml
get.feature.type <- function(feature.class) {
  checkmate::assertCharacter(feature.class)
  
  feature.types <- c(
    "numeric" = "numerical",
    "integer" = "numerical",
    "character" = "categorical",
    "factor" = "categorical",
    "ordered" = "categorical"
  )
  
  stopifnot(all(feature.class %in% names(feature.types)))
  feature.types[feature.class]
}

# iml
checkPrediction <- function(prediction, data) {
  checkmate::assert_data_frame(data)
  checkmate::assert_data_frame(prediction,
                               nrows = nrow(data), any.missing = FALSE,
                               types = c("numeric", "integerish", "factor")
  )
}

# iml, we actually need only a part of it, since PredictorAf only works for WrappedModel class
inferTaskFromModel <- function(model) {
  UseMethod("inferTaskFromModel")
}
inferTaskFromModel.WrappedModel <- function(model) {
  if (!requireNamespace("mlr")) {
    stop("Please install the mlr package.")
  }
  if (inherits(model, "WrappedModel")) {
    tsk <- mlr::getTaskType(model)
  }
  if (tsk == "classif") {
    if (model$learner$predict.type != "prob") {
      warning("Output seems to be class instead of probabilities. 
               Automatically transformed to 0 and 1 probabilities.
               You might want to set predict.type = 'prob' for Learner!")
    }
    return("classification")
  } else if (tsk == "regr") {
    return("regression")
  } else {
    stop(sprintf("mlr task type <%s> not supported", tsk))
  }
}
