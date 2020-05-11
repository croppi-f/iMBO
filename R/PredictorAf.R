# TO DO: correct documentation with import, export template
# this files are needed in order to create the object (not every function is used, but anyway)
# TO DO: all in one file 
# source("2_acquisition_function/2.2_Prediction_of_surrogate/Prediction_af/files_to_source-iml-molnar/utils-iml-molnar.R")
# source("2_acquisition_function/2.2_Prediction_of_surrogate/Prediction_af/files_to_source-iml-molnar/inferTask-iml-molnar.R")
# source("2_acquisition_function/2.2_Prediction_of_surrogate/Prediction_af/files_to_source-iml-molnar/find_y-iml-molnar.R")
# source("2_acquisition_function/2.2_Prediction_of_surrogate/Prediction_af/files_to_source-iml-molnar/Data-iml-molnar.R")

source("_iml_tools/2.2_FeatureEffectMBO/PredictorAf/files_to_source-iml-molnar/utils-iml-molnar.R")
source("_iml_tools/2.2_FeatureEffectMBO/PredictorAf/files_to_source-iml-molnar/inferTask-iml-molnar.R")
#source("_iml_tools/2.2_FeatureEffectMBO/PredictorAf/files_to_source-iml-molnar/find_y-iml-molnar.R")
source("_iml_tools/2.2_FeatureEffectMBO/PredictorAf/files_to_source-iml-molnar/Data-iml-molnar.R")

#' @title Predictor object that predicts the value of the AF in the MBO process.
#'
#' @description
#' `PredictorAf` is a extension of `Predictor`(inherits its behavior), which holds 
#' a `mlr` model and can be seen as a transformation of it. Therefore, it is a container for the acquisition function and the data.
#' of it. It can be used to understand the behavior of the Af within a MBO process, anlysed with 
#' `mlrMBO` using the tools provided in the `iml` package. So far, PreditorAf works for the
#' seven built in SingleObjective AF of mlrMBO.
#' PredictorAf is built on iml version ‘0.10.0’.Due to inherit, if latest versions of iml are released PredictorAf might generate errors. 
#'
#' @details
#' -`PredictorAf` has been created for regression tasks. For classfication taks,
#' we do not know how & if it works at all.
#' -Please use it only in combination with the `mlrMBO` package
#' @name PredictorAf
#' @export
PredictorAf = R6::R6Class("PredictorAf",
  inherit = iml::Predictor,
  public = list(
    #' @description Create a PredictorAf object
    #' @param model\cr
    #' A machine learning model from `mlr` package, used as surrogate model in the 
    #' MBO process, and needed in order to measure the Infill criteria. 
    #' @param data [data.frame]\cr
    #' The data for which we want to measure the Infill value, namely the seen points
    #' within an iteration of the MBO process. 
    #' @param y `character(1)` | [numeric] | [factor]\cr 
    #' The target vector or (preferably) the name of the target column in the `data` argument.
    #' PredictorAf tries to infer the target automatically from the model. PredictorAf objects
    #' can also be created if y is NULL.
    #' @param batch.size `numeric(1)`\cr
    #' The maximum number of rows to be input the model for prediction at once.
    #' Currently only respected for [FeatureImp], [Partial] and [Interaction].
    #' @param res.mbo [MBOSingleObjResult]\cr
    #' The results of the MBO process.
    #' @param design [data.frame]\cr
    #' The design or training set used to fit the surrogate model. Note, that such observation
    #' have Infill value of 0. The argument must include also a column with the target value.
    #' Note, that this argument corresponds to the data argument of the Predictor Obejct in `iml`.
    #' Allowed column classes are: [numeric], [factor], [integer], [ordered] and [character]. For some models the data can be extracted automatically.
    #' `PredictorAf$new()` throws an error when it can't extract the design automatically.
    #' @param iter `numeric(1)`\cr
    #' The iteration of interest within the MBO process
    initialize = function(
      # some arguments of the iml::Predictor 
      model = NULL, data = NULL, y = NULL, batch.size = 1000,
      # arguments needed for the Predictor of the Af
      res.mbo = NULL, design = NULL, iter = NULL
    ) {
      checkmate::assertClass(model, classes = "WrappedModel", null.ok = FALSE)
      checkmate::assertClass(res.mbo, classes = c("MBOSingleObjResult", "MBOResult"))
      opdf =  as.data.frame(res.mbo$opt.path)
      itmax = opdf[nrow(opdf), "dob"]
      checkmate::assertNumber(iter, lower = 1, upper = itmax)
      
      # this need to be fixed because the data argument in PredictorAf is different then 
      # in the normal Predictor obeject -- if NULL it extracts the data from model, which 
      # are theoretically our designs 
      if (is.null(design)) {
        tryCatch(
          {
            design <- prediction::find_data(model)
          },
          error = function(e) stop("Can't extract design from model, please provide via design=")
        )
      } else {
        # assertions works only for SingleObejective & SinglePoint Proposals
        checkmate::assertDataFrame(design,
          any.missing = FALSE, all.missing = FALSE,
          ncols = length(names(res.mbo$x)) + 1,
          nrows = nrow(res.mbo$final.opt.state$opt.problem$design) + iter - 1
        )
        checkmate::assertNames(names(design), identical.to = c(names(res.mbo$x), res.mbo$control$y.name))
      }
      #it's ok if y if found in model since the name is the same as in the data
      if (is.null(y)) {
        y = res.mbo$control$infill.crit$id
        # y not always needed, so ignore when not in data
        if (is.character(y) && !(y %in% names(data))) {
          y = NULL
        }
      }
      if (is.null(data)) {
        # notice that it extracts the data, but the value for the Af for those points is missing
        if (!is.null(res.mbo$seen.points)) data = res.mbo$seen.points[[iter]]
        else stop("Can't extract seen points. Provide them via data= or make sure to run the MBO with Savepts opt. methods")
      }
      self$data = Data$new(data, y = y)
      #self$class = class
      self$model = list(model)
      self$task = inferTaskFromModel(model)
      self$batch.size = batch.size
      # begin edited:
      self$res.mbo = res.mbo
      self$par.set = res.mbo$opt.path$par.set
      self$control = res.mbo$control
      self$design = list(design)
      # add error if y is not included in design does not include y
      self$iter = iter
      self$progress = getProgressAdaCB(res.mbo, iter)
      # the prdiction function is extracted from res.mbo, do not need an argument
      self$prediction.function = res.mbo$control$infill.crit$fun
      # end edited
    },
    #' @description Predict the value of the Af for newdata. Although the function
    #' works for design points, it theoretically makes no sense to predict the Af
    #' for such points, as their Af value is per definition 0. Resulting Af values
    #' will be very close to 0.
    #' @template newdata
    predict = function(newdata) {
      checkmate::assert_data_frame(newdata)
      # Makes sure it's not a data.table
      newdata = as.data.frame(newdata)
      # make sure only features are used
      newdata = newdata[, intersect(
        self$data$feature.names,
        colnames(newdata)
      ), drop = FALSE]
      # begin edited:
      prediction = self$prediction.function(
        points = newdata,
        models = self$model,
        control = self$control,
        par.set = self$par.set,
        designs = self$design,
        iter = self$iter,
        progress = self$progress,
        attributes = FALSE
      )
      prediction = data.frame(prediction)
      # end edited:
      if (!private$predictionChecked) {
        checkPrediction(prediction, newdata)
        private$predictionChecked = TRUE
      }
      # If S3 or function, we can only infer the task
      # once we see the predictions
      if (is.null(self$task)) {
        self$task = inferTaskFromPrediction(prediction)
      }
      rownames(prediction) = NULL
      data.frame(prediction, check.names = FALSE)
    },
    #' @description Print the PredictorAf object.
    print = function() {
      cat(
        "Prediction task of surrogate model:", self$task,
        ", Infill criteria:", self$res.mbo$control$infill.crit$id, "\n"
      )
    },
    
    #' @field data [data.frame]\cr
    data = NULL,
    
    #' @field model (any)\cr
    model = NULL,
    
    #' @field batch.size `numeric(1)`\cr
    #' The number of rows to be input the model for prediction at once.
    batch.size = NULL,
   
    #' @field task `character(1)`\cr
    #'   The inferred prediction task: `"classification"` or `"regression"`. So far,
    #'   you will encounter only `"regression"`.
    task = NULL,
   
    # begin edited:
    #' @field res.mbo [MBOSingleObjResult]
    #' the results of the MBO process
    res.mbo = NULL,
   
    #' @field prediction.function
    #' The acquisition function extracted from the MBO process. So far, the seven
    #' built in AF for SinglObejctive are implemented. A function which expects the 
    #' following parameters in exactly this order and return a numeric vector of 
    #' criteria values at the points: points, models, control, par.set, design, iter, 
    #' progress, attributes
    prediction.function = NULL,
   
    #' @field par.set [ParamSet]\cr
    #' the parameter set of the MBO process, describing different aspects of the
    #' objective function parameters
    par.set = NULL,
   
    #' @field control [MBOControl]\cr
    #' the control object of the MBO process
    control = NULL,
   
    #' @field design [data.frame]\cr
    #' the design used in the the specific iteration of the MBO process
    design = NULL,
   
    #' @field  iter `integer(1)`\cr
    #' The iteration of interest, in which we want to analyse the AF. Please make sure that the 
    #' corresponding surrogate model has been stored troughout the process (see store.model.at
    #' in makeMBOControl)
    iter = NULL,
   
    #' @field progress `numeric(1)`\cr
    #' A value between 0 and 1 indicating the progress of the optimization. Only needed in case of AdaCB,
    #' or other custom Adaptive Infill Criteria. The progress is calculated with intenal function 
    #' getProgressAdaCB.
    progress = NULL
  ),
  private = list(
    predictionChecked = FALSE
  )
)
