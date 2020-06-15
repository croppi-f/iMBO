library(mlrMBO)
library(iml)
library(pdp)
library(foreach)
library(doParallel)
library(randomForest)
library(rgenoud)
#' DistDec Function
#' Computes distance to decision for each parameter in each iteration. That is, 
#' it computes grid-values for the seen points from infill optimization, just like 
#' for PDPs. It then predicts the proposed point by minimizing the infill.crit with 
#' the altered seen points.  
#' 
#' @param res.mbo [\code{\link{MBOSingleObjResult}}]\cr A single-objective MBO result object 
#' @param type.distance [\code{character(1)}]\cr Should the distance to the proposed point 
#' exclude the dimension that is currently being evaluated (\code{"wopar"}) or not 
#' (\code{"wpar"}) or should the returned list contain both(\code{"both"})?
#' Default is \code{"both"}. 
#' @param attach.prop.points [\code{logical(1)}]\cr Should the actually proposed 
#' of the feature under investigation be attached? Default is \code{TRUE}. 
#' @param grid.size TODO
#' @template arg_control
#' @template arg_showinfo
#' @return [\code{list}] 
#' @export
#' @examples
#'   


dist2dec = function(res.mbo,
                   type.distance = "both", 
                   attach.prop.points = TRUE,
                   grid.size = 20
) {
  
  #input checking
  assert(check_class(res.mbo, "MBOSingleObjResult"), check_class(res.mbo, "MBOMultiObjResult"), 
         check_class(res.mbo, "MBOResult"))
  assert_character(type.distance)  
  assert_logical(attach.prop.points)
  
  # extract necessary objects from resulting mbo object res.mbo
  #####
  opt.path = as.data.frame(res.mbo$opt.path, discretes.as.factor = FALSE)
  n.init = length(opt.path$prop.type[opt.path$prop.type == "initdesign"])
  n.iter = res.mbo$control$iters * res.mbo$control$propose.points
  vars.total = colnames(res.mbo$seen.points[[1]]) # this way works for both vectorized and scalarized variables
  targets = res.mbo$control$y.name
  pars = vars.total[which(vars.total != targets)] 
  par.set = res.mbo$opt.path$par.set
  af.raw = res.mbo$control$infill.crit$fun
  seen.points = res.mbo$seen.points
  prop.points = opt.path[-(1:n.init), pars]
  #####
  
  # rewrite extracted acquisition function
  af = function(seen.points.dob, dob){
    
    af.raw(points = seen.points.dob,
           models = list(res.mbo$models[[dob]]),
           par.set = par.set,
           control = res.mbo$control,
           designs = list(opt.path[opt.path$dob < dob, c(pars, targets) ])
    )
    
  }
  
  # create grid 
  par.grid = getGrid(pars = pars, par.set = par.set, grid.size = grid.size)
  
  # compute proposed points for seen points with grid values
  af.prop = getPartialPropPts(af = af, n.iter = n.iter, pars = pars, par.set = par.set, 
                              seen.points = seen.points, grid.size = grid.size, 
                              par.grid = par.grid)
  
  # compute distance to actually proposed points
  dist.dec = switch(type.distance,
    "wpar" = compDistDecWPar(af = af, n.iter = n.iter, pars = pars, par.set = par.set, 
                             seen.points = seen.points, af.prop = af.prop, 
                             prop.points = prop.points, par.grid = par.grid),
    "wopar" = compDistDecWOPar(af = af, n.iter = n.iter, pars = pars, par.set = par.set, 
                               seen.points = seen.points, af.prop = af.prop, 
                               prop.points = prop.points, par.grid = par.grid),
    "both" = {
      dist.dec.wo.par = compDistDecWOPar(n.iter = n.iter, pars = pars, par.set = par.set, 
                                         seen.points = seen.points, af.prop = af.prop, 
                                         prop.points = prop.points, par.grid = par.grid)
      dist.dec.w.par = compDistDecWPar(n.iter = n.iter, pars = pars, par.set = par.set, 
                                       seen.points = seen.points, af.prop = af.prop, 
                                       prop.points = prop.points, par.grid = par.grid)
      list("Distance With Parameter" = dist.dec.w.par, "Distance without Parameter" = dist.dec.wo.par)
    }
  )
  
  dist.dec
}

  




getPartialPropPts <- function(af, n.iter, pars, par.set, 
                              seen.points, grid.size, par.grid) {
  
  # create list to store results
  af.prop = list()
  
  # create structure in af.prop: a nested list 
  for (dob in 1:n.iter) {
    af.prop[[dob]] = list()
    for (par in pars) {
      af.prop[[dob]][[par]] = list() 
    }
  }

  # obtain proposed points for each grid value for each parameter
  for (dob in 1:n.iter) {
    for (par.ind in 1:length(pars)) {
      par = pars[par.ind]
      if (par.set$pars[[par]]$type != "discrete") {
        # run the stuff for non-cat features
        for (grid.value in 1:grid.size) {
          seen.points.par = as.data.frame(seen.points[dob], stringsAsFactors = TRUE)
          # replace par data with grid values
          seen.points.par[,par] = rep(par.grid[[par]][grid.value], length(seen.points.par[,par]))
          # compute af values
          af.val = af(seen.points.par, dob)
          # obtain proposed point
          min.index = BBmisc::getMinIndex(af.val, ties.method = "random")
          af.prop[[dob]][[par]][[grid.value]] = seen.points.par[min.index,pars]
        }
      } else { # for cat features
        seen.points.par = as.data.frame(seen.points[dob], stringsAsFactors = TRUE)
        #replace par data with grid values (i.e. the categories)
        foreach (catg.ind = 1:length(par.grid[[par]])) %do% {
          catg = par.grid[[par]][catg.ind]
          levels = par.set$pars[[par]]$values
          seen.points.par[,par] = factor(rep(catg, length(seen.points.par[,par])), levels = levels)
          # compute af values
          af.val = af(seen.points.par, dob)
          # obtain proposed point
          min.index = BBmisc::getMinIndex(af.val, ties.method = "random")
          af.prop[[dob]][[par]][[catg]] = seen.points.par[min.index,pars]
        }
      }  
    }
  }
  af.prop
}



compDistDecWPar <- function(af, n.iter, pars, par.set, seen.points, af.prop, 
                            prop.points, par.grid) {
  
  # create nested list for all-dec-plot in app
  dist.af.dec.all = list()
  for (dob in 1:n.iter) {
    dist.af.dec.all[[dob]] = list()
    for (par in pars) {
      dist.af.dec.all[[dob]][[par]] = list() 
    }
  }
  
  # fill nested list for all-dec-plot in app
  for (dob in 1:iters) {
    af.prop.iter = af.prop[[dob]]
    names(af.prop.iter) = pars
    for (par.ind in 1:length(pars)) {
      par = pars[par.ind]
      # combine with actually proposed points
      af.prop.prop = lapply(af.prop.iter[[par]], rbind, prop.points[dob,])
      # compute distances to actually proposed points
      af.dec.dist.sgl = lapply(af.prop.prop, cluster::daisy, metric = "gower")
      dist.af.dec.all[[dob]][[par]] = data.frame("distance" = unlist(af.dec.dist.sgl), 
                                                 "parameter" = par.grid[par],
                                                 "prop.par.val" = prop.points[dob, par])
    }
  }
  dist.af.dec.all
}




compDistDecWOPar <- function(af, n.iter, pars, par.set, seen.points, af.prop, 
                             prop.points, par.grid) {
  
  
  # create nested list for all-dec-plot 
  dist.af.dec.all.wopar = list()
  for (dob in 1:n.iter) {
    dist.af.dec.all.wopar[[dob]] = list()
    for (par in pars) {
      dist.af.dec.all.wopar[[dob]][[par]] = list() 
    }
  }
  
  
  # fill nested list for all-dec-plot in app
  for (dob in 1:iters) {
    af.prop.iter = af.prop[[dob]]
    names(af.prop.iter) = pars
    for (par.ind in 1:length(pars)) {
      par = pars[par.ind]
      # combine with actually proposed points
      af.prop.prop.all = lapply(af.prop.iter[[par]], rbind, prop.points[dob,])
      # drop selected parameter
      af.prop.prop = lapply(af.prop.prop.all, dplyr::select, -par)
      # compute distances to actually proposed points (in all but the selected dimension)
      af.dec.dist.sgl = lapply(af.prop.prop, cluster::daisy, metric = "gower")
      dist.af.dec.all.wopar[[dob]][[par]] = data.frame("distance" = unlist(af.dec.dist.sgl), 
                                                       "parameter" = par.grid[par],
                                                       "prop.par.val" = prop.points[dob, par])
      }
  } 
  dist.af.dec.all.wopar
}




getGrid = function(pars, par.set, grid.size) {
  par.grid = list()
  for (par in pars) {
    if (par.set$pars[[par]][["type"]] != "discrete")
      par.grid[[par]] = seq(from = par.set$pars[[par]]$lower, to = par.set$pars[[par]]$upper, length.out = grid.size)
    else
      par.grid[[par]] = unlist(par.set$pars[[par]][["values"]])
  }
  par.grid
}


  
  

