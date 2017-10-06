#----------------------------------------------------------------------------#

#' @title *Fit a tree (classification regression) to a subset of the data and return predictions for (i) a (different) subset of the data and (ii) the original data.
#' 
#' @description *Tree constructed using rpart(..., method=anova) [rpart]. Observation weights (weight_var) are handled internally (method: Observation with heigher weights are selected with higher probability in the bootstrap samples used to construct the individual trees) in the training stage (not taken into account at the prediction stage). 
#' 
#' @export
#' @import rpart
#' @param df *Datatable containing the data [data.table].
#' @param LHS *Name of the independent variable [character].
#' @param RHS *Names of all dependent variables (may incl. group_var, control_var and id_var) [character].
#' @param from *Observations on which to train an algorithm [integer].
#' @param to *Observations on which to predict [integer].
#' @param weight *Vector of observation weights [integer or numeric].
#' @param alg_param *Tuning parameters [list - misc].
#' @param task *The nature of the prediction task  [regression (regression), classification (classification)].
#' @param quiet *Verbosity settings [0 (verbose), 1 (print only key stats and progress updates), 2 (silent) - 0.5 (test and development mode)].
#' @return *Vector of predictions (predicted value of Y  OR  predicted probability of Y==1) [pred pred_is] and the fitted learner [learner].
#' @examples

treealg <- function(df, LHS, RHS,from, to, weight_var=ExpLearning.Default$weight_var, 
  task=ExpLearning.Default$task, alg_param=ExpLearning.MLParameter[["tree_param"]][[1]], 
  quiet=ExpLearning.Default$quiet, ...) {

  # status
  if(quiet==0) {
    cat(paste0("Tree with ", paste(c(rbind(names(alg_param), ": ", 
      unlist(alg_param), " / ")),collapse = ""), "\n"))
    start_time_sub <- Sys.time()
  }
  
  # task
  if (task=="classification") {
      alg_param$method <- "class"
  } else if (task=="regression") {
      alg_param$method <- "anova"
  }

  # minsplit (default - 3*)
  if(is.null(alg_param$minsplit)) alg_param$minsplit <- 1*alg_param$minbucket

  # execute algorithm
  modelformula <- as.formula(paste0(LHS, " ~ ", paste(RHS, collapse=" + ")))

  thistree <- rpart(formula=modelformula,data=df[from,],method= alg_param$method,
                control=rpart.control(minbucket=alg_param$minbucket,
                maxdepth=alg_param$maxdepth,cp=alg_param$mingain, 
                min_split=alg_param$minsplit, 
                weights=df_temp[from, get(weight_var)]))
  
  # generate predictions
  pred    <- predict(thistree, newdata = df[to,mget(RHS)])
  pred_is <- predict(thistree, newdata = df[from,mget(RHS)])

  if (task=="classification") {
      pred    <- pred[,which(colnames(pred)=="1")]
      pred_is <- pred_is[,which(colnames(pred_is)=="1")]
  }


  # status
  if(!quiet) { print(paste0("Done in ",Sys.time() - start.time)) }
  
  return(list(pred=pred, learner=thistree, pred_is=pred_is))
  
}

#----------------------------------------------------------------------------#

