#----------------------------------------------------------------------------#

#' @title *Fit a linear regression to a subset of the data and return predictions for (i) a (different) subset of the data and (ii) the original data.
#' 
#' @description *Linear regression constructed using: lm (...) [base R]. Observation weights (weight_var) are handled internally (method: minimise weighted loss function) in the training stage (not taken into account at the prediction stage).
#' 
#' @export
#' @param df *Datatable containing the data [data.table].
#' @param LHS *Name of the independent variable [character].
#' @param RHS *Names of all dependent variables (may incl. group_var, control_var and id_var) [character].
#' @param from *Observations on which to train an algorithm [integer].
#' @param to *Observations on which to predict [integer].
#' @param weight *Vector of observation weights [integer or numeric].
#' @param task *The nature of the prediction task  [regression (regression), classification (classification)].
#' @param alg_param *Tuning parameters [list - misc].
#' @param quiet *Verbosity settings [0 (verbose), 1 (print only key stats and progress updates), 2 (silent) - 0.5 (test and development mode)].
#' @return *Vector of predictions (predicted value of Y) [pred pred_is] and the fitted learner [learner].
#' @examples

olsalg <- function(df, LHS, RHS,from, to, weight_var=ExpLearning.Default$weight_var, 
  task=ExpLearning.Default$task, alg_param=ExpLearning.MLParameter[["ols_param"]][[1]], 
  quiet=ExpLearning.Default$quiet, ...) {

  # status
  if(quiet==0) {
    cat(paste0("OLS\n"))
    start_time_sub <- Sys.time()
  }

  # execute algorithm
  modelformula <- as.formula(paste0(LHS, " ~ ", paste(RHS, collapse=" + ")))
  linmod <- lm(modelformula,df[from,], weights=df[from, get(weight_var)])
  
  # generate predictions
  pred    <- predict(linmod,df[to,])  
  pred_is <- predict(linmod,df[from,])  

  # status
  if(quiet==0) { cat(paste0("Done in ", Sys.time() - start_time_sub, "\n")) }

  return(list(pred=pred, learner=linmod, pred_is=pred_is))
}

#----------------------------------------------------------------------------#

