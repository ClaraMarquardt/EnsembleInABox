#----------------------------------------------------------------------------#

#' @title .
#' 
#' @description *Mean predictor constructed using mean() [base R]. Observation weights (weight_var) are not taken into account (i.e. no training stage).
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

interceptalg <- function(df, LHS, RHS,from, to, weight_var=ExpLearning.Default$weight_var, 
  task=ExpLearning.Default$task, alg_param=ExpLearning.MLParameter[["intercept_param"]][[1]], 
  quiet=ExpLearning.Default$quiet, ...) {
  
   # status
  if(quiet==0) {
    cat(paste0("Intercept Predictor (avg)\n"))
    start_time_sub <- Sys.time()
  }

  # generate predictions
  thisintercept <- 1

  pred    <- rep(thisintercept,nrow(df[to,]))
  pred_is <- rep(thisintercept,nrow(df[from,]))

  # status
  if(quiet==0) { cat(paste0("Done in ", Sys.time() - start_time_sub, "\n")) }

  return(list(pred=pred, learner=thisintercept, pred_is=pred_is))
}

#----------------------------------------------------------------------------#
