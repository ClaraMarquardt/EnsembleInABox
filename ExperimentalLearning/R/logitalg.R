#----------------------------------------------------------------------------#

#' @title *Fit a logistic regression to a subset of the data and return predictions for (i) a (different) subset of the data and (ii) the original data.
#' 
#' @description *Logistic regression constructed using glm (..., family=binomial)  [base R].  Observation weights (weight_var) are handled internally (method: minimise weighted loss function) in the training stage (not taken into account at the prediction stage). 
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
#' @return *Vector of predictions (predicted probability of Y==1) [pred pred_is] and the fitted learner [learner].
#' @examples

logitalg <- function(df, LHS, RHS,from, to, weight_var=ExpLearning.Default$weight_var, 
  task=ExpLearning.Default$task, alg_param=ExpLearning.MLParameter[["logit_param"]][[1]], 
  quiet=ExpLearning.Default$quiet, ...) {
  
  # status
  if(quiet==0) {
    cat(paste0("Logistic Predictor (avg)\n"))
    start_time_sub <- Sys.time()
  }

  # specify formula
  modelformula <- as.formula(paste0(LHS, " ~ ", paste(RHS, collapse=" + ")))

  # NO weights
  if (all(df[, get(weight_var)]==1)) {
   
    # execute algorithm
    linmod <- glm(modelformula,data=df[from,],family="binomial")

  # Weights (NOTE: weights argumeng in glm function does NOT refer to case weights)
  } else {
  
    # Note: from the help for svyglm: For binomial and Poisson families use ‘family=quasibinomial()’ and
    # ‘family=quasipoisson()’ to avoid a warning about non-integer
    # numbers of successes.  The `quasi' versions of the family objects
    # give the same point estimates and standard errors and do not give
    # the warning

    # execute algorithm
    linmod <- glm(modelformula,data=df[from,],family="quasibinomial", weights=df[from, 
      get(weight_var)])

  }
  

  # generate predictions
  pred    <- c(unlist(as.data.table(predict(linmod,newdata=df[to,],type="response"))[, c(1),with=F])) 
  pred_is <- c(unlist(as.data.table(predict(linmod,newdata=df[from,],type="response"))[, c(1),with=F]))

  # status
  if(quiet==0) { cat(paste0("Done in ", Sys.time() - start_time_sub, "\n")) }

  return(list(pred=pred, learner=linmod, pred_is=pred_is))

}

#----------------------------------------------------------------------------#

