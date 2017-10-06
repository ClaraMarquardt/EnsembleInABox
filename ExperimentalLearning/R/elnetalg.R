#----------------------------------------------------------------------------#

#' @title *Fit an elastic net regression (classification - logit regression - ols) to a subset of the data and return predictions for (i) a (different) subset of the data and (ii) the original data.
#' 
#' @description *Elastic net constructed using glmnet(..., method=gaussian binomial) [glmnet]. Observation weights (weight_var) are handled internally (method: minimise weighted loss function) in the training stage (not taken into account at the prediction stage). 
#' 
#' @export
#' @import glmnet
#' @param df *Datatable containing the data [data.table].
#' @param LHS *Name of the independent variable [character].
#' @param RHS *Names of all dependent variables (may incl. group_var, control_var and id_var) [character].
#' @param from *Observations on which to train an algorithm [integer].
#' @param to *Observations on which to predict [integer].
#' @param weight *Vector of observation weights [integer or numeric].
#' @param alg_param *Tuning parameters [list - misc].
#' @param task *The nature of the prediction task  [regression (regression), classification (classification)].
#' @param quiet *Verbosity settings [0 (verbose), 1 (print only key stats and progress updates), 2 (silent) - 0.5 (test and development mode)].
#' @return *Vector of predictions (predicted value of Y OR predicted probability of Y==1) [pred pred_is] and the fitted learner [learner].
#' @examples

elnetalg <- function(df, LHS, RHS,from, to, weight_var=ExpLearning.Default$weight_var, 
  task=ExpLearning.Default$task, alg_param=ExpLearning.MLParameter[["elnet_param"]][[1]], 
  quiet=ExpLearning.Default$quiet, ...) {
  
  # status
  if(quiet==0) {
    cat(paste0("Elastic net with ", paste(c(rbind(names(alg_param), ": ", unlist(alg_param), " / ")),collapse = ""), "\n"))
    start_time_sub <- Sys.time()
  }

  # data
  df_temp <- copy(df)

  # task
  if (task=="classification") {
      alg_param$family <- "binomial"
      df_temp[, c(LHS):=as.factor(get(LHS))]
  } else if (task=="regression") {
      alg_param$family <- "gaussian"
      df_temp[, c(LHS):=as.numeric(get(LHS))]
  }

  # execute algorithm  
  thisnet  <-  glmnet_custom(x=data.matrix(df_temp[from,mget(RHS)]),
                  y=as.vector(df_temp[from,get(LHS)]),
                  family=alg_param$family,
                  alpha=alg_param$alpha, 
                  lambda=alg_param$lambda, 
                  weights=df_temp[from, get(weight_var)])

  s_opt   <- alg_param$lambda

  # generate predictions
  pred    <- predict(thisnet, s=s_opt, newx=as.matrix(df_temp[to,mget(RHS)]), type="response", exact=FALSE)
  pred_is <- predict(thisnet, s=s_opt, newx=as.matrix(df_temp[from,mget(RHS)]), type="response",exact=FALSE)
  

  # status
  if(quiet==0) { print(paste0("Done in ",Sys.time() - start.time)) }

  return(list(pred=pred, learner=list(thisnet, s_opt), pred_is=pred_is))
}

#----------------------------------------------------------------------------#
