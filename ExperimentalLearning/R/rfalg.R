#----------------------------------------------------------------------------#

#' @title *Fit a random forest model (classification regression) to a subset of the data and return predictions for (i) a (different) subset of the data and (ii) the original data.
#' 
#' @description *Random forest constructed using ranger [ranger]. Observation weights (weight_var) are handled internally (method: Observation with heigher weights are selected with higher probability in the bootstrap samples used to construct the individual trees) in the training stage (not taken into account at the prediction stage).
#' 
#' @export
#' @import ranger
#' @param df *Datatable containing the data [data.table].
#' @param LHS *Name of the independent variable [character].
#' @param RHS *Names of all dependent variables (may incl. group_var, control_var and id_var) [character].
#' @param from *Observations on which to train an algorithm [integer].
#' @param to *Observations on which to predict [integer].
#' @param task *The nature of the prediction task  [regression (regression), classification (classification)].
#' @param weight *Vector of observation weights [integer or numeric].
#' @param alg_param *Tuning parameters [list - misc].
#' @param quiet *Verbosity settings [0 (verbose), 1 (print only key stats and progress updates), 2 (silent) - 0.5 (test and development mode)].
#' @return *Vector of predictions (predicted value of Y  OR  predicted probability of Y==1) [pred pred_is] and the fitted learner [learner].
#' @examples

rfalg <- function(df, LHS, RHS,from, to, weight_var=ExpLearning.Default$weight_var, 
  task=ExpLearning.Default$task, alg_param=ExpLearning.MLParameter[["rf_param"]][[1]], 
  quiet=ExpLearning.Default$quiet, ...) {

  # status
  if(quiet==0) {
    cat(paste0("Random forest with ", paste(c(rbind(names(alg_param), ": ", unlist(alg_param), " / ")),
      collapse = ""), "\n"))
    start_time_sub <- Sys.time()
  }

  # df
  df_temp <- copy(df)

  # determine data-dependent parameters
  alg_param$mtry <- ifelse(is.na(alg_param$pmtry), ceiling(sqrt(length(RHS))),  ceiling(alg_param$pmtry * length(RHS)))
  
  # task
  if (task=="classification") {
      alg_param$probability <- TRUE
      df_temp[, c(LHS):=as.factor(get(LHS))]
  } else if (task=="regression") {
      alg_param$probability <- FALSE
      df_temp[, c(LHS):=as.numeric(get(LHS))]
  }

  # execute algorithm
  thisrf <- ranger(data=df_temp[from, mget(c(LHS, RHS))], dependent.variable.name=LHS, 
                   case.weights = df_temp[from, get(weight_var)],
                   mtry = alg_param$mtry,  
                   sample.fraction = alg_param$sample.fraction, 
                   num.trees = alg_param$num.trees,
                   replace = alg_param$replace,
                   min.node.size = alg_param$min.node.size, 
                   probability = alg_param$probability,
                   importance = 'impurity'
                  )

  # generate predictions
  pred    <- predict(thisrf,  df_temp[to, mget(c(LHS, RHS))], 
            type="response")$predictions

  pred_is <- predict(thisrf,  df_temp[from, mget(c(LHS, RHS))], 
            type="response")$predictions

  if (task=="classification") {
      pred    <- pred[,which(colnames(pred)=="1")]
      pred_is <- pred_is[,which(colnames(pred_is)=="1")]
  }

  # status
  if(quiet==0) { cat(paste0("Done in ", Sys.time() - start_time_sub, "\n")) }

  return(list(pred=pred, learner=thisrf, pred_is=pred_is))

}

#----------------------------------------------------------------------------#
