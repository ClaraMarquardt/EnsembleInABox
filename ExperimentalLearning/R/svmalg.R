#----------------------------------------------------------------------------#

#' @title .
#' 
#' @description .
#' 
#' @export
#' @import e1071
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

svmalg <- function(df, LHS, RHS,from, to, weight_var=ExpLearning.Default$weight_var, 
  task=ExpLearning.Default$task, alg_param=ExpLearning.MLParameter[["svm_param"]][[1]], 
  quiet=ExpLearning.Default$quiet, ...) {

  # status
  if(quiet==0) {
    cat(paste0("SVM forest with ", paste(c(rbind(names(alg_param), ": ", unlist(alg_param), " / ")),
      collapse = ""), "\n"))
    start_time_sub <- Sys.time()
  }

  # df
  df_temp <- copy(df)
  
  # determine data-dependent parameters
  alg_param$gamma <- ifelse(is.na(alg_param$gamma), 1/length(RHS), alg_param$gamma )

  # task
  if (task=="classification") {
      alg_param$probability <- TRUE
      alg_param$method      <- "C-classification"
      df_temp[, c(LHS):=as.factor(get(LHS))]  
  } else if (task=="regression") {
      alg_param$probability <- FALSE
      alg_param$method      <- "eps-regression"
      df_temp[, c(LHS):=as.numeric(get(LHS))] 
  }

  # execute algorithm
  modelformula <- as.formula(paste0(LHS, " ~ ", paste(RHS, collapse=" + ")))

  thissvm <- svm(formula=modelformula, data=df_temp[from, mget(c(LHS, RHS))], 
                   probability = alg_param$probability,
                   method = alg_param$method,
                   kernel = alg_param$kernel,
                   cost = alg_param$cost,
                   gamma = alg_param$gamma )

  # generate predictions
  if (task=="classification") {
   
    pred    <- predict(thissvm,  df_temp[to, mget(c(LHS, RHS))], probability=TRUE)
    pred    <- attr(pred, "probabilities")[,which(colnames(attr(pred, "probabilities"))==1)]

    pred_is    <- predict(thissvm,  df_temp[from, mget(c(LHS, RHS))], probability=TRUE)
    pred_is    <- attr(pred_is, "probabilities")[,which(colnames(attr(pred_is, "probabilities"))==1)]

  } else if (task=="regression") {

    pred       <- predict(thissvm,  df_temp[to, mget(c(LHS, RHS))])
    pred_is    <- predict(thissvm,  df_temp[from, mget(c(LHS, RHS))])

  }

  # status
  if(quiet==0) { cat(paste0("Done in ", Sys.time() - start_time_sub, "\n")) }

  return(list(pred=pred, learner=thissvm, pred_is=pred_is))

}

#----------------------------------------------------------------------------#
