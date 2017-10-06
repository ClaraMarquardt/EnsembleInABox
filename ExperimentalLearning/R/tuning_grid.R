#----------------------------------------------------------------------------#

#' @title .
#' 
#' @description .
#' 
#' @export
#' @param df *Datatable containing the data [data.table].
#' @param LHS *Name of the independent variable [character].
#' @param RHS *Names of all dependent variables (may incl. group_var, control_var and id_var) [character].
#' @param weight_var
#' @param alg_param_grid 
#' @param algorithm *Name of individual algorithm to be tuned [character].
#' @param task *The nature of the prediction task  [regression (regression), classification (classification)].
#' @param losstype *Loss function used to tune the individiual learners and the ensemble neg_mse_brier, loglik, auc, accuracy, balanced_accuracy, r2].
#' @param max_core *The maximum number of cores to be used in all parallelised loops [integer].
#' @param quiet *Verbosity settings [0 (verbose), 1 (print only key stats and progress updates), 2 (silent) - 0.5 (test and development mode)].
#' @return .
#' @examples

tuning_grid <- function(df, LHS, RHS, algorithm, alg_param_grid, weight_var=ExpLearning.Default$weight_var, 
            task=ExpLearning.Default$task, max_core=ExpLearning.Default$max_core,
            losstype=ExpLearning.Default$losstype,
            quiet=ExpLearning.Default$quiet) {


  # initialise 
  dv_vector      <- df[,get(LHS)]
      
  ifold          <- unique(df[, get("inner_fold")])

  phat           <- matrix(rep(NA, nrow(df) * length(alg_param_grid)), ncol = 
                      length(alg_param_grid))
  
  mean_loss_fold <- matrix(rep(NA, length(ifold) * length(alg_param_grid)), ncol = 
                      length(alg_param_grid))

  # if xgb - disable parallelisation
  if (algorithm=="xgb") noparallel <- TRUE else noparallel <- FALSE

  # loop over inner loops
  phat_list <- mclapply_robust(X=1:length(ifold), FUN=function(ii) {

    # initialise
    ifold_out  <- which(df[, get("inner_fold")] == ifold[ii])
    ifold_in   <- which(df[, get("inner_fold")] != ifold[ii])        
      
    phat_temp <- copy(phat)[ifold_out,]

    # loop
    for (j in 1:length(alg_param_grid)) {

      if (quiet==0.5 & j %in% seq(1,length(alg_param_grid),5)) cat(sprintf("Tuning - %d/%d\n", 
          j, length(alg_param_grid)))

      thispred <- alg(algorithm)$algorithm(df=df,LHS=LHS,RHS=RHS, from=ifold_in,to=ifold_out, 
                      weight_var = weight_var, alg_param=alg_param_grid[[j]], task=task, 
                      quiet=quiet)
            
      phat_temp[,j] <- thispred$pred

    }

    return(phat_temp)

  }, max_core=max_core, quiet=FALSE, noparallel=noparallel)



  for (ii in 1:length(ifold)) {

    ifoldout                   <- which(df[, get("inner_fold")] == ifold[ii])
    phat[ifoldout,]            <- phat_list[[ii]]
    mean_loss_fold[ii, ]       <- apply(phat[ifoldout,], 2, function(x) loss(x, dv_vector[ifoldout], 
                                    weight=df[ifoldout,get(weight_var)], task=task, losstype=losstype))

  }
   
  # choose optimal parameters 
  mean_loss   <- apply(mean_loss_fold, 2, function(x) mean(x))

  pick_me     <- which.max(mean_loss)
  pick_param  <- alg_param_grid[[pick_me]]


  # return
  return(list(phat, mean_loss, pick_me, pick_param))

}

#----------------------------------------------------------------------------#



