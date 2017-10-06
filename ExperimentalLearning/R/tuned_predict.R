#----------------------------------------------------------------------------#

#' @title *Generate out of sample predictions based on in-sample cross-validation.
#' 
#' @description *.
#' 
#' @export
#' @param df *Datatable containing the data [data.table].
#' @param LHS *Name of the independent variable [character].
#' @param RHS *Names of all dependent variables (may incl. group_var, control_var and id_var) [character].
#' @param cluster_var *Name of variable containing observation level cluster identifiers (integer) [character].
#' @param tuning_method *Tuning method [character].
#' @param algorithm *Name of individual algorithm to be tuned [character].
#' @param weight *Vector of observation weights [integer or numeric].
#' @param innerfold *Number of CV folds used in inner CV routine [integer].
#' @param rand_grid_iter *Number of random grid search iterations (performed if tuning_method==""rand_grid"") [integer]. 
#' @param fold_method *Method used to generate CV folds (and the train-holdout split) [cluster_count (target cluster count), obs_count (target observation count), stratification (target stratification)].
#' @param task *The nature of the prediction task  [regression (regression), classification (classification)].
#' @param custom_param *Custom tuning parameters supplied to the prediction function and used in place of the default values (if tuning_method==""no_tuning"") [list - misc]. 
#' @param losstype *Loss function used to tune the individiual learners and the ensemble neg_mse_brier, loglik, auc, accuracy, balanced_accuracy, r2].
#' @param output_path *Directory within which all model output (e.g. tuning graphs) is stored [path].
#' @param execution_id *Unique ID generated at the beginning of each model construction process - all output is associated with this ID [character].
#' @param max_core *The maximum number of cores to be used in all parallelised loops [integer].
#' @param quiet *Verbosity settings [0 (verbose), 1 (print only key stats and progress updates), 2 (silent) - 0.5 (test and development mode)].
#' @return *A list of out of sample predictions along with the optimal parameters selected.
#' @examples

tuned_predict <- function(df,LHS, RHS,algorithm, custom_param=ExpLearning.Default$custom_param, 
            innerfold=ExpLearning.Default$innerfold,rand_grid_iter=ExpLearning.Default$rand_grid_iter, 
            tuning_method=ExpLearning.Default$tuning_method, 
            max_core=ExpLearning.Default$max_core, 
            weight_var=ExpLearning.Default$weight_var, fold_var=ExpLearning.Default$weight_var,
            cluster_var=ExpLearning.Default$cluster_var, fold_method=ExpLearning.Default$fold_method, 
            losstype=ExpLearning.Default$losstype, task=ExpLearning.Default$task, 
            output_path=eval(parse(text=ExpLearning.Default$output_path)), quiet=ExpLearning.Default$quiet,
            execution_id=eval(parse(text=ExpLearning.Default$execution_id))) {


  pred           <- rep(NA, nrow(df))
  param_list     <- list()
  loss_list      <- list()
  loss_list_auc  <- list()
  
  # prepare the folds 
  fold <- unique(df[, get(fold_var)])

  for (i in 1:length(fold)) {
    
    if(quiet<2) { cat(paste0("\nFold ", i, " of ", length(fold))) }
    
    fold_out  <- which(df[, get(fold_var)] == fold[i])
    fold_in   <- which(df[, get(fold_var)]!= fold[i])
    
    df_temp <- copy(df)
    tuned <- tuning(df=df_temp,LHS=LHS,RHS=RHS,cluster_var=cluster_var, rand_grid_iter=rand_grid_iter,
                    fold_in=fold_in,fold_out=fold_out, tuning_method = tuning_method, 
                    algorithm=algorithm, weight_var = weight_var, fold_method=fold_method, 
                    task=task, losstype=losstype,custom_param=custom_param,  innerfold=innerfold,
                    quiet=quiet, output_path=output_path, execution_id=execution_id, 
                    fold_out_id=i, max_core=max_core)

    pred[fold_out]                          <- tuned$pred
    param_list[[paste0(algorithm, "_", i)]] <- tuned$param
    loss_list_auc[i]                        <- tuned$pred_loss_auc
    loss_list[i]                            <- tuned$pred_loss
    
    if (quiet==0.5) {
      cat(paste0("\nOOS* (tuned/fitted on fold!=", i, ") auc: ", round(loss_list_auc[[i]],5)))
    }

  }

  # Determine the CV loss
  cv_loss     <- mean(c(unlist(loss_list)))
  cv_loss_auc <- mean(c(unlist(loss_list_auc)))
  
  # Returns pred
  return(list(pred=pred, param=param_list, cv_loss=cv_loss, cv_loss_auc=cv_loss_auc, 
    fold_loss=c(unlist(loss_list)), fold_loss_auc=c(unlist(loss_list_auc))))

}

#----------------------------------------------------------------------------#
