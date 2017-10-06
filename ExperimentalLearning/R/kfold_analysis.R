#----------------------------------------------------------------------------#

#' @title *Analyse the predictive performance of the CV-based (out of sample) predictions derived from the model.
#' 
#' @description *.
#' 
#' @export
#' @param model_result_raw *List of outputs returned from the holdout_(opt)_outcome_predict or kfold_(opt)_outcome_predict functions [list - misc].
#' @param perm_return *List of outputs returned from the kfold_(opt)_outcome_predict functions for each of the permuted data sets [list - misc].
#' @param dict *List with the names of all key variables initialised at the beginning of each model construction process [list - character].
#' @param setting *List with key settings generated at the beginning of each model construction process [list - misc].
#' @return *List of losses, a p-value and a data.table with the original data alongside the final, CV-based (out of sample predictions) for the ensemble and each of the individual learners. 
#' @examples

kfold_analysis <- function(model_result_raw,perm_return,dict,setting) {

  # holdout df
  kfolddf <- model_result_raw$outdf[,mget(c(dict$assignment_var,dict$prediction_var,
                  dict$cluster_var,dict$id_var, dict$control_var, dict$control_var_unbalanced, dict$weight_var, grep("ind_pred", 
                  names(model_result_raw$outdf), value=T), 
                  grep("nnls", names(model_result_raw$outdf), value=T)))]

  # model loss
  mainloss <- loss(kfolddf[, get(dict$prediction_var)],kfolddf[,get(dict$assignment_var)], 
                    task=setting$task, losstype=setting$losstype, 
                    weight=kfolddf[,get(dict$weight_var)])

  # model loss auc
  mainloss_auc <- loss(kfolddf[, get(dict$prediction_var)],kfolddf[,get(dict$assignment_var)], 
                    task=setting$task,losstype="auc", weight=kfolddf[,get(dict$weight_var)])

 if (!(is.null(kfolddf[, get(paste0(dict$prediction_var, "_nnls"))][1]))) {
  
    # model loss
    mainloss_nnls <- loss(kfolddf[, get(paste0(dict$prediction_var, "_nnls"))],kfolddf[,get(dict$assignment_var)], 
                      task=setting$task, losstype=setting$losstype, weight=kfolddf[,get(dict$weight_var)])
  
    # model loss auc
    mainloss_auc_nnls <- loss(kfolddf[, get(paste0(dict$prediction_var, "_nnls"))],kfolddf[,get(dict$assignment_var)], 
                        task=setting$task,losstype="auc", weight=kfolddf[,get(dict$weight_var)])

  } else {

    mainloss_nnls     <- NULL
    mainloss_auc_nnls <- NULL

  }


  # individual learner oos losses
  ind_loss <- sapply(grep("ind_pred", names(kfolddf), value=T), function(pred_var) {

    temp_loss <- loss(kfolddf[, get(pred_var)],kfolddf[,get(dict$assignment_var)], 
    				task=setting$task, losstype=setting$losstype, 
    				weight=kfolddf[,get(dict$weight_var)])

    return(temp_loss)

  })
  
  if (length(ind_loss)>0) {
    names(ind_loss) <- paste0("mainloss_", setting$predictor)
  }

  ind_loss_auc <- sapply(grep("ind_pred", names(kfolddf), value=T), function(pred_var) {

    temp_loss <- loss(kfolddf[, get(pred_var)],kfolddf[,get(dict$assignment_var)], task=setting$task, 
                    losstype="auc", weight=kfolddf[,get(dict$weight_var)])

    return(temp_loss)

  })
  
  if (length(ind_loss_auc)>0) {
    names(ind_loss_auc) <- paste0("mainloss_auc_", setting$predictor)
  }

  perm_result <- lapply(1:length(setting$perm_test), function(i) {

    # permutation losses  
    temp <- perm_testing(kfolddf, prediction_var=dict$prediction_var, 
                 permutation=setting$permutation,  
                 assignment_var=dict$assignment_var, 
                 cluster_var=dict$cluster_var, 
                 fold_var=dict$fold_var, 
                 losstype=setting$losstype,  
                 weight=dict$weight_var, 
                 task=setting$task, 
                 within_fold=FALSE, 
                 perm_test=setting$perm_test[i],
                 quiet=setting$quiet, 
                 control_var=dict$control_var,
                 control_var_unbalanced=dict$control_var_unbalanced,  
                 perm_df_seq=perm_return[[i]], 
                 max_core=setting$max_core, 
                 perm_testing_mode="reuse_df", 
                 perm_strategy=setting$perm_st)

    return(temp)

  })

  p                      <- lapply(perm_result, function(x) x[[1]]$p)
  names(p)               <- paste0("p_", setting$perm_test)
  permloss               <- lapply(perm_result, function(x) x[[1]]$perm_loss)
  names(permloss)        <- paste0("perm_loss_", setting$perm_test)
  permloss_mean          <- lapply(perm_result, function(x) mean(x[[1]]$perm_loss))
  names(permloss_mean)   <- paste0("perm_loss_mean_", setting$perm_test)
  p_95_ci                <- lapply(perm_result, function(x) x[[1]]$p_95_ci)
  names(p_95_ci)         <- paste0("p_95_ci_", setting$perm_test)
  p_90_ci                <- lapply(perm_result, function(x) x[[1]]$p_90_ci)
  names(p_90_ci)         <- paste0("p_90_ci_", setting$perm_test)

  # permutation plot
  if ((setting$quiet < 2) & ("signal_a" %in% setting$perm_test | "baseline_balance" %in% setting$perm_test)) {
    
    if ("signal_a" %in% setting$perm_test) {
      
      # generate a permutation testing plot
      perm_plot <- suppressMessages(perm_analysis(main_loss=mainloss,
                    perm_loss=permloss$perm_loss_signal_a, 
                    pval=p,losstype=setting$losstype, pval_95_ci=p_95_ci,
                     perm_test="signal_a"))

      current_date_time_id <- paste0(as.character(format(Sys.time(), "%d_%m_%Y_%H_%M_%OS")), 
        paste0(sample(letters, 10),collapse=""))
   
      suppressMessages(ggsave(paste0(setting$output_path,"permanalysis_signal_a_", 
        setting$execution_id,"_",current_date_time_id,".pdf"),perm_plot))  
    } 

    if ("baseline_balance" %in% setting$perm_test) {
      
      # generate a permutation testing plot
      perm_plot <- suppressMessages(perm_analysis(main_loss=mainloss,
                    perm_loss=permloss$perm_loss_baseline_balance, 
                    pval=p,losstype=setting$losstype, pval_95_ci=p_95_ci, 
                    perm_test="baseline_balance"))

      current_date_time_id <- paste0(as.character(format(Sys.time(), "%d_%m_%Y_%H_%M_%OS")), 
        paste0(sample(letters, 10),collapse=""))
   
      suppressMessages(ggsave(paste0(setting$output_path,"permanalysis_baseline_balance_", 
        setting$execution_id,"_",current_date_time_id,".pdf"),perm_plot))  
    } 


  }


  # aggregate the results
  analysis    <- list.append(c(permloss_mean, model_loss=mainloss,
                  permloss, p, p_95_ci, p_90_ci))
  
  analysis$df                 <- model_result_raw$outdf[, c(paste0(dict$prediction_var, "_nnls")):=NULL]
  analysis$main_loss_auc      <- mainloss_auc
  analysis$main_loss_auc_nnls <- mainloss_auc_nnls
  analysis$main_loss_nnls     <- mainloss_nnls

  analysis <- c(analysis, ind_loss, ind_loss_auc)

  # return the results
  return(analysis)
}

#----------------------------------------------------------------------------#
