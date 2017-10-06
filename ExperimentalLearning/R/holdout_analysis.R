#----------------------------------------------------------------------------#

#' @title *Analyse the predictive performance of the out of sample predictions derived from the model.
#' 
#' @description *.
#' 
#' @export
#' @param model_result_raw *List of outputs returned from the holdout_(opt)_outcome_predict or kfold_(opt)_outcome_predict functions [list - misc].
#' @param dict *List with the names of all key variables initialised at the beginning of each model construction process [list - character].
#' @param setting *List with key settings generated at the beginning of each model construction process [list - misc].
#' @return *List of losses, a p-value and a data.table with the original data alongside the final, out of sample predictions for the ensemble and each of the individual learners. 
#' @examples

holdout_analysis <- function(model_result_raw,dict,setting) {

  # holdout df
  holdoutdf <- model_result_raw$outdf[,mget(c(dict$assignment_var,dict$prediction_var,dict$group_var,
                  dict$cluster_var,dict$id_var, dict$control_var, dict$control_var_unbalanced,
                  dict$weight_var, grep("ind_pred", 
                  names(model_result_raw$outdf), value=T), grep("nnls", 
                  names(model_result_raw$outdf), value=T)))]

  # model loss
  mainloss <- loss(holdoutdf[, get(dict$prediction_var)],holdoutdf[,get(dict$assignment_var)], 
                    task=setting$task, losstype=setting$losstype, weight=holdoutdf[,get(dict$weight_var)])

  # model loss auc
  mainloss_auc <- loss(holdoutdf[, get(dict$prediction_var)],holdoutdf[,get(dict$assignment_var)], 
                    task=setting$task,losstype="auc", weight=holdoutdf[,get(dict$weight_var)])

  if (!(is.null(holdoutdf[, get(paste0(dict$prediction_var, "_nnls"))][1]))) {
  
    # model loss
    mainloss_nnls <- loss(holdoutdf[, get(paste0(dict$prediction_var, "_nnls"))],holdoutdf[,get(dict$assignment_var)], 
                      task=setting$task, losstype=setting$losstype, weight=holdoutdf[,get(dict$weight_var)])
  
    # model loss auc
    mainloss_auc_nnls <- loss(holdoutdf[, get(paste0(dict$prediction_var, "_nnls"))],holdoutdf[,get(dict$assignment_var)], 
                          task=setting$task,losstype="auc", weight=holdoutdf[,get(dict$weight_var)])

  } else {

    mainloss_nnls     <- NULL
    mainloss_auc_nnls <- NULL

  }

  # individual learner oos losses
  ind_loss <- sapply(grep("ind_pred", names(holdoutdf), value=T), function(pred_var) {

    temp_loss <- loss(holdoutdf[, get(pred_var)],holdoutdf[,get(dict$assignment_var)], task=setting$task, 
                    losstype=setting$losstype, weight=holdoutdf[,get(dict$weight_var)])

    return(temp_loss)

  })

  if (length(ind_loss)>0) {
    names(ind_loss) <- paste0("mainloss_", setting$predictor)
  }

  ind_loss_auc <- sapply(grep("ind_pred", names(holdoutdf), value=T), function(pred_var) {

    temp_loss <- loss(holdoutdf[, get(pred_var)],holdoutdf[,get(dict$assignment_var)], task=setting$task, 
                    losstype="auc", weight=holdoutdf[,get(dict$weight_var)])

    return(temp_loss)

  })

  if (length(ind_loss_auc)>0) {
    names(ind_loss_auc) <- paste0("mainloss_auc_", setting$predictor)
  }

  # permutation losses  
  perm_result <- lapply(setting$perm_test, function(x) {

    temp <- perm_testing(df=holdoutdf, prediction_var=dict$prediction_var, 
                    permutation=setting$permutation,  
                    assignment_var=dict$assignment_var, 
                    cluster_var=dict$cluster_var, 
                    fold_var=dict$fold_var, 
                    losstype=setting$losstype,  
                    weight=dict$weight_var, 
                    task=setting$task, 
                    model_predict_function=model_result_raw$model_predict, 
                    perm_test=x, 
                    control_var=dict$control_var,
                    control_var_unbalanced=dict$control_var_unbalanced,
                    max_core=setting$max_core, 
                    within_fold=FALSE, 
                    quiet=setting$quiet, 
                    perm_strategy=setting$perm_strategy)

    # temp_result$perm_df_seq     <- perm_result$df
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
  perm_df_class_balance               <- lapply(perm_result, function(x) c(unlist(x$df_class_balance)))
  names(perm_df_class_balance)        <- paste0("perm_df_class_balance_", setting$perm_test)

  if (setting$ind_learner_p==TRUE) {

    perm_result_learner <- lapply(setting$predictor, function(x)  {
     
       temp <- perm_testing(holdoutdf, prediction_var=paste0("ind_pred.", x), 
                       permutation=setting$permutation,  
                       assignment_var=dict$assignment_var, 
                       cluster_var=dict$cluster_var, 
                       fold_var=dict$fold_var, 
                       losstype=setting$losstype,  
                       weight=dict$weight_var, 
                       task=setting$task, 
                       control_var=dict$control_var,
                       control_var_unbalanced=dict$control_var_unbalanced,
                       max_core=setting$max_core, 
                       within_fold=FALSE, 
                       quiet=setting$quiet, 
                       model_predict_function=function() {
                        do.call(model_result_raw$model_predict, list(learner=x))}, 
                       perm_strategy=setting$perm_strategy, 
                       perm_test="signal_a")
    
        return(temp)

     })

    p_learner                      <- lapply(perm_result_learner, function(x) x[[1]]$p)
    names(p_learner)               <- paste0("p_ind_", setting$predictor)
    permloss_learner               <- lapply(perm_result_learner, function(x) x[[1]]$perm_loss)
    names(permloss_learner)        <- paste0("perm_loss_ind_", setting$predictor)
    permloss_mean_learner          <- lapply(perm_result_learner, function(x) mean(x[[1]]$perm_loss))
    names(permloss_mean_learner)   <- paste0("perm_loss_ind_mean_", setting$predictor)
    p_95_ci_learner                <- lapply(perm_result_learner, function(x) x[[1]]$p_95_ci)
    names(p_95_ci_learner)         <- paste0("p_95_ci_ind_", setting$predictor)
    p_90_ci_learner                <- lapply(perm_result_learner, function(x) x[[1]]$p_90_ci)
    names(p_90_ci_learner)         <- paste0("p_90_ci_ind_", setting$predictor)
 

  } else {
     
     p_learner             <- list()
     permloss_learner      <- list()
     permloss_mean_learner <- list()
     p_95_ci_learner       <- list()
     p_90_ci_learner       <- list()

  }

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
                  permloss, perm_df_class_balance, p, p_95_ci, p_90_ci, 
                  permloss_mean_learner, permloss_learner, p_learner, p_95_ci_learner, 
                  p_90_ci_learner))

  analysis$df                 <- model_result_raw$outdf[, c(paste0(dict$prediction_var, "_nnls")):=NULL]
  analysis$main_loss_auc      <- mainloss_auc
  analysis$main_loss_auc_nnls <- mainloss_auc_nnls
  analysis$main_loss_nnls     <- mainloss_nnls

  analysis <- c(analysis, ind_loss, ind_loss_auc)
  
  # print the key OOS results
  if (setting$quiet==0.5) {

    cat("\n## OS LOSS - Ind Learner \n")
    
    cat(paste0("\n# LOSS: ",setting$losstype,"\n\n")) 
    inv_lapply(1:length(ind_loss), function(x) cat(paste0(names(ind_loss)[x], ":", 
      round(ind_loss[x],5), "\n")))
    cat(paste0("\n# LOSS: ","AUC","\n\n"))  
    inv_lapply(1:length(ind_loss), function(x) cat(paste0(names(ind_loss_auc)[x], ":", 
      round(ind_loss_auc[x],5), "\n")))

    cat("\n## OS LOSS - Ensemble \n")
    
    cat(paste0("\n# LOSS: ",setting$losstype,"\n\n"))   
    cat(paste0("ensemble:", round(mainloss,4)))
    cat("\n")
    cat(paste0("\n# LOSS: ","AUC","\n\n"))  
    cat(paste0("ensemble:", round(mainloss_auc,4)))
    cat("\n\n")

  }



  return(analysis)
}

#----------------------------------------------------------------------------#
