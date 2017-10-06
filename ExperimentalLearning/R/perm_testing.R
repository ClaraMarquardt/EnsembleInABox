#----------------------------------------------------------------------------#

#' @title *Derive a permutation testing based p-value for a given model.
#' 
#' @description *.
#' 
#' @export
#' @param df *Datatable containing the data [data.table].
#' @param prediction_var *Name of variable containing observation level predictions (numeric) [character].
#' @param permutation *Number of permutations performed to generate the p-values [integer].
#' @param assignment_var *Name of outcome variable [character].
#' @param cluster_var *Name of variable containing observation level cluster identifiers (integer) [character].
#' @param fold_var *Name of variable containing observation level fold identifiers (integer) [character].
#' @param losstype *Loss function used to tune the individiual learners and the ensemble neg_mse_brier, loglik, auc, accuracy, balanced_accuracy, r2].
#' @param within_fold *Whether  permutations are performed within folds or across folds [logical - TRUE, FALSE]. 
#' @param task *The nature of the prediction task  [regression (regression), classification (classification)].
#' @param weight_var *Name of variable containing observation level weights (integer) [character].
#' @param perm_testing_mode *Wheter to permute the data or to (re)use a provided, (permuted) df as the basis for the permutationt test [character].
#' @param max_core *The maximum number of cores to be used in all parallelised loops [integer].
#' @param quiet *Verbosity settings [0 (verbose), 1 (print only key stats and progress updates), 2 (silent) - 0.5 (test and development mode)].
#' @param control_var *Vector of control variable names (balanced across treatment conditions ) (RHS) [character].
#' @param control_var_unbalanced *Vector of control variable names ((potentially) unabalanced across treatment conditions, i.e. random assignemnt is assumed to hold conditional on these variables) (RHS) [character].
#' @param model_predict_function *Ensemble predict function.
#' @param perm_strategy *Whether to permute the assignment variable by (a) exchanging variable values at the cluster level (entire_cluster) or (b) exchanging lvariable values within clusters (within_cluster) [character].
#' @param perm_test *(Permutation) test(s) to perform [character].
#' @return *List with p-values, permuted losses and model losses. 
#' @examples

perm_testing <- function(df, assignment_var, control_var, control_var_unbalanced, 
                  perm_df_seq=NULL, model_predict_function=NULL,
                  perm_testing_mode=ExpLearning.Default$perm_testing_mode,
                  prediction_var=ExpLearning.Default$prediction_var, 
                  permutation=ExpLearning.Default$permutation, 
                  cluster_var=ExpLearning.Default$cluster_var, 
                  fold_var=ExpLearning.Default$fold_var, 
                  losstype=ExpLearning.Default$losstype, 
                  weight_var=ExpLearning.Default$weight_var, 
                  within_fold=ExpLearning.Default$within_fold, 
                  max_core=ExpLearning.Default$max_core,
                  perm_strategy=ExpLearning.Default$perm_strategy,
                  task=ExpLearning.Default$task, 
                  perm_test=ExpLearning.Default$perm_test,
                  quiet=ExpLearning.Default$quiet) {

 # determine the model loss - non-adjusted
 model_loss <- lapply(losstype, function(x) {

    temp_loss <- loss(prediction=df[, get(prediction_var)],
                    outcome=df[,get(assignment_var)], task=task, 
                    losstype=x, weight=df[, get(weight_var)])
    return(temp_loss)

  })

  # determine the model loss - adjusted (to correct for constant predictions)
  df_mod <- copy(df)
  df_mod[, c(prediction_var):=get(prediction_var)+rnorm(nrow(df_mod), 0, 0.001)]

  model_loss_mod <- lapply(losstype, function(x) {

  temp_loss <- loss(prediction=df_mod[, get(prediction_var)],
                  outcome=df_mod[,get(assignment_var)], task=task, 
                  losstype=x, weight=df_mod[, get(weight_var)])
    
    return(temp_loss)

  })

  # obtain the permutation losses
  perm_loss_raw  <- mclapply_robust(X=1:permutation, FUN=function(i) {
    
    if (is.null(perm_df_seq) & perm_testing_mode=="default") {
        
        df_temp <- copy(df)

        perm_df <- perm(df=df_temp,assignment_var=assignment_var, cluster_var=cluster_var, 
                    fold_var=fold_var, within_fold=within_fold,
                    control_var_unbalanced=control_var_unbalanced,
                    perm_strategy=perm_strategy, 
                    control_var=control_var, 
                    perm_test=perm_test)


      } else if (!is.null(perm_df_seq) & perm_testing_mode=="reuse_df")  {

      perm_df <- copy(perm_df_seq[[i]])

    }

    # class balance
    class_balance_temp <- weighted.mean(perm_df[,get(assignment_var)],perm_df[,get(weight_var)])

    # permuted loss
    temp <- lapply(losstype, function(x) {
            
      loss_temp <- loss(perm_df[, get(prediction_var)],
          perm_df[,get(assignment_var)], task=task, 
          losstype=x, weight=perm_df[, get(weight_var)])

      loss_temp_mod <- loss(df_mod[, get(prediction_var)],
          perm_df[,get(assignment_var)], task=task, 
          losstype=x, weight=perm_df[, get(weight_var)])

      return(list(loss_temp, loss_temp_mod))
    
    })

    return(list(result=temp, df=perm_df[, get(assignment_var)], 
      class_balance=class_balance_temp))

  }, max_core=max_core)

  perm_loss       <- lapply(1:length(losstype), function(i) 
                        c(unlist(lapply(perm_loss_raw, function(x) x$result[[i]][1]))))
  perm_loss_mod_1 <- lapply(1:length(losstype), function(i) 
                        c(unlist(lapply(perm_loss_raw, function(x) x$result[[i]][2]))))

  perm_df                       <- lapply(perm_loss_raw, function(x) x$df)
  perm_df_class_balance         <- lapply(perm_loss_raw, function(x) x$class_balance)

  # determine the p-values 
  p_result <- lapply(1:length(losstype), function(x) {

    perm_loss_temp       <- perm_loss[[x]]
    perm_loss_temp_mod_1 <- perm_loss_mod_1[[x]]

    p       <- mean(perm_loss_temp>=model_loss[[x]])
    p_mod_1 <- mean(perm_loss_temp_mod_1>=model_loss_mod[[x]])

    # generate confidence intervals
    p_ci <- function(p, n=length(perm_loss_temp), ci) {

        ci_stat_list <- list(sign_0.95 = 1.96, sign_0.9 = 1.645)
        ci_stat <- ci_stat_list[[paste0("sign_", as.character(ci))]]

        ci_temp <- paste0(as.character(max(0, round(p-ci_stat*sqrt((p*(1-p))/n), 4))), " - ", 
                    as.character(min(1, round(p+ci_stat* sqrt((p*(1-p))/n), 4))))

        return(ci_temp)
    }

    p_95_ci <- p_ci(p, ci=0.95)
    p_90_ci <- p_ci(p, ci=0.90)

    p_mod_95_ci <- p_ci(p_mod_1, ci=0.95)
    p_mod_90_ci <- p_ci(p_mod_1, ci=0.90)

    return(list(p=p, p_95_ci=p_95_ci, p_90_ci=p_90_ci, p_mod=p_mod_1, p_mod_95_ci=p_mod_95_ci, 
      p_mod_90_ci=p_mod_90_ci, perm_loss=perm_loss_temp,perm_loss_mod=perm_loss_temp_mod_1,
      model_loss=model_loss[[x]], model_loss_mod=model_loss_mod[[x]]))
  
  })

  names(p_result) <- losstype

  if (quiet==0.5) {
    cat("\nPermutation Class Balance Distribution (Weighted):\n")
    print(summary(c(unlist(perm_df_class_balance))))
  }

  # return the p values, permuted losses and model losses
  return(list.append(p_result, df=perm_df, 
    df_class_balance=perm_df_class_balance))

}

#----------------------------------------------------------------------------#
