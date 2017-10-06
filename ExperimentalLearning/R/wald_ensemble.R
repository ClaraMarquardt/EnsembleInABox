#----------------------------------------------------------------------------#

#' @title *Perform a series of Wald tests to test the significance of the group_var and control_var in a regression of the assignment var on the  group_var, control_var and unbalanced_control_var.
#' 
#' @description *.
#' 
#' @export
#' @param df *Datatable containing the data [data.table].
#' @param holdout_var *Name of variable containing observation level training vs. holdout set identifiers (TRUE, FALSE - TRUE: holdout) [character].
#' @param wald_test *Whether to conduct and report a series of Wald tests based on a linear model of the LHS as a function of all RHS variables (note: only applicable in the case of the holdout, optimised holdout, optimized CV approach) [logical]. 
#' @param assignment_var *Name of outcome variable [character].
#' @param group_var *Name of predictor variable [character].
#' @param permutation *Number of permutations performed to generate the p-values [integer].
#' @param max_core *The maximum number of cores to be used in all parallelised loops [integer].
#' @param perm_strategy *Whether to permute the assignment variable by (a) exchanging variable values at the cluster level (entire_cluster) or (b) exchanging lvariable values within clusters (within_cluster) [character].
#' @param perm_test *(Permutation) test(s) to perform [character].
#' @param cluster_var *Name of variable containing observation level cluster identifiers (integer) [character].
#' @param control_var *Vector of control variable names (balanced acroos treatment conditions ) (RHS) [character].
#' @param control_var_unbalanced *Vector of control variable names ((potentially) unabalanced acroos treatment conditions, i.e. random assignemnt is assumed to hold conditional on these variables) (RHS) [character].
#' @param quiet *Verbosity settings [0 (verbose), 1 (print only key stats and progress updates), 2 (silent) - 0.5 (test and development mode)].
#' @return *A list of p-values for each of the wald tests.
#' @examples

wald_ensemble <- function(df, assignment_var, group_var,
  control_var,control_var_unbalanced,
  wald_test=ExpLearning.Default$wald_test, 
  holdout_var=ExpLearning.Default$holdout_var, 
  permutation=ExpLearning.Default$permutation,
  perm_strategy=ExpLearning.Default$perm_strategy,
  max_core=ExpLearning.Default$max_core,
  cluster_var=ExpLearning.Default$cluster_var,
  quiet=ExpLearning.Default$quiet, perm_test="baseline_signal") {

  # intialise
  LHS           <- assignment_var
  RHS           <- c(group_var, control_var,control_var_unbalanced)
  modelformula  <- as.formula(paste0(assignment_var, " ~ ", paste(RHS, collapse=" + ")))
  from          <- which(df[, get(holdout_var)] == FALSE)
  to            <- which(df[, get(holdout_var)] == TRUE)

  linmod_list                            <- list()
  
  linmod_list$linmod_all                 <- lm(modelformula,df)
  linmod_list$linmod_is                  <- lm(modelformula,df[from,])

  if (length(to)>0) {
    linmod_list$linmod_oos               <- lm(modelformula,df[to,])
  } 

  colinear_var  <- unique(c(unlist(sapply(linmod_list, function(x) names(x$coef[is.na(x$coef)])))))
  RHS           <- setdiff(c(group_var, control_var,control_var_unbalanced), 
                      c(colinear_var))
  modelformula  <- as.formula(paste0(assignment_var, " ~ ", paste(RHS, collapse=" + ")))

  # wald test
  if (wald_test=="ind") {

      linmod_all                 <- lm(modelformula,df)
      linmod_is                  <- lm(modelformula,df[from,])
      
      vcov_is                    <- vcov(linmod_is)

      if (length(to)>0) {
        linmod_oos               <- lm(modelformula,df[to,])
        vcov_oos                 <- vcov(linmod_oos)
      }

      ## generate the hypothesis matrix (apply to all var)
      hypothesis_matrix               <- matrix(0, nrow=length(RHS), ncol=(length(RHS)+1))
      for (i in 1:length(RHS)) {
         hypothesis_matrix[i,(i+1)]   <- 1
      }
      qvec                            <- rep(0,length(RHS))

      # all 
      wald_pval_all              <- linearHypothesis(linmod_all,hypothesis_matrix,
                                        test = "F")$`Pr(>F)`[[2]]


      if (length(to)>0) {

        # insample
        # -------------
        wald_pval_is               <- linearHypothesis(linmod_is,hypothesis_matrix,
                                        test = "F", vcov=vcov(linmod_is))$`Pr(>F)`[[2]]

        # oos
        # -------------
        wald_pval_oos              <- linearHypothesis(linmod_oos,hypothesis_matrix,
                                        test = "F", vcov=vcov(linmod_oos))$`Pr(>F)`[[2]]

        # is_oos - restricted
        # -------------
        linmod_mod_rest                 <- copy(linmod_is)
  
        ## change only residuals
        linmod_mod_rest$fitted.values   <- predict(linmod_mod_rest, newdata=df[to,])
        linmod_mod_rest$residuals       <- df[to, get(LHS)] - linmod_mod_rest$fitted.values
        
        ## vcov_is_oos        
        res <- as.matrix(linmod_mod_rest$residuals)
        n   <- length(res)              
        X   <- as.matrix(cbind(c(rep(1, nrow(linmod_mod_rest$model))), 
                linmod_mod_rest$model[, c(2:ncol(linmod_mod_rest$model))]))
        k   <- ncol(X)

        vcov_is_oos_rest <- 1/(n-k) * as.numeric(t(res)%*%res) * solve(t(X)%*%X)
  
        wald_pval_is_oos_rest_result    <- linearHypothesis(linmod_mod_rest,hypothesis_matrix,
                                                test = "F", vcov=vcov_is_oos_rest)$F[2]
        wald_pval_is_oos_rest           <- pf(wald_pval_is_oos_rest_result, length(RHS), 
                                              nrow(df[to, ]), lower.tail=FALSE)
  
        # is_oos 
        # -------------
        linmod_mod                 <- copy(linmod_is)
  
        ## change residuals & Xs
        linmod_mod$fitted.values   <- predict(linmod_mod, newdata=df[to,])
        linmod_mod$residuals       <- df[to, get(LHS)] - linmod_mod$fitted.values
        linmod_mod$model           <- linmod_oos$model
        
        ## vcov_is_oos 
        res <- as.matrix(linmod_mod$residuals)
        n   <- length(res)                       
        X   <- as.matrix(cbind(c(rep(1, nrow(linmod_mod$model))), 
                linmod_mod$model[, c(2:ncol(linmod_mod$model))]))
        k   <- ncol(X)
        
        vcov_is_oos <- 1/(n-k) * as.numeric(t(res)%*%res) * solve(t(X)%*%X)
  
        wald_pval_is_oos_result    <- linearHypothesis(linmod_mod,hypothesis_matrix,
                                            test = "F", vcov=vcov_is_oos)$F[2]
        wald_pval_is_oos           <- pf(wald_pval_is_oos_result, length(RHS), 
                                            nrow(df[to, ]), lower.tail=FALSE)
        
        # is_oos  -  permuted
        # -------------
        perm_stat  <- mclapply_robust(X=1:(permutation+1), FUN=function(i) {
  
            if(i==1) {
  
              perm_df <- copy(df)
  
            } else {
              
              perm_df <- perm(df, assignment_var=assignment_var, 
                cluster_var=cluster_var, within_fold=FALSE, 
                perm_test=perm_test, 
                perm_strategy=perm_strategy, control_var=control_var, 
                control_var_unbalanced=control_var_unbalanced)
  
            } 
      
            linmod_is                  <- lm(modelformula,perm_df[from,])
            linmod_oos                 <- lm(modelformula,perm_df[to,])
            linmod_mod                 <- copy(linmod_is)
            linmod_mod$fitted.values   <- predict(linmod_mod, newdata=perm_df[to,])
            linmod_mod$residuals       <- perm_df[to, get(LHS)] - linmod_mod$fitted.values
            linmod_mod$model           <- linmod_oos$model

             ## vcov_is_oos        
             res <- as.matrix(linmod_mod$residuals)
             n   <- length(res)              
             X   <- as.matrix(cbind(c(rep(1, nrow(linmod_mod$model))), 
                      linmod_mod$model[, 
                      c(2:ncol(linmod_mod$model))]))
             k   <- ncol(X)
             
             vcov_is_oos <- 1/(n-k) * as.numeric(t(res)%*%res) * solve(t(X)%*%X)
  
             wald_F_temp               <- linearHypothesis(linmod_mod,hypothesis_matrix,
                                            vcov=vcov_is_oos, test = "F")$F[2]
            return(wald_F_temp)
  
        })
  
        model_F                       <- perm_stat[[1]]
        perm_F                        <- c(unlist(perm_stat[2:length(perm_stat)]))
        wald_pval_is_oos_perm         <- mean(perm_F>=model_F)

      } else {
         
         wald_pval_is                 <- wald_pval_all
         wald_pval_oos                <- NULL
         wald_pval_is_oos_rest        <- NULL
         wald_pval_is_oos             <- NULL
         wald_pval_is_oos_perm        <- NULL
         perm_F                       <- NULL
         model_F                      <- NULL

      }

  } else if (wald_test=="clust") {


      wald_pval_all                <- NULL
      wald_pval_is                 <- NULL
      wald_pval_oos                <- NULL
      wald_pval_is_oos_rest        <- NULL
      wald_pval_is_oos             <- NULL
      wald_pval_is_oos_perm        <- NULL
      perm_F                       <- NULL
      model_F                      <- NULL


  } else {
     
      wald_pval_all                <- NULL
      wald_pval_is                 <- NULL
      wald_pval_oos                <- NULL
      wald_pval_is_oos_rest        <- NULL
      wald_pval_is_oos             <- NULL
      wald_pval_is_oos_perm        <- NULL
      perm_F                       <- NULL
      model_F                      <- NULL

  }


  return(list(ensemble_wald_pval_all=wald_pval_all,
    ensemble_wald_pval_is=wald_pval_is,
    ensemble_wald_pval_oos=wald_pval_oos,
    ensemble_wald_pval_is_oos_rest=wald_pval_is_oos_rest,
    ensemble_wald_pval_is_oos=wald_pval_is_oos,
    ensemble_wald_pval_is_oos_perm=wald_pval_is_oos_perm,
    ensemble_wald_perm_F=perm_F,ensemble_wald_model_F=model_F))

}


#----------------------------------------------------------------------------#


 