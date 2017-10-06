#----------------------------------------------------------------------------#

#' @title *Generate a cluster-level holdout set.
#' 
#' @description *Holdout set construction optionally targeting (i) the cluster count (ii) the sample size or (iii) stratification.
#' 
#' @export
#' @import data.table
#' @param df *Datatable containing the data [data.table].
#' @param holdout *Whether or not to construct and use a holdout set [logical - TRUE, FALSE].
#' @param cluster_var *Name of variable containing observation level cluster identifiers (integer) [character].
#' @param fold_method *Method used to generate CV folds (and the train-holdout split) [cluster_count (target cluster count), obs_count (target observation count), stratification (target stratification)].
#' @param stratify_var *Name of variable on which to stratify (when generating CV folds, test vs. holdout splits)  [character].
#' @param stratify_cut *Number of intervals into which the stratification variable is split (if fold_method==stratification) [integer] .
#' @param weight_var
#' @param return_index *Whether to return a modified data.table with an appended holdout, fold column [FALSE] or a vector of holdout, fold ids [logical - TRUE, FALSE]. 
#' @param holdout *Whether or not to construct and use a holdout set [logical - TRUE, FALSE].
#' @param quiet *Verbosity settings [0 (verbose), 1 (print only key stats and progress updates), 2 (silent) - 0.5 (test and development mode)].
#' @return *Modified data.table containing a holdout set identifier (TRUE, FALSE)  for each observation.
#' @examples 
#' gen_holdout(test_df, holdout_prop=0.2, cluster_var="cluster", fold_method="cluster_count", 
#'  stratify_var="treatment", stratify_cut=20) 
#' gen_holdout(test_df, holdout_prop=0.2, cluster_var="cluster", fold_method="obs_count", 
#'  stratify_var="treatment", stratify_cut=20) 
#' gen_holdout(test_df, holdout_prop=0.2, cluster_var="cluster", fold_method="stratification", 
#'  stratify_var="treatment", stratify_cut=20) 

gen_holdout <- function(df, cluster_var=ExpLearning.Default$cluster_var, 
                holdout_prop=ExpLearning.Default$holdout_prop,
                weight_var=ExpLearning.Default$weight_var,
                fold_method=ExpLearning.Default$fold_method, 
                stratify_var=NULL, stratify_cut=ExpLearning.Default$stratify_cut, 
                holdout_var=ExpLearning.Default$holdout_var,
                return_index=ExpLearning.Default$return_index,
                quiet=ExpLearning.Default$quiet) {
  


  # initialise
  # ---------------------------------
  df[,id_temp:=1:nrow(df)]
  df_temp <- copy(df)
  
  if (holdout_prop==0) {

    holdout_index <- FALSE
 
  } else {

    # generate holdout set - target cluster count
    # ---------------------------------
    if (fold_method=="cluster_count") {
    
      cl <- unique(df_temp[, get(cluster_var)])
      cl <- cl[order(cl)]
  
      holdout_size  <- floor(length(cl) * holdout_prop)
      holdout_index <- ifelse(df_temp[, get(cluster_var)] %in% 
                          sample(unique(df_temp, by=c(cluster_var))[, get(cluster_var)])[1:holdout_size], 
                          TRUE,FALSE)
    
    # generate holdout set - target obs count
    # ---------------------------------
    } else if (fold_method=="obs_count") {
  
      # df_temp[, obs_count:=.N,by=c(cluster_var)]
      df_temp[, weighted_obs_count:=sum(get(weight_var)),by=c(cluster_var)]
  
      holdout_index <- blocked_stratification(df_temp, stratify_var="weighted_obs_count", cluster_var=cluster_var, 
                         prop_vector=c(holdout_prop, 1-holdout_prop), stratify_cut=stratify_cut)
      holdout_index <- ifelse(holdout_index==1, TRUE, FALSE)
  
  
    # generate holdout set - target stratification
    # ---------------------------------
    } else if (fold_method=="stratification") {
  
      # df_temp[, mean_var:=mean(get(stratify_var)),by=c(cluster_var)]
      df_temp[, mean_var_weighted:=weighted.mean(get(stratify_var), get(weight_var)),by=c(cluster_var)]
      
      holdout_index <- blocked_stratification(df_temp, stratify_var="mean_var_weighted", cluster_var=cluster_var, 
                         prop_vector=c(holdout_prop, 1-holdout_prop), stratify_cut=stratify_cut)
      holdout_index <- ifelse(holdout_index==1, TRUE, FALSE)
  
    }
  
  }

  # merge holdout index into df
  # ---------------------------------
  df_temp[, holdout_temp:=holdout_index]
  
  # report overview stats
  # ---------------------------------
  if (quiet==0.5) {
  
    holdout_prop_actual_obs     <-  paste0(round(nrow(df_temp[holdout_temp==TRUE])/nrow(df_temp),3), " (", 
                                        nrow(df_temp[holdout_temp==TRUE]),")")
    holdout_prop_actual_cluster <-  paste0(round(nrow(unique(df_temp[holdout_temp==TRUE], by=c(cluster_var)))/
                                      nrow(unique(df_temp, by=c(cluster_var))),3), " (", 
                                      nrow(unique(df_temp[holdout_temp==TRUE], by=c(cluster_var))), ")")
    
    if (!is.null(stratify_var)) {
        
        class_balance_train         <- nrow(df_temp[holdout_temp==FALSE & get(stratify_var)==1])/
                                     nrow(df_temp[holdout_temp==FALSE])
        class_balance_holdout       <- nrow(df_temp[holdout_temp==TRUE & get(stratify_var)==1])/
                                      nrow(df_temp[holdout_temp==TRUE])
        
          
        class_balance_train_weighted      <- weighted.mean(df_temp[holdout_temp==FALSE, get(stratify_var)], 
                                              df_temp[holdout_temp==FALSE, get(weight_var)])
        class_balance_holdout_weighted    <- weighted.mean(df_temp[holdout_temp==TRUE, get(stratify_var)], 
                                              df_temp[holdout_temp==TRUE, get(weight_var)])
        

    } else {
  
      class_balance_train   <-  "/"
      class_balance_holdout <-  "/"
  
    }
  
    cat("\n\nHoldout Set Generation:\n")
    cat(sprintf("* Holdout proportion specified: %f / Method: %s \n",holdout_prop, fold_method))
    cat(sprintf("* Holdout proportion realised - cluster count: %s\n", holdout_prop_actual_cluster))
    cat(sprintf("* Holdout proportion realised - observation count: %s:\n", holdout_prop_actual_obs))
    cat(sprintf("* Class balance realised - (i) training set: %f, (ii) holdout set: %f:\n", 
      class_balance_train, class_balance_holdout))
    cat(sprintf("* Weighted class balance realised - (i) training set: %f, (ii) holdout set: %f:\n\n", 
      class_balance_train_weighted, class_balance_holdout_weighted))

  }

  
  # return the modified df
  # ---------------------------------
  df_temp <- df_temp[,.(id_temp, holdout_temp)][df, on=c("id_temp")][, 
    id_temp:=NULL]
  setnames(df_temp, "holdout_temp", holdout_var)

  if (return_index==FALSE) {
      return(df_temp)
  } else {
     return(df_temp[, get(holdout_var)])
  }
}

#----------------------------------------------------------------------------#




