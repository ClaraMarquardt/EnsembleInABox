#----------------------------------------------------------------------------#

#' @title *Generate cluster-level CV folds.
#' 
#' @description *Holdout set construction optionally targeting (i) the cluster count (ii) the sample size or (iii) stratification.
#' 
#' @export
#' @import data.table
#' @param df *Datatable containing the data [data.table].
#' @param nfold *Number of CV folds (used in both the inner and outer CV routine) [integer].
#' @param cluster_var *Name of variable containing observation level cluster identifiers (integer) [character].
#' @param fold_method *Method used to generate CV folds (and the train-holdout split) [cluster_count (target cluster count), obs_count (target observation count), stratification (target stratification)].
#' @param stratify_var *Name of variable on which to stratify (when generating CV folds, test vs. holdout splits)  [character].
#' @param stratify_cut *Number of intervals into which the stratification variable is split (if fold_method==stratification) [integer] .
#' @param return_index *Whether to return a modified data.table with an appended holdout, fold column [FALSE] or a vector of holdout, fold ids [logical - TRUE, FALSE]. 
#' @param weight_var
#' @param fold_var *Name of variable containing observation level fold identifiers (integer) [character].
#' @return *Modified data.table containing fold IDs for each observation.
#' @examples 
#' gen_fold(test_df, nfold=5, cluster_var="cluster", fold_method="cluster_count", 
#'  stratify_var="treatment", stratify_cut=20) 
#' gen_fold(test_df, nfold=5, cluster_var="cluster", fold_method="obs_count", 
#'  stratify_var="treatment", stratify_cut=20) 
#' gen_fold(test_df, nfold=5, cluster_var="cluster", fold_method="stratification", 
#'  stratify_var="treatment", stratify_cut=20) 

gen_fold <- function(df, 
                  cluster_var=ExpLearning.Default$cluster_var, 
                  nfold=ExpLearning.Default$holdout_var, 
                  fold_method=ExpLearning.Default$fold_method, stratify_var=NULL, 
                  stratify_cut=ExpLearning.Default$stratify_cut,
                  fold_var=ExpLearning.Default$fold_var, 
                  return_index=ExpLearning.Default$return_index, 
                  weight_var=ExpLearning.Default$weight_var, 
                  quiet=ExpLearning.Default$quiet) {
  
  # initialise
  # ---------------------------------
  df[,id_temp:=1:nrow(df)]
  df_temp   <- copy(df)

  # generate folds - target cluster count
  # ---------------------------------
  if (fold_method=="cluster_count") {

    cl         <- unique(df_temp[,get(cluster_var)])
    fold_size  <- ceiling(length(cl)/nfold)

    fold_index <- rep(1:nfold, fold_size)
    fold_index <- fold_index[order(fold_index)]
    fold_index <- fold_index[1:length(cl)]
    fold_index <- data.table(fold=sample(fold_index), cluster=cl)
    fold_index <- fold_index[df_temp, on=c(cluster=cluster_var)]$fold
  
    # generate folds - target obs count
  # ---------------------------------
  } else if (fold_method=="obs_count") {

    fold_prop <- 1/nfold

    # df_temp[, obs_count:=.N,by=c(cluster_var)]
    df_temp[, weighted_obs_count:=sum(get(weight_var)),by=c(cluster_var)]
 
    fold_index <- blocked_stratification(df_temp, stratify_var="weighted_obs_count", cluster_var=cluster_var, 
                       prop_vector=c(rep(fold_prop, nfold)), stratify_cut=stratify_cut)

  # generate folds - target stratification
  # ---------------------------------
  } else if (fold_method=="stratification") {

    fold_prop <- 1/nfold

    # df_temp[, mean_var:=mean(get(stratify_var)),by=c(cluster_var)]
    df_temp[, mean_var_weighted:=weighted.mean(get(stratify_var), get(weight_var)),
      by=c(cluster_var)]
  
    fold_index <- blocked_stratification(df_temp, stratify_var="mean_var_weighted", cluster_var=cluster_var, 
                       prop_vector=c(rep(fold_prop, nfold)),stratify_cut=stratify_cut)

  }

  # merge fold index into df
  # ---------------------------------
  df_temp[, fold_temp:=fold_index]


  # report overview stats
  # ---------------------------------
  if (quiet==0.5) {

    fold_prop_actual_obs        <-  sapply(1:nfold, function(x) 
      paste0(round(nrow(df_temp[fold_temp==x])/nrow(df_temp), 3), " (", 
      nrow(df_temp[fold_temp==x]),")"))
    fold_prop_actual_cluster    <-  sapply(1:nfold, function(x) 
      paste0(round(nrow(unique(df_temp[fold_temp==x], by=c(cluster_var)))/
      nrow(unique(df_temp, by=c(cluster_var))),3), " (",  
      nrow(unique(df_temp[fold_temp==x], by=c(cluster_var))), ")"))

    
    if (!is.null(stratify_var)) {
        
      class_balance             <- round(sapply(1:nfold, function(x) nrow(df_temp[fold_temp==x & get(stratify_var)==1])/
                                      nrow(df_temp[fold_temp==x])),3)
      
      class_balance_weighted    <- round(sapply(1:nfold, function(x) weighted.mean(df_temp[fold_temp==x, get(stratify_var)], 
                                      df_temp[fold_temp==x, get(weight_var)])),3)

    } else {

      class_balance               <-  "/"
      class_balance_weighted      <-  "/"

    }

    cat("\n\nFold Generation:\n")
    cat(sprintf("* Number of folds specified: %d (implied fold proportion: %f) / Method: %s \n", nfold, 
      1/nfold,  fold_method))
    cat(sprintf("* Holdout proportions realised - cluster count: %s\n", paste0(fold_prop_actual_cluster, collapse=" - ")))
    cat(sprintf("* Holdout proportions realised - observation count: %s\n", paste0(fold_prop_actual_obs, collapse=" - ")))
    cat(sprintf("* Class balance realised : %s\n", paste0(class_balance, collapse=" - ")))
    cat(sprintf("* Weighted class balance realised : %s\n\n", paste0(class_balance_weighted, collapse=" - ")))

  }
  
  # return the modified df
  # ---------------------------------
  df_temp <- df_temp[,.(id_temp, fold_temp)][df, on=c("id_temp")][, 
          id_temp:=NULL]
  setnames(df_temp, "fold_temp", fold_var)


  if (return_index==FALSE) {
      return(df_temp)
  } else {
     return(df_temp[, get(fold_var)])
  }
}


#----------------------------------------------------------------------------#
