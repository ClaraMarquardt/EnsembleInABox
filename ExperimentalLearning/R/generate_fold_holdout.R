#----------------------------------------------------------------------------#

#' @title .
#' 
#' @description *.
#' 
#' @export
#' @param df 
#' @param setting
#' @param dict
#' @param regenerate
#' @return 
#' @examples

generate_fold_holdout <- function(df, setting, dict, regenerate=FALSE) {

   df_mod <- copy(df)

  if(setting$holdout) {
    
    if (!(dict$holdout_var %in% names(df_mod)) | regenerate==TRUE) {

            df_mod <- gen_holdout(df=df_mod,holdout_prop=setting$holdout_prop,
                cluster_var=dict$cluster, holdout_var=dict$holdout_var, 
                fold_method=setting$fold_method, stratify_var=dict$assignment_var, 
                quiet=setting$quiet, weight_var=dict$weight_var)

    } else {

      if(setting$quiet<2) print("Using existing holdout/train split")

    }
    
    if (!(dict$fold_var %in% names(df_mod)) | regenerate==TRUE) {

         df_mod[get(dict$holdout_var)==FALSE, c(dict$fold_var):= 
              gen_fold(df=df_mod[get(dict$holdout_var)==FALSE], nfold=setting$nfold, 
              cluster_var=dict$cluster_var, fold_var=dict$fold_var, 
              fold_method=setting$fold_method, stratify_var=dict$assignment_var, 
              return_index=TRUE, quiet=setting$quiet, weight_var=dict$weight_var)]
    } else {

      if(setting$quiet<2) print("Using existing folds")

    }

  } else {

    df_mod[, c(dict$holdout_var ):=FALSE]

    if (!(dict$fold_var %in% names(df_mod)) | regenerate==TRUE) {

        df_mod <- gen_fold(df=df_mod, nfold=setting$nfold, cluster_var=dict$cluster_var, 
                   fold_var=dict$fold_var, fold_method=setting$fold_method, weight_var=dict$weight_var, 
                    stratify_var=dict$assignment_var, quiet=setting$quiet)
    
    } else {

      if(setting$quiet<2) print("Using existing folds")

    }

  }



  return(df_mod)

}


#----------------------------------------------------------------------------#
