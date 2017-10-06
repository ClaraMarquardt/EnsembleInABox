#----------------------------------------------------------------------------#

#' @title *Prepare a data.table containing the raw data for further processing with the ExpLearning Package and generate a configuration dictionary.
#' 
#' @description *.
#' 
#' @export
#' @import data.table
#' @param df *Datatable containing the data [data.table].
#' @param assignment_var *Name of outcome variable [character].
#' @param group_var *Name of predictor variable [character].
#' @param control_var *Vector of control variable names (balanced across treatment conditions ) (RHS) [character].
#' @param task *The nature of the prediction task  [regression (regression), classification (classification)].
#' @param weight *Vector of observation weights [integer or numeric].
#' @param cluster_var *Name of variable containing observation level cluster identifiers (integer) [character].
#' @param id_var *Name of variable containing unique observation level identifiers (integer) [character].
#' @param fold_var *Name of variable containing observation level fold identifiers (integer) [character].
#' @param clusterweight_norm *Whether to normalise weights at the cluster level [logical - TRUE, FALSE].
#' @param control_var_unbalanced *Vector of control variable names ((potentially) unabalanced across treatment conditions, i.e. random assignemnt is assumed to hold conditional on these variables) (RHS) [character].
#' @param quiet *Verbosity settings [0 (verbose), 1 (print only key stats and progress updates), 2 (silent) - 0.5 (test and development mode)].
#' @return *List with the modified data.table (df) and the configuration dictionary (dict).
#' @examples

prepare_data <- function(df, assignment_var, group_var, fold_var=NULL, holdout_var=NULL,
                         control_var=NULL, weight_var=NULL, cluster_var=NULL,id_var=NULL, 
                         control_var_unbalanced=NULL, 
                        clusterweight_norm=ExpLearning.Default$clusterweight_norm, 
                         task=ExpLearning.Default$task, quiet=ExpLearning.Default$quiet) {
  
  # ensure correct format (data.table)
  df <- as.data.table(df)

  # The code expects clusters, weight, folds, id, and stratum (as well as a holdout/fold variable): 
  # - If no clusters treat each individual obs as its own cluster ("cluster")
  # - If no weight set the weight of each var to 1 ("weight")
  # - If no id construct id as row number ("id")
  # - Construct fold variable and holdout variable ("fold"/"holdout")
  # Note: dictionary contains additonal variables not yet initialised in DF (prediction_var)
  
  # If no clusters, construct them
  if(is.null(cluster_var)) {
    cluster_var <- ExpLearning.Default$cluster_var
    df[, c(cluster_var):=1:nrow(df)]
  }

  # If no weight, construct them
  if(is.null(weight_var)) {
    weight_var <- ExpLearning.Default$weight_var
    df[, c(weight_var):=1]
  }

  # If no id, construct it
  if(is.null(id_var)) {
    id_var <- ExpLearning.Default$id_var
    df[, c(id_var):=1:nrow(df)]
  }
  
  # If requested, normalize weight so that add up to one per cluster
  if(clusterweight_norm) {
    weight_norm  <- df[,get(weight_var)] / tapply(df[,get(weight_var)],df[,get(cluster_var)],function(x) 
      sum(x))[df[,get(cluster_var)]]
    df[, c(weight_var):=weight_norm]
  }
  
  # Construct fold/holdout variable
  fold_var    <- ExpLearning.Default$fold_var
  holdout_var <- ExpLearning.Default$holdout_var

  var_list  <- c(assignment_var,group_var,control_var, control_var_unbalanced, 
                weight_var,cluster_var,id_var, holdout_var, fold_var)
 
  var_list  <- var_list[var_list %in% names(df)]
  var_other <- c(id_var, setdiff(names(df), var_list))

  df_other <- df[,mget(var_other)]
  df       <- df[,mget(var_list)]

  # Format assignemnt variable according to task
  if (task=="classification") {

    df[, c(assignment_var):=as.integer(get(assignment_var))]

  } else if (task=="regression") {

     df[, c(assignment_var):=as.numeric(get(assignment_var))]

  }

  # Format other var types
  # ok: integer & numeric // problematic: character & factor

  char_var   <- which(sapply(df, function(x) class(x))=="character")
  factor_var <- which(sapply(df, function(x) class(x))=="factor")

  if (length(c(factor_var, char_var))>0) {
    df <- one_hot_encoding(df, c(char_var, factor_var))
  }

  # Return data and settings
  return(list(df=df,df_other=df_other, dict=list(assignment_var=assignment_var,group_var=group_var,
        control_var=control_var,weight_var=weight_var,cluster_var=cluster_var,id_var=id_var, 
        fold_var=fold_var,holdout_var=holdout_var, control_var_unbalanced=control_var_unbalanced,
        prediction_var=ExpLearning.Default$prediction_var, 
        add_var=setdiff(var_other, id_var))))
}

#----------------------------------------------------------------------------#
