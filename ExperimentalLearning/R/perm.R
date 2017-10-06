#----------------------------------------------------------------------------#

#' @title *Perform cluster-level permutations of a specified variable in a given data.table.
#' 
#' @description *.
#' 
#' @export
#' @import testit
#' @param df *Datatable containing the data [data.table].
#' @param assignment_var *Name of outcome variable [character].
#' @param cluster_var *Name of variable containing observation level cluster identifiers (integer) [character].
#' @param fold_var *Name of variable containing observation level fold identifiers (integer) [character].
#' @param within_fold *Whether  permutations are performed within folds or across folds [logical - TRUE, FALSE]. 
#' @param perm_strategy *Whether to permute the assignment variable by (a) exchanging variable values at the cluster level (entire_cluster) or (b) exchanging lvariable values within clusters (within_cluster) [character].
#' @param perm_test *(Permutation) test(s) to perform [character].
#' @param control_var_unbalanced *Vector of control variable names ((potentially) unabalanced across treatment conditions, i.e. random assignemnt is assumed to hold conditional on these variables) (RHS) [character].
#' @param control_var *Vector of control variable names (balanced across treatment conditions ) (RHS) [character].
#' @return *Modified data.table with permuted variable.
#' @examples

perm <- function(df,assignment_var,control_var, control_var_unbalanced,
          cluster_var=ExpLearning.Default$cluster_var,
          fold_var=ExpLearning.Default$fold_var, 
          within_fold=ExpLearning.Default$within_fold,
          perm_strategy=ExpLearning.Default$perm_strategy, 
          perm_test=ExpLearning.Default$perm_test) {
  
  df_temp <- copy(df)

  # subsets to one fold at a time and then performs permutation within that fold. Loop with recursive call. 
  if(within_fold==TRUE) {

    # temporary observation id
    df_temp[, temp_obs_id_fold:=1:.N]

    # If respect folds, permute per fold (recursive!)
    fold      <- unique(df_temp[, get(fold_var)])
    df_temp   <- do.call(rbind,lapply(fold,function(f) perm(df=copy(df_temp[get(fold_var)==f]),
                   assignment_var=assignment_var,cluster_var=cluster_var,
                   fold_var=fold_var, within_fold=FALSE, control_var=control_var, 
                   control_var_unbalanced=control_var_unbalanced,
                   perm_strategy=perm_strategy, perm_test=perm_test)))

    ## remove temporary id
    setorder(df_temp, temp_obs_id_fold)
    df_temp[, temp_obs_id_fold:=NULL]


  } else {

    # Otherwise, actually do it

    # entire cluster perm
    # -----------
    if(perm_strategy=="entire_cluster") {

      # temporary observation id
      df_temp[, temp_obs_id:=1:.N]

      #### TREATMENT
      # confirm that treatment is asssigned at the cluster level
      assignment_var_mean <- as.data.table(aggregate(df_temp[, mget(assignment_var)], 
          by = list(df_temp[, get(cluster_var)]), FUN = "sd"))
        setnames(assignment_var_mean, c(cluster_var, assignment_var))

      assert("treatment assignment by cluster (perm())", 
          sum(c(unlist(assignment_var_mean[, mget(assignment_var)])), 
          na.rm=T)==0)

      # computes mean of the var (LHS when called) for each cluster
      cluster_tab <- as.data.table(aggregate(df_temp[, get(assignment_var)], 
        by = list(df_temp[, get(cluster_var)]), FUN = "mean"))
      setnames(cluster_tab, c(cluster_var, assignment_var))

      #### BALANCED CONTROLS (X)
      if (perm_test %in% c("signal_b") && length(control_var)>0) { 

        # confirm that control vars are asssigned at the cluster level
        control_var_sd <- as.data.table(aggregate(df_temp[, mget(control_var)], 
          by = list(df_temp[, get(cluster_var)]), FUN = "sd"))
        setnames(control_var_sd, c(cluster_var, control_var))

        assert("control var assignment by cluster (perm())", 
          sum(c(unlist(control_var_sd[, mget(control_var)])), 
            na.rm=T)==0)

        # computes mean of the var for each cluster
        control_tab <- as.data.table(aggregate(df_temp[, mget(control_var)], 
          by = list(df_temp[, get(cluster_var)]), FUN = "mean"))
        setnames(control_tab, c(cluster_var, control_var))
        
      } else if (length(control_var)>0) {
        control_tab <- as.data.table(df_temp[, mget(c(control_var, "temp_obs_id"))])
      }

      #### (UN)BALANCED CONTROLS (e.g. Missingness) (X0)
      if (perm_test %in% c("baseline_signal", "signal_b") && length(control_var_unbalanced)>0) {## permute control var (X0)

        # confirm that control vars are asssigned at the cluster level
        control_var_unbalanced_sd <- as.data.table(aggregate(df_temp[, mget(control_var_unbalanced)], 
          by = list(df_temp[, get(cluster_var)]), FUN = "sd"))
        setnames(control_var_unbalanced_sd, c(cluster_var, control_var_unbalanced))

        assert("control var assignment by cluster (perm())", 
          sum(c(unlist(control_var_unbalanced_sd[, mget(control_var_unbalanced)])), 
            na.rm=T)==0)

        # computes mean of the var for each cluster
        control_tab_unbalanced <- as.data.table(aggregate(df_temp[, mget(control_var_unbalanced)], 
          by = list(df_temp[, get(cluster_var)]), FUN = "mean"))
        setnames(control_tab_unbalanced, c(cluster_var, control_var_unbalanced))

      } else if (length(control_var_unbalanced)>0) {
          control_tab_unbalanced <- as.data.table(df_temp[, mget(c(control_var_unbalanced,"temp_obs_id"))])
      }


      # here sample is from 'base' random permutation, so replacing the variable means 
      # with a random reordering / reassigning to cluster
      cluster_index_perm <- sample(1:nrow(cluster_tab), nrow(cluster_tab))

      #### TREATMENT
      cluster_tab[, c(assignment_var):=get(assignment_var)[cluster_index_perm]]

      #### BALANCED CONTROLS (X)
      if (perm_test %in%  c("signal_b") && length(control_var)>0) {
        control_tab[, c(control_var):=
          lapply(control_var, function(x) get(x)[cluster_index_perm])]
      } 

      #### (UN)BALANCED CONTROLS (X)
      if (perm_test %in%  c("baseline_signal","signal_b") && length(control_var_unbalanced)>0) {
        control_tab_unbalanced[, c(control_var_unbalanced):=
          lapply(control_var_unbalanced, function(x) get(x)[cluster_index_perm])]
      } 

      # subset to all columns except for var (LHS) 
      df_temp <- df_temp[, mget(setdiff(names(df_temp), c(assignment_var, control_var,
        control_var_unbalanced)))]
      
      # merge in the reordered var values by cluster, keep all initial obs (now every cluster 
      # will be the mean of some random cluster, w/o replacement)
      df_temp <- merge(df_temp, cluster_tab, by = cluster_var, all.x = TRUE)
    
      #### BALANCED CONTROLS (X)
      if (perm_test %in%  c("signal_b") && length(control_var)>0 ) {
       df_temp <- merge(df_temp, control_tab, by = cluster_var, all.x = TRUE)
      } else if(length(control_var)>0) {
       df_temp <- merge(df_temp, control_tab, by = "temp_obs_id", all.x = TRUE)
      }

      #### (UN)BALANCED CONTROLS (X)
      if (perm_test %in%  c("baseline_signal", "signal_b") && length(control_var_unbalanced)>0) {
        df_temp <- merge(df_temp, control_tab_unbalanced, by = cluster_var, all.x = TRUE)
      } else if (length(control_var_unbalanced)>0) {
        df_temp <- merge(df_temp, control_tab_unbalanced, by = "temp_obs_id", all.x = TRUE)
      }

      ## remove temporary id
      setorder(df_temp, temp_obs_id)
      df_temp[, temp_obs_id:=NULL]

  # within_cluster cluster perm
  # -----------
  } else if (perm_strategy=="within_cluster") {

      # temporary observation id
      df_temp[, temp_obs_orig:=1:.N]
      
      # order by cluster
      setorderv(df_temp, cluster_var)
      df_temp[, temp_obs_id:=1:.N]

      # index
      index_perm <- c(unlist(sapply(unique(df_temp[, get(cluster_var)]), 
        function(x) df_temp[get(cluster_var)==x]$temp_obs_id[sample(x=c(1:
        length(df_temp[get(cluster_var)==x]$temp_obs_id)), 
        size=length(df_temp[get(cluster_var)==x]$temp_obs_id))])))

      # shuffle within each cluster group
      df_temp[,c(assignment_var):=get(assignment_var)[index_perm]]
      
      #### BALANCED CONTROLS (X)
      if (perm_test %in%  c("signal_b") && length(control_var)>0 ) {
        df_temp[, c(control_var):=lapply(control_var, function(x) 
          get(x)[index_perm])]
      } 

      #### (UN)BALANCED CONTROLS (X)
      if (perm_test %in%  c("baseline_signal","signal_b") && length(control_var_unbalanced)>0) {
        df_temp[, c(control_var_unbalanced):=lapply(control_var_unbalanced, function(x) 
          get(x)[index_perm])]
      } 

      # order by cluster
      setorderv(df_temp, "temp_obs_orig")

      ## remove temporary id
      df_temp[, temp_obs_orig:=NULL]
      df_temp[, temp_obs_id:=NULL]
  }

  }

  # check that permutation has been implemented correctly
  if (perm_test %in% c("signal_a", "baseline_balance")) {

    # only the assignment var has changed
    assert("only the assignment var is permuted (baseline_balance/signal_a) (perm())", 
        all(df_temp[,mget(setdiff(names(df_temp), assignment_var))]==df[,mget(setdiff(names(df_temp), 
        assignment_var))]))
    # assert("assignment var is permuted (baseline_balance/signal_a) (perm())", 
    #     any(df_temp[,mget(names(df_temp))]!=df[,mget(names(df_temp))]))

  }



  # return
  return(df_temp)

}

#----------------------------------------------------------------------------#
