#----------------------------------------------------------------------------#

#' @title *Generate a permutation testing plot (plot of the permuted losses and the model loss).
#' 
#' @description *.
#' 
#' @export
#' @param main_loss *Out of sample ensemble loss [numeric].
#' @param perm_loss *Out of sample permutation losses [numeric].
#' @param pval *Final p-value.
#' @param perm_test
#' @param losstype *Loss function used to tune the individiual learners and the ensemble neg_mse_brier, loglik, auc, accuracy, balanced_accuracy, r2].
#' @return *Permutation testing plot.
#' @examples


perm_analysis <- function(main_loss, perm_loss, pval, pval_95_ci, perm_test, 
  losstype=ExpLearning.Default$losstype) {

    # Visualize permutation results
    plot <- ggplot(data=data.table(TS=perm_loss)) +
            geom_histogram(aes(x=TS)) + 
            labs(
              title=paste0("Permutation Testing (Test: ",perm_test, ")"),
              x=losstype, 
              y="Number of Permutations",
              subtitle=sprintf("P-value: %f [95 percent CI: %s]", pval, pval_95_ci)
            ) + 
            geom_vline(xintercept = main_loss) + 
            theme_basic()

    return(plot)

}



#----------------------------------------------------------------------------#
