#----------------------------------------------------------------------------#

#' @title *Generate and save a plot of tuning parameters and the resulting losses.
#' 
#' @description *Aggregate rather than parameter-specific tuning plot.
#' 
#' @export
#' @import data.table
#' @import ggplot2
#' @import plotly
#' @param tuned_loss *(Mean) cross-validation loss (inner tuning routine, i.e. for a given fold). 
#' @param losstype *Loss function used to tune the individiual learners and the ensemble neg_mse_brier, loglik, auc, accuracy, balanced_accuracy, r2].
#' @param alg_param *Tuning parameters [list - misc].
#' @param alg_param_opt *Index of chosen tuning parameters set.
#' @param algorithm *Name of individual algorithm to be tuned [character].
#' @param fold_out_id *Fold on which to predict [integer].
#' @param output_path *Directory within which all model output (e.g. tuning graphs) is stored [path].
#' @param execution_id *Unique ID generated at the beginning of each model construction process - all output is associated with this ID [character].
#' @param tuning_method *Tuning method [character].
#' @param quiet
#' @return *.
#' @examples

agg_tuning_plot <- function(tuned_loss, losstype, alg_param, alg_param_opt, output_path, execution_id,
                       fold_out_id, algorithm, tuning_method, quiet) {
  
  # perf data.table
  space_seq  <- c(rep(c("","","","<br>"), times=ceiling(length(alg_param[[1]])/4)))[1:length(alg_param[[1]])]
  perf_dt    <- data.table(loss=tuned_loss, iteration=1:length(tuned_loss), 
                parameter= sapply(alg_param, function(x) paste0(sapply(c(rbind(space_seq,names(x), x)),
                function(y) paste0(y,collapse=" ")), collapse=" / ")))
  perf_dt[alg_param_opt, opt:=1]
  perf_dt[1, no_tuning:=1]

  # gain
  tuning_gain <- round(perf_dt[opt==1]$loss-perf_dt[no_tuning==1]$loss, 4)

  # ensure that home variable is set (required for pandoc to work)
  if (Sys.getenv("HOME")=="" | is.null(Sys.getenv("HOME"))) {
      original_unset <- 1
      try(Sys.setenv(HOME=getwd()))
  } else {
      original_unset <- 0
  }

  # Attempt to generate plotly graph - otherwise default to pdf plot
  # -------------
  # backup pdf routine
  # -------------
  pdf_plot <- function() {

    if (quiet==0.5) print("PDF Tuning Plot (Backup)")

    # delete plotly folder if it has been created
    if(exists("file_name")) {
      if (dir.exists(paste0(file_name ,"_files"))) unlink(paste0(file_name ,"_files"), recursive=TRUE)
      if (file.exists(paste0(file_name ,".html"))) file.remove(paste0(file_name ,".html"))
    }

    # plot annotation text
    iteration_text     <- paste0("Optimal Parameters: \n", losstype, ": ", round(perf_dt$loss, 4),"\n", 
                            perf_dt$parameter)
    iteration_text_opt <- iteration_text[which(perf_dt$opt==1)]

    # baseplot
    plot_temp <- ggplot(data=NULL) + 
      geom_point(data=perf_dt, aes(x=iteration, y=loss), size=0.5) + 
      labs(
        x="\nIteration\n",
        y=paste0(losstype, " (CV)", "\n\n"),
        title="Tuning Plot", 
        subtitle= paste0(sprintf("Algorithm: %s / Tuning Strategy: %s / Fold_Out: %s / Iterations: %d", algorithm, 
                 tuning_method, as.character(fold_out_id), length(tuned_loss)), "\n\nTuning Gain (Default vs. Tuned Opt): ", 
                  tuning_gain), 
        caption=iteration_text_opt
      ) + 
      geom_vline(aes(xintercept=c(perf_dt[opt==1]$iteration, perf_dt[opt==1]$iteration)), 
        linetype="dashed") + 
      geom_vline(aes(xintercept=c(perf_dt[no_tuning==1]$iteration, perf_dt[no_tuning==1]$iteration)), 
        linetype="dashed") +
      geom_text(aes(x=perf_dt[no_tuning==1]$iteration+0.05, y=mean(c(min(perf_dt$loss), max(perf_dt$loss)))), 
        label="Default Parameters", angle=270, size=2) + 
      geom_text(aes(x=perf_dt[opt==1]$iteration-0.05, y=mean(c(min(perf_dt$loss), max(perf_dt$loss)))), 
        label="Optimal Parameters", angle=270, size=2) + 
      theme_basic() + 
      theme_legend_bottom() +
      theme(legend.position = "none")

    # save
    current_date_time_id <- paste0(as.character(format(Sys.time(), "%d_%m_%Y_%H_%M_%OS")), 
      paste0(sample(letters, 10),collapse=""))
    file_name <- paste0(output_path, "tuning_plot_foldout_", 
      fold_out_id, "_alg_", algorithm, "_losstype_", losstype, "_", execution_id, "_", 
      current_date_time_id) 
    ggsave(paste0(file_name ,".pdf"), plot_temp)


  }

  # plotly routine
  # -------------
  tryCatch({

    # plot annotation text
    iteration_text <- paste0(losstype, ": ", round(perf_dt$loss, 4),"<br>", 
                        perf_dt$parameter)
    
    # base plot
    plot_temp <- with(perf_dt, plot_ly(x=iteration, y=loss, 
                    mode = "markers", text=iteration_text, type = 'scatter', hoverinfo="text"))  %>%
                    layout(
                      title = "Tuning Plot",
                      yaxis = list(title = paste0(losstype, " (CV)", "<br><br><br>")),
                      xaxis = list(title = paste0("<br>Iteration<br>",
                        sprintf("Algorithm: %s / Tuning Strategy: %s / Fold_Out: %s / Iterations: %d", algorithm, 
                          tuning_method, as.character(fold_out_id), length(tuned_loss)), "<br> Tuning Gain (Default vs. Tuned Opt): ", 
                          tuning_gain),titlefont=10), showlegend = FALSE)

    # format & add optimum
    plot_temp  <- plot_temp %>%    
                  add_trace(data=NULL, x=c(perf_dt[opt==1]$iteration, perf_dt[opt==1]$iteration), 
                    y=c(min(perf_dt$loss)+0.001, max(perf_dt$loss)-0.001), inherit=FALSE,mode="lines",type="scatter",
                    hoverinfo="none", line = list(dash = 'dash'))  %>%    
                  add_trace(data=NULL, x=c(perf_dt[no_tuning==1]$iteration, perf_dt[no_tuning==1]$iteration), 
                    y=c(min(perf_dt$loss)+0.001, max(perf_dt$loss)-0.001), inherit=FALSE,mode="lines", type="scatter",
                    hoverinfo="none", line = list(dash = 'dash'))   %>%
                   add_annotations(text = "Default Parameters", data=NULL, inherit=FALSE, x=perf_dt[no_tuning==1]$iteration+0.05, 
                    y=mean(c(min(perf_dt$loss), max(perf_dt$loss))), showarrow=FALSE, textangle=270) %>%
                   add_annotations(text = "Optimal Parameters", data=NULL, inherit=FALSE, x=perf_dt[opt==1]$iteration-0.05, 
                    y=mean(c(min(perf_dt$loss), max(perf_dt$loss))), showarrow=FALSE, textangle=270) %>%
                   layout(xaxis = list(dtick = 1))
    
    plot_temp  <- plot_temp %>%
                    layout(margin=list(l=50, r=50, b=100, t=50, pad=0))  %>%
                    config(showLink = F)   %>%
                    config(displayModeBar= F)
    
    # save
    current_date_time_id <- paste0(as.character(format(Sys.time(), "%d_%m_%Y_%H_%M_%OS")), 
      paste0(sample(letters, 10),collapse=""))
    file_name <- paste0(output_path, "tuning_plot_foldout_", 
      fold_out_id, "_alg_", algorithm, "_losstype_", losstype, "_", execution_id, "_", 
      current_date_time_id) 
    htmlwidgets::saveWidget(plot_temp,paste0(file_name ,".html"))
    unlink(paste0(file_name ,"_files"), recursive=TRUE)



  }, error=function(e) {pdf_plot()})


  # restore initial home variable
    if (original_unset==1) {
      try(Sys.unsetenv("HOME"))
  } 

}

#----------------------------------------------------------------------------#
