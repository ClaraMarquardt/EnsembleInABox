#----------------------------------------------------------------------------#

#' @title *Calculate specified regression, classification loss (or gain) given a vector of predictions and a vector of labels. 
#'
#' @description *.
#' 
#' @export
#' @import testit
#' @param prediction *Vector of predictions [numeric].
#' @param outcome *Vector of outcomes [integer or numeric].
#' @param weight *Vector of observation weights [integer or numeric].
#' @param task *The nature of the prediction task  [regression (regression), classification (classification)].
#' @param losstype *Loss function used to tune the individiual learners and the ensemble neg_mse_brier, loglik, auc, accuracy, balanced_accuracy, r2].
#' @param class_cutoff *Probability cutoff used to convert predicted probabilities into predicted classes in order to calcuate the accuracy of the predictions (if losstype==accuracy or balanced_accuracy) [numeric - 0 to 1].
#' @param round_digit *Number of digits to which results are rounded [integer].
#' @return *Loss.
#' @examples
#' prediction <- test_df$prediction
#' outcome    <- test_df$treatment
#' weight     <- test_df$weight
#' loss(prediction, outcome, weight, task="classification", loss="neg_mse_brier")
#' loss(prediction, outcome, weight, task="classification", loss="loglik")
#' loss(prediction, outcome, weight, task="classification", loss="auc")
#' loss(prediction, outcome, weight, task="classification", loss="accuracy")
#' loss(prediction, outcome, weight, task="classification", loss="balanced_accuracy")
#' loss(prediction, outcome, weight, task="regression", loss="r2")


loss <- function(prediction, outcome, 
					weight=ExpLearning.Default$weight, 
					task=ExpLearning.Default$task,
					losstype=ExpLearning.Default$losstype, 
					class_cutoff=ExpLearning.Default$class_cutoff, 
					round_digit=ExpLearning.Default$round_digit, 
					loss_validity_check=ExpLearning.Default$loss_validity_check) {

	# confirm that losstype is valid/supported
	# --------------------------------- 
	assert("valid losstype (loss())", losstype %in% c("neg_mse_brier", "loglik", "auc", 
		"accuracy", "balanced_accuracy", "r2"))

	# NOTE: ALL LOSS TYPES ARE DEFINED SO THAT 'higher return values are better than lower return values'

	# normalise the weights
	if (is.null(weight)) weight <- rep(1, length(prediction))
	weight_norm <- weight/sum(weight)

	# NEG MSE (regression) / brier score (binary classification)
	# ---------------------------------
	if(losstype=="neg_mse_brier") {

		if(loss_validity_check==TRUE) {
			# confirm that outcome is binary & task==classification (brier score) OR task==regression (RMSE)
			assert("regression OR binary classification - RMSE OR binary brier score implementation (loss())", 
				((task=="regression")||(sort(unique(outcome))==c(0,1) & task=="classification")))
		}

		# note: 
		## (*) optimally: maximise (i.e. minimise rmse_brier) (0-1 range)
		## (i) classic RMSE implementation (regression)
		## (i) brier score formula represents a proper scoring function in the case of binary outcomes (i.e. 
		## in this case the formula would (in expectation) assign the lowest score to a prediction
		## which corresponds to the actual probability distribution (p, 1-p) (alternative interpretation:
		## uncertainty (max for 50:50 outcome) + reliability [how reliable are the predictions] (0: 80%
		## predicted outcomes- happen 80% of times) - resolution [how extreme are the predictions] 
		## (0: obs probabilities always (across prediction bins) == mean(aggregate obs probability) )

		# rmse_brier <- mean((prediction - outcome)^2) # unweighted (& normalised)
		rmse_brier   <- sum(weight_norm*(prediction - outcome)^2) # weighted (& normalised)
		neg_rmse_brier <- (-1) * rmse_brier
		
		# return rmse/brier score
		return(round(neg_rmse_brier, round_digit))

	# log-likelihood [binominal] (binary classification)
	# ---------------------------------
	} else if(losstype=="loglik") {

		if(loss_validity_check==TRUE) {

			# confirm that outcome is binary & task==classification 
			assert("binary classification - binary log-loss (loss())", 
				task=="classification" && sort(unique(outcome))==c(0,1))
		}

		# note: 
		## (*) optimally: maximise (0-1 range)
		## log-likelihood of the actual data given the model 

		# following scikit-learn's log-loss:  Log loss is undefined for p=0 or p=1, so probabilities are
        # clipped to max(eps, min(1 - eps, p)) where eps=1e-15
        eps=1e-15
        prediction <- sapply(prediction , function(x) max(eps, min(1 - eps, x)))

		# loglik <- mean((outcome*log(prediction)+(1-outcome)*log(1-prediction))) # unweighted (& normalised)
		loglik   <- sum(weight_norm*(outcome*log(prediction)+(1-outcome)*log(1-prediction))) # weighted (& normalised)

		return(round(loglik, round_digit))

	# AUC (binary classification)
	# ---------------------------------
	} else if(losstype=="auc") {

		if(loss_validity_check==TRUE) {

			# confirm that outcome is binary & task==classification
			assert("binary classification - binary classification auc implementation | regression (NA) (loss())", 
				((task=="regression")||(task=="classification" && sort(unique(outcome))==c(0,1))))

		}

		if (task=="regression") {

			auc <- NA

		} else if (task=="classification") {

			# note: 
			# (*) optimally: maximise (0-1 range)
			# (i) auc formula reflects interpretation of auc as a concordance metric, i.e.
			# taking all possible pairs of observations where the outcome of one is 1 ('pos obs') and 
			# the outcome of the other is 0 ('neg obs')- what is the probability that 
			# the predicted outcome for the 'pos obs' >= 'neg obs'?
	
			# dt_temp <- data.table(prediction=prediction, outcome=outcome)
			# dt_temp_0 <- dt_temp[outcome==0]
			# dt_temp_1 <- dt_temp[outcome==1]
			
			# out <- outer(dt_temp_1$prediction, dt_temp_0$prediction, "-")
			# auc <- mean((out>0) + .5*(out==0))
	
	
			# alternative auc formula: (a) determine the (weighted) TPR (sensitivity) and (weighted) 1-TNR (specificity) 
			# at all relevant probability cutoffs (b) calculate the auc based on the area under the curve
			# at each threshold (rectangle + triangle)
	
			# determine the sensitivity (TPR) and 1-TNR (specificity) (i.e. FPR) (use cumsum - works because of sorting
			# by the predicted probabilities (!))
	 		ord <- order(prediction)
	 		y <- outcome[ord]
	 		w <- weight_norm[ord]
	 		y.hat <- prediction[ord]
	 		is.positive <- y == 1
	 		is.negative <- y == 0
	 		w.positive <- w.negative <- w
	 		w.positive[is.negative] <- 0
	 		w.negative[is.positive] <- 0
	 		cum.positive <- cumsum(w.positive)
	 		cum.negative <- cumsum(w.negative)
	 		is.end <- c(diff(y.hat) != 0, TRUE)
	 		n <- length(y)
	 		threshold <- c(y.hat[is.end], Inf)
	 		total.positive <- cum.positive[n]
	 		total.negative <- cum.negative[n]
	 		FN <- c(0, cum.positive[is.end])
	 		FNR <- FN/total.positive
	 		TPR <- 1-FNR
	 		TN <- c(0, cum.negative[is.end])
	 		FP <- total.negative - TN
	 		FPR <- FP/total.negative
	 		tpr.fpr <- data.frame(TPR, FPR, threshold)
		
			# determine the area under the curve at each threshold -> determine the total area under the curve (auc)
	  		right <- tpr.fpr[-nrow(tpr.fpr),]
	  		left <- tpr.fpr[-1,]
	  		width <- right$FPR - left$FPR
	  		rect.area <- left$TPR * width
	  		triangle.h <- right$TPR - left$TPR
	  		triangle.area <- triangle.h * width / 2
	  		auc <- sum(rect.area, triangle.area)

	  		auc <- round(auc, round_digit)

	  	}
	
			# return auc value
			return(auc)

	# Accuracy
	# ---------------------------------
	} else if (losstype=="accuracy") {

		if (loss_validity_check==TRUE) {

			# confirm that task==classification
			assert("classification - classification accuracy (loss())", 
				task=="classification")
			
		}

		# note: 
		# (*) optimally: maximise (0-1 range)

		prediction_class <- ifelse(prediction>class_cutoff, 1, 0) 
		# accuracy <- sum(prediction_class==outcome)/length(outcome) # unweighted (& normalised)
		accuracy   <- sum((prediction_class==outcome)*weight_norm) # weighted (& normalised)

		# return accuracy value
		return(round(accuracy, round_digit))

	# Balanced Accuracy
	# ---------------------------------
	} else if (losstype=="balanced_accuracy") {

		if (loss_validity_check==TRUE) {

			# confirm that task==classification
			assert("classification task - balanced classification accuracy (loss())", 
				task=="classification")

		}

		# note: 
		# (*) optimally: maximise (0-1 range)
		# (i) balanced accuracy: Mean (Sensitivity (accuracy for cases where the outcome==1 (TP/FN rate)), 
		# Specificity (accuracy for cases where the outcome==1 (TN/FP rate)))
		
		prediction_class <- ifelse(prediction>class_cutoff, 1, 0)
		# accuracy_pos <- sum(prediction_class==outcome & prediction_class==1)/(sum(outcome==1))  # unweighted (& normalised)
		# accuracy_neg <- sum(prediction_class==outcome & prediction_class==0)/(sum(outcome==0))
		accuracy_pos <- sum((prediction_class==outcome & prediction_class==1)*weight)/(sum(outcome==1)) # weighted (& normalised)
		accuracy_neg <- sum((prediction_class==outcome & prediction_class==0)*weight)/(sum(outcome==0)) 
	    balanced_accuracy <- mean(c(accuracy_pos, accuracy_neg))

		# return accuracy value
		return(round(balanced_accuracy, round_digit))

	# R2 [unadjusted] (regression)
	# ---------------------------------
	} else if (losstype=="r2") {

		if (loss_validity_check==TRUE) {

			# confirm that task==regression
			assert("regression task - R2 (loss())", task=="regression")

		}

		# note: 
		# (*) optimally: maximise (0-1 range)
		# (i) R2: 1 - (unexplained sum of squares/total sum of squares) 

	    # r2 <- 1 - (sum((prediction-outcome)^2)/sum((outcome-mean(outcome))^2))   # unweighted (& normalised)
	    r2   <- 1 - (sum(weight_norm*(prediction-outcome)^2)/sum(weight_norm*(outcome-sum(weight_norm*outcome))^2)) # weighted (& normalised)

		# return R2 value
		return(round(r2, round_digit))

	}

}

#----------------------------------------------------------------------------#
