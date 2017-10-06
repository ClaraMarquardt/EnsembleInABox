#----------------------------------------------------------------------------#

# Purpose:     Define tuning parameters for individual learners. 
# Author:      Clara Marquardt
# Date:        2017

#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#
#                                    CONTROL                                 #
#----------------------------------------------------------------------------#

# parameters
wd_path         <- commandArgs(trailingOnly = TRUE)[1]
package_name    <- commandArgs(trailingOnly = TRUE)[2]

print(sprintf("wd_path: %s",  wd_path))
print(sprintf("package_name: %s",  package_name))

# dependencies
library("devtools")
library(roxygen2)
library(data.table)
library(ExperimentalLearning)

# paths
setwd(paste0(wd_path, "/", package_name))

#----------------------------------------------------------------------------#
#                                    Code                                    #
#----------------------------------------------------------------------------#

# intercept (*) 
#----------------------------------------------------------------------------#

intercept_param <- NULL

# ols (designed for regression)
#----------------------------------------------------------------------------#

# implementation: lm() [base R]
# documentation: https://stat.ethz.ch/R-manual/R-devel/library/stats/html/lm.html

ols_param <- NULL

# avg  (designed for regression/classification)
#----------------------------------------------------------------------------#

# implementation: mean() [base R]
# documentation: /

avg_param <- NULL

# logit (designed for classification)
#----------------------------------------------------------------------------#

# implementation: glm(..., family="binomial") [base R]
# documentation: https://stat.ethz.ch/R-manual/R-devel/library/stats/html/glm.html

logit_param <- NULL

# rf (regression or classification forest)
#----------------------------------------------------------------------------#

# implementation: ranger() [ranger]
# documentation: https://www.rdocumentation.org/packages/ranger/versions/0.6.0/topics/ranger

rf_param        <- list() # list of default parameters
rf_param_tuning <- list() # list of parameter tuning ranges

# Fraction of variables to possibly split at in each node (numeric - 0 to 1)
rf_param$pmtry             <- NA # default as per ranger package: sqrt(number of predictors)
rf_param_tuning$pmtry      <- unique(c(rf_param$pmtry, c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)))

# Fraction of observations to split at in each node 
rf_param$sample.fraction        <- 0.6 #default as per ranger package (for replace=FALSE)
rf_param_tuning$sample.fraction <- unique(c(rf_param$sample.fraction, c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)))

# Number of trees (integer - >=1)
rf_param$num.trees        <- 250
rf_param_tuning$num.trees <- unique(c(rf_param$num.trees, c(75, 100, 125, 150, 175, 
	                            200, 250, 300,500)))

# Minimal node size (terminating condition) (integer - >=1)
rf_param$min.node.size        <- 1 # default as per ranger package
rf_param_tuning$min.node.size <- unique(c(rf_param$min.node.size, c(5, 10, 20, 30, 50, 
	                                75, 100, 150, 200, 300, 500, 1000)))
	                    
# Whether to sample with replacement (logical - TRUE/FALSE)
rf_param$replace        <- FALSE
rf_param_tuning$replace <- rf_param$replace

# format parameters
rf_param_raw          <- copy(rf_param)
rf_param    		  <- expand_split_grid(alg_param_tuning=rf_param, 
                      	   param_split_method="grid")
rf_param_tuning      <- copy(rf_param_tuning)
rf_param_tuning_rand <- expand_split_grid(alg_param_tuning=rf_param_tuning, 
                           param_split_method="grid")
rf_param_tuning_grid <- expand_split_grid(alg_param_tuning=rf_param_tuning, 
                           alg_param_default=rf_param_raw,  
                           param_split_method="non_grid")

print(length(rf_param_tuning_rand))
## 6552

# xgb (regression or classification boosted tree)
#----------------------------------------------------------------------------#

# # implementation: xgboost() [xgb]
# # documentation: http://xgboost.readthedocs.io/en/latest/R-package/xgboostPresentation.html

xgb_param            <- list() # list of default parameters
xgb_param_tuning     <- list() # list of parameter tuning ranges
xgb_param_tuning_add <- list()

# number of trees, i.e. individual learners
xgb_param$nrounds                  <- 500
xgb_param_tuning$nrounds           <- xgb_param$nrounds # note: tree number tuned 'automatically' 
									      					   # using inbuilt cv function (xgb.cv)
# learning rate
xgb_param$eta                      <- 0.01
xgb_param_tuning$eta               <- xgb_param$eta   # note: eta tuned along with  
													  # inbuilt cv function (xgb.cv)
xgb_param_tuning_add$eta     	   <- unique(c(xgb_param$eta, c(0.0001, 0.001, 0.01, 0.1, 0.2)))			      

# fraction of observations to use to build each tree
xgb_param$subsample                <- 0.8
xgb_param_tuning$subsample         <- unique(c(xgb_param$subsample, seq(from=0.3, to=0.9, by=0.1)))

# fraction of features to use to build each tree
xgb_param$colsample_bytree         <- 0.8   
xgb_param_tuning$colsample_bytree  <- unique(c(xgb_param$colsample_bytree , seq(from=0.3, to=0.9, by=0.1)))

# ~ min. number of instances needed in each terminal node (
# in non-linear case - same logic vs based on hessian weights..)
xgb_param$min_child_weight         <- 1
xgb_param_tuning$min_child_weight  <- unique(c(xgb_param$min_child_weight, seq(from=4, to=20, by=4)))

# max depth of a tree
xgb_param$max_depth                <- 3
xgb_param_tuning$max_depth         <- unique(c(xgb_param$max_depth, seq(from=5, to=14, by=2)))

# min loss reduction required to make a further split
xgb_param$gamma                    <- 0 
xgb_param_tuning$gamma             <- xgb_param$gamma 
xgb_param_tuning_add$gamma         <- unique(c(xgb_param$gamma, c(0.01,0.1,0.25,0.5,0.75,1,2,4,10)))	


# early stopping (used for tree_cv only)
xgb_param$early_stopping           <- 150
xgb_param_tuning$early_stopping    <- xgb_param$early_stopping 

# max nrounds (used for tree_cv only)
xgb_param$nrounds_max              <- 5000
xgb_param_tuning$nrounds_max       <- xgb_param$nrounds_max  

# format parameters
xgb_param_raw         <- copy(xgb_param)
xgb_param    		  <- expand_split_grid(alg_param_tuning=xgb_param, 
                      		param_split_method="grid")
xgb_param_tuning      <- copy(xgb_param_tuning)
xgb_param_tuning_rand <- expand_split_grid(alg_param_tuning=xgb_param_tuning, 
                            param_split_method="grid")
xgb_param_tuning_grid <- expand_split_grid(alg_param_tuning=xgb_param_tuning, 
                            alg_param_default=xgb_param_raw,  
                            param_split_method="non_grid")
xgb_param_tuning_add <- copy(xgb_param_tuning_add)

print(length(xgb_param_tuning_rand))
## 1764


# elnet (ols (regression) or logit (classification))
#----------------------------------------------------------------------------#

# implementation: glmnet(..., family="gaussian") [glmnet]
# documentation: https://www.rdocumentation.org/packages/glmnet/versions/2.0-5/topics/glmnet

elnet_param               <- list() # list of default parameters
elnet_param_tuning        <- list() # list of parameter tuning ranges

elnet_param$lambda        <- 0 
elnet_param_tuning$lambda <- unique(c(elnet_param$lambda, exp(-seq(-4,by=0.5)),exp(-seq(-0.1,7,by=0.1))))


elnet_param$alpha        <- 1 # Lasso (var selection)
elnet_param_tuning$alpha <- unique(c(elnet_param$alpha , c(0,0.125,0.25,0.375, 0.5,0.625, 0.75,0.875, 1)))

# format parameters
elnet_param_raw         <- copy(elnet_param)
elnet_param    		    <- expand_split_grid(alg_param_tuning=elnet_param, 
                      		 param_split_method="grid")
elnet_param_tuning      <- copy(elnet_param_tuning)
elnet_param_tuning_rand <- expand_split_grid(alg_param_tuning=elnet_param_tuning, 
                             param_split_method="grid")
elnet_param_tuning_grid <- expand_split_grid(alg_param_tuning=elnet_param_tuning, 
                              alg_param_default=elnet_param_raw,  
                              param_split_method="non_grid")

print(length(elnet_param_tuning_rand))
## 738

# lasso (ols (regression) or logit (classification))
#----------------------------------------------------------------------------#

# implementation: glmnet(..., family="gaussian") [glmnet]
# documentation: https://www.rdocumentation.org/packages/glmnet/versions/2.0-5/topics/glmnet

lasso_param               <- list() # list of default parameters
lasso_param_tuning        <- list() # list of parameter tuning ranges

lasso_param$lambda        <- 0 
lasso_param_tuning$lambda <- unique(c(lasso_param$lambda, exp(-seq(-4,by=0.5)),exp(-seq(-0.1,7,by=0.1))))


lasso_param$alpha        <- 1 # Lasso (var selection)
lasso_param_tuning$alpha <- 1

# format parameters
lasso_param_raw         <- copy(lasso_param)
lasso_param    		    <- expand_split_grid(alg_param_tuning=lasso_param, 
                      		 param_split_method="grid")
lasso_param_tuning      <- copy(lasso_param_tuning)
lasso_param_tuning_rand <- expand_split_grid(alg_param_tuning=lasso_param_tuning, 
                             param_split_method="grid")
lasso_param_tuning_grid <- expand_split_grid(alg_param_tuning=lasso_param_tuning, 
                              alg_param_default=lasso_param_raw,  
                              param_split_method="non_grid")

print(length(lasso_param_tuning_rand))
## 82

# tree (tree - regression or classification tree)
#----------------------------------------------------------------------------#

# implementation: rpart(..., method="annova") [rpart]
# documentation: https://stat.ethz.ch/R-manual/R-devel/library/rpart/html/rpart.html

tree_param        <- list() # list of default parameters
tree_param_tuning <- list() # list of parameter tuning ranges

# min bucket
tree_param$minbucket        <- 1
tree_param_tuning$minbucket <- unique(c(tree_param$minbucket , c(1,5,10,20,30,50,70,100,150,200,500)))

# max tree depth
tree_param$maxdepth          <- 5
tree_param_tuning$maxdepth   <- unique(c(tree_param$maxdepth , seq(1,30,2)))

# min gain (complexity parameter)
tree_param$mingain           <- 0 # correspond to cp parameter
tree_param_tuning$mingain    <- unique(c(tree_param$mingain,seq(0, 0.05, 0.01)))

# min split 
tree_param$minsplit          <- NULL
tree_param_tuning$minsplit   <- tree_param$minsplit 

# NOTE #1: min_split (the minimum number of observations that must exist in a node 
# in order for a split to be attempted) defaults to 3*minbucket)

# NOTE #2: to force splits even if no gain (gain=0) - set mingain (i.e. cp) to -1

# format parameters
tree_param_raw         <- copy(tree_param)
tree_param    		   <- expand_split_grid(alg_param_tuning=tree_param, 
                      		   param_split_method="grid")
tree_param_tuning      <- copy(tree_param_tuning)
tree_param_tuning_rand <- expand_split_grid(alg_param_tuning=tree_param_tuning, 
                               param_split_method="grid")
tree_param_tuning_grid <- expand_split_grid(alg_param_tuning=tree_param_tuning, 
                                alg_param_default=tree_param_raw,  
                                param_split_method="non_grid")

print(length(tree_param_tuning_rand))
## 990

# svm (svm - regression or classification tree)
#----------------------------------------------------------------------------#

# implementation: svm(...) [e1071]
# documentation: https://www.rdocumentation.org/packages/e1071/versions/1.6-8/topics/svm

svm_param        <- list() # list of default parameters
svm_param_tuning <- list() # list of parameter tuning ranges

# min bucket
svm_param$kernel           <- "radial"
svm_param_tuning$kernel    <- "radial"

# max svm depth
svm_param$cost          <- 1
svm_param_tuning$cost   <- unique(c(svm_param$cost,c(10^(-3:2))))

# min gain (complexity parameter)
svm_param$gamma           <- NA 
svm_param_tuning$gamma    <- unique(c(svm_param$gamma, c(2^(-3:2))))

# format parameters
svm_param_raw          <- copy(svm_param)
svm_param    		   <- expand_split_grid(alg_param_tuning=svm_param, 
                      		   param_split_method="grid")
svm_param_tuning       <- copy(svm_param_tuning)
svm_param_tuning_rand  <- expand_split_grid(alg_param_tuning=svm_param_tuning, 
                               param_split_method="grid")
svm_param_tuning_grid  <- expand_split_grid(alg_param_tuning=svm_param_tuning, 
                                alg_param_default=svm_param_raw,  
                                param_split_method="non_grid")

print(length(svm_param_tuning_rand))
## 42


# combine parameters
#----------------------------
ExpLearning.MLParameter <- list(
							intercept_param=intercept_param,
							ols_param=ols_param, 
							avg_param=avg_param,
							logit_param=logit_param, 
						    ## svm
							svm_param_raw=svm_param_raw, svm_param=svm_param,
							svm_param_tuning=svm_param_tuning, svm_param_tuning_rand=svm_param_tuning_rand, 
							svm_param_tuning_grid=svm_param_tuning_grid,								
						    ## rf
							rf_param_raw=rf_param_raw, rf_param=rf_param,
							rf_param_tuning=rf_param_tuning, rf_param_tuning_rand=rf_param_tuning_rand, 
							rf_param_tuning_grid=rf_param_tuning_grid,						
							## xgb
							xgb_param_raw=xgb_param_raw, xgb_param=xgb_param,
							xgb_param_tuning=xgb_param_tuning, xgb_param_tuning_rand=xgb_param_tuning_rand, 
							xgb_param_tuning_grid=xgb_param_tuning_grid,xgb_param_tuning_add=xgb_param_tuning_add,						
							## elnet
							elnet_param_raw=elnet_param_raw, elnet_param=elnet_param,
							elnet_param_tuning=elnet_param_tuning, elnet_param_tuning_rand=elnet_param_tuning_rand, 
							elnet_param_tuning_grid=elnet_param_tuning_grid,						
							## lasso
							lasso_param_raw=lasso_param_raw, lasso_param=lasso_param,
							lasso_param_tuning=lasso_param_tuning, lasso_param_tuning_rand=lasso_param_tuning_rand, 
							lasso_param_tuning_grid=lasso_param_tuning_grid,						
							## tree
							tree_param_raw=tree_param_raw, tree_param=tree_param,
							tree_param_tuning=tree_param_tuning, tree_param_tuning_rand=tree_param_tuning_rand, 
							tree_param_tuning_grid=tree_param_tuning_grid)

# save to use with data
#----------------------------
devtools::use_data(ExpLearning.MLParameter, overwrite = TRUE)

#----------------------------------------------------------------------------#
#                                    End                                     #
#----------------------------------------------------------------------------#

