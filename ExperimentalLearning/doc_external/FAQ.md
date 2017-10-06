### [0] Conceptual background
----

For a brief overview of the conceptual background and motivation behind the _ExperimentalLearning_ Package see:
https://github.com/sysmedlab/chicago/tree/master/ExperimentalLearning/doc_external/conceptual_background.pdf

### [1] Loading and updating the _ExperimentalLearning_ Package 
----
##### CONTROL SECTION
````package_path="[ TO SPECIFY: path to package/]"````

##### CODE SECTION 
[1] _Obtain the source files_
````
#! shell
cd ${package_path}
## option - 1: SSH
git clone git@github.com:sysmedlab/chicago.git
## option - 2: HTTPS
git clone https://github.com/sysmedlab/chicago.git
````
[2] _Install the package  (locally)_
````
#! R
setwd(paste0(package_path, "ExperimentalLearning"))
library(devtools)
install("ExperimentalLearning", dependencies = TRUE)
library("ExperimentalLearning", character.only = TRUE)
````

*[3] _Source the scripts locally (this step is only needed if local code changes are to be respected)_
````
source(paste0(package_path, 
"ExperimentalLearning/misc/dep_load_local.R")) # load dependencies locally

for (x in list.files(paste0(package_path, "ExperimentalLearning/R"))) {

	print(sprintf("source: %s", x))

	source(paste0(package_path,"ExperimentalLearning/R/",x))

}
````



### [2] Executing the _'effect_on_group'_ function
----
_Note: The below code uses the 'test_df' (a small scale simulated dataset) included in the ExperimentalLearning 
package_

##### CONTROL SECTION
````output_path="[ TO SPECIFY: path in which to store all output/]"````

##### CODE SECTION
````
library(data.table)
temp_df <- copy(test_df)[1:1000]

ensemble_result <- effect_on_group(
	df=temp_df, 
	assignment_var="treatment", 
	group_var=c("control_num_1", "control_num_2", "control_num_3", "control_num_4"),
	control_var=c("control_cat_1", "control_cat_2", "control_cat_3", "control_cat_4"),
	id_var="id",
	cluster_var=NULL, 
	quiet=0.5,
	max_core=2, 
	rand_grid_iter=4,
	perm_test="baseline_balance",
	permutation=10,
	tuning_method="rand_grid", 
	predictor=list("logit","avg", "rf"),
	output_path=output_path)

print(ensemble_result)
````

### [3] Understanding (and changing) the arguments passed to the different functions
----

[1] _Function Overview_
- See _doc/function_overview.csv_ for an overview of all functions in the package (incl. a description of their function and output).

[2] _Argument Overview_
- Once the package is loaded overviews of all parameters are easily accessible:
  * _ExpLearning.Parameter_: An overview of all arguments, their purpose and (where applicable) their default values  
  * _ExpLearning.MLParameter_: An overview of all tuning parameters and their default values


### [4] Analysing the output of the _'effect_on_group'_ function
----
The _effect_on_group_ function returns a list of results - individual results in this list are documented below:


##### OUTPUTS RETURNED BY THE FUNCTION

##Loss
- **_ensemble_loss_** : The out-of-sample/CV loss associated with the ensemble. The returned loss corresponds to the loss specified via the 'losstype' argument (e.g. auc, neg-mse, etc.). 
- **_[name of individual learner]__loss_** : The out-of-sample/CV loss associated with each of the individual learners. The returned loss corresponds to the loss specified via the 'losstype' argument (e.g. AUC, (neg) MSE, etc.). 
- **_ensemble_loss_auc_** : The out-of-sample/CV AUC associated with the ensemble. 
- **_[name of individual learner]__loss_auc_** : The out-of-sample/CV AUC associated with each of the individual learners. 
- **_ensemble_loss_nnls_** : [Result returned for testing/development purposes] The out-of-sample/CV loss associated with the ensemble when using negative-least squares to stack the individual learners. The returned loss corresponds to the loss specified via the 'losstype' argument (e.g. auc, neg-mse, etc.) [NUll if ensemble_agg!='ols']. 
- **_ensemble_loss_auc_nnls_** : [Result returned for testing/development purposes] The out-of-sample AUC associated with the ensemble when using negative-least squares to stack the individual learners [NUll if ensemble_agg!='ols']. 

##P-values and Permutation Tests
- **_ensemble_pval_baseline_balance_** : Permutation-based p-value associated with the ensemble - baseline_balance permutation test. 
- **_ensemble_pval_baseline_signal_** : Permutation-based p-value associated with the ensemble - baseline_signal permutation test. 
- **_ensemble_pval_signal_a_** : Permutation-based p-value associated with the ensemble - supplementary signal test (a) permutation test. 
- **_ensemble_pval_signal_b_** : Permutation-based p-value associated with the ensemble - supplementary signal test (b) permutation test. 
- **_ensemble_pval__[90/95]__[test name]__** : 90%/95% CI for the permutation-based p-value associated with the ensemble for the specified permutation test.  

- **_[name of individual learner]__pval_baseline_signal_** : Permutation-based p-value associated with the specified individual learner - baseline_signal permutation test. 
- **_[name of individual learner]__pval__[90/95]__baseline_signal_** : 90%/95% CI for the permutation-based p-value associated with the specified individual learner - baseline_signal permutation test. 

- **_ensemble_perm_loss__[test name]__** : The permuted out-of-sample/CV losses associated with the ensemble for the specified permutation test, i.e. one loss per permutation. The returned loss corresponds to the loss specified via the 'losstype' argument (e.g. auc, neg-mse, etc.).
- **_ensemble_mean_perm_loss__[test name]__** : The permuted out-of-sample/CV losses associated with the ensemble for the specified permutation test, averaged over all permutations. The returned loss corresponds to the mean loss specified via the 'losstype' argument (e.g. auc, neg-mse, etc.).

##Wald Tests
- **_ensemble_wald$ensemble_wald_pval_all_** : [Result returned for testing/development purposes]. P-value for a Wald 'baseline' signal test using the entire dataset. 
- **_ensemble_wald$ensemble_wald_pval_is_** : [Result returned for testing/development purposes]. P-value for a Wald 'baseline' signal test  test using the training data only. 
- **_ensemble_wald$ensemble_wald_pval_is_oos_** : [Result returned for testing/development purposes]. P-value for a 'holdout' Wald 'baseline' signal test  test (estimated using the training data / assessed using the holdout data). 
- **_ensemble_wald$ensemble_wald_pval_is_oos_perm_** : [Result returned for testing/development purposes]. P-value for a permutation-based 'holdout' 'baseline' signal test Wald test (estimated using the training data / assessed using the holdout data). 
- **_ensemble_wald$ensemble_wald_perm_F_** : [Result returned for testing/development purposes]. The permuted F-stats associated with the permutation-based 'holdout' 'baseline' signal test Wald test, i.e. one F-stat per permutation. 
- **_ensemble_wald$ensemble_wald_model_F_** : [Result returned for testing/development purposes]. The F-stat associated with the linear model underlying the permutation-based 'holdout' 'baseline' signal test Wald test. 

##Learner and Ensemble Construction 
- **_ensemble_weight_** : Ensemble weights (intercept + weight for each of the individual learners). 
- **_ensemble_weight_nnls_** : [Result returned for testing/development purposes] Ensemble weights (intercept + weight for each of the individual learners) when using negative-least squares to stack the individual learners. 
- **_[name of individual learner]__outer_param_** : Tuning parameters for each of the individual learners selected in the outer fold (the training data in its entirety). 
- **_[name of individual learner]__inner_param_** : Tuning parameters for each of the individual learners selected in each of the inner folds (subsets of the training data). 

##Data and Predictions_
- **_df_** : Data.table containing the data (along with any ID/control variables attached to the input data) along with (i) the ensemble predictions ('prediction') and (ii) the individual learner predictions ('ind_pred.[name of individual learner]'). If within_sample==FALSE (the default) the returned data corresponds to the holdout set (holdout==TRUE) and the predictions correspond to the out-of-sample predictions. 

##Misc
- **_model_predict_function_** : Ensemble prediction function - function to generate predictions for any given dataset using the fitted/tuned ensemble. 
- **_learner_list_** : List storing each of the individual fitted/tuned learners. 
- **_run_time_** : Runtime. 
- **_setting_** : Settings. 
- **_dict_** : Variable names.  



##### OUTPUTS SAVED IN THE SPECIFIED OUTPUT FOLDER

Note: All graphical output is surpressed if the 'quiet' argument passed to the effect_on_group function is set to 2.  

Note: All output is associated with, i.e. easily retrievable based-on, the 'exec_id' specified at the time of the _effect_on_group_ function execution. 

- ***permanalysis_baseline_signal__[exec_id].pdf*** : Graph pertaining to the generation of the permutation-based p-value for the final ensemble (see **'ensemble_pval'** above)

- ***tuning_plot_foldout__[id/name of fold on which algorithm is being tuned/fitted (using CV)]__alg__[name of algorithm being tuned]__losstype__[losstype used to tune the algorithm]__[exec id].html*** : Tuning plot capturing the performance of the algorithm in question across each tuning iteration, i.e. each parameter set. Note that each *.html* file is (for now) accompanied by a folder which can be safely ignored. 

### [5] Potential Installation Problems
----

- On MacOSx the installation of RcppEigen may potentially fail (which may or may not impact the execution of the package). See below for installation instructions to address this problem.
````
# shell
curl https://cran.r-project.org/bin/macosx/mavericks/contrib/3.3/RcppEigen_0.3.2.9.1.tgz
R CMD INSTALL RcppEigen_0.3.2.9.1.tgz

# R
library(RcppEigen) # test if installation was successful

````

- On MacOSx the installation of devtools may potentially fail due a git2r dependency. See below for installation instructions to address this problem.
````
# Assuming - Homebrew install of R

# Install dependencies
brew install openssl
brew install

# Ensure that the path does NOT point to a non-brew openssl  (see https://github.com/hadley/devtools/issues/961
which openssl # return: /usr/bin/openssl

# Install
wget https://cran.r-project.org/src/contrib/git2r_0.19.0.tar.gz
R CMD INSTALL --configure-args='--with-libssl-include=/opt/local/include \
--with-libssl-lib=/opt/local/lib' git2r_0.19.0.tar.gz
````

- The installation of the required xgboost/ranger packages may fail due to a compiler issue. See below for a recommended solution to this problem. 

````
# Assuming - Anaconda install of R (e.g. on Linux) 

# Installa anaconda (mini) 
cd ~
wget https://repo.continuum.io/miniconda/Miniconda2-latest-MacOSX-x86_64.sh ## adjust if not on MaxOsX
bash Miniconda2-latest-MacOSX-x86_64.sh

# Update R (see: https://repo.continuum.io/pkgs/r/linux-64/ to check for the most up-to-date version of R)
conda install -c r r=3.3.2

# Update the compiler
conda install -c anaconda gcc=4.8.5

# Append to bashrc (~/.bashrc)
export PATH=~/anaconda2/bin/:$PATH
export LD_LIBRARY_PATH=~/miniconda2/lib:$LD_LIBRARY_PATH

# Update
source ~/.bashrc

# Confirm that the set-up was sucessfull
which R
## ~/miniconda2/bin/R
which gcc 
##  ~/miniconda2/bin/gcc
gcc --version
## >=4.8.5

````

