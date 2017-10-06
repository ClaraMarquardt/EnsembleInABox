#----------------------------------------------------------------------------#

# Purpose:     Update the parameter definitions in the ExperimentalLearning
#              Package
# Author:      Clara Marquardt
# Date:        2017


#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
#                                    CONTROL                                 #
#----------------------------------------------------------------------------#

# parameters
wd_path         <- commandArgs(trailingOnly = TRUE)[1]
package_name    <- commandArgs(trailingOnly = TRUE)[2]
backup_path     <- commandArgs(trailingOnly = TRUE)[3]
os              <- commandArgs(trailingOnly = TRUE)[4]

print(sprintf("wd_path: %s",  wd_path))
print(sprintf("package_name: %s",  package_name))
print(sprintf("backup_path: %s",  backup_path))
print(sprintf("os: %s",  os))

# dependencies
library("devtools")
library(roxygen2)
library(data.table)
library(ExperimentalLearning)

# paths
setwd(paste0(wd_path, "/", package_name,"/", "R"))
param_file <- paste0(wd_path, package_name, "/doc/parameter_def_reference.csv")
function_file <- paste0(wd_path, package_name, "/doc/function_def_reference.csv")
def_file_folder <- paste0(wd_path, package_name, "/", "R")

#----------------------------------------------------------------------------#
#                                      CODE                                  #
#----------------------------------------------------------------------------#

# read in values 
#----------------------------
param_overview <- fread(param_file)
param_overview <- param_overview[!(parameter_desc=="")]

function_overview <- fread(function_file)
function_overview <- function_overview[!(function_title=="")]


# format
#----------------------------
param_overview[,parameter_name:=paste0("@param ",parameter_name ), 
	by=1:nrow(param_overview)]
param_overview[,parameter_desc:=paste0(parameter_name, " *",  parameter_desc), 
	by=1:nrow(param_overview)]

function_overview[,function_title:=paste0("@title", " *",  function_title), 
	by=1:nrow(function_overview)]
function_overview[,function_desc:=paste0("@description", " *",  function_desc), 
	by=1:nrow(function_overview)]
function_overview[,function_return:=paste0("@return", " *",  function_return), 
	by=1:nrow(function_overview)]

# generate backup
#----------------------------
move_command <- paste0("cp -R ", getwd(), " ", backup_path, "R_", 
	as.character(format(Sys.time(), "%d_%m_%Y_%H_%M_%S")), "/")
system(move_command, wait=T)

# loop - parameter definitions
#----------------------------
for (def_file in list.files(def_file_folder)) {

	# status
	print(sprintf("Updating: %s", def_file))

	# loop over terms
	invisible(lapply(1:nrow(param_overview), function(param_id) {


		param     <- param_overview$parameter_name[param_id]
		param_def <- param_overview$parameter_desc[param_id]
		def_file   <- paste0(def_file_folder, "/", def_file)

		print(param)
		# print(param_def)

		if (os=="linux") {
			sed_command <- paste0("sed -i -e 's/\\(", param," \\)\\(.*\\)/", 
							param_def, "/g' ", def_file)
		} else if (os=="mac") {
			sed_command <- paste0("sed -i '' -e 's/\\(", param," \\)\\(.*\\)/", 
							param_def, "/g' ", def_file)
		}
		system(sed_command, wait=T)

	}))

}

print("sucessfully updated all parameter definitions")

# loop - function title, description, return 
#----------------------------
for (def_file in list.files(def_file_folder)) {

	# status
	print(sprintf("Updating: %s", def_file))

	# loop over terms
	invisible(lapply(1:nrow(function_overview), function(function_id) {


		if (def_file %like% paste0(function_overview$function_name[function_id], 
			"(_data)*\\.R" )) {

			title  <- function_overview[function_id]$function_title
			desc   <- function_overview[function_id]$function_desc
			return <- function_overview[function_id]$function_return
			def_file   <- paste0(def_file_folder, "/", def_file)

			print(title)
			print(desc)
			print(return)

			if (os=="linux") {
				sed_command <- paste0("sed -i -e 's/\\(", "@title","\\)\\(.*\\)/", 
							title, "/g' ", def_file)
			} else if (os=="mac") {
				sed_command <- paste0("sed -i '' -e 's/\\(", "@title","\\)\\(.*\\)/", 
							title, "/g' ", def_file)
			}

			system(sed_command, wait=T)


			if (os=="linux") {
				sed_command <- paste0("sed -i -e 's/\\(", "@description","\\)\\(.*\\)/", 
							desc, "/g' ", def_file)
			} else if (os=="mac") {
				sed_command <- paste0("sed -i '' -e 's/\\(", "@description","\\)\\(.*\\)/", 
							desc, "/g' ", def_file)
			}
			
			system(sed_command, wait=T)

			if (os=="linux") {
		    	sed_command <- paste0("sed -i -e 's/\\(", "@return","\\)\\(.*\\)/", 
							return, "/g' ", def_file)
		    } else if (os=="mac") {
		    	sed_command <- paste0("sed -i '' -e 's/\\(", "@return","\\)\\(.*\\)/", 
							return, "/g' ", def_file)
	
		    }
			
			system(sed_command, wait=T)
		}


	}))

}

print("sucessfully updated all function definitions")

#----------------------------------------------------------------------------#
#                                       END                                  #
#----------------------------------------------------------------------------#

