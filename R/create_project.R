# create_project.R
# -------------------------------------------------------------------------
# A script detailing the function used to create a new R project with
# some ready-to-use scripts for analysis
# -------------------------------------------------------------------------
# Created by Bert on 11-07-2016
# -------------------------------------------------------------------------
# Last edited by Bert on 12-07-2016
# -------------------------------------------------------------------------



#' Create a new R Project.
#'
#' Create some common sub-directories used in an R project, and import
#' some scripts containing headers for common analysis tasks.
#'
#' @param project_name The name that the project will take
#' @param base_dir The base directory for the project
#' @param within Optional path to a subdirectory within base_dir where the project will be located
#' @param sub_dirs Optional names for any additional sub-direcories that the project should contain
#'
#' @return Logical, indicating that the project has been successfully created
#' @export create_project
create_project <- function(project_name,
                           base_dir = "M:/clients",
                           within = NULL,
                           sub_dirs = NULL) {

  # The default sub-directories that should be created by the function
  base_sub_dirs <- c("data", "data/processed", "src", "src/rmd", "src/functions", "src/shiny", "output", "output/tables", "output/figures", "doc")

  # The root directory for the project takes the project name
  if(!is.null(within)){
    root_dir <- file.path(base_dir, within, project_name)
  } else {
    root_dir <- file.path(base_dir, project_name)
  }

  # Add any additional user-specified sub-directories to the defaults
  sub_dirs <- c(base_sub_dirs, sub_dirs)

  # create the directories
  sapply(sub_dirs, FUN = function(x) {dir.create(file.path(root_dir, x), recursive = TRUE)})

  # create the .Rproj file with default specifications
  rproject_specs <- "Version: 1.0

RestoreWorkspace: Default
SaveWorkspace: Default
AlwaysSaveHistory: Default

EnableCodeIndexing: Yes
UseSpacesForTab: Yes
NumSpacesForTab: 2
Encoding: UTF-8

RnwWeave: Sweave
LaTeX: pdfLaTeX"

  cat(paste(rproject_specs), file = file.path(root_dir, paste(project_name, "Rproj", sep = ".")))

  # import the script templates to the root directory

  create_init_script(project_name = project_name, root_dir = root_dir)

  create_processing_script(project_name = project_name, root_dir = root_dir)

  create_results_script(project_name = project_name, root_dir = root_dir)

  create_eda_script(project_name = project_name, root_dir = root_dir)
}




#' Create an initialising script.
#'
#' Builds the skeleton of an initialisation script in the specified root directory.
#' Primarily for internal use in create_project().
#'
#' @param project_name The name of the R project.
#' @param root_dir The root directory of the R project.
#'
#' @return Creates a script within the directory specified.
#' @export create_init_script
create_init_script <- function(project_name, root_dir) {
  init_text <- c("# 0_", project_name, "_initialise.R
# Created by ", Sys.info()["user"], ", on ", format(Sys.Date(), "%d %m %Y"), "
# -------------------------------------------------------------------------
# A script designed to import the required libraries and functions for the
# ", project_name, " analysis
#
# -------------------------------------------------------------------------
# WORKFLOW
#   0. INITIALISE
#
# -------------------------------------------------------------------------
# Last edited ", format(Sys.Date(), "%d %m %Y"), " by create_project()
#   - Created
# -------------------------------------------------------------------------


# 0. INITIALISE -----------------------------------------------------------

# clear the workspace
rm(list = setdiff(ls(), c(\"base_dir\", \"archive\")))
gc()
spatial_packages <- FALSE

# import packages
library(magrittr)
library(plyr)           # data manipulation
library(dplyr)          # data manipulation
library(reshape2)       # data manipulation
library(readr)          # improved data importing
library(ggplot2)        # grammar of graphics
library(scales)         # axis labelling functions
library(lubridate)      # time/date functions
library(shiny)
library(grid)
library(gridExtra)
library(stringr)
library(xlsx)
library(marketview)
if(spatial_packages){
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(spatstat)
}
rm(spatial_packages)

# set directories
data_dir <- file.path(base_dir, \"data\")
output_dir <- file.path(base_dir, \"output\")
")

  cat(paste(init_text, collapse = ""), file = file.path(root_dir, "src", paste0("0_", project_name, "_initialise.R")))
}


#' Create a processing script.
#'
#' Builds the skeleton of a data processing script in the specified root directory.
#' Primarily for internal use in create_project().
#'
#' @param project_name The name of the R project.
#' @param root_dir The root directory of the R project.
#'
#' @return Creates a script within the directory specified.
#' @export create_processing_script
create_processing_script <- function(project_name, root_dir) {
  procces_text <- c("# 1_", project_name, "_processing.R
# Created by ", Sys.info()["user"], ", on ", format(Sys.Date(), "%d %m %Y"), "
# -------------------------------------------------------------------------
# A script designed to process the raw ", project_name, " data for analysis
# -------------------------------------------------------------------------
# WORKFLOW
#   0. INITIALISE
#   1. IMPORT DATA
#
# -------------------------------------------------------------------------
# Last edited ", format(Sys.Date(), "%d %m %Y"), " by create_project()
#   - Created
# -------------------------------------------------------------------------


# 0. INITIALISE -----------------------------------------------------------

source(file.path(base_dir, \"src\", \"0_", project_name, "_initialise.R\"))

# 1. IMPORT DATA ----------------------------------------------------------


# 2. SAVE THE PROCESSED DATA ----------------------------------------------


save( ,
file = file.path(data_dir, \"processed\", \"", project_name, "_data.RData\"))
")

  cat(paste(procces_text, collapse = ""), file = file.path(root_dir, "src", paste0("1_", project_name, "_processing.R")))

}


#' Create a results script.
#'
#' Builds the skeleton of a results script in the specified root directory.
#' Primarily for internal use in create_project().
#'
#' @param project_name The name of the R project.
#' @param root_dir The root directory of the R project.
#'
#' @return Creates a script within the directory specified.
#' @export create_results_script
create_results_script <- function(project_name, root_dir) {
  results_text <- c("# 2_", project_name, "_results.R
# Created by ", Sys.info()["user"], ", on ", format(Sys.Date(), "%d %m %Y"), "
# -------------------------------------------------------------------------
# A script designed to render and save the ", project_name, " results
# -------------------------------------------------------------------------
# WORKFLOW
#   0. INITIALISE
#   1. IMPORT DATA
#   2. RESULTS HERE
# -------------------------------------------------------------------------
# Last edited ", format(Sys.Date(), "%d %m %Y"), " by create_project()
#   - Created
# -------------------------------------------------------------------------

base_dir <- \"", root_dir, "\"
archive <- FALSE

# 0. INITIALISE -----------------------------------------------------------

source(file.path(base_dir, \"src\", \"0_", project_name, "_initialise.R\"))

# 1. IMPORT DATA ----------------------------------------------------------

data_refresh <- file.info(file.path(base_dir, \"src\", \"1_", project_name, "_processing.R\"))$mtime > file.info(file.path(data_dir, \"processed\", \"", project_name, "_data.RData\"))$mtime
if(is.na(data_refresh)) data_refresh <- TRUE

if(data_refresh) {
source(file.path(base_dir, \"src\", \"1_", project_name, "_processing.R\"))
} else {
load(file.path(data_dir, \"processed\", \"", project_name, "_data.RData\"))
}



if(archive) {
# Archive previous versions of the outputs
archive_time <- Sys.time() %>%
gsub(x = ., pattern = \" \", repl = \"_\") %>%
gsub(x = ., pattern = \"[^0-9a-z_]\", repl = \"\")
# create an archive for old files
dir.create(path = file.path(base_dir, \"archive\", archive_time), recursive = TRUE)
# copy the old files
file.copy(from = output_dir, to = file.path(base_dir, \"archive\", archive_time), recursive = TRUE)
# remove the files once they are archived
file.remove(list.files(output_dir, full.names = TRUE, recursive = TRUE))
}

# initialise the labelling and set the output subdirectory
fig_num <- 1
tab_num <- 1


# 2. RESULTS HERE ---------------------------------------------------------


")

  cat(paste(results_text, collapse = ""), file = file.path(root_dir, "src", paste0("2_", project_name, "_results.R")))

}


#' Create an EDA script.
#'
#' Builds the skeleton of an EDA markdown script in the specified root directory.
#' Primarily for internal use in create_project().
#'
#' @param project_name The name of the R project.
#' @param root_dir The root directory of the R project.
#'
#' @return Creates a script within the directory specified.
#' @export create_eda_script
create_eda_script <- function(project_name, root_dir) {
  eda_text <- c("---
title: \"", project_name, " EDA\"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(fig.width=12, fig.height=8, fig.path='Figs/',
echo=FALSE, warning=FALSE, message=FALSE)
```

```{r init, results = \"hide\"}
base_dir <- \"", root_dir, "\"

# 0. INITIALISE -----------------------------------------------------------

source(file.path(base_dir, \"src\", \"0_", project_name, "_initialise.R\"))

# 1. IMPORT DATA ----------------------------------------------------------

data_refresh <- file.info(file.path(base_dir, \"src\", \"1_", project_name, "_processing.R\"))$mtime > file.info(file.path(data_dir, \"processed\", \"", project_name, "_data.RData\"))$mtime
if(is.na(data_refresh)) data_refresh <- TRUE

if(data_refresh) {
source(file.path(base_dir, \"src\", \"1_", project_name, "_processing.R\"))
} else {
load(file.path(data_dir, \"processed\", \"", project_name, "_data.RData\"))
}

```

")

  cat(paste(eda_text, collapse = ""), file = file.path(root_dir, "src", paste0(project_name, "_EDA.Rmd")))

}
