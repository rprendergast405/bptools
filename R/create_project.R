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
  base_sub_dirs <- c("data", "data/processed", "R", "R/rmd", "R/functions", "R/shiny", "output", "output/tables", "output/figures", "Report")

  # The root directory for the project takes the project name
  if(!is.null(within)){
    root_dir <- file.path(base_dir, within, project_name)
  } else {
    root_dir <- file.path(base_dir, project_name)
  }

  # Add any additional user-specified sub-directories to the defaults
  sub_dirs <- c(base_sub_dirs, sub_dirs)

  # create the directories
  cat("Creating directories\n")
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

  cat("Creating .RProj file\n")
  cat(paste(rproject_specs), file = file.path(root_dir, paste(project_name, "Rproj", sep = ".")))

  # import the script templates to the root directory
  cat("Creating script templates:\n")
  cat(paste0("  0 ", project_name, " initialise.R ..."))
  create_init_script(project_name = project_name, root_dir = root_dir)
  cat(" Done\n")

  cat(paste0("  1 ", project_name, " processing.R ..."))
  create_processing_script(project_name = project_name, root_dir = root_dir)
  cat(" Done\n")

  cat(paste0("  2 ", project_name, " results.R ..."))
  create_results_script(project_name = project_name, root_dir = root_dir)
  cat(" Done\n")

  cat(paste0("  3 ", project_name, " report.R ..."))
  create_report_script(project_name = project_name, root_dir = root_dir)
  cat(" Done\n")

  cat(paste0("  ", project_name, " RUN SCRIPT.R ..."))
  create_run_script(project_name = project_name, root_dir = root_dir)
  cat(" Done\n")

  cat(paste0("  R/rmd/", project_name, " EDA.Rmd ..."))
  create_eda_script(project_name = project_name, root_dir = root_dir)
  cat(" Done\n")

  cat(paste0("Copying mvl_template.pptx to ", root_dir, " ..."))
  file.copy(from = file.path("M:/R/mvl_template.pptx"),
            to = root_dir)

  file.copy(from = file.path("M:/R/mvl_template_old.pptx"),
            to = root_dir)
  cat(" Done\n")

  cat("Project created successfully")
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
create_init_script <- function(project_name, root_dir) {
  init_text <- c("# 0 ", project_name, " initialise.R
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
library(tidyverse)      # imports the 'tidyverse' libraries
library(stringr)
library(forcats)
library(scales)         # axis labelling functions
library(lubridate)      # time/date functions
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
fig_dir <- file.path(output_dir, \"figures\")
tab_dir <- file.path(output_dir, \"tables\")

# Source any function scripts
source_dir(file.path(base_dir, \"R/functions\"))
")

  cat(paste(init_text, collapse = ""), file = file.path(root_dir, "R", paste0("0 ", project_name, " initialise.R")))
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
create_processing_script <- function(project_name, root_dir) {
  procces_text <- c("# 1 ", project_name, " processing.R
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

base_dir <- \"", root_dir, "\"

# 0. INITIALISE -----------------------------------------------------------

source(file.path(base_dir, \"R\", \"0 ", project_name, " initialise.R\"))

# 1. IMPORT DATA ----------------------------------------------------------


# 2. SAVE THE PROCESSED DATA ----------------------------------------------


save(list = ls(all.names = TRUE)[!grepl(\"dir\", ls(all.names = TRUE))],
     file = file.path(data_dir, \"processed\", \"", project_name, " data.RData\"))
")

  cat(paste(procces_text, collapse = ""), file = file.path(root_dir, "R", paste0("1 ", project_name, " processing.R")))

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
create_results_script <- function(project_name, root_dir) {
  results_text <- c("# 2 ", project_name, " results.R
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

# 0. INITIALISE -----------------------------------------------------------

source(file.path(base_dir, \"R\", \"0 ", project_name, " initialise.R\"))

# 1. IMPORT DATA ----------------------------------------------------------

data_import(base_dir)


# initialise the labelling and set the output subdirectory
fig_num <- 1
tab_num <- 1


# 2. RESULTS HERE ---------------------------------------------------------


")

  cat(paste(results_text, collapse = ""), file = file.path(root_dir, "R", paste0("2 ", project_name, " results.R")))

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

source(file.path(base_dir, \"R\", \"0 ", project_name, " initialise.R\"))
library(knitr)
# 1. IMPORT DATA ----------------------------------------------------------


data_import(base_dir)


```

")

  cat(paste(eda_text, collapse = ""), file = file.path(root_dir, "R", "rmd", paste0(project_name, " EDA.Rmd")))

}


#' Create a report script.
#'
#' Builds the skeleton of a script that creates a powerpoint report in the specified root directory.
#' Primarily for internal use in create_project().
#'
#' @param project_name The name of the R project.
#' @param root_dir The root directory of the R project.
#'
#' @return Creates a script within the directory specified.
create_report_script <- function(project_name, root_dir) {
  report_text <- c("# 3 ", project_name, " report.R
# Created by ", Sys.info()["user"], ", on ", format(Sys.Date(), "%d %m %Y"), "
# -------------------------------------------------------------------------
# A script designed to compile a powerpoint report containing the
# ", project_name, " results.
# -------------------------------------------------------------------------
# WORKFLOW
#   0. INITIALISE
#   1. IMPORT DATA
#   2. COMPILE THE REPORT
# -------------------------------------------------------------------------
# Last edited ", format(Sys.Date(), "%d %m %Y"), " by create_project()
#   - Created
# -------------------------------------------------------------------------
base_dir <- \"", root_dir, "\"

# 0. INITIALISE -----------------------------------------------------------

source(file.path(base_dir, \"R\", \"0 ", project_name, " initialise.R\"))
library(ReporteRs)

# 1. IMPORT DATA ----------------------------------------------------------


data_import(base_dir)


# 2. COMPILE THE REPORT ---------------------------------------------------------

# create the report object ----
ppt_report <- pptx(\" \", \"mvl_template.pptx\")


# Add a title slide ----
ppt_report <- ppt_report %>%
  addSlide(slide.layout = \"Title Slide\") %>%
  addTitle(\"CLIENT\") %>%
  addSubtitle(\"", project_name, "\") %>%
  addSubtitle(paste(\"Prepared for: CONTACT,\", Sys.Date() %>% format(\"%d %B, %Y\"))


# Add Content -------------------------------------------------------------


# Save the finished report ------------------------------------------------

writeDoc(ppt_report, file = file.path(base_dir, \"Report/", project_name, ".pptx\"))
")

  cat(paste(report_text, collapse = ""), file = file.path(root_dir, "R", paste0("3 ", project_name, " report.R")))

}



#' Create a run script.
#'
#' Builds the skeleton of a run script in the specified root directory.
#' Primarily for internal use in create_project().
#'
#' @param project_name The name of the R project.
#' @param root_dir The root directory of the R project.
#'
#' @return Creates a script within the directory specified.
create_run_script <- function(project_name, root_dir) {
  run_text <- c("# ", project_name, " RUN SCRIPT.R
# Created by ", Sys.info()["user"], ", on ", format(Sys.Date(), "%d %m %Y"), "
# -------------------------------------------------------------------------
# A script designed to run the ", project_name, " project from start to
# finish
# -------------------------------------------------------------------------
# WORKFLOW
#   0. INITIALISE
#   1. IMPORT DATA
#   2. PRODUCE THE RESULTS
#   3. COMPILE THE REPORT
# -------------------------------------------------------------------------
# Last edited ", format(Sys.Date(), "%d %m %Y"), " by create_project()
#   - Created
# -------------------------------------------------------------------------

base_dir <- \"", root_dir, "\"

# 0. INITIALISE -----------------------------------------------------------

source(file.path(base_dir, \"R\", \"0 ", project_name, " initialise.R\"))

# 1. IMPORT DATA ----------------------------------------------------------

# Update the processed data if necessary ----

data_refresh <- file.info(file.path(base_dir, \"R\", \"1 ", project_name, " processing.R\"))$mtime > file.info(file.path(data_dir, \"processed\", \"", project_name, " data.RData\"))$mtime
if(is.na(data_refresh)) data_refresh <- TRUE

if(data_refresh) {
  source(file.path(base_dir, \"R\", \"1 ", project_name, " processing.R\"))
} else data_import(base_dir)

# 2. PRODUCE THE RESULTS --------------------------------------------------

# Archive the old results ----
output_archive(base_dir)

# Re-run the results script ----
source(file.path(base_dir, \"R\", \"2 ", project_name, " results.R\"))


# 3. COMPILE THE REPORT ---------------------------------------------------

# Re-run the results script ----
source(file.path(base_dir, \"R\", \"3 ", project_name, " report.R\"))
")

  cat(paste(run_text, collapse = ""), file = file.path(root_dir, "R", paste0(project_name, " RUN SCRIPT.R")))

}
