#' Create a new R script
#' Creates a new R script with the headers and workflow mapped out in the same
#' manner as a standard results scrpt from create_project()
#'
#'
#' @param name The name for the new R script
#' @param project_name Name of the project (should always be the current working directory)
#' @param location Directory where the script should be created - default is the /R directory
#'
#' @export create_script
create_script <- function(name, project_dir = getwd(), location = "R") {

  project_name <- gsub("^.*/", "", project_dir)

script_text <- c("# ", name, ".R
# Created by ", Sys.info()["user"], ", on ", format(Sys.Date(), "%d %m %Y"), "
# -------------------------------------------------------------------------
# A script designed to ", name, " for the ", project_name, " project
# -------------------------------------------------------------------------
# WORKFLOW
#   0. INITIALISE
#   1. IMPORT DATA
#   2. RESULTS HERE
# -------------------------------------------------------------------------
# Last edited ", format(Sys.Date(), "%d %m %Y"), " by create_project()
#   - Created
# -------------------------------------------------------------------------

# 0. INITIALISE -----------------------------------------------------------

source(file.path(\"", project_dir, "\", \"R\", \"0 ", project_name, " initialise.R\"))

# 1. IMPORT DATA ----------------------------------------------------------

data_import()
")

  cat(paste(script_text, collapse = ""), file = file.path(project_dir, location, paste0(name, ".R")))
}
