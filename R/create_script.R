#' Create a new R script
#' Creates a new R script with the headers and workflow mapped out in the same
#' manner as a standard results scrpt from create_project()
#'
#'
#' @param name The name for the new R script
#' @param location Directory where the script should be created - default is the /R directory
#'
#' @export create_script
create_script <- function(name, location = "R") {

  project_dir <- getwd()
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

source(file.path(\"R\", \"0 ", project_name, " initialise.R\"))

# 1. IMPORT DATA ----------------------------------------------------------

data_import()
")

  cat(paste(script_text, collapse = ""), file = file.path(location, paste0(name, ".R")))
}


#' Create a new R Markdown script
#' Creates a new R Markdown script with the headers and workflow mapped out in the same
#' manner as a standard scrpt from create_project()
#'
#'
#' @param name The name for the new RMd script
#' @param location Directory where the script should be created - default is the root directory
#'
#' @export create_rmd
create_rmd <- function(name, location = "/") {

  project_dir <- getwd()
  project_name <- gsub("^.*/", "", project_dir)

  script_text <- c("---
title: \"\"
output:
html_document:
css: templates/mvlstyle.css
toc: TRUE
toc_float: TRUE
number_sections: TRUE
editor_options:
chunk_output_type: console
---

![](templates/Generic Banner.png)

# **", name, "** {-}
*`r  gsub(\"^0\", \"\", format(Sys.Date(), \"%d %B, %Y\"))`*

```{r setup, include=FALSE}
knitr::opts_chunk$set(fig.width = 8, fig.height = 5, fig.path = 'output/figures',
echo = FALSE, warning = FALSE, message = FALSE)
```

```{r init, results = \"hide\"}

# 0. INITIALISE -----------------------------------------------------------

source(\"R/0 ", project_name, " initialise.R\")
library(knitr)

# 1. IMPORT DATA ----------------------------------------------------------


data_import()

```

")

  cat(paste(script_text, collapse = ""), file = file.path(project_dir, location, paste0(name, ".RMd")))
}

