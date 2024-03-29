% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_project.R
\name{create_report_script}
\alias{create_report_script}
\title{Create a report script.}
\usage{
create_report_script(project_name, root_dir)
}
\arguments{
\item{project_name}{The name of the R project.}

\item{root_dir}{The root directory of the R project.}
}
\value{
Creates a script within the directory specified.
}
\description{
Builds the skeleton of a script that creates a powerpoint report in the specified root directory.
Primarily for internal use in create_project().
}
