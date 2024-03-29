% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{find_org_level}
\alias{find_org_level}
\title{Find which ids of this levels are appropriate.}
\usage{
find_org_level(levels, organisms)
}
\arguments{
\item{levels}{A x with the oranism recevining the grant}

\item{organisms}{A data.frame from \code{\link[=organism]{organism()}}.}
}
\value{
A subset of the organism that matches the levels
}
\description{
Find which ids of this levels are appropriate.
}
