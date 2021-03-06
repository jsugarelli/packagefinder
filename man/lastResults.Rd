% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/packagefinder.r
\name{lastResults}
\alias{lastResults}
\title{Searching for packages on CRAN}
\usage{
lastResults(display = "viewer")
}
\arguments{
\item{display}{Describes where the search results shall be shown. Either \code{"viewer"}, \code{"console"} or \code{"browser"}. If \code{"viewer"}, the results are shown in RStudio's Viewer pane if the RStudio IDE is being used. If \code{results = "console"} the search results are shown as a text table in the R console. \code{results = "browser"} shows the search results in the web browser.}
}
\value{
No return value.
}
\description{
Shows the results of the last search with \code{\link{findPackage}()}.
}
\author{
Joachim Zuckarelli \email{joachim@zuckarelli.de}
}
