% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_summarize_years}
\alias{fars_summarize_years}
\title{\code{fars_summarize_years} Creates a data frame with a number of year(s) file(s) entered in the years element using the fars_read_years
 function from this package. Desired files need to be in the working directory.}
\usage{
fars_summarize_years(years)
}
\arguments{
\item{years}{a numberic vector (or string of numberic values) of desired accident file years.}
}
\value{
A tibble summarizing the number of years of accident files loaded.
}
\description{
\code{fars_summarize_years} Creates a data frame with a number of year(s) file(s) entered in the years element using the fars_read_years
 function from this package. Desired files need to be in the working directory.
}
\note{
A warning will appear if any year entered into years doesn't have a corresponding file.

An error will occur if a non-numeric value is entered into years.
}
\examples{
\dontrun{
fars_summarize_years("2013, 2014, 2015")
}

}
