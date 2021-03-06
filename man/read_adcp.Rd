% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_adcp.R
\name{read_adcp}
\alias{read_adcp}
\title{Read Sontek ADCP exported mat file}
\usage{
read_adcp(file, include_edges = FALSE)
}
\arguments{
\item{file}{\code{Character} value, file name to be imported.}

\item{include_edges}{\code{Logical} value, option to include or omit
importing cells at the bank edges. Default is \code{FALSE}.}
}
\value{
\code{List} object with imported ADCP data.
}
\description{
Read a Sontek ADCP exported Matlab *.mat file to R.
}
\examples{

x = 1

}
\author{
Michael Dietze
}
\keyword{ADCP}
