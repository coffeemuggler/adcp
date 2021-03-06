% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/difference_adcp.R
\name{difference_adcp}
\alias{difference_adcp}
\title{Calculate and append deviation of velocity values from the mean}
\usage{
difference_adcp(x, m = "mean")
}
\arguments{
\item{x}{\code{List}, ADCP data set to be manipulated. Only the
velocity values will be processed.}

\item{m}{\code{Character} keyword, type of average from which to calculate
the difference. One out of \code{"mean"}, \code{"median"} and
\code{q0} (quantile, where n denotes which quantile, e.g. \code{q0.95}).
Default is \code{"mean"}.}
}
\value{
\code{List}, ADCP data set with difference data sets appended
}
\description{
This function evaluates and appends the deviation of values from the
depth-wise averages. Consequently, the following keywords and list elements
will be added:
}
\details{
\tabular{ll}{
  \strong{keyword} \tab \strong{description}\cr
  vel_l_diff \tab Velocity along stream, deviation from average\cr
  vel_x_diff \tab Velocity across stream, deviation from average\cr
  vel_v_diff \tab Velocity vertical, deviation from average\cr
  vel_s_diff \tab Velocity vector sum, deviation from average\cr
  vel_e_diff \tab Velocity east component, deviation from average\cr
  vel_n_diff \tab Velocity north component, deviation from average\cr
  vel_u_diff \tab Velocity up component, deviation from average\cr
  vel_d_diff \tab Velocity difference component, deviation from average\cr
}

The function can only be applied to previously interpolated data sets. See
\code{interpolate_adcop} for details.
}
\examples{

## load exampe data set
data(adcp_ex)

## interpolate data set
x_int <- interpolate_adcp(x = x, n = 10)

## calculate velocity differences from mean
x_diff <- difference_adcp(x = x_int)

## calculate velocity differences from quantile 0.75
x_diff <- difference_adcp(x = x_int, m = "q0.75")

## plot difference data set
plot_adcp(x_diff, what = "vel_l_diff")

}
\author{
Michael Dietze
}
\keyword{ADCP}
