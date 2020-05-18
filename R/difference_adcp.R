#' Calculate and append deviation of velocity values from the mean
#'
#' This function evaluates and appends the deviation of values from the
#' depth-wise averages. Consequently, the following keywords and list elements
#' will be added:
#'
#' \tabular{ll}{
#'   \strong{keyword} \tab \strong{description}\cr
#'   vel_l_diff \tab Velocity along stream, deviation from average\cr
#'   vel_x_diff \tab Velocity across stream, deviation from average\cr
#'   vel_v_diff \tab Velocity vertical, deviation from average\cr
#'   vel_s_diff \tab Velocity vector sum, deviation from average\cr
#'   vel_e_diff \tab Velocity east component, deviation from average\cr
#'   vel_n_diff \tab Velocity north component, deviation from average\cr
#'   vel_u_diff \tab Velocity up component, deviation from average\cr
#'   vel_d_diff \tab Velocity difference component, deviation from average\cr
#' }
#'
#' The function can only be applied to previously interpolated data sets. See
#' \code{interpolate_adcop} for details.
#'
#' @param x \code{List}, ADCP data set to be manipulated. Only the
#' velocity values will be processed.
#'
#' @param m \code{Character} keyword, type of average from which to calculate
#' the difference. One out of \code{"mean"}, \code{"median"} and
#' \code{q0} (quantile, where n denotes which quantile, e.g. \code{q0.95}).
#' Default is \code{"mean"}.
#'
#' @return \code{List}, ADCP data set with difference data sets appended
#'
#' @author Michael Dietze
#'
#' @keywords ADCP
#'
#' @examples
#'
#' ## load exampe data set
#' data(adcp_ex)
#'
#' ## interpolate data set
#' x_int <- interpolate_adcp(x = x, n = 10)
#'
#' ## calculate velocity differences from mean
#' x_diff <- difference_adcp(x = x_int)
#'
#' ## calculate velocity differences from quantile 0.75
#' x_diff <- difference_adcp(x = x_int, m = "q0.75")
#'
#' ## plot difference data set
#' plot_adcp(x_diff, what = "vel_l_diff")
#'
#' @export difference_adcp

difference_adcp <- function(

  x,
  m = "mean"
) {

  ## define data sets for which to calculate difference from mean
  p <- c("vel_l", "vel_x", "vel_v", "vel_s",
         "vel_e", "vel_n", "vel_u", "vel_d")

  ## check validity of keywords
  if(!(m == "mean" | m == "median" | grepl(x = m, pattern = "q"))) {

    stop("Keyword for m not supported!")
  }

  ## loop through all data sets to process
  for(i in 1:length(p)) {

    ## assign values to process
    p_i <- p[i]

    ## convert ADCP object to matrix
    x_p <- adcp_matrix(x = x, what = p_i)

    ## calculate depth-wise average
    x_m <- apply(X = x_p, MARGIN = 1, FUN = function(x_p, m) {

      if(m == "mean") {

        mean(x_p, na.rm = TRUE)
      } else if(m == "median") {

        stats::median(x_p, na.rm = TRUE)
      } else {

        prob <- as.numeric(substr(x = m, start = 2, stop = nchar(m)))
        stats::quantile(x = x_p, probs = prob, na.rm = TRUE)
      }

    }, m)

    ## substract average from data set
    x <- lapply(X = x, FUN = function(x, p_i, x_m) {

      x[[length(x) + 1]] <- as.numeric(x[[p_i]] - x_m)

      names(x)[length(x)] <- paste0(p_i, "_diff")

      return(x)

    }, p_i, x_m)
  }

  ## return output data set
  return(x)
}
