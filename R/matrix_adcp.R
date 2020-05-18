#' Convert matrix to ADCP list object element
#'
#' Convert matrix to ADCP list object element
#'
#' @param x \code{Matrix} data set to be converted to ADCP object
#'
#' @param adcp \code{ADCP} object to be changed. Object is a list as, for
#' example created by \code{rad_adcp}.
#'
#' @param what \code{Character} value, name of the data set to convert. If
#' the same as one of the already existing ADCP object names, that will be
#' overwritten. Otherwise, the data will be appended as new element of the
#' ADCp object
#'
#' @return \code{ADCP} object.
#'
#' @author Michael Dietze
#'
#' @keywords ADCP
#'
#' @examples
#'
#' x = 1
#'
#' @export matrix_adcp

matrix_adcp <- function(

  x,
  adcp,
  what
) {

  ## check/correct data set length
  x_fill <- matrix(nrow = nrow(x),
                   ncol = length(adcp))
  x_fill[,1:ncol(x)] <- x

  ## process each ADCP measurement
  for(i in 1:length(adcp)) {

    adcp[[i]][[what]] <- x_fill[,i]
  }

  ## return output
  return(adcp)
}
