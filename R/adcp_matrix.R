#' Convert elements of ADCP list object to matrix
#'
#' The function extracts one set of variables from an interpolated adcp
#' data set and converts it to a numeric matrix.
#'
#' The data structure of adcp objects (lists with each measurement being
#' an own list element, containing all measured variables) is not suitable
#' for many subsequent analysis steps. Thus, to effectively work with distinct
#' variable across the data set, the data needs to be extracted and organised
#' in a usefule data structure, hence a mastrix. Not that before extracting
#' variables, the imported DCP data set needs to be interpolated to equal
#' depth intervals, thus creating regularly spaced data. Interpolation should
#' be done with the function \code{interpolate_adcp}. A typical error message
#' for the case of not yet interpolated data is the following:
#' \code{number of rows of result is not a multiple of vector length (arg 1)}.
#'
#' @param x \code{R} object, interpolated ADCP data set to be converted
#'
#' @param what \code{Character} value, name of the data set to convert. See
#' \code{list_adcp()} for available keywords or see help of function
#' \code{plot_adcp}.
#'
#' @return \code{Matrix} with extracted ADCP data.
#'
#' @author Michael Dietze
#'
#' @keywords ADCP
#'
#' @examples
#'
#' ## load example data set
#' data(adcp_ex)
#'
#' ## interpolate data set
#' x_int <- interpolate_adcp(x = x, n = 10)
#'
#' ## extract stream-wise velocity as matrix
#' m_vel_l <- adcp_matrix(x = x_int, what = "vel_l")
#'
#' @export adcp_matrix

adcp_matrix <- function(

  x,
  what
) {

  x_matrix <- do.call(cbind, lapply(X = x, FUN = function(x, what) {

    x[[what]]
  }, what))

  return(x_matrix)
}
