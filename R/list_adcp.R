#' Output of adcp object elements
#'
#' Prints the elements of an imported ADCP data set, along with a short
#' description of the element names.
#'
#' @return List object with adcp object elements and short description.
#'
#' @examples
#'
#' list_adcp()
#'
#' @export list_adcp

list_adcp <- function() {

  object <- c("xy", "track", "depth", "depth_plot", "angle",
              "vel_l", "vel_x", "vel_v", "vel_s",
              "vel_e", "vel_n", "vel_u", "vel_d",
              "snr_e", "snr_n", "snr_u", "snr_d", "snr_mean",
              "cor_e", "cor_n", "cor_u", "cor_d", "cor_mean")

  description <- c("XY coordinates",
                   "Device track coordinates",
                   "Measurement cell depth vector",
                   "Plotting depths (upper-lower boundaries)",
                   "Device orientation angle",
                   "Velocity along stream",
                   "Velocity across stream",
                   "Velocity vertical",
                   "Velocity vector sum",
                   "Velocity east component",
                   "Velocity north component",
                   "Velocity up component",
                   "Velocity difference component",
                   "SNR east component",
                   "SNR north component",
                   "SNR up component",
                   "SNR difference component",
                   "SNR average",
                   "Correlation east component",
                   "Correlation north component",
                   "Correlation up component",
                   "Correlation difference component",
                   "Correlation average")

  print(data.frame(object,
                   description))
}
