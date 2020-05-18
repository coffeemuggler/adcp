#' Interpolate (rasterise) ADCP measurements to equal intervals
#'
#' This function interpolates cell-based ADCP data to equal intervals,
#' constant across all measurements.
#'
#' @param x \code{list}, ADCP data set to be interpolated
#'
#' @param n \code{Numerical} value, number of output depth values. If
#' omitted, it is generated from the largest number of cells of the input
#' data set.
#'
#' @param extend \code{Logical} value, option to provide values also
#' outside the original data range of individual measurements. Default
#' is \code{TRUE} (extension of interpolation outside original data range).
#'
#' @return Interpolated (rasterised) ADCP data set
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
#' ## interpolate and plot data set
#' x_int <- interpolate_adcp(x = x, n = 10)
#' plot_adcp(x = x_int)
#'
#' ## interpolate and plot data set without extrapolation
#' x_int_2 <- interpolate_adcp(x = x, n = 10, extend = FALSE)
#' plot_adcp(x = x_int_2)
#'
#' @export interpolate_adcp

interpolate_adcp <- function(
  x,
  n,
  extend = TRUE
  ) {

  ## get number of cells per measurement
  n_list <- lapply(X = x, FUN = function(x) {

    length(x$depth)
  })

  ## get maximum number of cells
  n_max <- max(do.call(c, n_list))

  ## assign interpolation resolution
  n <- ifelse(test = missing(x = n), yes = n_max, no = n)

  ## set extrapolation rule
  if(extend == TRUE) {
    rules <- c(2, 2)
  } else {
    rules <- c(1, 1)
  }

  ## get depth values for each measurement
  d_list <- lapply(X = x, FUN = function(x) {

    x$depth
  })

  ## get range of depth values
  d_range <- range(do.call(c, d_list), na.rm = TRUE)

  ## create interpolated depth values
  d_int <- seq(from = d_range[1],
               to = d_range[2],
               length.out = n)

  x_int <- lapply(X = x, FUN = function(x, d_int) {

    x$vel_l <- try(if(sum(is.na(x$vel_l)) == length(x$vel_l)) {

      rep(NA, n)
    } else {

      stats::approx(x = x$depth,
                    y = x$vel_l,
                    xout = d_int,
                    rule = rules)$y
    })

    x$vel_x <- try(if(sum(is.na(x$vel_x)) == length(x$vel_x)) {

      rep(NA, n)
    } else {

      stats::approx(x = x$depth,
                    y = x$vel_x,
                    xout = d_int,
                    rule = rules)$y
    })

    x$vel_v <- try(if(sum(is.na(x$vel_v)) == length(x$vel_v)) {

      rep(NA, n)
    } else {

      stats::approx(x = x$depth,
                    y = x$vel_v,
                    xout = d_int,
                    rule = rules)$y
    })

    x$vel_s <- try(if(sum(is.na(x$vel_s)) == length(x$vel_s)) {

      rep(NA, n)
    } else {

      stats::approx(x = x$depth,
                    y = x$vel_s,
                    xout = d_int,
                    rule = rules)$y
    })

    x$vel_e <- try(if(sum(is.na(x$vel_e)) == length(x$vel_e)) {

      rep(NA, n)
    } else {

      stats::approx(x = x$depth,
                    y = x$vel_e,
                    xout = d_int,
                    rule = rules)$y
    })

    x$vel_n <- try(if(sum(is.na(x$vel_n)) == length(x$vel_n)) {

      rep(NA, n)
    } else {

      stats::approx(x = x$depth,
                    y = x$vel_n,
                    xout = d_int,
                    rule = rules)$y
    })

    x$vel_u <- try(if(sum(is.na(x$vel_u)) == length(x$vel_u)) {

      rep(NA, n)
    } else {

      stats::approx(x = x$depth,
                    y = x$vel_u,
                    xout = d_int,
                    rule = rules)$y
    })

    x$vel_d <- try(if(sum(is.na(x$vel_d)) == length(x$vel_d)) {

      rep(NA, n)
    } else {

      stats::approx(x = x$depth,
                    y = x$vel_d,
                    xout = d_int,
                    rule = rules)$y
    })

    x$snr_e <- try(if(sum(is.na(x$snr_e)) == length(x$snr_e)) {

      rep(NA, n)
    } else {

      stats::approx(x = x$depth,
                    y = x$snr_e,
                    xout = d_int,
                    rule = rules)$y
    })

    x$snr_n <- try(if(sum(is.na(x$snr_n)) == length(x$snr_n)) {

      rep(NA, n)
    } else {

      stats::approx(x = x$depth,
                    y = x$snr_n,
                    xout = d_int,
                    rule = rules)$y
    })

    x$snr_u <- try(if(sum(is.na(x$snr_u)) == length(x$snr_u)) {

      rep(NA, n)
    } else {

      stats::approx(x = x$depth,
                    y = x$snr_u,
                    xout = d_int,
                    rule = rules)$y
    })

    x$snr_d <- try(if(sum(is.na(x$snr_d)) == length(x$snr_d)) {

      rep(NA, n)
    } else {

      stats::approx(x = x$depth,
                    y = x$snr_d,
                    xout = d_int,
                    rule = rules)$y
    })

    x$snr_mean <- try(if(sum(is.na(x$snr_mean)) == length(x$snr_mean)) {

      rep(NA, n)
    } else {

      stats::approx(x = x$depth,
                    y = x$snr_mean,
                    xout = d_int,
                    rule = rules)$y
    })

    x$cor_e <- try(if(sum(is.na(x$cor_e)) == length(x$cor_e)) {

      rep(NA, n)
    } else {

      stats::approx(x = x$depth,
                    y = x$cor_e,
                    xout = d_int,
                    rule = rules)$y
    })

    x$cor_n <- try(if(sum(is.na(x$cor_n)) == length(x$cor_n)) {

      rep(NA, n)
    } else {

      stats::approx(x = x$depth,
                    y = x$cor_n,
                    xout = d_int,
                    rule = rules)$y
    })

    x$cor_u <- try(if(sum(is.na(x$cor_u)) == length(x$cor_u)) {

      rep(NA, n)
    } else {

      stats::approx(x = x$depth,
                    y = x$cor_u,
                    xout = d_int,
                    rule = rules)$y
    })

    x$cor_d <- try(if(sum(is.na(x$cor_d)) == length(x$cor_d)) {

      rep(NA, n)
    } else {

      stats::approx(x = x$depth,
                    y = x$cor_d,
                    xout = d_int,
                    rule = rules)$y
    })

    x$cor_mean <- try(if(sum(is.na(x$cor_mean)) == length(x$cor_mean)) {

      rep(NA, n)
    } else {

      stats::approx(x = x$depth,
                    y = x$cor_mean,
                    xout = d_int,
                    rule = rules)$y
    })

    ## create new plotting depth vector
    x$depth <- d_int
    d_depth <- mean(diff(x$depth))
    x$depth_plot <- c(x$depth - d_depth, max(x$depth) + d_depth)

    ## return output data set
    return(x)

  }, d_int)
}
