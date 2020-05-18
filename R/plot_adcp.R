#' Plot ADCP data set
#'
#' Plots one of a series of selectable ADCP data set variables.
#'
#' Available keywords are:
#'
#' \tabular{ll}{
#'   \strong{keyword} \tab \strong{description}\cr
#'   xy \tab XY coordinates\cr
#'   track \tab Device track coordinates\cr
#'   depth \tab Measurement cell depth vector\cr
#'   depth_plot \tab Plotting depths (upper-lower boundaries)\cr
#'   angle \tab Device orientation angle\cr
#'   vel_l \tab Velocity along stream\cr
#'   vel_x \tab Velocity across stream\cr
#'   vel_v \tab Velocity vertical\cr
#'   vel_s \tab Velocity vector sum\cr
#'   vel_e \tab Velocity east component\cr
#'   vel_n \tab Velocity north component\cr
#'   vel_u \tab Velocity up component\cr
#'   vel_d \tab Velocity difference component\cr
#'   snr_e \tab SNR east component\cr
#'   snr_n \tab SNR north component\cr
#'   snr_u \tab SNR up component\cr
#'   snr_d \tab SNR difference component\cr
#'   snr_mean \tab SNR average\cr
#'   cor_e \tab Correlation east component\cr
#'   cor_n \tab Correlation north component\cr
#'   cor_u \tab Correlation up component\cr
#'   cor_d \tab Correlation difference component\cr
#'   cor_mean \tab Correlation average\cr
#'}
#'
#' Data sets that have been subject to the function \code{difference_adcp}
#' will contain the following additional elements:
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
#' @param x \code{list}, ADCP data set to be plotted. Should be generated
#' using \code{read_adcp} or \code{interpolate_adcp}.
#'
#' @param what \code{Character} value, variable to be plotted. See
#' \code{list_adcp()} for available keywords. Default is \code{vel_l}
#' (velocity along stream).
#'
#' @param ref \code{Character} value, x-axis reference. Can be one out of
#' \code{"track"} (GPS based track position), \code{xy} (GPS based
#' coordinates) and \code{ID} (measurement sequence ID). Default
#' is \code{"ID"}.
#'
#' @param \dots Additional arguments passed to the plot function.
#'
#' @return Image plot of the data set.
#'
#' @author Michael Dietze
#'
#' @keywords ADCP
#'
#' @examples
#'
#' ## load example data
#' data(adcp_ex)
#'
#' ## plot data set
#' plot_adcp(x = x)
#'
#' ## plot data set with adjusted scale limits
#' plot_adcp(x = x,
#'           xlim = c(5, 10),
#'           ylim = c(0.5, 0.3),
#'           zlim = c(0.2, 0.5))
#'
#' ## plot data set with customised axes labels
#' plot_adcp(x = x,
#'           xlab = "Measurement ID",
#'           ylab = "Depth",
#'           zlab = "Velocity along stream (m/s)")
#'
#' ## plot data set with track as reference
#' plot_adcp(x = x,
#'           ref = "track")
#'
#' ## plot across-stream velocity
#' plot_adcp(x = x, what = "vel_x")
#'
#' ## create customised colour scale
#' col_user <- colorRampPalette(c("grey20", "grey50", "red",
#'                                "orange", "yellow", "white"))
#'
#' ## plot data set with customised colour scale
#' plot_adcp(x = x, col = col_user(200))
#'
#' @export plot_adcp

plot_adcp <- function(
  x,
  what = "vel_l",
  ref = "ID",
  ...
) {

  ## check/set arguments
  dot_args <- list(...)

  if("col" %in% names(dot_args)) {

    cols <- dot_args$col
  } else {

    col <- grDevices::colorRampPalette(colors = c("purple", "blue",
                                                  "green", "yellow", "red"))

    cols <- col(200)
  }

  if("xlab" %in% names(dot_args)) {

    xlab <- dot_args$xlab
  } else {

    xlab <- ""
  }

  if("ylab" %in% names(dot_args)) {

    ylab <- dot_args$ylab
  } else {

    ylab <- "Depth"
  }

  if("zlab" %in% names(dot_args)) {

    zlab <- dot_args$zlab
  } else {

    zlab <- ""
  }

  if("main" %in% names(dot_args)) {

    main <- dot_args$main
  } else {

    main <- what
  }

  ## extract x-values
  x_plot <- lapply(X = x, FUN = function(x, ref) {

    if(ref == "xy") {

      x_out <- x$xy
    } else {

      x_out <- x$track
    }

    return(x_out)

  }, ref = ref)

  x_plot <- do.call(rbind, x_plot)
  x_plot <- sqrt(x_plot[,1]^2 + x_plot[,2]^2)
  x_plot <- cumsum(c(0, abs(diff(x_plot))))
  x_plot <- t(t(x_plot) - t(x_plot)[,1])
  x_plot <- c(x_plot, x_plot[length(x_plot)] +
                x_plot[length(x_plot)] - x_plot[length(x_plot) - 1])

  if(ref == "ID") {

    x_plot <- seq(from = 1,
                  to = length(x) + 1)
  }

  ## extract y-values
  y_plot <- lapply(X = x, FUN = function(x) {

    c(x$depth_plot)
  })

  if("ylim" %in% names(dot_args)) {

    ylim <- dot_args$ylim
  } else {

    ylim <- range(unlist(y_plot),
                  na.rm = TRUE)
  }

  if("xlim" %in% names(dot_args)) {

    xlim <- dot_args$xlim
  } else {

    xlim <- range(x_plot)
  }

  ## extract z-values
  z_plot <- lapply(X = x, FUN = function(x, what) {

    return(x[what][[1]])
  }, what = what)

  if("zlim" %in% names(dot_args)) {

    zlim <- dot_args$zlim
  } else {

    zlim <- range(do.call(c, z_plot), na.rm = TRUE)
  }

  z_plot <- lapply(X = z_plot, FUN = function(z_plot, z_lim) {

    z_plot[z_plot < zlim[1]] <- zlim[1]
    z_plot[z_plot > zlim[2]] <- zlim[2]
    return(z_plot)
  }, zlim)

  ## assign colour map
  col_cls <- seq(from = zlim[1],
                 to = zlim[2],
                 length.out = length(cols))

  col_plt <- lapply(X = z_plot, FUN = function(z_plot, col_cls) {

    as.numeric(cut(x = z_plot,
                   breaks = col_cls,
                   include.lowest = TRUE,
                   include.highest = TRUE))
  }, col_cls)

  ## save original plot parameters
  par_0 <- graphics::par(no.readonly = TRUE)

  ## prepare plot device
  graphics::par(mar = c(par_0$mar[1:3], 5),
                xaxs = "i",
                yaxs = "i")

  ## create empty plot area
  graphics::plot(NA,
                 xlim = xlim,
                 ylim = rev(ylim),
                 ann = F,
                 axes = F)
  graphics::box()
  graphics::title(main = main)
  graphics::axis(side = 1)
  graphics::mtext(side = 1,
                  text = xlab,
                  line = 3)
  graphics::axis(side = 2,
                 at = pretty(rev(ylim)),
                 labels = -rev(pretty(-ylim)))
  graphics::mtext(side = 2,
                  text = ylab,
                  line = 3)

  for(i in 1:length(x)) {

    for(j in 1:length(z_plot[[i]])) {

      graphics::polygon(x = x_plot[c(i, i, i + 1, i + 1)],
                        y = y_plot[[i]][c(j, j + 1, j + 1, j)],
                        border = NA,
                        col = cols[col_plt[[i]][j]])
    }
  }

  ## add legend
  graphics::par(new = TRUE,
                mar = c(par_0$mar[1:3], 4.1))
  graphics::plot(NA,
                 xlim = c(0, 1),
                 ylim = zlim,
                 ann = FALSE,
                 axes = FALSE)
  graphics::points(x = rep(1, length(cols)),
                   y = seq(from = zlim[1],
                           to = zlim[2],
                           length.out = length(cols)),
                   col = cols,
                   pch = 15, cex = 2)
  graphics::axis(side = 4)
  graphics::mtext(text = zlab,
                  side = 4,
                  line = 2.5)
  graphics::abline(v = 1)
}
