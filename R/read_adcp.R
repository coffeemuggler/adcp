#' Read Sontek ADCP exported mat file
#'
#' Read a Sontek ADCP exported Matlab *.mat file to R.
#'
#' @param file \code{Character} value, file name to be imported.
#'
#' @param include_edges \code{Logical} value, option to include or omit
#' importing cells at the bank edges. Default is \code{FALSE}.
#'
#' @return \code{List} object with imported ADCP data.
#'
#' @author Michael Dietze
#'
#' @keywords ADCP
#'
#' @examples
#'
#' x = 1
#'
#' @export read_adcp

read_adcp <- function(

  file,
  include_edges = FALSE
) {

  ## read Matlab object
  x <- R.matlab::readMat(file)

  ## extract step IDs
  s <- x$System[[9]][,1]
  s_ok <- rep(TRUE, length(s))
  if(include_edges == FALSE) {

    s_ok[s != 3] <- FALSE
  }

  ## extract cell vectors
  n <- x$Summary[[8]][s_ok,1]
  d <- x$Summary[[7]][s_ok,1]

  ## extract depth vectors
  d_0 <- x$System[[7]][s_ok,1]
  d_z <- x$System[[8]][s_ok,1]

  D <- vector(mode = "list",
              length = length(n))

  for(i in 1:length(D)) {

    ## assign depth vector
    depth <- d_0[i] + seq(from = d_z[i] / 2,
                          by = d_z[i],
                          length.out = n[i])
    depth_plot <- c(d_0[i], d_0[i] + seq(from = d_z[i],
                                         by = d_z[i],
                                         length.out = n[i]))

    ## get location corrdinates
    xy <- x$GPS[[12]][s_ok,][i,]

    ## get track coordinates
    trk <- x$Summary[[9]][s_ok,][i,]

    ## get direction of device
    a <- x$System[[12]][s_ok,][i]

    ## check for sufficient number of cells in measurement
    if(n[i] > 1) {

      ## extract correlation data
      cor_enud <- x$WaterTrack[[3]][1:n[i],,s_ok][,,i]
      cor_mean <- rowMeans(cor_enud)

      ## extract SNR for all measurements
      snr_enud <- x$System[[4]][1:n[i],,s_ok][,,i]
      snr_mean <- rowMeans(snr_enud)

      ## extract velocity matrices
      vel_enud <- x$WaterTrack[[1]][1:n[i],,s_ok][,,i]

      ## rotate vector components
      ## (l along stream, x across stream, v vertical, s vector sum)
      vel_lxvs <- vel_enud
      vel_lxvs[,1] <- sin(a * pi / 180) * vel_enud[,1] +
        cos(a * pi / 180) * vel_enud[,2]
      vel_lxvs[,2] <- cos(a * pi / 180) * vel_enud[,1] -
        sin(a * pi / 180) * vel_enud[,2]
      vel_lxvs[,4] <- sqrt(vel_lxvs[,1]^2 +
                             vel_lxvs[,2]^2 +
                             vel_lxvs[,3]^2)
    } else {

      cor_enud <- matrix(nrow = n[i], ncol = 4)
      cor_mean <- NA
      snr_enud <- matrix(nrow = n[i], ncol = 4)
      snr_mean <- NA
      vel_enud <- matrix(nrow = n[i], ncol = 4)
      vel_lxvs <- matrix(nrow = n[i], ncol = 4)
    }



    ## assign output data
    D[[i]] <- list(xy = xy,
                   track = trk,
                   depth = depth,
                   depth_plot = depth_plot,
                   angle = a,
                   vel_l = vel_lxvs[,1],
                   vel_x = vel_lxvs[,2],
                   vel_v = vel_lxvs[,3],
                   vel_s = vel_lxvs[,4],
                   vel_e = vel_enud[,1],
                   vel_n = vel_enud[,2],
                   vel_u = vel_enud[,3],
                   vel_d = vel_enud[,4],
                   snr_e = snr_enud[,1],
                   snr_n = snr_enud[,2],
                   snr_u = snr_enud[,3],
                   snr_d = snr_enud[,4],
                   snr_mean = snr_mean,
                   cor_e = cor_enud[,1],
                   cor_n = cor_enud[,2],
                   cor_u = cor_enud[,3],
                   cor_d = cor_enud[,4],
                   cor_mean = cor_mean)
  }

  return(D)
}
