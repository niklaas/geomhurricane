#' compute_group function for StatHurricane
#'
#' @importFrom magrittr %<>%
compute_StatHurricane <- function(data, scales) {
  radii  <- list(data$r_ne, data$r_se, data$r_sw, data$r_nw)
  pieces <- c("ne", "se", "sw", "nw")

  calc_bearing <- function(piece) {
    stopifnot(is.character(pieces),
              piece %in% pieces)

    piece       <- as.numeric(factor(piece, levels = pieces))
    piece_width <- 365 / 4
    start_piece <- (piece_width * (piece - 1)) + 1
    end_piece   <- piece_width * piece
    bearing     <- start_piece:end_piece

    bearing
  }

  calc_radius <- function(radius) {
    stopifnot(is(radius, "units"))
    radius <- units::set_units(radius, "m")

    as.numeric(radius)
  }

  pieces_coords <-
    purrr::map2(pieces, radii, function(piece, radius) {
                  bearing <- calc_bearing(piece)
                  radius <- calc_radius(radius)

                  # TODO: Vectorize this
                  piece_coords <-
                    geosphere::destPoint(c(data$lng, data$lat),
                                         b = bearing,
                                         d = radius)

                  piece_coords <- as.data.frame(piece_coords)
                  piece_coords$group <- piece

                  piece_coords %<>% dplyr::rename(x = lon, y = lat)

                  piece_coords
                })

  pieces_coords <- do.call(rbind, pieces_coords)

  pieces_coords
}

StatHurricane <-
  ggplot2::ggproto("StatHurricane", ggplot2::Stat,
                   compute_group = compute_StatHurricane,
                   required_aes = c("lng", "lat",
                                    "r_ne", "r_se", "r_nw", "r_sw"))

#' export
stat_hurricane <-
  function(mapping = NULL, data = NULL, geom = "polygon",
           position = "identity", show.legend = NA, inherit.aes = TRUE, ...) {
    ggplot2::layer(stat = StatHurricane,
                   data = data,
                   mapping = mapping,
                   geom = geom,
                   position = position,
                   show.legend = show.legend,
                   inherit.aes = inherit.aes,
                   params = list(...))
  }
