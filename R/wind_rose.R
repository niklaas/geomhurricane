#' Coordinates of piece of wind rose
#'
#' @param lng Longitude of center
#' @param lat Latitude of center
#' @param piece_key A string of either c("ne", "se", "sw", "nw")
#' @param radius The radius of the piece
#'
#' @return Latitude and Longitude coordinates for a piece of a wind rose
wind_rose_piece <- function(lng, lat, radius, piece_key) {
  # TODO: While this already is quite elegant, it would be cool to refacotr
  # this in such a way that radii could be provided in a vector and the number
  # of pieces and each pieces radius would be calculated automatically. Then it
  # would be possible to abolish `pieces` and everything related with it.
  #
  # TODO: Another bottleneck of the current implementation is that the lines
  # connecting each piece don't hit the center of the rose. This is because the
  # groups in `bearings` don't include the max/min of the neighbouring group.
  pieces        <- c("ne", "se", "sw", "nw")

  stopifnot(piece_key %in% pieces)

  bearing_range <- 1:360
  bearings      <- split(bearing_range,
                         cut(bearing_range,
                             breaks = length(pieces),
                             labels = pieces))

  coords <-
    geosphere::destPoint(c(lng, lat),
                         b = bearings[[piece_key]],
                         d = radius * 1852) %>%
      as.data.frame()

  coords %<>%
    dplyr::rename(x = lon, y = lat)

  coords
}
