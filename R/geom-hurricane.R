GeomHurricane <- ggplot2::ggproto("GeomHurricane", ggplot2::GeomPolygon,
                                  required_aes = c("lng", "lat"))

geom_hurricane <- function(mapping = NULL, data = NULL,
                           position = "identity", na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(stat = StatHurricane, geom = GeomHurricane, data = data,
                 mapping = mapping, position = position,
                 show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(na.rm = na.rm, ...))
}
