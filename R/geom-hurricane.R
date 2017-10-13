GeomHurricane <-
  ggplot2::ggproto("GeomHurricane", ggplot2::Geom,
                   required_aes = c("x", "y",
                                    "r_ne", "r_se", "r_nw", "r_sw"),

                   default_aes = ggplot2::aes(colour = "NA",
                                              fill = "grey20",
                                              size = 0.5,
                                              linetype = 1,
                                              alpha = 0.8,
                                              scale_radii = 1),

                   draw_key = ggplot2::draw_key_polygon,

                   draw_group = function(data, panel_scales, coord) {
                     first_row <- data[1, ]

                     x      <- first_row$x
                     y      <- first_row$y
                     colour <- first_row$colour
                     alpha  <- first_row$alpha
                     fill   <- first_row$fill

                     rose_ne <- wind_rose_piece(x, y, first_row$r_ne, "ne")
                     rose_se <- wind_rose_piece(x, y, first_row$r_se, "se")
                     rose_sw <- wind_rose_piece(x, y, first_row$r_sw, "sw")
                     rose_nw <- wind_rose_piece(x, y, first_row$r_nw, "nw")

                     rose <- do.call(rbind, list(rose_ne,
                                                 rose_se,
                                                 rose_sw,
                                                 rose_nw))

                     coords <- coord$transform(rose, panel_scales)

                     grid::polygonGrob(x = coords$x,
                                       y = coords$y,
                                       gp = grid::gpar(col = colour,
                                                       fill = fill,
                                                       alpha = alpha))
                   })

geom_hurricane <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(stat = stat, geom = GeomHurricane, data = data,
                 mapping = mapping, position = position,
                 show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(na.rm = na.rm, ...))
}
