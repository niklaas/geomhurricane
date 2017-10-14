GeomHurricane <-
  ggplot2::ggproto(
    "GeomHurricane",
    ggplot2::Geom,

    required_aes = c("x", "y", "r_ne", "r_se", "r_nw", "r_sw"),

    default_aes = ggplot2::aes(colour = "NA",
                               fill = "grey20",
                               size = 0.5,
                               linetype = 1,
                               alpha = 0.8,
                               scale_radii = 1),

    draw_key = ggplot2::draw_key_polygon,

    draw_group = function(data, panel_scales, coord) {
      # TODO: This implementation is unflexible. It is because it only works if
      # an `aes` `fill` and a single (!) hurricane is provided.
      # `wind_rose_piece`, which calls `geosphere::destPoint`, isn't vectorized
      # properly. It can't handle `data$x` and `data$y` to be vecors of `length
      # != 1`.
      #
      # While `geosphere::destPoint` is vectorized (input must then be given as
      # a matrix), grouping its returns together properly isn't trivial. Each
      # wind rose consists of four (n) pieces. Each of these pieces consists of
      # 3 (n) levels. In the end one would need to deal with four times three
      # (n times n) `geosphere::destPoint` return matrices (each having a `lon`
      # and `lat` column) -- multiplied by `length(data$x)` or
      # `length(data$y)`. I have not solved this yet. (The notation in brackets
      # refers to the case when one could also provide multiple storms. In this
      # case one would need to group each storm by `lat`, `lng`, and `date`.
      #
      # To circumvent this complexity, I cut data to a single row. `nrow(data)
      # == 1` if `fill` and only a single storm is provided anyway. But if this
      # isn't the case, the following line will prevent `wind_rose_piece`
      # (actually `geosphere::destPoint`) from failing.
      data <- data[1, ]

      # TODO: Disobeys DRY principle
      data %<>%
        dplyr::mutate(r_ne = r_ne * scale_radii,
                      r_se = r_se * scale_radii,
                      r_sw = r_sw * scale_radii,
                      r_nw = r_nw * scale_radii)

      # TODO: Disobeys DRY principle
      rose_ne <- wind_rose_piece(data$x, data$y, data$r_ne, "ne")
      rose_se <- wind_rose_piece(data$x, data$y, data$r_se, "se")
      rose_sw <- wind_rose_piece(data$x, data$y, data$r_sw, "sw")
      rose_nw <- wind_rose_piece(data$x, data$y, data$r_nw, "nw")

      rose <- do.call(rbind, list(rose_ne,
                                  rose_se,
                                  rose_sw,
                                  rose_nw))

      coords <- coord$transform(rose, panel_scales)

      grid::polygonGrob(x = coords$x,
                        y = coords$y,
                        gp = grid::gpar(col = data$colour,
                                        fill = data$fill,
                                        alpha = data$alpha))
    }
  )

#' Create a windrose of a storm
#'
#' @section Aesthetics:
#'
#' Required aesthetics are bold
#'
#' \itemize{
#'  \item \strong{x}
#'  \item \strong{y}
#'  \item \strong{r_ne}
#'  \item \strong{r_se}
#'  \item \strong{r_sw}
#'  \item \strong{r_nw}
#'  \item fill
#'  \item scale_radii
#' }
geom_hurricane <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(stat = stat, geom = GeomHurricane, data = data,
                 mapping = mapping, position = position,
                 show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(na.rm = na.rm, ...))
}
