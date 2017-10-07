StatHurricane <-
  ggplot2::ggproto("StatHurricane", Stat,
                   compute_group = function(data, scales) {
                     # TODO: compute here
                   },
                   required_aes = c("x", "y",
                                    "r_ne", "r_se", "r_nw", "r_sw"))

stat_hurricane <-
  function(mapping = NULL, data = NULL, geom = "polygon",
           position = "identity", show.legend = NA, outliers = TRUE,
           inherit.aes = TRUE, ...) {
    ggplot2::layer(stat = StatHurricane,
                   data = data,
                   mapping = mapping,
                   geom = geom,
                   position = position,
                   show.legend = show.legend,
                   inherit.aes = inherit.aes,
                   params = list(outliers = outliers, ...))
  }
