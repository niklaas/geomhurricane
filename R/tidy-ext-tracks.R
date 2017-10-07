#' Tidy ext_tracks data
#'
#' @param ext_tracks ext_tracks data frame object
#'
#' @return A tidied version of ext_tracks as recommended by the authors of the
#'  Coursera course
#'
#' @importFrom dplyr %>%
#' @importFrom magrittr %<>%
tidy_ext_tracks <- function(ext_tracks) {
  ext_tracks %<>%
    dplyr::select(-distance_to_land,
                  -eye_diameter,
                  -final,
                  -max_wind,
                  -rad_max_wind,
                  -storm_type,
                  -dplyr::contains("pressure"))

  # Tidy column `date`
  ext_tracks %<>%
    tidyr::unite(date, year, month, day, sep = "-") %>%
    tidyr::unite(date, date, hour, sep = " ") %>%
    dplyr::mutate(date = paste0(date, ":00:00")) %>%
    dplyr::mutate(date = lubridate::ymd_hms(date))

  # Generate columns `wind_speed`, `ne`, `nw`, `se`, `sw`
  ext_tracks %<>%
    tidyr::gather(key = "wind_speed_direction",
                  value = "radius",
                  dplyr::starts_with("radius_")) %>%
    dplyr::mutate(wind_speed_direction =
                  stringr::str_replace(wind_speed_direction,
                                       "radius_", "")) %>%
    tidyr::separate(wind_speed_direction,
                    into = c("wind_speed", "direction"), sep = "_") %>%
    tidyr::spread(direction, radius)

  ext_tracks
}
