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
    tidyr::unite(date, year, month, day, sep = "-") %>%
    tidyr::unite(date, date, hour, sep = " ") %>%
    dplyr::mutate(date = paste0(date, ":00:00")) %>%
    dplyr::mutate(date = lubridate::ymd_hms(date))

  ext_tracks
}
