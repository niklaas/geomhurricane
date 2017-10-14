#' Extract single hurricane from tidied ext_tracks
#'
#' @param .data Tidied ext_tracks data.frame
#' @param name Name (ID) of the hurricane as stored in .data
#' @param .date date-time string, such as "2008-09". The function will filter
#'  with \code{stringr::str_detect}
#'
#' @return A single observation of a hurricane in a data.frame
hurricane <- function(.data, name, .date = NULL) {
  stopifnot(is(.data, "data.frame"),
            "storm_id" %in% names(.data),
            "date" %in% names(.data))

  if(name %in% .data$storm_id) {
    hurricane <- .data %>%
      dplyr::filter(storm_id == name) %>%
      dplyr::filter(ne != 0, nw != 0, se != 0, sw != 0)

    if (!is.null(.date)) {
      hurricane %<>%
        dplyr::filter(stringr::str_detect(date, .date))
    }

    dates <- unique(hurricane$date)

    hurricane %<>%
      dplyr::filter(date == sample(dates, size = 1))

    structure(hurricane, class = c("hurricane", class(hurricane)))
  } else {
    stop(paste0(name, " is not in database"))
  }
}
