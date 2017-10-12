hurricane <- function(.data, name, date = NULL) {
  stopifnot(is(.data, "data.frame"),
            "storm_id" %in% names(.data))

  if (!is.null(date))
    stopifnot(is(date, "dttm"))

  if(name %in% .data$storm_id) {
    hurricane <- .data %>%
      dplyr::filter(storm_id == name) %>%
      dplyr::sample_n(1)

    structure(hurricane, class = c("hurricane", class(hurricane)))
  } else {
    stop(paste0(name, " is not in database"))
  }
}
