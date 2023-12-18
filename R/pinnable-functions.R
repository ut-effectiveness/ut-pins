#' Get the data-preparation timings for a `pinnable` that has successfully prepared it's data
#'
#' @param   x   A publishable `pinnable` (it's data has been prepared).
#' @return   Data-frame with columns `pin_name`, `start`, `end` and `duration`.
#'
#' @export

extract_times <- function(x) {
  assert_publishable(x)

  tibble::tibble(
    pin_name = x$pin_name,
    start = x$prep_start,
    end = x$prep_end,
    duration = x$prep_end - x$prep_start
  )
}

assert_publishable <- function(x) {
  stopifnot(inherits(x, "pinnable"))
  stopifnot("data" %in% names(x))
  stopifnot("pin_name" %in% names(x) && !is.null(x$pin_name))
}

#' Generate the pin-name as used on Connect (ie, "{pin_author}/{pin_name}")
#' 
#' If `pin_author` is NULL, then the full pin name matches the input `pin_name`. Otherwise, the
#' fully-specified pin-name is `{pin_author}/{pin_name}`.
#' 
#' @param   pin_name   Scalar character. The name of the pin.
#' @param   pin_author   Scalar character (or NULL). The author of the pin.

full_pin_name <- function(pin_name, pin_author = NULL) {
 if (!is.null(pin_author)) {
    paste0(pin_author, "/", pin_name)
  } else {
    pin_name
  } 
}
