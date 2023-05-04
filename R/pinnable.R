#' S3 class for generating/storing pin preparation/publishing details
#'
#' @param   prepare_fn   Function for preparing the data that will be stored in the associated pin.
#' @param   pin_name   Name of the pin.
#' @param   pin_subgroup   Optional scalar character. This gives you a way to annotate
#'   sub-groupings of pins. Some may relate to staffing, or to students. Handle that annotation
#'   here.
#' @param   pin_cadence   Scalar character. The frequency with which a pin should be updated.
#'   Either "daily", "weekly" or "monthly".
#' @param   pin_type   Storage type for the pin (must be a format usable by `pin_write`).
#'
#' @export

pinnable <- function(prepare_fn = NULL,
                     pin_name = NULL,
                     pin_subgroup = NULL,
                     pin_cadence = c("daily", "weekly", "monthly"),
                     pin_type = "rds") {
  pin_cadence <- match.arg(pin_cadence)

  structure(
    list(
      prepare_fn = prepare_fn,
      pin_name = pin_name,
      pin_subgroup = pin_subgroup,
      pin_cadence = pin_cadence,
      pin_type = pin_type
    ),
    class = "pinnable"
  )
}

#' S3 class for generating/storing pin preparation/publishing details
#'
#' When publishing using `pin_write`, this class will output verbose HTTP details.
#'
#' @inheritParams   pinnable
#' @export

verbose_pinnable <- function(prepare_fn = NULL,
                             pin_name = NULL,
                             pin_subgroup = NULL,
                             pin_cadence = c("daily", "weekly", "monthly"),
                             pin_type = "rds") {
  p <- pinnable(
    prepare_fn = prepare_fn,
    pin_name = pin_name,
    pin_subgroup = pin_subgroup,
    pin_cadence = pin_cadence,
    pin_type = pin_type
  )
  class(p) <- c("verbose_pinnable", class(p))
  p
}

#' Prepare a dataset for publication to pins
#'
#' @param   x   An object to be published. Typically a `pinnable` object for writing a pin to a
#'   pins board.
#'
#' @export
prepare <- function(x) {
  UseMethod("prepare")
}

#' @export
prepare.default <- function(x) {
  stop("Default prepare method is unimplemented")
}

#' @export
prepare.pinnable <- function(x) {
  x$prep_start <- Sys.time()
  x$data <- x$prepare_fn()
  x$prep_end <- Sys.time()

  return(x)
}

#' Write a pinnable dataset to a pins board
#'
#' @param   x   An object to be published. Typically a `pinnable` object for writing a pin to a
#'   pins board.
#' @param   ...  Other arguments defining where the object should be published. Use
#'   `pin_board = the_board` to define the {pins} board where pins should be written.
#'
#' @export

publish <- function(x, ...) {
  UseMethod("publish")
}

#' @export
publish.default <- function(x, ...) {
  stop("Default publish method is unimplemented")
}

#' `publish()` method for pinnable objects
#' @param   x   A `pinnable` object.
#' @param   pin_board   A {pins} board object. This defines the location where the pin will be
#'   written.
#' @param   ...   Other parameters. Unused here.
#'
#' @export

publish.pinnable <- function(x, pin_board, ...) {
  assert_publishable(x)

  pin_path <- pins::pin_write(
    board = pin_board,
    x = x$data,
    name = x$pin_name,
    type = x$pin_type
  )

  x$pin_path <- pin_path
  x
}

#' publish() method for verbose-pinnable objects
#'
#' @inheritParams   publish.pinnable
#'
#' @export

publish.verbose_pinnable <- function(x, pin_board, ...) {
  httr::with_verbose(
    data_out = TRUE,
    data_in = TRUE,
    info = TRUE,
    ssl = TRUE,
    expr = publish.pinnable(x, pin_board)
  )
}

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
