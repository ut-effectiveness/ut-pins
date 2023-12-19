# Prepare -------------------------------------------------------------------------------------

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

# Publish -------------------------------------------------------------------------------------

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
#' @param   pin_author   The name of the author of this pin, may differ from the name of the
#'   connect-account used to access the Connect board.
#' @param   ...   Other parameters. Unused here.
#'
#' @export

publish.pinnable <- function(x, pin_board, pin_author = NULL, ...) {
  assert_publishable(x)

  pin_name <- full_pin_name(x$pin_name, pin_author)

  pin_path <- pins::pin_write(
    board = pin_board,
    x = x$data,
    name = pin_name,
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

publish.verbose_pinnable <- function(x, pin_board, pin_author = NULL, ...) {
  httr::with_verbose(
    data_out = TRUE,
    data_in = TRUE,
    info = TRUE,
    ssl = TRUE,
    expr = publish.pinnable(x, pin_board, pin_author = pin_author, ...)
  )
}

# Prune ---------------------------------------------------------------------------------------

#' Write a pinnable dataset to a pins board
#'
#' @param   x   Typically a `pinnable` object for a pin that has been written to a pins board.
#' @param   ...  Other arguments defining where the object should be published. Use
#'   `pin_board = the_board` to define the {pins} board where pins were written.
#'
#' @export

prune <- function(x, ...) {
  UseMethod("prune")
}

#' @export
prune.default <- function(x, ...) {
  stop("Default prune method is unimplemented")
}

#' `prune()` method for pinnable objects
#' @param   x   A `pinnable` object.
#' @param   pin_board   A {pins} board object. This defines the location where the pin was
#'   written.
#' @param   pin_author   The name of the author of this pin, may differ from the name of the
#'   connect-account used to access the Connect board.
#' @inheritParams   pins::pin_versions_prune
#'
#' @export

prune.pinnable <- function(x, pin_board, pin_author = NULL, n = NULL, days = NULL, ...) {
  pin_name <- full_pin_name(x$pin_name, pin_author)
  
  pins::pin_versions_prune(
    board = pin_board,
    name = pin_name,
    n = n,
    days = days,
    ...
  )
}
