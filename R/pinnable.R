pinnable <- function(prepare_fn = NULL,
                     pin_board = NULL,
                     pin_name = NULL,
                     pin_type = "rds",
                     verbose = FALSE) {
  structure(
    list(
      prepare_fn = prepare_fn,
      pin_board = pin_board,
      pin_name = pin_name,
      pin_type = pin_type,
      verbose = verbose
    ),
    class = "pinnable"
  )
}

#' Prepare a dataset for publication to pins
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
#' @export
publish <- function(x) {
  UseMethod("publish")
}

#' @export
publish.default <- function(x) {
  stop("Default publish method is unimplemented")
}

#' @export
publish.pinnable <- function(x) {
  assert_publishable(x)

  data <- x$data

  pins::pin_write(
    board = x$pin_board,
    x = data,
    name = x$pin_name,
    type = x$pin_type
  )
}

assert_publishable <- function(x) {
  stopifnot(is(x, "pinnable"))
  stopifnot("data" %in% names(x))
  stopifnot("pin_name" %in% names(x) && !is.null(x$pin_name))
  stopifnot("pin_board" %in% names(x) && is(x$pin_board, "pins_board"))
}
