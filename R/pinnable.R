pinnable <- function(prepare_fn = NULL) {
  structure(
    list(
      prepare_fn = prepare_fn
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
  x$data <- x$prepare_fn()
  x
}
