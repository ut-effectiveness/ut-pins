#' Call 'prepare' on each of a list of pinnable objects
#'
#' @param   pinnables   A list of 'pinnable' objects
#'
#' @return   A list of two lists. The "result" entry contains all pins whose data was successfully
#' generated. The "error" entry contains those pins where an error was thrown during the data
#' preparation step.
#'
#' @export

prepare_all <- function(pinnables) {
  map_safely(
    pinnables,
    prepare
  )
}

#' Call 'publish' on each of a list of 'pinnable' objects
#'
#' @param   pinnables   A list of 'pinnable' objects
#' @param   pin_board   A {pins} board where the pins should be written
#' @param   pin_author   The name of the author of this pin, may differ from the name of the
#'   connect-account used to access the Connect board. Assumed identical for all pins (if it isn't,
#'   add the pin-author name to the "name" argument in `pinnable()`).
#'
#' @return   A list of two lists. The "result" entry contains all pins whose data was successfully
#' pinned to the pin board. The "error" entry contains those pins which failed during the pin
#' writing step.
#'
#' @export

publish_all <- function(pinnables, pin_board, pin_author = NULL) {
  map_safely(
    pinnables,
    publish,
    pin_board = pin_board,
    pin_author = pin_author
  )
}

#' Map a function over a collection, using `purrr::safely` to catch any errors
#'
#' @inheritParams   purrr::map
#'
#' @return   A list of two lists. The "result" entry contains all results from running the function
#'   `.f` without error. The "error" entry contains any failures.

map_safely <- function(.x, .f, ...) {
  purrr::map(
    .x,
    purrr::safely(.f),
    ...
  ) %>%
    partition_successes()
}
