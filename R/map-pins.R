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
  purrr::map(
    pinnables,
    purrr::safely(prepare)
  ) %>%
    partition_successes()
}

#' Call 'publish' on each of a list of 'pinnable' objects
#'
#' @param   pinnables   A list of 'pinnable' objects
#' @param   pin_board   A {pins} board where the pins should be written
#'
#' @return   A list of two lists. The "result" entry contains all pins whose data was successfully
#' pinned to the pin board. The "error" entry contains those pins which failed during the pin
#' writing step.
#'
#' @export

publish_all <- function(pinnables, pin_board) {
  purrr::map(
    pinnables,
    purrr::safely(publish),
    pin_board = pin_board
  ) %>%
    partition_successes()
}
