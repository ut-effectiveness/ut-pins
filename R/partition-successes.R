#' Partition a list of `safely` values into a results set and an errors set
#'
#' @param   x   List of values. The values must each have a "result" and "error" entry, as is
#'   typical after calling `purrr::safely(some_function)` on a value. We assume that NULL values
#'   as the default fail-case entry (so `result = NULL` when error is non-`NULL` and vice-versa).
#'
#' @return   A list containing two entries: `result` and `error`. Each of the entries is a list.
#'   If an entry of the input list passed the `safely()`-wrapped function without error, it will
#'   be in the `result` entry of the output. If, however, it threw an error, it will be in the
#'   `error` entry.

partition_successes <- function(x) {
  x %>%
    purrr::list_transpose(simplify = FALSE) %>%
    purrr::map(purrr::discard, is.null)
}
