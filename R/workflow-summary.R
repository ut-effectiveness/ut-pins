#' Create a data-frame to summarize the success/failures for data preparation and publishing
#'
#' @param   pinnables,prepared_pinnables,published_pinnables   Lists of `pinnable` objects. These
#'   contain, respectively: 1) all pinnables used in the workflow; 2) those pinnables that were
#'   successful during data-preparation; 3) those pinnables that were successful during data
#'   publication.
#'
#' @return   A data-frame with columns `pin_name`, `subgroup`, `prepared`, `published`. There will
#'   be a row for each `pinnable` in `pinnables`.
#'
#' @export

pins_workflow_report <- function(pinnables,
                                 prepared_pinnables,
                                 published_pinnables) {
  pinnables %>%
    purrr::imap(function(x, idx) {
      tibble::tibble(
        pin_name = x$pin_name,
        subgroup = x$pin_subgroup,
        prepared = idx %in% names(prepared_pinnables),
        published = idx %in% names(published_pinnables)
      )
    }) %>%
    purrr::list_rbind()
}

#' Summarize why each pinnable didn't succeed at data-preparation or publication
#'
#' @param   failures   List of errors arising from the `error` entry returned by `publish_all` or
#'   `prepare_all`.
#' @param   pinnables   List of `pinnable` objects. There must be an entry for each entry in
#'   `failures`.
#'
#' @return   Data-frame with columns `pin_name`, `subgroup` (taken from `pinnables`), `message`
#'   and `call`. The `call` entry can be extensive, but contains details of the call stack prior
#'   to failure.
#'
#' @export

failure_report <- function(failures, pinnables) {
  failures %>%
    purrr::imap(
      function(x, idx) {
        tibble::tibble(
          pin_name = pinnables[[idx]]$pin_name,
          subgroup = pinnables[[idx]]$pin_subgroup,
          message = x$message,
          call = as.character(x$call)
        )
      }
    ) %>%
    purrr::list_rbind()
}
