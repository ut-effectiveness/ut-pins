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
