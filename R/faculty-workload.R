#' Get a data-frame containing instructional or non-instructional workloads for faculty members
#'
#' @param   workload_type   Scalar character. Either "instructional" or "non_instructional".
#' @export

get_faculty_workload <- function(workload_type = c(
                                   "instructional", "non_instructional"
                                 )) {
  workload_type <- match.arg(workload_type)
  sql_names <- c(
    "instructional" = "faculty_term_instructional_workload.sql",
    "non_instructional" = "faculty_term_non_instructional_workload.sql"
  )
  sql_path <- system.file(
    "sql", "faculty_workload", sql_names[workload_type],
    package = "utPins"
  )
  workload <- utHelpR::get_data_from_sql_file(sql_path, dsn = "***REMOVED***")

  faculty_term <- get_faculty_term_df()

  dplyr::left_join(
    workload,
    faculty_term,
    by = c("sis_id", "term_code")
  )
}

get_faculty_term_df <- function() {
  faculty_term <- utHelpR::get_data_from_sql_file(
    system.file("sql", "faculty_workload", "faculty_term.sql", package = "utPins"),
    dsn = "***REMOVED***"
  )
  data("contracted_workload", package = "utPins", envir = environment())

  format_faculty_term_df(faculty_term, contracted_workload)
}

#' Format and combine the faculty term and contracted workflow datasets together
#'
#' @param   faculty_term   Data-frame containing faculty-term data.
#' @param   contracted_workload   Data-frame containing contracted-workload data.
#'
#' @importFrom   rlang   .data

format_faculty_term_df <- function(faculty_term, contracted_workload) {
  faculty_term %>%
    dplyr::mutate(
      faculty_info = paste0(.data[["full_name"]], " | D", .data[["sis_id"]])
    ) %>%
    dplyr::left_join(
      contracted_workload,
      by = c("sis_id", "term")
    ) %>%
    dplyr::mutate(
      contracted_workload = dplyr::if_else(
        grepl("Summer", .data[["term"]]),
        0,
        .data[["contracted_workload"]]
      )
    )
}
