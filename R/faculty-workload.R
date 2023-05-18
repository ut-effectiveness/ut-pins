#' Get a data-frame containing instructional or non-instructional workloads for faculty members
#'
#' @param   workload_type   Scalar character. Either "instructional" or "non_instructional".
#' @inheritParams   get_data_from_sql_query
#'
#' @export

get_faculty_workload <- function(workload_type = c(
                                   "instructional", "non_instructional"
                                 ),
                                 dsn = "***REMOVED***") {
  workload_type <- match.arg(workload_type)
  query_type <- paste0("faculty_term_", workload_type, "_workload")
  sql_query <- get_query_string(query_type)

  workload <- get_data_from_sql_query(sql_query, dsn = dsn)
  faculty_term <- get_faculty_term_df(dsn = dsn)

  dplyr::left_join(
    workload,
    faculty_term,
    by = c("sis_id", "term_code")
  )
}

#' Get a data-frame containing faculty term data
#'
#' @inheritParams   get_data_from_sql_query
#'
#' @export

get_faculty_term_df <- function(dsn = "***REMOVED***") {
  sql_query <- get_query_string("faculty_term")
  faculty_term <- get_data_from_sql_query(sql_query, dsn = dsn)

  format_faculty_term_df(faculty_term, utPins::contracted_workload)
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
