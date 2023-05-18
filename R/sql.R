#' Query a Utah Tech database with a SQL string
#'
#' @param   query_string   Scalar character. An SQL query string for use when querying a database.
#' @param   dsn   A DSN entry: `***REMOVED***`, `***REMOVED***`, `***REMOVED***`, `***REMOVED***`, etc...
#'
#' @return   data.frame. The cleaned-up database entries that match the query string.
#' @export

get_data_from_sql_query <- function(query_string, dsn) {
  conn <- utHelpR::get_connection_object(dsn)

  df <- DBI::dbGetQuery(conn, query_string) %>%
    utHelpR::mung_dataframe()

  return(df)
}

#' Obtain a SQL query string from one of the files in `./inst/sql/`
#'
#' This defines the query that should be made against the UTAH databases for a given query_type
#'
#' @param   query_type   Scalar character. Valid values are `daily_enrollment`, `faculty_term`,
#'   `faculty_term_instructional_workload`, `faculty_term_non_instructional_workload`,
#'   `student_type_determination_variables`. Defines the type of query that is to be made. Invalid
#'   values will evoke an error.
#'
#' @return   SQL query string for use in `DBI::dbGetQuery`.
#' @export

get_query_string <- function(query_type) {
  utpins_path <- function(...) system.file(..., package = "utPins", mustWork = TRUE)

  sql_files <- list(
    daily_enrollment = utpins_path(
      "sql", "admissions", "daily_enrollment.sql"
    ),
    faculty_term = utpins_path(
      "sql", "faculty_workload", "faculty_term.sql"
    ),
    faculty_term_instructional_workload = utpins_path(
      "sql", "faculty_workload", "faculty_term_instructional_workload.sql"
    ),
    faculty_term_non_instructional_workload = utpins_path(
      "sql", "faculty_workload", "faculty_term_non_instructional_workload.sql"
    ),
    student_type_determination_variables = utpins_path(
      "sql", "student_type_audit", "student_type_determination_variables.sql"
    )
  )
  stopifnot(query_type %in% names(sql_files))

  readr::read_file(sql_files[[query_type]])
}
