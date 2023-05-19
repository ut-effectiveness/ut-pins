#' Get a data-frame containing an audit report
#'
#' @param   audit_type   Scalar character. The type of audit report to generate. Allowed values:
#'   `students`, `courses`, `spbpers`, `sorhsch`, `goradid`, `student_courses`.
#' @inheritParams   get_data_from_sql_query
#' @export

get_audit_report <- function(audit_type = c(
                               "students",
                               "courses",
                               "spbpers",
                               "sorhsch",
                               "goradid",
                               "student_courses"
                             ),
                             dsn = "***REMOVED***") {
  audit_type <- match.arg(audit_type)

  sql_basename <- paste0("audit_reports_", audit_type, ".sql")
  sql_file <- system.file(
    "sql", "audit_reports", sql_basename,
    package = "utPins", mustWork = TRUE
  )
  sql_query <- readr::read_file(sql_file)

  get_data_from_sql_query(sql_query, dsn = dsn)
}

#' Add "audit_reports_" to the start of a string
#'
#' @param   x   String
#'
#' @export

as_audit_report_name <- function(x) {
  paste0("audit_reports_", x)
}
