#' Get a data-frame containing an audit report
#'
#' @param   audit_type   Scalar character. The type of audit report to generate. Allowed values:
#'   `students`, `courses`, `spbpers`, `sorhsch`, `goradid`, `student_courses`.
#' @param   dsn   A DSN entry: `***REMOVED***`, `***REMOVED***`, `***REMOVED***`, `***REMOVED***`, etc...
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

  sql_file <- system.file(
    "sql", "audit_reports", paste0(audit_type, ".sql", package = "utPins", mustWork = TRUE)
  )
  utHelpR::get_data_from_sql_file(sql_file, dsn = dsn)
}

as_audit_report_name <- function(x) {
  paste0("audit_reports_", x)
}
