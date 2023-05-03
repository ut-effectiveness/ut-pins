#' Get a data-frame containing a data-steward-audit dataset
#'
#' @param   audit_type   Scalar character. The type of audit report to generate. Allowed values:
#'   `student`, `student_course`, `room`, `graduation`, `course`, `building`.
#' @param   dsn   A DSN entry: `***REMOVED***`, `***REMOVED***`, `***REMOVED***`, `***REMOVED***`, etc...
#'
#' @export

get_data_steward_audit <- function(audit_type = c(
                                     "student",
                                     "student_course",
                                     "room",
                                     "graduation",
                                     "course",
                                     "building"
                                   ),
                                   dsn = "***REMOVED***") {
  audit_type <- match.arg(audit_type)

  sql_file <- system.file(
    "sql", "data_steward_audit", paste0(audit_type, ".sql", package = "utPins", mustWork = TRUE)
  )
  utHelpR::get_data_from_sql_file(sql_file, dsn = dsn)
}
