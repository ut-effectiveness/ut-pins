#' Get a data-frame containing a data-steward-audit dataset
#'
#' @param   audit_type   Scalar character. The type of audit report to generate. Allowed values:
#'   `student`, `student_course`, `room`, `graduation`, `course`, `building`.
#' @inheritParams   get_data_from_sql_query
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
    "sql", "data_steward_audit", paste0(audit_type, ".sql"),
    package = "utPins", mustWork = TRUE
  )
  sql_query <- readr::read_file(sql_file)

  get_data_from_sql_query(sql_query, dsn = dsn)
}
