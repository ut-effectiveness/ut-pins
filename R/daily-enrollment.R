#' Get a data-frame containing daily-enrollment data
#'
#' @param   dsn   A DSN entry: `***REMOVED***`, `***REMOVED***`, `***REMOVED***`, `***REMOVED***`, etc...
#'
#' @export

get_daily_enrollment <- function(dsn = "***REMOVED***") {
  sql_file <- system.file(
    "sql", "admissions", "daily_enrollment.sql",
    package = "utPins", mustWork = TRUE
  )
  utHelpR::get_data_from_sql_file(sql_file, dsn = dsn)
}
