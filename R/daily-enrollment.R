#' Get a data-frame containing daily-enrollment data
#'
#' @inheritParams   get_data_from_sql_query
#'
#' @export

get_daily_enrollment <- function(dsn = "***REMOVED***") {
  query_string <- get_query_string("daily_enrollment")

  get_data_from_sql_query(query_string, dsn = dsn)
}
