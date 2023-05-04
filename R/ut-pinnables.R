#' Get a list of `pinnable` objects related to audit reports
#'
#' @param   dsn   A DSN entry: `***REMOVED***`, `***REMOVED***`, `***REMOVED***`, `***REMOVED***`, etc...
#'
#' @return   List of `pinnable`s.

get_audit_report_pinnables <- function(dsn = "***REMOVED***") {
  pins <- list()

  audit_types <- c("students", "courses", "spbpers", "sorhsch", "goradid", "student_courses")
  audit_reports <- as_audit_report_name(audit_types) # audit_reports_{x}

  audit_pin_fn <- function(x) {
    pinnable(
      name = paste0(as_audit_report_name(x), "_pin"),
      prepare_fn = function() {
        get_audit_report(audit_type = x, dsn = dsn)
      },
      cadence = "daily",
      subgroup = "audit report"
    )
  }

  pins[audit_reports] <- audit_types %>%
    purrr::set_names(audit_reports) %>%
    purrr::map(audit_pin_fn)

  pins
}

#' Get a list of `pinnable` objects related to faculty-workload
#'
#' @param   dsn   A DSN entry: `***REMOVED***`, `***REMOVED***`, `***REMOVED***`, `***REMOVED***`, etc...
#'
#' @return   List of `pinnable`s.

get_faculty_workload_pinnables <- function(dsn = "***REMOVED***") {
  pins <- list()

  daily_pinnable <- purrr::partial(pinnable, cadence = "daily", subgroup = "faculty_workload")

  pins$instructional_faculty_workload_pin <- daily_pinnable(
    name = "instructional_faculty_workload_pin",
    prepare_fn = function() {
      get_faculty_workload("instructional", dsn = dsn)
    }
  )

  pins$non_instructional_faculty_workload_pin <- daily_pinnable(
    name = "non_instructional_faculty_workload_pin",
    prepare_fn = function() {
      get_faculty_workload("non_instructional", dsn = dsn)
    }
  )

  pins$summarized_faculty_workload_by_term <- daily_pinnable(
    name = "summarized_faculty_workload_by_term",
    prepare_fn = function() {
      instructional_workload <- get_faculty_workload("instructional", dsn = dsn)
      non_instructional_workload <- get_faculty_workload("non_instructional", dsn = dsn)

      get_summarized_faculty_workload_df(
        instructional_workload,
        non_instructional_workload,
        time_aggregator = "term"
      )
    }
  )

  pins$summarized_faculty_workload_by_academic_year <- daily_pinnable(
    name = "summarized_faculty_workload_by_academic_year",
    prepare_fn = function() {
      instructional_workload <- get_faculty_workload("instructional", dsn = dsn)
      non_instructional_workload <- get_faculty_workload("non_instructional", dsn = dsn)

      get_summarized_faculty_workload_df(
        instructional_workload,
        non_instructional_workload,
        time_aggregator = "academic_year"
      )
    }
  )

  pins$faculty_term_pin <- daily_pinnable(
    name = "faculty_term_pin",
    prepare_fn = function() {
      get_faculty_term_df(dsn = dsn)
    }
  )

  return(pins)
}

#' Get a list of `pinnable` objects related to the APR supplemental analysis
#'
#' @return   List of `pinnable`s

get_apr_pinnables <- function() {
  pins <- list()

  pins$apr_supplemental_pin <- pinnable(
    name = "apr_supplemental_pin",
    cadence = "daily",
    subgroup = "apr",
    prepare_fn = function() {
      utPins::apr_supplemental_data
    }
  )

  return(pins)
}

#' Get a pinnable object for testing that pin-writes work
#'
#' @param   cadence   Scalar character. Either 'daily', 'weekly' or 'monthly'.
#' @param   verbose   Scalar logical (default `TRUE`). Should a verbose pinnable be made?
#'
#' @return   A single `pinnable` object with pin name `<cadence>_test_pin`

get_test_pinnable <- function(cadence = c("daily", "weekly", "monthly"),
                              verbose = TRUE) {
  cadence <- match.arg(cadence)
  pin_name <- paste0(cadence, "_test_pin")
  constructor <- if (verbose) verbose_pinnable else pinnable

  constructor(
    name = pin_name,
    prepare_fn = function() {
      datasets::iris
    }
  )
}
