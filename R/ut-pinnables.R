#' Get a list of `pinnable` objects related to student types
#'
#' @param   parameter_term,term_two_terms_ago   Term string (e.g., "202320").
#' @param   dsn   A DSN entry: `***REMOVED***`, `***REMOVED***`, `***REMOVED***`, `***REMOVED***`, etc...
#'
#' @return   List of `pinnable`s.
#'
#' @export

get_student_type_pinnables <- function(parameter_term,
                                       term_two_terms_ago,
                                       dsn = "***REMOVED***") {
  pins <- list()
  cadence <- "weekly"
  subgroup <- "student type"

  pins$student_type_determination_variables <- pinnable(
    name = "student_type_audit_student_type_determination_variables_pin",
    prepare_fn = function() {
      get_student_type_variables(dsn = dsn)
    },
    cadence = cadence,
    subgroup = subgroup
  )

  pins$student_type_audit_audit_calculated_student_types <- pinnable(
    name = "student_type_audit_calculated_student_types_pin",
    prepare_fn = function() {
      type_determination_variables <- get_student_type_variables(dsn = dsn)
      student_types <- calculate_student_types(
        type_determination_variables,
        parameter_term = parameter_term,
        term_two_terms_ago = term_two_terms_ago
      )
      student_types
    },
    cadence = cadence,
    subgroup = subgroup
  )

  pins
}

#' Get a list of `pinnable` objects related to the APR supplemental analysis
#'
#' @return   List of `pinnable`s
#'
#' @export

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
#'
#' @export

get_test_pinnable <- function(cadence = c("daily", "weekly", "monthly"),
                              verbose = TRUE) {
  cadence <- match.arg(cadence)
  pin_name <- paste0(cadence, "_test_pin")
  constructor <- if (verbose) verbose_pinnable else pinnable

  constructor(
    name = pin_name,
    prepare_fn = function() {
      datasets::iris
    },
    cadence = cadence,
    subgroup = "test"
  )
}
