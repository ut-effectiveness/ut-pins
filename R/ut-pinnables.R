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
