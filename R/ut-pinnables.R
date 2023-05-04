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
