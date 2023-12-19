#' S3 class for generating/storing pin preparation/publishing details
#'
#' @param   prepare_fn   Function for preparing the data that will be stored in the associated pin.
#' @param   name   Name of the pin.
#' @param   subgroup   Optional scalar character. This gives you a way to annotate
#'   sub-groupings of pins. Some may relate to staffing, or to students. Handle that annotation
#'   here.
#' @param   cadence   Scalar character. The frequency with which a pin should be updated.
#'   Either "daily", "weekly" or "monthly".
#' @param   type   Storage type for the pin (must be a format usable by `pin_write`).
#'
#' @export

pinnable <- function(prepare_fn = NULL,
                     name = NULL,
                     subgroup = NULL,
                     cadence = c("daily", "weekly", "monthly"),
                     type = "rds") {
  cadence <- match.arg(cadence)

  structure(
    list(
      prepare_fn = prepare_fn,
      pin_name = name,
      pin_subgroup = subgroup,
      pin_cadence = cadence,
      pin_type = type
    ),
    class = "pinnable"
  )
}

#' S3 class for generating/storing pin preparation/publishing details
#'
#' When publishing using `pin_write`, this class will output verbose HTTP details.
#'
#' @inheritParams   pinnable
#' @export

verbose_pinnable <- function(prepare_fn = NULL,
                             name = NULL,
                             subgroup = NULL,
                             cadence = c("daily", "weekly", "monthly"),
                             type = "rds") {
  p <- pinnable(
    prepare_fn = prepare_fn,
    name = name,
    subgroup = subgroup,
    cadence = cadence,
    type = type
  )
  class(p) <- c("verbose_pinnable", class(p))
  p
}
