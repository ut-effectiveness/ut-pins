#' Filter a list of pinnables on the names, classes or cadences of those objects
#'
#' @param   x   List of `pinnables`.
#' @param   pin_names,pin_subgroups,pin_cadences   Character vectors. The list of `pinnables` will
#'   be filtered to keep only those that have a name, subgroup and cadence in these vectors. If any
#'   vector is `NULL` that will not contribute to filtering. If multiple vectors are non-NULL, any
#'   `pinnable` that is present in the returned vector will pass each of the comparisons.
#'
#' @return   Subset of the list of `pinnables`, `x`.
#'
#' @export

filter_pinnables <- function(x,
                             pin_names = NULL,
                             pin_subgroups = NULL,
                             pin_cadences = NULL) {
  filter_fn <- function(pinnable) {
    is_in_set(pinnable$pin_name, pin_names) &&
      is_in_set(pinnable$pin_subgroup, pin_subgroups) &&
      is_in_set(pinnable$pin_cadence, pin_cadences)
  }

  purrr::keep(x, filter_fn)
}

is_in_set <- function(x, set, value_if_null_set = TRUE) {
  if (is.null(set)) {
    return(value_if_null_set)
  }

  return(x %in% set)
}
