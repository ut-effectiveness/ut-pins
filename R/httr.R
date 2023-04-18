with_verbose_http_details <- function(expr) {
  httr::with_verbose(
    data_out = TRUE,
    data_in = TRUE,
    info = TRUE,
    ssl = TRUE,
    expr = expr
  )
}
