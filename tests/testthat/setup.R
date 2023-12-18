get_iris_pinnable <- function() {
  pinnable(
    name = "iris",
    prepare_fn = function() datasets::iris
  )
}
