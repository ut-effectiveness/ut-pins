describe("map_safely", {
  it("puts all successes into a 'result' list", {
    f <- function(x) x
    input <- list(a = 1, b = "Hello", c = 1:10)
    observed <- map_safely(input, f)
    
    expect_equal(unname(observed$result), unname(input))
    expect_equal(unname(observed$error), list())
  })
  
  it("puts all failures into an 'error' list", {
    f <- function(x) {
      stop("That didn't work")
    }
    input <- list(a = 1, b = "Hello", c = 1:10)
    observed <- map_safely(input, f)
    
    expect_equal(unname(observed$result), list())
    expect_true(
      length(observed$error) == 3 &&
      all(
        purrr::map_lgl(observed$error, ~ inherits(.x, "simpleError"))
      )
    )
  })
})