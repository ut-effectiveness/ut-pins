describe("extract_times", {
  iris_pinnable <- get_iris_pinnable()

  it("returns a data-frame containing timings", {
    now <- as.POSIXct("2023-05-02 13:38:55 BST")
    then <- as.POSIXct("2023-05-02 13:37:55 BST")
    mock_time <- mockery::mock(then, now)

    with_mock(Sys.time = mock_time, {
      iris_prepared <- prepare(iris_pinnable)
      expect_equal(
        extract_times(iris_prepared),
        tibble::tibble(
          pin_name = "iris", start = then, end = now, duration = now - then
        )
      )
    })
  })

  it("fails with an unprepared pinnable", {
    expect_error(extract_times(iris_pinnable))
  })
})

describe("assert_publishable", {
  it("breaks if pin name is not defined", {
    no_name <- pinnable(
      type = "rds",
      prepare_fn = function() letters
    )
    no_name <- prepare(no_name)

    expect_error(assert_publishable(no_name))
  })
})

describe("full_pin_name", {
  it("returns the input pin-name, if no pin-author provided", {
    expect_equal(
      full_pin_name("myPin"),
      "myPin"
    )
  })
  it("prepends pin-author (when provided) to pin-name", {
    expect_equal(
      full_pin_name("pinMe", "myAccount"),
      "myAccount/pinMe"
    )
  })
})
