describe("pinnable()", {
  it("creates a `pinnable` object", {
    p <- pinnable()

    expect_is(p, "pinnable")
  })
})

describe("prepare(pinnable)", {
  iris_pinnable <- pinnable(
    prepare_fn = function() datasets::iris
  )

  it("creates a new `pinnable` with a `data` entry", {
    prepared_pin <- prepare(iris_pinnable)
    expect_true(
      "data" %in% names(prepared_pin)
    )
    expect_equal(
      prepared_pin$data,
      datasets::iris
    )
  })

  it("appends start and end time of data-prep to the pinnable", {
    prepared_pin <- prepare(iris_pinnable)
    expect_true(
      "prep_start" %in% names(prepared_pin)
    )
    expect_is(prepared_pin$prep_start, "POSIXct")

    expect_true(
      "prep_end" %in% names(prepared_pin)
    )
    expect_is(prepared_pin$prep_end, "POSIXct")
  })
})

describe("publish(pinnable)", {
  it("writes 'data' to the 'pin_name' pin", {
    pin_name <- "numbers"
    pin_board <- pins::board_temp()

    numbers_pinnable <- pinnable(
      pin_name = pin_name,
      pin_board = pin_board,
      prepare_fn = function() rnorm(10)
    )

    prepared_pin <- prepare(numbers_pinnable)
    published_pin <- publish(prepared_pin)

    stored_numbers <- pins::pin_read(pin_board, pin_name)
    expect_equal(
      stored_numbers,
      prepared_pin$data
    )
  })
})
