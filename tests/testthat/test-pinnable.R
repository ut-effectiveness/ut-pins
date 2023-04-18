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
  describe("on writing 'data' to the 'pin_name' pin", {
    pin_name <- "numbers"
    pin_board <- pins::board_temp()

    numbers_pinnable <- pinnable(
      pin_name = pin_name,
      pin_board = pin_board,
      prepare_fn = function() rnorm(10)
    )

    prepared_pin <- prepare(numbers_pinnable)
    published_pin <- publish(prepared_pin)

    it("returns the pinnable object", {
      expect_is(published_pin, "pinnable")
    })
    it("appends the pin_path to the pinnable", {
      expect_equal(published_pin$pin_path, pin_name)
    })
    it("creates a pin that can be read back", {
      stored_numbers <- pins::pin_read(
        published_pin$pin_board,
        published_pin$pin_path
      )
      expect_equal(stored_numbers, prepared_pin$data)
    })
  })

  it("throws an error if 'data' does not exist", {
    pin_name <- "letters"
    pin_board <- pins::board_temp()

    letters_pinnable <- pinnable(
      pin_name = pin_name,
      pin_board = pin_board
    )

    if ("data" %in% names(letters_pinnable)) {
      stop("'data' entry should not exist in this test")
    }

    expect_error(
      publish(letters_pinnable)
    )
  })
})

describe("assert_publishable", {
  it("breaks if pin board is not defined", {
    no_board <- pinnable(
      pin_name = "letters",
      pin_type = "rds",
      prepare_fn = function() letters
    )
    no_board <- prepare(no_board)

    expect_error(assert_publishable(no_board))
  })

  it("breaks if pin name is not defined", {
    no_name <- pinnable(
      pin_board = pins::board_temp(),
      pin_type = "rds",
      prepare_fn = function() letters
    )
    no_name <- prepare(no_name)

    expect_error(assert_publishable(no_name))
  })
})
