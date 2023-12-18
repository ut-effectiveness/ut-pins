describe("prepare(pinnable)", {
  iris_pinnable <- get_iris_pinnable()

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
      name = pin_name,
      prepare_fn = function() rnorm(10)
    )

    prepared_pin <- prepare(numbers_pinnable)
    published_pin <- publish(prepared_pin, pin_board)

    it("returns the pinnable object", {
      expect_is(published_pin, "pinnable")
    })
    it("appends the pin_path to the pinnable", {
      expect_equal(published_pin$pin_path, pin_name)
    })
    it("creates a pin that can be read back", {
      stored_numbers <- pins::pin_read(
        pin_board,
        published_pin$pin_path
      )
      expect_equal(stored_numbers, prepared_pin$data)
    })
    it("writes to pin_author/pin_name if pin_author is provided", {
      pin_author <- "my_username"
      published_pin_name <- paste0(pin_author, "/", pin_name)
      
      mock_writer <- mockery::mock(TRUE)
      mockery::stub(publish.pinnable, "pins::pin_write", mock_writer)
      author_specific_pin <- publish(prepared_pin, pin_board, pin_author = pin_author)
      
      # We can't use author/name pins on a pins::board_temp, so we have to catch the arguments to
      # pin_write instead.
      # We just want to ensure that name == published_pin_name == pin_author/pin_name
      mockery::expect_args(
        mock_writer,
        1,
        board = pin_board,
        x = prepared_pin$data,
        name = published_pin_name,
        type = prepared_pin$pin_type
      )
    })
  })

  it("throws an error if 'data' does not exist", {
    pin_name <- "letters"
    pin_board <- pins::board_temp()

    letters_pinnable <- pinnable(
      name = pin_name
    )

    if ("data" %in% names(letters_pinnable)) {
      stop("'data' entry should not exist in this test")
    }

    expect_error(
      publish(letters_pinnable, pin_board)
    )
  })
})

describe("publish(verbose_pinnable)", {
  it("uses verbose HTTP output when calling pin_write", {
    # GIVEN: a verbose_pinnable object that contains a 'data' element
    pin_name <- "beatles"
    pin_board <- pins::board_temp()

    beatles_pinnable <- verbose_pinnable(
      name = pin_name,
      prepare_fn = function() c("John", "Paul", "George", "Ringo")
    ) %>%
      prepare()

    # WHEN: publish() is called and the pin is written
    mock_httr <- mockery::mock(TRUE)
    mockery::stub(publish.verbose_pinnable, "httr::with_verbose", mock_httr)
    publish(beatles_pinnable, pin_board)

    # THEN: verbose HTTP output is created using httr::with_verbose
    mockery::expect_called(mock_httr, 1)
  })
})
