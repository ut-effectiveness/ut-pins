describe("pinnable()", {
  it("creates a `pinnable` object", {
    p <- pinnable()

    expect_is(p, "pinnable")
  })
})

describe("prepare(pinnable)", {
  it("creates a new `pinnable` with a `data` entry", {
    p <- pinnable(
      prepare = function() datasets::iris
    )

    expect_true(
      "data" %in% names(prepare(p))
    )
    expect_equal(
      prepare(p)$data,
      datasets::iris
    )
  })
})
