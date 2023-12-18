describe("pinnable()", {
  it("creates a `pinnable` object", {
    p <- pinnable()

    expect_is(p, "pinnable")
  })
})
