describe(
  "get_pins_board",
  it("returns a local pins board when under `test` setting", {
    pb <- get_pins_board(setting = "test")

    expect_is(pb, "pins_board_folder")
  })
)
