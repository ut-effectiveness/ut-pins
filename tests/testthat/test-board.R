describe(
  "get_pins_board",
  {
    it("returns a local pins board when under `test` setting", {
      pb <- get_pins_board(setting = "test")

      expect_is(pb, "pins_board_folder")
    })
    it("errors with an incorrect API key", {
      withr::local_envvar("***REMOVED***" = "NOT-AN-API-KEY")

      expect_error(
        get_pins_board(setting = "prod")
      )
    })
  }
)
