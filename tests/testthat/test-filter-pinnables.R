describe("filter_pinnables", {
  pinnables <- list(
    pinnable(name = "abc", cadence = "daily", subgroup = "staffing"),
    pinnable(name = "def", cadence = "weekly", subgroup = "audit"),
    pinnable(name = "ghi", cadence = "monthly", subgroup = "rooms"),
    pinnable(name = "hjkl", cadence = "daily", subgroup = "audit")
  )
  it("keeps all input if names/types/cadences are all NULL", {
    expect_equal(
      filter_pinnables(pinnables),
      pinnables
    )
  })
  it("keeps all matching pin_names", {
    expect_equal(
      filter_pinnables(pinnables, pin_names = "def"),
      pinnables[2]
    )
  })
  it("keeps all matching pin_subgroups", {
    expect_equal(
      filter_pinnables(pinnables, pin_subgroup = "rooms"),
      pinnables[3]
    )
  })
  it("keeps all matching pin_cadences", {
    expect_equal(
      filter_pinnables(pinnables, pin_cadences = c("daily", "weekly")),
      pinnables[c(1, 2, 4)]
    )
  })
  it("intersects on names/subgroups/cadences", {
    expect_equal(
      filter_pinnables(pinnables, pin_cadences = "daily", pin_subgroups = "audit"),
      pinnables[4]
    )
  })
})
