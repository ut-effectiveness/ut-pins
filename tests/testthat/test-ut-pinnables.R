pinnables_fixture <- function() {
  # TODO:
  # - include `get_student_type_pinnables`, and `get_faculty_workload_pinnables` in this fixture
  # - these additional pinnables don't just use simple SQL queries, so will need additional mocked
  # functions
  c(
    list(
      daily_test_pin = get_test_pinnable("daily"),
      weekly_test_pin = get_test_pinnable("weekly"),
      monthly_test_pin = get_test_pinnable("monthly")
    )
  )
}

empty_named_list <- function() {
  l <- list()
  names(l) <- character(0)
  l
}

describe("the set of all UT pinnables", {
  it("can be created without error", {
    expect_silent(pinnables_fixture())
  })
})
