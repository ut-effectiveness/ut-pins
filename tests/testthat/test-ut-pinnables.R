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
    ),
    get_audit_report_pinnables(),
    get_enrollment_pinnables(),
    get_data_steward_pinnables(),
    get_apr_pinnables()
  )
}

empty_named_list <- function() {
  l <- list()
  names(l) <- character(0)
  l
}

x <- list()

describe("the set of all UT pinnables", {
  it("can be created without error", {
    expect_silent(pinnables_fixture())
  })
  it("can be 'prepare'd without error (when SQL query is mocked)", {
    pinnables <- pinnables_fixture()

    mockthat::local_mock(
      get_data_from_sql_query = function(...) data.frame()
    )

    expect_equal(
      prepare_all(pinnables)$error,
      empty_named_list()
    )
  })
})
