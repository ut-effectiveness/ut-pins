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
    get_data_steward_pinnables()
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
  it("can be 'prepare'd without error (when SQL query is mocked)", {
    # GIVEN: a list of pinnable objects
    # AND: the SQL query for each of those pinnables is patched to return an empty data.frame
    # (so no database access occurs here)
    pinnables <- pinnables_fixture()
    mockthat::local_mock(
      get_data_from_sql_query = function(...) data.frame()
    )

    # WHEN: the data-preparation function for each pinnable is called
    prepared_pinnables <- prepare_all(pinnables)

    # THEN: no data-preparation errors occur
    # (this indicates that a valid SQL-query file is found, and that valid arguments are passed to
    # the data-preparation function from the pinnable)
    expect_equal(
      prepared_pinnables$error,
      empty_named_list()
    )
  })
})
