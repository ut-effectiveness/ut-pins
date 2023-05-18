describe("get_query_string", {
  it("obtains the contents of a .sql file from within the package", {
    query_type <- "daily_enrollment"

    # Copied from the `daily_enrollment.sql` file in `inst/sql/admissions`
    expected_prefix <- "^SELECT a.days_to_class_start AS days_to_term_start"

    query_string <- get_query_string(query_type)

    expect_match(query_string, expected_prefix)
  })

  it("fails when provided an invalid query-type", {
    # `get_query_string` can only return the contents of a .sql file if a matching .sql file exists
    # in the package
    # See the list of valid `query_type`s in the body of `get_query_string`
    query_type <- "not_a_query_type"

    expect_error(
      get_query_string(query_type)
    )
  })
})
