describe("get_query_string", {
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
