describe("partition_successes", {
  it("puts all errors in an 'error' list", {
    n_entries <- 5
    sf <- purrr::safely(stop)

    all_errors <- purrr::map(seq_len(n_entries), sf)

    partitioned <- partition_successes(all_errors)

    expect_equal(partitioned$result, list())
    expect_equal(length(partitioned$error), n_entries)
  })

  it("puts all successes in a 'result' list", {
    n_entries <- 4
    sf <- purrr::safely(identity)

    all_results <- purrr::map(seq_len(n_entries), sf)

    partitioned <- partition_successes(all_results)

    expect_equal(length(partitioned$result), n_entries)
    expect_equal(partitioned$result, as.list(seq_len(n_entries)))
    expect_equal(partitioned$error, list())
  })
})
