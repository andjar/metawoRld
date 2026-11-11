test_that("identifiers are normalized", {
  expect_equal(
    normalize_identifier(c(" https://doi.org/10.1000/ABC ", "doi:10.1234/X", "PMID: 123", NA)),
    c("10.1000/abc", "10.1234/x", "123", "")
  )
  expect_equal(identifier_type(c("10.1000/abc", "123", "Smith2020")), c("doi", "pmid", "other"))
})

test_that("sanitize_id is reversible and vectorized", {
  ids <- c("10.1000/a:b?c", "with space", "back\\slash", "")
  expect_false(any(grepl("[/:?\\\\ ]", sanitize_id(ids))))
  expect_equal(desanitize_id(sanitize_id(ids)), ids)
  expect_equal(sanitize_id(NULL), character(0))
})
