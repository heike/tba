test_that("tba_ranking works", {
  tba_rankings <- tba_ranking("cttd", 2024)
  expect_type(tba_rankings, "list")

  expect_equal(nrow(tba_rankings), 42)
})
