test_that("compare_teams works", {
  expect_message(gg <- compare_teams("2024cttd", alliances_score))
  expect_message(gg <- compare_teams("2024cttd", alliances_score, verbose = FALSE))
  expect_equal(class(gg), c("gg", "ggplot"), ignore_attr=TRUE)
  
  expect_message(
  gg <- compare_teams("2024cttd", alliances_score, color = "Wrong", verbose=FALSE),
  'Skipping color lines: color = "Wrong"; must be one of "Captain", "Pick 1", "Pick 2", "Pick 3"')
  
  # need at least one variable
  expect_error(compare_teams("2024cttd"))
})
