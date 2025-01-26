test_that("compare_teams works", {
  expect_message(gg <- compare_teams("2024cttd", score))
  expect_message(gg <- compare_teams("2024cttd", score, verbose = FALSE))
  expect_equivalent(class(gg), c("gg", "ggplot"))
  
  expect_message(
  gg <- compare_teams("2024cttd", score, color = "Wrong", verbose=FALSE),
  'Skipping color lines: color = "Wrong"; must be one of "Captain", "Pick 1", "Pick 2", "Pick 3"')
  
  # need at least one variable
  expect_error(compare_teams("2024cttd"))
})
