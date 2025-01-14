test_that("calculate_tba_ranking works", {
  tba_rankings <- tba_ranking("cttd", 2024)
  
  
  cttd_matches <- get_records("event/2024cttd/matches")
  #' # get the score breakdown for each of the teams in each match
  cttd_details <- get_match_details(cttd_matches)
  #' # now get the rankings:
  rankings <- calculate_tba_ranking(cttd_details)

  expect_type(rankings, "list")
  
  expect_equal(nrow(rankings), 42)
  expect_equal(rankings$team_key, tba_rankings$team_key)
  
  # check that there is a message
  cttd_details$event_key <- "2023cttd"
  expect_message(calculate_tba_ranking(cttd_details), "TBA ranking is not implemented yet for the 2023 season.")
  cttd_details$event_key <- "2025cttd"
  expect_message(calculate_tba_ranking(cttd_details), "TBA ranking is not implemented yet for the 2025 season.")

  
  cttd_details$event_key <- sample(c("2022cttd", "2023cttd"), size=nrow(cttd_details), replace = TRUE)
  # Warning: Matches from multiple events are included: '2022cttd, 2023cttd'
  # Message: scoring not implemented for 2023 or 2022
  expect_message(expect_warning(calculate_tba_ranking(cttd_details)))
  
})
