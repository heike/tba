test_that("get_ranking_by_one works", {
  matches <- get_records("event/2024cttd/matches")
  # get the score breakdown for each of the teams in each match
  match_details <- get_match_details(matches)
  
  suppressMessages(require(dplyr))
  # now get the contribution to the score:
  ranking <- dplyr::filter(match_details, comp_level == "qm") %>% # get the qualifying matches
    get_ranking_by_one(score)
  expect_equal(round(ranking$`rating(score)`[1:5],2), c(44.56, 35.60,31.56, 30.72, 28.06))
  expect_equal(round(rev(ranking$`rating(score)`)[1:5],2), rev(c(3.78, 3.62, 1.84, 1.17, -2.97)))
})
