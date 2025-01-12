
test_that("get_ranking_by works", {
  matches <- get_records("event/2024cttd/matches")
  expect_type(matches, "list")

  #' # get the score breakdown for each of the teams in each match
  match_details <- get_match_details(matches)
  expect_equal(nrow(match_details), 6*nrow(matches))
  #' 
  #' # now get the contribution to the score:
  ranking <- match_details %>% filter(comp_level == "qm") %>% # get the qualifying matches
     get_ranking_by(score)
  expect_equal(round(ranking$score[1:5],2), c(44.56, 35.60,31.56, 30.72, 28.06))
  expect_equal(round(rev(ranking$score)[1:5],2), rev(c(3.78, 3.62, 1.84, 1.17, -2.97)))
})
