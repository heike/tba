test_that("get_ranking_by works", {
  matches <- get_records("event/2024cttd/matches")
  expect_type(matches, "list")

  #' # get the score breakdown for each of the teams in each match
  match_details <- get_match_details(matches)
  expect_equal(nrow(match_details), 6*nrow(matches))
  #' 
  #' # now get the contribution to the score:
  suppressMessages(require(dplyr))
  ranking <-  dplyr::filter(match_details, comp_level == "qm") %>% # get the qualifying matches
     get_ranking_by(score)
  #  ranking <- match_details %>% filter(comp_level == "qm") %>% # get the qualifying matches
#     get_ranking_by(score, score-foulPoints)
  expect_equal(round(ranking$`rating(score)`[1:5],2), c(44.56, 35.60,31.56, 30.72, 28.06))
  expect_equal(round(rev(ranking$`rating(score)`)[1:5],2), rev(c(3.78, 3.62, 1.84, 1.17, -2.97)))

  
  ranking_glm <-  dplyr::filter(match_details, comp_level == "qm") %>% # get the qualifying matches
    get_ranking_by(score, method="loglinear")
  expect_equal(round(ranking_glm$`rating(score)`[1:5],2), c(1.77, 1.62, 1.62, 1.57, 1.52))
  
  # now check the error
  #  Error: You need to specify at least one dependent variable
  expect_error(get_ranking_by(match_details))
  #  Error: object not found
  expect_error(get_ranking_by(match_details, `this variable doesnt exist`))
  # Error: The variable(s) <match_number> must be included in the data set for the ranking.
  expect_error(get_ranking_by(match_details %>% select(-match_number), score))
  
  # Warning when match_numbers alone are not unique
  expect_warning(get_ranking_by(match_details %>% select(-set_number), score))
  
  # Warning about negative scores
  expect_warning(dplyr::filter(match_details, comp_level == "qm") %>% # get the qualifying matches
    get_ranking_by(score-100, method="loglinear"))
})
