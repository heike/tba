test_that("get_ranking_by_diff works", {
  matches <- get_records("event/2024cttd/matches")
  expect_type(matches, "list")

  #' # get the score breakdown for each of the teams in each match
  match_details <- get_match_details(matches)
  expect_equal(nrow(match_details), 6*nrow(matches))
  #' 
  #' # now get the contribution to the score:
  suppressMessages(require(dplyr))
  ranking <-  dplyr::filter(match_details, comp_level == "qm") %>% # get the qualifying matches
     get_ranking_by_diff(alliances_score)
  #  ranking <- match_details %>% filter(comp_level == "qm") %>% # get the qualifying matches
#     get_ranking_by(score, score-foulPoints)
  expect_equal(round(ranking$`rating_diff(alliances_score)`[1:5],1), c(35.8, 17.4, 16.8, 15.3, 11.3))

  ranking_shift <-  dplyr::filter(match_details, comp_level == "qm") %>% # get the qualifying matches
    get_ranking_by_diff(alliances_score-10)
  # for the score difference, the values don't change when the score shifts systematically:
  expect_equal(round(ranking_shift$`rating_diff(alliances_score - 10)`,2), round(ranking$`rating_diff(alliances_score)`,2))
  
  ranking_glm <-  dplyr::filter(match_details, comp_level == "qm") %>% # get the qualifying matches
    get_ranking_by_diff(alliances_score, method="loglinear")
  expect_equal(round(ranking_glm$`rating_diff(alliances_score)`[1:5],1), c(1.2, 1.2, 1.0, 0.7, 0.4))
  
  # now check the error
  #  Error: You need to specify at least one dependent variable
  expect_error(get_ranking_by_diff(match_details))
  #  Error: object not found
  expect_error(get_ranking_by_diff(match_details, `this variable doesnt exist`))
  # Error: The variable(s) <match_number> must be included in the data set for the ranking.
  expect_error(get_ranking_by_diff(match_details %>% select(-match_number), alliances_score))
  
  # Warning when match_numbers alone are not unique
  expect_error(get_ranking_by_diff(match_details %>% select(-set_number), alliances_score))
  
})
