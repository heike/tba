#' Rank teams in the qualifying matches of an event by a scoring variable
#' 
#' All teams are ranked (if possible) by their contribution to the scoring variable.
#' 
#' If all values in the scoring variable are positive, a log linear model is 
#' fitted otherwise a normal (Gaussian) model is fitted. The OPR 
#' @param data tibble of match information - it is assumed that each line corresponds 
#' to the score breakdown of one team in one event's match.
#' @param variable numeric measurement. 
#' @param method character which model should be calculated? Either `normal` or `loglinear`.
#' @returns tibble of the ranked teams and the number of matches (`n`) that the ranking is based on.
#' @export
#' @importFrom purrr pluck
#' @importFrom readr parse_number
#' @importFrom dplyr n desc mutate arrange summarize group_by
#' @examples
#' # example code
#' # get all matches for one event:
#' clash_matches <- get_records("event/2024cttd/matches")
#' # get the score breakdown for each of the teams in each match
#' clash_details <- get_match_details(clash_matches)
#' 
#' # now get the contribution to the score:
#' clash_details %>% filter(comp_level == "qm") %>% # get the qualifying matches
#'   get_ranking_by(score)
#' # OPR is calculated with a normal model of the score without foulpoints
#' clash_details %>% filter(comp_level == "qm") %>% # get the qualifying matches
#'   get_ranking_by(score-foulPoints)
get_ranking_by <- function(data, variable, method = "normal") {
  data <- data %>% mutate(dependent = {{variable}})
  
  qualifiers <- data 
  model_data <- qualifiers %>%
    select(team_key, dependent, match_number, alliance) %>%
    mutate(ones = 1) %>% 
    pivot_wider(names_from=team_key, values_from = ones, values_fill=0) %>% 
    select(-match_number, -alliance)
  
  if ((all(model_data$dependent) >=0) & (method == "loglinear")) {
    model <- glm(dependent~.-1, data = model_data, family = poisson(link="log"))
    
  } else {
    model <- glm(dependent~.-1, data = model_data, family = gaussian(link="identity"))
    if (method == "loglinear") warning("Can't fit a log-linear model - some values in the scoring variable are negative. Fitting a normal model instead.")
    method <- "normal"
  }
  coefs <- sort(coefficients(model), decreasing = TRUE)
  #if (method=="loglinear") coefs <- exp(coefs)

  
  result <- as_tibble(data.frame(team_key = names(coefs),  coefs)) %>%
    left_join(qualifiers %>% group_by(team_key) %>% count(), by="team_key")
  names(result)[2] <- as_label(enquo(variable))
  result                         
}