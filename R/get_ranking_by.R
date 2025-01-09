#' Rank teams in the qualifying matches of an event by a scoring variable
#' 
#' All teams are ranked (if possible) by their contribution to the scoring variable. 
#' 
#' In order to model each team's contribution, a wide data set has to be constructed, with each
#' team as a variable, and each row as one of the alliances in one match and their score. 
#' An alliance in a match consists of exactly three robots. This requirement is checked before modelling
#' and if not fulfilled, results in a warning. 
#' A normal model is then fitted, or, if `method = "loglinear"` a log-linear model is fitted, if all values in the scoring variable are non-negative.
#' The coefficients of this model can be interpreted as the average (log) contribution of that
#' team to the score of a match. The OPR (offensive power rating) corresponds 
#' to a call to `get_ranking_by` with `score` as the variable. 
#' 
#' @param data tibble of match information - it is assumed that each line corresponds 
#' to the score breakdown of one team in one event's match.
#' @param variable numeric measurement. 
#' @param method character which model should be calculated? Either `normal` or `loglinear`.
#' @returns tibble of the ranked teams and the number of matches (`n`) that the ranking is based on.
#' @export
#' @importFrom dplyr n desc mutate arrange summarize group_by count
#' @importFrom tidyr pivot_wider
#' @importFrom rlang as_label enquo
#' @importFrom stats coefficients gaussian glm na.omit poisson var
#' @importFrom purrr quietly discard
#' @examples
#' # example code
#' # get all matches for one event:
#' clash_matches <- get_records("event/2024cttd/matches")
#' # get the score breakdown for each of the teams in each match
#' clash_details <- get_match_details(clash_matches)
#' 
#' library(dplyr, quietly=TRUE)
#' # now get the contribution to the score:
#' clash_details %>% filter(comp_level == "qm") %>% # get the qualifying matches
#'   get_ranking_by(score)
#' # These coefficients correspond to the OPR 
#' 
#' # team contribution to the score without counting the opponents' fouls:
#' clash_details %>% filter(comp_level == "qm") %>% # get the qualifying matches
#'   get_ranking_by(score-foulPoints)
get_ranking_by <- function(data, variable, method = "normal") {
  data <- data %>% mutate(dependent = {{variable}})
  not_found <- setdiff(c("team_key", "match_number", "alliance"), names(data))
  if (length(not_found) > 0) {
    stop(sprintf(
      "The variable(s) <%s> must be included in the data set for the ranking.", 
      paste(not_found, collapse=", ")
      ))
  }

  match_identifiers <- c("comp_level", "match_number", "set_number")
  qualifiers <- data 
  
  # this is a bit hairy, but only combines all of the available information 
  # into a single key and calls that match_number
  qualifiers$match_number <- qualifiers %>% 
    select(any_of(match_identifiers)) %>% 
    mutate(across(everything(), as.character)) %>%
    rowwise() %>%
    mutate(match_number = paste(c_across(everything()), collapse="_")) %>% pluck("match_number")
  
  # check that match_number is unique, i.e we do not want to see six or more teams with the same
  # alliance and the same match_number
  teams_in_match <- qualifiers %>% count(.data$match_number, .data$alliance)
  if (any(teams_in_match$n >= 6)) 
    warning(sprintf("Match number is not unique - there are up to <%s> teams involved in an alliance. Consider adding `set_number` or `comp_level` in the data set.", max(teams_in_match$n)))
  
  model_data <- qualifiers %>%
    select(.data$team_key, .data$dependent, .data$match_number, .data$alliance) %>%
    mutate(ones = 1) %>% 
    pivot_wider(names_from=.data$team_key, values_from = .data$ones, values_fill=0) %>% 
    select(-.data$match_number, -.data$alliance)
  
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
    left_join(qualifiers %>% group_by(.data$team_key) %>% count(), by="team_key")
  names(result)[2] <- as_label(enquo(variable))
  result                         
}