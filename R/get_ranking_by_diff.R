#' Rank teams in the qualifying matches of an event by a scoring variable
#' 
#' All teams are ranked (if possible) by their contribution to the difference in score in (qualifying) matches of the First Robotics Compettition.
#' These rankings are generally based on matches from the qualifying round and provide teams with 
#' more information to pick other teams when they form alliances.
#' 
#' In order to model each team's contribution to the score difference, a wide data set has to be constructed, with each
#' team as a variable, and each row as one of the alliances in one match with the difference in score as the dependent variable. 
#' An alliance in a match consists of exactly three robots. This requirement is checked before modelling
#' and if not fulfilled, results in a warning. 
#' A normal model is then fitted, or, if `method = "loglinear"` a log-linear model is fitted, if all values in the scoring variable are non-negative.
#' The coefficients of this model can be interpreted as the average (log) contribution of that
#' team to the difference in score of a match. Based on this information, an expected score difference can be calculated for an alliance. 
#' 
#' @param data tibble of match information - it is assumed that each line corresponds 
#' to the score breakdown of one team in one event's match.
#' @param ... numeric variables measuring aspects a team's performance (in qualifying matches). 
#' @param method character which model should be calculated? Either `normal` or `loglinear`.
#' @returns tibble of the ranked teams and the number of matches (`n`) that the ranking is based on.
#' @export
#' @importFrom dplyr n desc mutate arrange summarize group_by count across
#' @importFrom dplyr rowwise c_across any_of everything all_of 
#' @importFrom tidyselect quos
#' @importFrom tidyr pivot_wider
#' @importFrom rlang as_label 
#' @importFrom stats coefficients gaussian glm na.omit poisson var
#' @importFrom purrr quietly discard  
#' @importFrom tibble as_tibble
#' @examples
#' # example code
#' # get all matches for one event:
#' matches <- get_records("event/2024cttd/matches")
#' # get the score breakdown for each of the teams in each match
#' match_details <- get_match_details(matches)
#' 
#' library(dplyr, quietly=TRUE)
#' # now get the contribution to the score:
#' match_details %>% filter(comp_level == "qm") %>% # get the qualifying matches
#'   get_ranking_by_diff(alliances_score)
#' 
#' # additionally get the team contributions to the score without counting the opponents' fouls:
#' match_details %>% filter(comp_level == "qm") %>% 
#'   get_ranking_by_diff(alliances_score, alliances_score-score_breakdown_foulPoints)
get_ranking_by_diff <- function(data, ... , method = "normal") {
  dependent_list <- quos(..., .ignore_empty = "all")
  if (length(dependent_list) == 0) {
    stop("You need to specify at least one variable to rank by, eg 'get_ranking_by_diff(<data>, score)'")
  }
  result <- as_tibble(
    data.frame(
      names_list = dependent_list %>% sapply(FUN = as_label),
      rankings = I(lapply(dependent_list, FUN = function(x) {
        res <- get_ranking_by_diff_one(data, {{x}}, method = method)
        names(res)[ncol(res)-1] <- "coefficient"
        res
      }))))
  result <- result %>% tidyr::unnest(cols = "rankings") %>%
    mutate(names_list = sprintf("rating_diff(%s)", .data$names_list))

  result %>% pivot_wider(names_from="names_list", values_from = "coefficient") 
}



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
#' team to the score of a match. 
#' 
#' @param data tibble of match information - it is assumed that each line corresponds 
#' to the score breakdown of one team in one event's match.
#' @param variable numeric measurement. 
#' @param method character which model should be calculated? Either `normal` or `loglinear`.
#' @returns tibble of the teams (`team_key`), their rating(s)  and the number of matches (`n`) the ratings are based on.
#' @export
#' @importFrom dplyr n desc mutate arrange summarize group_by count across
#' @importFrom dplyr rowwise c_across any_of everything all_of ungroup
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_wider
#' @importFrom rlang as_label 
#' @importFrom tidyselect enquo
#' @importFrom stats coefficients gaussian glm na.omit poisson var
#' @importFrom purrr quietly discard  
#' @examples
#' # example code
#' # get all matches for one event:
#' matches <- get_records("event/2024iawes/matches")
#' # get the score breakdown for each of the teams in each match
#' match_details <- get_match_details(matches)
#' 
#' library(dplyr, quietly=TRUE)
#' # now get the contribution to the score:
#' match_details %>% filter(comp_level == "qm") %>% # get the qualifying matches
#'   get_ranking_by_diff_one(alliances_score)
#' # These coefficients correspond to the OPR 
#' 
#' # team contribution to the score without counting the opponents' fouls:
#' match_details %>% filter(comp_level == "qm") %>% # get the qualifying matches
#'   get_ranking_by_diff_one(alliances_score-score_breakdown_foulPoints)
get_ranking_by_diff_one <- function(data, variable, method = "normal") {
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
    stop(sprintf("Match number is not unique - there are up to <%s> teams involved in an alliance. Add an additional identifier to the data, such as `set_number` or `comp_level` in the data set.", max(teams_in_match$n)))
  model_data <- qualifiers %>%
    select("team_key", "dependent", "match_number", "alliance") %>% #arrange(match_number) %>%
    # first subtract blue from red alliance
    mutate(ones = ifelse(.data$alliance=="red", 1, -1), 
           dependent = ifelse(.data$alliance=="red", 1, -1)*.data$dependent) %>% 
    select(-"alliance") %>%
    group_by(.data$match_number) %>%
    mutate(n = n(), dependent = sum(.data$dependent)/(n()/2)) %>%
    mutate(ones = ifelse(.data$dependent < 0, -1, 1)*.data$ones, 
           dependent = abs(.data$dependent)) %>% 
    pivot_wider(names_from="team_key", values_from = "ones", values_fill=0) %>% 
    ungroup() %>%
    select(-"match_number") 
    

  stopifnot(all(model_data$n == 6)) # shouldn't happen, safety-precaution
  model_data <- model_data %>% select(-"n")
    
  if (method == "loglinear") {
    model <- glm(dependent~.-1, data = model_data, family = poisson(link="log"))
    
  } else {
    model <- glm(dependent~.-1, data = model_data, family = gaussian(link="identity"))
  }
  coefs <- sort(coefficients(model), decreasing = TRUE)
  #if (method=="loglinear") coefs <- exp(coefs)

  
  result <- as_tibble(data.frame(team_key = names(coefs),  coefs)) %>%
    left_join(qualifiers %>% group_by(.data$team_key) %>% count(), by="team_key")
  names(result)[2] <- sprintf("rating_diff(%s)",as_label(enquo(variable)))
  result                         
}
