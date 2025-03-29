#' Reformat match information into an approximate wide form
#' 
#' The records from TBA come in the form of one line for each match, where the 
#' two alliances and their corresponding score breakdowns are in list variables. 
#' The `get_match_details` function returns the score breakdown for each team and each match in the 
#' competition, i.e. the new dataset will have about 6 times the number of rows of the original.  
#' HH: still need to test: what happens, when there is a surrogate team used in a match?
#' @param data tibble of match records,  assumed to be in the form of a result 
#' from a call to `get_records(/event/{event_key}/matches)`. 
#' @returns tibble of scores and score breakdowns for each match and each team
#' @importFrom dplyr `%>%` left_join select mutate
#' @importFrom purrr is_empty  list_rbind
#' @importFrom tidyr unnest unnest_longer ends_with
#' @importFrom rlang .data 
#' @export
#' @examples
#' clash_matches <- get_records("event/2024iawes/matches")
#' clash_details <- get_match_details(clash_matches)
#' clash_matches <- get_records("event/2025iacf/matches")
#' clash_details <- get_match_details(clash_matches)
get_match_details <- function(data) {
  # data is assumed to be in the form of a result from a `get_records` call 
  # with call = "/event/{event_key}/matches"
  data_plus <- data %>% select(-"videos") %>%
    unnest(c("alliances", "score_breakdown"), names_sep = "_") 
  
  scores <- data %>% 
    unnest(c("alliances", "score_breakdown"), names_sep = "_") %>% 
    unnest(c("alliances_blue", "alliances_red"), names_sep = "_") %>% 
    unnest(c("score_breakdown_blue", "score_breakdown_red"), names_sep = "_")

  reds <- grep("_red_", names(scores), value = TRUE)
  blues <- grep("_blue_", names(scores), value = TRUE)
  
  
  team_blue <- scores %>% select(-all_of(reds)) 
  names(team_blue) <- gsub("blue_", "", names(team_blue))
  team_blue$alliance <- "blue"

  team_red <- scores %>% select(-all_of(blues))
  names(team_red) <- gsub("red_", "", names(team_red))
  team_red$alliance <- "red"
  
  
  # team_blue <- data_plus %>% 
  #   select("comp_level", "match_number", "set_number", ends_with("_blue")) %>%
  #   unnest(cols = ends_with("_blue")) %>% discard(.p = function(d) is_empty(compact(d))) %>%
  #   mutate(alliance = "blue")
  # team_red <- data_plus %>% 
  #   select("comp_level", "match_number", "set_number", ends_with("_red")) %>%
  #   unnest(cols = ends_with("_red")) %>% discard(.p = function(d) is_empty(compact(d))) %>%
  #   mutate(alliance = "red")
  # 
  # team_blue <- team_blue %>%
  #   left_join(data %>% select(-"alliances", -"score_breakdown"), 
  #             by = c("comp_level", "match_number", "set_number"))
  # team_red <- team_red %>% 
  #   left_join(data %>% select(-"alliances", -"score_breakdown"), 
  #             by = c("comp_level", "match_number", "set_number"))
  # 
  in_common <- intersect(names(team_blue), names(team_red))
  in_common <- setdiff(in_common, c("surrogate_team_keys", "videos")) 
  # # take surrogate keys and other list variables out

  
  result <- list_rbind(list(team_blue[,in_common], team_red[,in_common])) %>% 
    unnest_longer(col="alliances_team_keys", values_to = "team_key") 
  result
}

  

