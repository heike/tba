#' Calculate the TBA rankings from qualifying match scores for the current season
#' 
#' TBA rankings can be calculated from partial information, for example before all of the qualifying
#' matches are played or scores are posted.
#' @param data tibble of match information - it is assumed that each line corresponds 
#' to the score breakdown of one team in one event's match.
#' @export
#' @importFrom purrr pluck
#' @importFrom readr parse_number
#' @importFrom dplyr n desc mutate arrange summarize group_by
#' @examples
#' # example code
#' \dontrun{
#' # get all matches for one event:
#' clash_matches <- get_records("event/2025cttd/matches")
#' # get the score breakdown for each of the teams in each match
#' clash_details <- get_match_details(clash_matches)
#' # now get the rankings:
#' calculate_tba_ranking(clash_details)
#' }
calculate_tba_ranking <- function(data) {
  stopifnot(!is.null(data$event_key))
  
  event <- data %>% pluck("event_key") %>% unique()
  if (length(event) > 1) 
    warning(sprintf("Matches from multiple events are included: '%s'", paste(event, collapse=",")))
  
  season <- parse_number(event)
  stopifnot(all(season==2025))
  message("TBA ranking is not implemented yet for the 2025 season.")
  # data %>%
  #   select(match_number, alliance, team_key,
  #          score_no_fouls, winning_alliance, ensembleBonusAchieved, melodyBonusAchieved,
  #          coopertitionBonusAchieved, autoPoints, endGameTotalStagePoints) %>%
  #   mutate(tie = ifelse(winning_alliance=="", 1, 0),
  #          win = ifelse(winning_alliance==alliance, 2, 0),
  #          ranking_score = win+tie+ensembleBonusAchieved+melodyBonusAchieved) %>%
  #   group_by(team_key) %>%
  #   summarize(
  #     ranking_score = mean(ranking_score),
  #     avg_coop =mean(coopertitionBonusAchieved),
  #     avg_match = mean(score_no_fouls),
  #     avg_auto = mean(autoPoints),
  #     avg_stage = mean(endGameTotalStagePoints),
  #     record = sprintf("%d-%d-%d", sum(win)/2, n()-sum(win)/2-sum(tie), sum(tie)),
  #   ) %>% arrange(desc(ranking_score), desc(avg_coop), desc(avg_match)) %>% mutate(
  #     ranking = 1:n()
  #   )  
}


#' (Re-calculate) the TBA rankings from qualifying match scores
#' 
#' Note that rankings for historic events can be sourced from TBA with a call of the form 
#' `get_records("/event/{event_key}/rankings")$rankings`
#' @param data tibble of match information - it is assumed that each line corresponds 
#' to the score breakdown of one team in one event's match.
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
#' # now get the rankings:
#' calculate_tba_ranking_2024(clash_details)
calculate_tba_ranking_2024 <- function(data) {
  stopifnot(!is.null(data$event_key))
  
  event <- data %>% pluck("event_key") %>% unique()
  if (length(event) > 1) 
    warning(sprintf("Matches from multiple events are included: '%s'", paste(event, collapse=",")))
  season <- parse_number(event)
  stopifnot(all(season==2024))

  data %>% 
    mutate(
      score_no_fouls = score - foulPoints 
    ) %>%
    dplyr::filter(comp_level == "qm") %>%
    select(match_number, alliance, team_key,
           score_no_fouls, winning_alliance, ensembleBonusAchieved, melodyBonusAchieved,
           coopertitionBonusAchieved, autoPoints, endGameTotalStagePoints) %>%
    mutate(tie = ifelse(winning_alliance=="", 1, 0),
           win = ifelse(winning_alliance==alliance, 2, 0),
           ranking_score = win+tie+ensembleBonusAchieved+melodyBonusAchieved) %>%
    group_by(team_key) %>%
    summarize(
      ranking = 1, # place holder
      event = event,
      ranking_score = mean(ranking_score),
      avg_coop =mean(coopertitionBonusAchieved),
      avg_match = mean(score_no_fouls),
      avg_auto = mean(autoPoints),
      avg_stage = mean(endGameTotalStagePoints),
      record = sprintf("%d-%d-%d", sum(win)/2, n()-sum(win)/2-sum(tie), sum(tie)),
    ) %>% arrange(desc(ranking_score), desc(avg_coop), desc(avg_match)) %>% mutate(
      ranking = 1:n()
    )  
}


#' Get team rankings for an event
#'
#' @param event_code character string for an event
#' @param season four-digit year
#' @returns tibble of the rankings for all teams that participated in the event
#' @importFrom tidyr unnest_wider starts_with
#' @export
#' @examples
#' tba_ranking("cttd", 2024) # same team ranking as in example for `calc_ranking_tba_2024`
tba_ranking <- function(event_code, season) {
  event <- paste0(season, event_code)
  ranking_list <- get_records(sprintf("/event/%s/rankings", event))

  rankings <- ranking_list$rankings %>% 
    unnest_wider(.data$sort_orders, names_sep = "_") %>%
    select(.data$team_key, .data$rank, starts_with("sort_order"))
  
  names(rankings)[-(1:2)] <- c(ranking_list$sort_order_info$name, ranking_list$extra_stats_info$name)
  
  rankings %>% left_join(ranking_list$rankings %>% select(-sort_orders), by=c("team_key", "rank"))
}