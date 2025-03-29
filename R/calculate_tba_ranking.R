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
#' clash_matches <- get_records("event/2024cttd/matches")
#' # get the score breakdown for each of the teams in each match
#' clash_details <- get_match_details(clash_matches)
#' # now get the rankings:
#' calculate_tba_ranking(clash_details)
#' }
calculate_tba_ranking <- function(data) {
  stopifnot(!is.null(data$event_key))
  
  event <- data %>% pluck("event_key") %>% unique()
  if (length(event) > 1) 
    warning(sprintf("Matches from multiple events are included: '%s'\nOnly using the first event's scoring method.", paste(event, collapse=", ")))
  
  season <- parse_number(event[1])
  if (season == 2024) return(calculate_tba_ranking_2024(data))
  
  message(sprintf("TBA ranking is not implemented yet for the %d season.", season))
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
  
  # bit messy, but based on definition of ranking system in Game Manual  
  names(data) <- gsub("score_breakdown_", "", names(data))
  data %>% 
    mutate(
      score_no_fouls = .data$alliances_score - .data$foulPoints 
    ) %>%
    dplyr::filter(.data$comp_level == "qm") %>%
    select("match_number", "alliance", "team_key",
           "score_no_fouls", "winning_alliance", "ensembleBonusAchieved", "melodyBonusAchieved",
           "coopertitionBonusAchieved", "autoPoints", "endGameTotalStagePoints") %>%
    mutate(tie = ifelse(.data$winning_alliance=="", 1, 0),
           win = ifelse(.data$winning_alliance==.data$alliance, 2, 0),
           ranking_score = .data$win+.data$tie+
             .data$ensembleBonusAchieved+.data$melodyBonusAchieved) %>%
    group_by(.data$team_key) %>%
    summarize(
      ranking = 1, # place holder
      event = event,
      ranking_score = mean(.data$ranking_score),
      avg_coop =mean(.data$coopertitionBonusAchieved),
      avg_match = mean(.data$score_no_fouls),
      avg_auto = mean(.data$autoPoints),
      avg_stage = mean(.data$endGameTotalStagePoints),
      record = sprintf("%d-%d-%d", sum(.data$win)/2, 
                       n()-sum(.data$win)/2-sum(.data$tie), sum(.data$tie)),
    ) %>% arrange(desc(.data$ranking_score), desc(.data$avg_coop), 
                  desc(.data$avg_match)) %>% 
    mutate(
      ranking = 1:n()
    )  
}

