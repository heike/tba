
#' Get team rankings for an event
#'
#' @param event_code character string for an event -  usually an eight-character string consisting of a four-digit year followed by an acronym for the event, such as 'cttd' for "Cow Town Throw Down".
#' @returns tibble of the rankings for all teams that participated in the event
#' @importFrom tidyr unnest_wider starts_with
#' @export
#' @examples
#' tba_ranking("2024cttd") # same team ranking as in example for `calc_ranking_tba_2024`
tba_ranking <- function(event_code) {
  parse_quietly <- quietly(parse_number)
  
  event <- season_first(event_code)
  ranking_list <- get_records(sprintf("/event/%s/rankings", event))

  rankings <- ranking_list$rankings %>% 
    unnest_wider("sort_orders", names_sep = "_") %>%
    select("team_key", "rank", starts_with("sort_order"))
  
  rankings <- rankings %>% discard(.p = function(x) {if (is.numeric(x)) var(na.omit(x)) == 0 else FALSE})
  indices <- na.omit(parse_quietly(names(rankings))$result)
  
  names(rankings)[-(1:2)] <- c(ranking_list$sort_order_info$name, ranking_list$extra_stats_info$name)[indices]
  
  rankings %>% left_join(
    ranking_list$rankings %>% select(-"sort_orders", -"extra_stats"), 
    by=c("team_key", "rank"))
}

season_first <- function(event_code) {
  # there seems to be some confusion in the API about whether the event_code should
  # consist of season first or last. 
  # This function ensures that the season is included first
  first4 <- substr(event_code, 1, 4)
  second <- substr(event_code, 5, nchar(event_code))
  parse_quietly <- quietly(parse_number)
  
  if (!is.na(parse_quietly(first4)$result)) return(event_code)
  sprintf("%s%s", second, first4)
}