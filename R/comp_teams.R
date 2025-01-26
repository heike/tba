#' Get a parallel coordinate plot for an event
#' 
#' Show the parallel coordinate plot of an event based on the qualification matches 
#' and the TBA ranking. 
#' 
#' Note that this function is a wrapper of  `tba` functionality and is therefore
#' quite opinionated. In case you want to make changes, 
#' you would need to work with the basic `tba` functions.
#' Assumptions/Decisions:
#'  - comparisons are based on qualifying matches only. 
#' @param event_code character string of an event
#' @param ... set of variables that should be used in the comparison of teams
#' @param color character value, one of "Captain", "Pick 1", "Pick 2", "Pick 3". Colors all 
#' identified members of an Alliance up to the specified level. Use `NA` for no coloring.
#' @param verbose logical value. Should the function provide messages during runtime? Defaults to TRUE.
#' @return a parallel coordinate plot 
#' @importFrom dplyr select filter 
#' @importFrom ggplot2 aes ggplot xlab ylab scale_colour_manual 
#' @importFrom ggpcp geom_pcp aes_pcp theme_pcp geom_pcp_labels pcp_select pcp_scale
#' @importFrom stats reorder
#' @export
#' @examples
#' # just get the plot
#' compare_teams("2024iacf", score)
compare_teams <- function(event_code, ..., color = "Pick 1", verbose = TRUE) {
  if (verbose) message("Looking up event ... ")
  matches <- get_records(sprintf("event/%s/matches", event_code))

  stopifnot(length(matches) > 0)
  
  if (verbose) message("Loading match details ...")
  
  scores <- get_match_details(matches)
  
  if (verbose) message("Ranking teams ...")
  
  detailed <- scores %>% dplyr::filter(.data$comp_level=="qm") %>%
    get_ranking_by(...) 
  
  if (verbose) message("Adding contribution in score differences ...")
  
  # team contribution to the difference in scores between red and blue
  score_diff <- scores %>% dplyr::filter(.data$comp_level=="qm") %>%
    get_ranking_by_diff(.data$score) 
  detailed <- left_join(detailed, score_diff, by = "team_key")
  
  if (verbose) message("Loading TBA rankings ...")
  
  # add the tba rating to the mix:
  tba_rating <- tba_ranking(event_code)
  detailed <- detailed %>% left_join(tba_rating %>% select("team_key", TBA="rank"), by="team_key")
  detailed <- detailed %>% 
    mutate(
      team_key = factor(.data$team_key),
      team_key = reorder(.data$team_key, .data$TBA)
    )
  
  if (verbose) message("Finding alliance information ...")
  
  frc_roles <- c("Captain", "Pick 1", "Pick 2", "Pick 3")
  
  # get chosen alliances
  alliances <- get_records(sprintf("event/%s/alliances", event_code))

  alliances <- alliances %>% select("name", "picks") %>% unnest(.data$picks) 
  
  alliances <- alliances %>% group_by(.data$name) %>%
    mutate(
      role = frc_roles[1:n()],
      role = factor(.data$role, levels = frc_roles, ordered = TRUE)
    ) %>% ungroup()

  if (verbose) message("Preparing plot ...\n")
  
 # library(ggpcp)
  
  cttd <- detailed %>% 
    left_join(alliances, by=c("team_key"="picks")) %>% 
    mutate(
      TBA = max(.data$TBA)-.data$TBA,
      team_key = reorder(factor(.data$team_key), .data$TBA)
    ) %>% 
    ggpcp::pcp_select("team_key", "TBA", starts_with("rating"), "team_key") %>%
    pcp_scale() 

  alliance_picks <- list()
  
  if (!is.na(color)) {
    which_role <- which(frc_roles==color)
    if (length(which_role) == 0) 
      message(sprintf('Skipping color lines: color = "%s"; must be one of "Captain", "Pick 1", "Pick 2", "Pick 3"', color))
    else {
    
    if (verbose) message(sprintf("Alliance picks up to %s are shown in color.", color))
    
    alliance_picks <-  geom_pcp(aes(colour = .data$name), 
       data = cttd %>% filter(as.numeric(.data$role) <= which_role) %>% arrange(desc(.data$name)), linewidth=1) 
    # observations are ordered from last to first Alliance. This puts 
    # the line segments corresponding to Alliance 1 on top
    }
  }
    
  cttd %>%  
    ggplot2::ggplot(aes_pcp()) + 
    geom_pcp(colour = "grey70") + 
    alliance_picks + 
    geom_pcp_labels(size=3) + 
    xlab("") + ylab("") + 
#    theme_update(axis.text.x = element_text(angle=30, hjust=1)) +
    scale_colour_manual(values = c( "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "grey60")) +
    theme_pcp() 
    
    
  
}
