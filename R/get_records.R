#' Get a dataframe with records from TBA
#'
#' The Blue Alliance API v3 is specified at https://www.thebluealliance.com/apidocs/v3. 
#' The function `get_records` takes a `call` to any of the TBA lists under `list` and returns, if possible, all records in a data frame.
#' In case the results can not be converted to a data frame, they are returned as list. 
#' @param call call string from `list`  https://www.thebluealliance.com/apidocs/v3#data-tag-list
#' @returns tibble (or list) of the content.
#' @importFrom purrr compact
#' @importFrom httr2 request req_headers req_headers_redacted req_url_path_append req_perform resp_body_json
#' @importFrom tibble as_tibble
#' @export
#' @examples
#' events <- get_records("events/2024")
#' events[grep("Corn", events$name),] # yep, there is an event with 'Corn' in it
#' 
#' table(events$state_prov)
#' 
#' # now draw a map of all events in North America
#' require(ggplot2)
#' require(dplyr)
#' events %>% slice(grep("America", timezone)) %>% 
#'   filter(lat > 20) %>% # sorry, South America
#'   ggplot(aes(x = lng, y = lat)) + geom_point() + 
#'   coord_equal()
#' 
get_records <- function(call) {
  # use any call from `list` calls on https://www.thebluealliance.com/apidocs/v3
#  browser()
  records <- httr::GET(
    paste0(tba_base, call), httr::add_headers("X-TBA-Auth-Key" = get_api_key())
  )
  stopifnot(check_valid(records))
  
  base_url <- "https://www.thebluealliance.com/api/v3"
  req <- request(base_url)
  
  req <- req |>
    req_headers("Accept"="application/json") |>
    req_headers_redacted("X-TBA-Auth-Key" = get_api_key()) |>
    req_url_path_append(call)
  
  resp <- req |> req_perform()
  
  results <- resp |> resp_body_json(simplifyVector=TRUE)  
  
#  records_frame <- jsonlite::fromJSON(records_frame, encoding = "UTF-8")
  # records |> 
  # if (is.null(nrow(records))) {
  #    records <- purrr::compact(records)
  # }
  # results <- records
  # res <- try({
  #   results <- as_tibble(results)
  #   }, silent = TRUE)
  results
}
