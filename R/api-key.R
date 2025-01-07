#' Read and set the API key for read access to TBA 
#' 
#' `get_api_key` checks whether the key for the API to The Blue Aliance (TBA) is available  
#' in the system environment. The function will warn the user in case the key is not found and
#' will provide instructions on how to set the key. 
#' In order to access the API, the user will have to sign up for a (free) account with \url{https://www.thebluealliance.com/account}.
#' The functions in this package expect access to TBA's read APIv3. Once created, 
#' call the function `set_api_key` with the string found under `X-TBA-Auth-Key`.  
#' `set_api_key` makes the specified key available as a variable in the system environment for the current session. 
#' @return `get_api_key` returns the key as an invisible object. `set_api_key` sets the specified key as environment variable. 
#' @export
get_api_key <- function() {
  key <- Sys.getenv("TBA_KEY", unset = NA)
  if (is.na(key)) {
    warning("No key for the API for The Blue Alliance found.
            Sign up for an account at https://www.thebluealliance.com/apidocs/v3.
            Call `set_api_key` with your key to give this package access to the API.")
  }
  invisible(key)
}

#' @rdname get_api_key
#' @importFrom glue glue
#' @param key character string. Authorization key for read access to TBA's APIv3 (2025). 
#' @param check Boolean. Should the key be checked for validity?
#' @export
set_api_key <- function(key, check=TRUE) {
  key_there <- Sys.getenv("TBA_KEY", unset = NA)
  if (!is.na(key_there)) {
    if (key == key_there) { 
      message("Key is set.") 
    } else {
      char <- ""
      while (!(char %in% c("y", "Y", "n", "N")))
        char <- readline("Overwrite existing key with a new API key. Proceed? (y/n): ")
      if (char %in% c("n", "N")) invisible(return())
    }
  }
  if (check) {
    if (!invisible(check_key(key))) stop("Invalid API request likely due to the wrong key. Check your TBA account for the correct key.")
  }
  invisible(Sys.setenv(TBA_KEY=key)) # read with Sys.getenv("TBA_KEY")
}

#' Check the API key
#' 
#' Run a status check on the given key. Returns `TRUE` when the key is valid. 
#' @importFrom httr GET content
#' @param key character string. Authorization key for read access to TBA's APIv3 (2025). 
#' @return Boolean. TRUE, for successful status check.
#' @export
check_key <- function(key) {
  status <- httr::GET(paste0(tba_base, "status"), httr::add_headers("X-TBA-Auth-Key" = key))
  invisible(check_valid(status))
}

# helper function - only status code of 200 is indicative of a valid response
check_valid <- function(httr_result) {
  content <- httr::content(httr_result, type = "application/json")
  if (httr_result$status_code != 200) {
    message(sprintf("Checking access to TBA's APIv3 results in error code.\n Status code %d: %s", 
                    httr_result$status_code, content$Error))
    return(FALSE)
  }
  TRUE
}