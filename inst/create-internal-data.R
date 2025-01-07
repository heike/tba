tba_base <- "https://www.thebluealliance.com/api/v3/"

usethis::use_data(tba_base, internal=TRUE, overwrite = TRUE)


##############################
## -- Very simple insecure crypto --
rot <- function(ch, k = 13) {
  p0 <- function(...) paste(c(...), collapse = "")
  A <- c(letters, LETTERS, 0:9)
  I <- seq_len(k); chartr(p0(A), p0(c(A[-I], A[I])), ch)
}

pw <- "my secret pass phrase 1234"
(crypw <- rot(pw, 13)) #-> you can send this off

## now ``decrypt'' :
rot(crypw, 62 - 13) # -> the original:
