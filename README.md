
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tba

<!-- badges: start -->

<!-- badges: end -->

The goal of the tba package is to provide easy access to match data for
events run in the first robotics division. Access is based on the api of
[The Blue Alliance](https://www.thebluealliance.com/).

## Installation

You can install the development version of tba from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("heike/tba")
```

In order to access the API of The Blue Alliance, you will need to [sign
up for a (free) account](https://www.thebluealliance.com/apidocs/v3).
Then run

``` r
tba::set_api_key(key = "jnn1ZiUfqPuA9UJyV9EVa3jtL5qudBBlYXKPEVQDeQgQgz5L8BypwjTzVKxtCs3W") 
# key is not valid, get your own!
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(tba)
## basic example code

# Team Neutrino has team code `frc3928` 
get_records("team/frc3928")
#> # A tibble: 1 × 11
#>   city  country key     name        nickname postal_code rookie_year school_name
#>   <chr> <chr>   <chr>   <chr>       <chr>    <chr>             <int> <chr>      
#> 1 Ames  USA     frc3928 Iowa 4-H F… Team Ne… 50011              2012 4-H        
#> # ℹ 3 more variables: state_prov <chr>, team_number <int>, website <chr>

# In the 2024 season, the team attended the following events:
events <- get_records("team/frc3928/events/2024")
events$name
#> [1] "Cow Town Throwdown"        "Galileo Division"         
#> [3] "Iowa Regional"             "Clash in the Corn"        
#> [5] "Central Missouri Regional"
```

The matches from Clash in the Corn can be accessed using the event code
`2024iawes`:

``` r
library(dplyr, quietly=TRUE)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
matches <- get_records("event/2024iawes/matches")
count(matches, comp_level)
#> # A tibble: 3 × 2
#>   comp_level     n
#>   <chr>      <int>
#> 1 f              2
#> 2 qm            22
#> 3 sf             5
# 22 qualifying matches
# 5 semi-finals
# 2 finals
```

The function `get_match_details` turns the records into a more
manageable form by turning the match-based rows from the `get_records`
results into team-based results, i.e. every row corresponds to the
detailed scores of each team in each one of the matches:

``` r
scores <- get_match_details(matches)
head(scores %>% select(comp_level, match_number, set_number, alliance, team_key, score, ends_with("Points")))
#> # A tibble: 6 × 25
#>   comp_level match_number set_number alliance team_key score adjustPoints
#>   <chr>             <int>      <int> <chr>    <chr>    <int>        <int>
#> 1 f                     1          1 blue     frc9998     38            0
#> 2 f                     1          1 blue     frc6419     38            0
#> 3 f                     1          1 blue     frc4646     38            0
#> 4 f                     2          1 blue     frc9998     44            0
#> 5 f                     2          1 blue     frc6419     44            0
#> 6 f                     2          1 blue     frc4646     44            0
#> # ℹ 18 more variables: autoAmpNotePoints <int>, autoLeavePoints <int>,
#> #   autoPoints <int>, autoSpeakerNotePoints <int>, autoTotalNotePoints <int>,
#> #   endGameHarmonyPoints <int>, endGameNoteInTrapPoints <int>,
#> #   endGameOnStagePoints <int>, endGameParkPoints <int>,
#> #   endGameSpotLightBonusPoints <int>, endGameTotalStagePoints <int>,
#> #   foulPoints <int>, teleopAmpNotePoints <int>, teleopPoints <int>,
#> #   teleopSpeakerNoteAmplifiedPoints <int>, teleopSpeakerNotePoints <int>, …
```
