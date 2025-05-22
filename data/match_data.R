library(tidyverse)
library(worldfootballR)

sleep_time <- 6.1 # Request limit of 10 per minute

# J1 league
j1l_matches <- tibble()

for (year in 2014:2025) {
  print(year)
  match_results <- fb_match_results(country = "JPN", gender = "M", season_end_year = year, tier = "1st")
  j1l_matches <- bind_rows(j1l_matches, match_results)
  Sys.sleep(sleep_time) # Rate limit of 10 requests per minute
}

j1l_matches <- j1l_matches |>
  mutate(match_id = gsub('.*/matches/([^/]+)/.*', '\\1', MatchURL))

j1_lineups <- tibble()

for (year in 2014:2025) {
  print(year)
  match_urls <- j1l_matches |> filter(Season_End_Year == year) |> pull(MatchURL)
  match_lineups <- fb_match_lineups(match_urls, time_pause = sleep_time)
  j1_lineups <- bind_rows(j1_lineups, match_lineups)
}

j1l_lineups <- j1l_lineups |>
  mutate(match_id = gsub('.*/matches/([^/]+)/.*', '\\1', MatchURL))

# J2 league
# ...