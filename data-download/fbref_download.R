library(here)
library(tidyverse)
library(worldfootballR)

get_matches <- function(filename, years, ...) {
  if (file.exists(filename)) {
    fb_matches <- read_csv(filename)
  } else {
    fb_matches <- tibble()
    
    for (year in years) {
      print(year)
      match_results <- fb_match_results(season_end_year = year, ...)
      fb_matches <- bind_rows(fb_matches, match_results)
      Sys.sleep(6.1) # Rate limit of 10 requests per minute
    }
    
    fb_matches <- fb_matches |>
      mutate(IsComplete = !grepl('.*/stathead/matchup/teams/.*', MatchURL),
             MatchID = ifelse(IsComplete,
                              gsub('.*/matches/([^/]+)/.*', '\\1', MatchURL),
                              NA))
    
    write_csv(fb_matches, filename)
  }

  return(fb_matches)
}

get_lineups <- function(filename, match_urls, batch_size = 100) {
  filtered_match_urls <- match_urls
  
  match_url_bl_filename <- sprintf("%s.bl.csv", filename)
  if (file.exists(match_url_bl_filename)) {
    match_url_bl <- read_csv(match_url_bl_filename)
    filtered_match_urls <- filtered_match_urls[which(!filtered_match_urls %in% match_url_bl$MatchURL)]
  } else {
    match_url_bl <- tibble()
  }
  
  if (file.exists(filename)) {
    fb_lineups <- read_csv(filename) |>
      mutate(Player_Num = as.character(Player_Num))
    filtered_match_urls <- filtered_match_urls[which(!filtered_match_urls %in% fb_lineups$MatchURL)]
  } else {
    fb_lineups <- tibble()
  }

  if (length(filtered_match_urls) > 0) {
    batch_groups <- rep(1:(as.integer(length(filtered_match_urls) / batch_size) + 1),
                        each=batch_size, length.out=length(filtered_match_urls))
    match_url_batches <- split(filtered_match_urls, batch_groups)

    for (match_url_batch in match_url_batches) {
      print(match_url_batch[1])
      match_lineups <- fb_match_lineups(match_url_batch, time_pause = 6.1)

      missing_urls <- match_url_batch
      
      if (nrow(match_lineups) > 0) {
        missing_urls <- match_url_batch[which(!match_url_batch %in% match_lineups$MatchURL)]
      }
      
      if (length(missing_urls) > 0) {
        match_url_bl <- bind_rows(match_url_bl, tibble(MatchURL = missing_urls))
        write_csv(match_url_bl, match_url_bl_filename)
      }
      
      if (nrow(match_lineups) > 0) {
        fb_lineups <- bind_rows(fb_lineups, match_lineups)
        
        fb_lineups <- fb_lineups |>
          mutate(MatchID = gsub('.*/matches/([^/]+)/.*', '\\1', MatchURL))
        
        write_csv(fb_lineups, filename)
      }
    }
  }
  
  return(fb_lineups)
}

j1l_matches <- get_matches(here("data", "j1l_matches.csv"), 2014:2025,
                           country = "JPN", gender = "M", tier = "1st")

j1l_lineups <- get_lineups(here("data", "j1l_lineups.csv"),
                           j1l_matches |>
                             filter(IsComplete) |>
                             pull(MatchURL))

# j2l_matches <- get_matches(here("data", "j2l_matches.csv"), 2014:2025,
#                            country = "JPN", gender = "M", tier = "2nd")

# j2l_lineups <- get_lineups(here("data", "j2l_lineups.csv"),
#                            j2l_matches |>
#                              filter(IsComplete) |>
#                              pull(MatchURL))