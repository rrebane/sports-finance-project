library(here)
library(tidyverse)

source("fbref_download.R")

force_rewrite_files <- FALSE

write_to_file_if_not_exists <- function(data, path, force = FALSE) {
  if (!file.exists(path) | force) {
    write_csv(data, path)
  }
}

striker_positions <- c("FW", "AM", "LM", "RM", "LW", "FW,MF", "RW", "CM")
center_positions <- c("FW", "AM", "CM")

j1l_matches_fix <- j1l_matches |>
  mutate(Wk = case_when(Season_End_Year %in% c(2015, 2016) & Round == "First stage" ~ 2 * Wk - 1,
                        Season_End_Year %in% c(2015, 2016) & Round == "Second stage" ~ 2 * Wk,
                        .default = Wk))

teams <- c(j1l_matches_fix$Home, j1l_matches_fix$Away) |>
  unique() |>
  sort()

j1l_points <- tibble()

for (team in teams) {
  match_points <- j1l_matches_fix |>
    filter(!is.na(Wk), (Home == team | Away == team)) |>
    mutate(HomePoints = case_when(HomeGoals > AwayGoals ~ 3,
                                  HomeGoals == AwayGoals ~ 1,
                                  HomeGoals < AwayGoals ~ 0),
           AwayPoints = case_when(HomeGoals < AwayGoals ~ 3,
                                  HomeGoals == AwayGoals ~ 1,
                                  HomeGoals > AwayGoals ~ 0)) |>
    group_by(Season_End_Year) |>
    arrange(Wk) |>
    mutate(CumulativePoints = cumsum(ifelse(Home == team, HomePoints, AwayPoints)),
           CumulativePoints = lag(CumulativePoints, default = 0),
           Team = team) |>
    ungroup() |>
    select(Season_End_Year, Team, Wk, CumulativePoints) |>
    arrange(Season_End_Year, Wk)
  
  j1l_points <- bind_rows(j1l_points, match_points)
}

j1l_transformed <- j1l_matches_fix |>
  filter(Season_End_Year >= 2016, Season_End_Year <= 2024,
         IsComplete, !is.na(Wk)) |>
  select(MatchID, Wk, Date, Competition_Name, Season_End_Year, Home, Away) |>
  left_join(j1l_lineups |>
              filter(!is.na(Gls)) |>
              select(MatchID, Home_Away, Player_Name, Min, Gls, Pos),
            by = join_by(MatchID)) |>
  left_join(j1l_points |> rename(Home = Team),
            by = join_by(Season_End_Year, Wk, Home)) |>
  rename(HomePoints = CumulativePoints) |>
  left_join(j1l_points |> rename(Away = Team),
            by = join_by(Season_End_Year, Wk, Away)) |>
  rename(AwayPoints = CumulativePoints) |>
  rename(id_match = MatchID,
         goals_in_match = Gls,
         season = Season_End_Year,
         gameday = Wk,
         name_league = Competition_Name,
         kick_off = Date,
         name_player = Player_Name,
         min_played = Min) |>
  mutate(goal = ifelse(goals_in_match > 0, 1, 0),
         home_pitch = ifelse(Home_Away == "Home", 1, 0),
         name_team = ifelse(Home_Away == "Home", Home, Away),
         name_opp = ifelse(Home_Away == "Away", Home, Away),
         points_team = ifelse(Home_Away == "Home", HomePoints, AwayPoints),
         points_opp = ifelse(Home_Away == "Away", HomePoints, AwayPoints),
         points_diff = points_team - points_opp,
         share_played = ifelse(is.na(min_played), 0, min_played / 90),
         is_center = Pos %in% center_positions,
         id_league = "J1") |>
  select(-c(Home_Away, Home, Away, HomePoints, AwayPoints, Pos)) |>
  # There are some matches that have finished but don't have lineup data yet
  filter(!is.na(name_player)) |>
  group_by(name_player) |>
  arrange(season, gameday) |>
  # Some approximation of the "share_center" variable from the original study
  mutate(N_games_center = reduce(1:18, function(x, n) { x + lag(is_center, n=n, default = 0) }, .init = 0),
         N_games_other = reduce(1:18, function(x, n) { x + lag(!is_center, n=n, default = 0) }, .init = 0)) |>
  mutate(share_center = ifelse(N_games_center + N_games_other > 0,
                               N_games_center / (N_games_center + N_games_other), 0)) |>
  ungroup()

write_to_file_if_not_exists(
  j1l_transformed,
  here("data", "j1l_transformed.csv"),
  force_rewrite_files
)
 
notable_players <- c(
  "Kengo Nakamura", "Yu Kobayashi", "Akihiro Ienaga", "Teruhito Nakagawa",
  "Michael Olunga", "Leandro Damião", "Tomoki Iwata", "Yuya Osako",
  "Yoshinori Mutō", "Daizen Maeda", "Yuma Suzuki", "Yosuke Ideguchi",
  "Kuryū Matsuki"
)

seasons_played <- j1l_transformed |>
  group_by(season, name_player) |>
  summarise(season = first(season),
            name_player = first(name_player)) |>
  group_by(name_player) |>
  summarise(n_seasons = n()) |>
  ungroup()

high_scoring_players <- j1l_transformed |>
  filter(name_player %in% (seasons_played |> filter(n_seasons >= 2) |> pull(name_player))) |>
  group_by(name_player) |>
  summarise(games = n(),
            goals = sum(goals_in_match),
            goals_per_game = goals / games) |>
  filter(games >= 18, goals_per_game >= 1/3) |>
  arrange(desc(goals_per_game)) |>
  pull(name_player) |>
  sort()

striker_players <- j1l_lineups |>
  filter(Pos %in% striker_positions,
         Player_Name %in% (seasons_played |> filter(n_seasons >= 4) |> pull(name_player))) |>
  pull(Player_Name) |>
  unique() |>
  sort()

j1l_transformed_filtered <- j1l_transformed |>
  filter(name_player %in% notable_players |
           name_player %in% high_scoring_players |
           name_player %in% striker_players)

write_to_file_if_not_exists(
  j1l_transformed_filtered,
  here("data", "j1l_transformed_small.csv"),
  force_rewrite_files
)

# https://footystats.org/japan/j1-league/odds
players_with_wages <- c(
  "Adaílton", "Akihiro Ienaga", "Alexander Scholz", "Anderson Lopes",
  "Andrés Iniesta", "Arthur", "Bryan Linssen", "David Moberg Karlsson", "Dawhan",
  "Diego Oliveira", "Diego Pituca", "Élber", "Gabriel", "Gōtoku Sakai",
  "Hiroki Sakai", "Hiroto Yamami", "Hotaru Yamaguchi", "Jakub Słowik", "Jesiel",
  "João Schmidt", "Jordy Croux", "Jung Sung-ryong", "Kasper Junker",
  "Kwon Kyung-won", "Leonardo", "Lukian Araújo de Almeida", "Marcinho",
  "Marcos Júnior", "Marius Høibråten", "Masaaki Higashiguchi", "Matej Jonjić",
  "Mateus", "Matheus Sávio", "Mitchell Langerak", "Naomichi Ueda", "Neta Lavi",
  "Patric", "Pedro Perotti", "Shinji Kagawa", "Shusaku Nishikawa",
  "Song Bumkeun", "Stefan Mugoša", "Takahiro Ogihara", "Takashi Usami",
  "Tarik Elyounoussi", "Yoshinori Mutō", "Yuma Suzuki", "Yuto Nagatomo",
  "Yuya Osako"
)

elite_players <- c(notable_players, high_scoring_players, players_with_wages) |>
  unique() |>
  sort()

elite_players <- elite_players[(elite_players %in% j1l_transformed_filtered$name_player)]

write_to_file_if_not_exists(
  tibble(player=elite_players),
  here("data", "j1l_elite_players.csv"),
  force_rewrite_files
)

replacement_players <- j1l_transformed_filtered |>
  filter(!(name_player %in% elite_players)) |>
  pull(name_player) |>
  unique() |>
  sort()

write_to_file_if_not_exists(
  tibble(player=replacement_players),
  here("data", "j1l_replacement_players.csv"),
  force_rewrite_files
)