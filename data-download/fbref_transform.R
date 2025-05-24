library(tidyverse)

source("fbref_download.R")

j1l_matches_fix <- j1l_matches |>
  mutate(Wk = case_when(Season_End_Year == 2016 & Round == "First stage" ~ 2 * Wk - 1,
                        Season_End_Year == 2016 & Round == "Second stage" ~ 2 * Wk,
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
              select(MatchID, Home_Away, Player_Name, Gls, Pos),
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
         name_player = Player_Name) |>
  mutate(goal = ifelse(goals_in_match > 0, 1, 0),
         home_pitch = ifelse(Home_Away == "Home", 1, 0),
         name_team = ifelse(Home_Away == "Home", Home, Away),
         name_opp = ifelse(Home_Away == "Away", Home, Away),
         points_team = ifelse(Home_Away == "Home", HomePoints, AwayPoints),
         points_opp = ifelse(Home_Away == "Away", HomePoints, AwayPoints),
         points_diff = points_team - points_opp,
         is_center = Pos %in% c("AM", "FW"), # https://fbref.com/en/about/errata
         id_league = "J1") |>
  select(-c(Home_Away, Home, Away, HomePoints, AwayPoints, Pos)) |>
  # There are some matches that have finished but don't have lineup data yet
  filter(!is.na(name_player)) |>
  group_by(name_player) |>
  arrange(season, gameday) |>
  # Some approximation of the "share_center" variable from the original study
  mutate(N_games_center = reduce(1:38, function(x, i) { x + lag(is_center, n=i, default = 0) }, .init = 0),
         N_games_other = reduce(1:38, function(x, i) { x + lag(!is_center, n=i, default = 0) }, .init = 0),
         share_center = ifelse(N_games_center + N_games_other > 0,
                               N_games_center / (N_games_center + N_games_other), 0)) |>
  ungroup()

write_csv(j1l_transformed, here("data", "j1l_transformed.csv"))

high_market_value_players <- c(
  "Hayao Kawabe", "Samuel Gustafson", "Rafael Ratão", "Deniz Hümmet",
  "Erison", "Yuma Suzuki", "Welton", "Vitor Bueno", "Caetano",
  "Rafael Elias", "Mateus", "Anderson Lopes", "Léo Ceará",
  "Keisuke Osako", "Everton Galdino", "Takuro Kaneko", "Yan Matheus",
  "Aleksandar Cavric", "Ibrahim Dresevic", "Mao Hosoya", "Thuler",
  "Danilo Boza", "Shinnosuke Nakatani", "Hayato Araki",
  "Yasuto Wakizaka", "Kasper Junker", "Matheus Savio", "Yuki Soma",
  "Shunta Tanaka", "Erik", "Jeison Quiñónes", "Ikuma Sekigawa",
  "Shunki Higashi", "Marius Høibråten", "Tsukasa Morishima",
  "Thiago", "Makoto Mitsuta", "Lucas Fernandes", "Taisei Miyashiro",
  "Yoshinori Muto", "Naomichi Ueda", "Tomoya Miki", "Kota Watanabe",
  "Kei Chinen", "Ryotaro Araki", "Yuta Higuchi", "Sandy Walsh",
  "Kento Tachibanada", "Marcinho", "Shuto Nakano", "Mutsuki Kato",
  "Kosei Tani", "Takuma Nishimura", "Takuya Ogiwara", "Yuta Nakayama",
  "Hiroyuki Mae", "Murilo de Souza", "Patrick William",
  "Sota Kawasaki", "Koki Morita", "Riku Handa", "Taiyo Koga",
  "Daiya Maekawa", "Takahiro Ko", "Thiago Andrade", "Katsuya Nagato",
  "Kazuya Konno", "Élber", "Takuya Kida", "Hiroki Akiyama",
  "Shin Yamada", "Satoshi Tanaka", "Ryoma Watanabe", "Sang-ho Na",
  "Masaya Okugawa", "Tolgay Arslan", "Tokuma Suzuki", "Neta Lavi",
  "Keisuke Kurokawa", "Ryuya Nishio", "Tetsushi Yamakawa",
  "Kosuke Saito", "Shota Fujio", "Taichi Hara", "Marcelo Ryan",
  "Shinnosuke Hatanaka", "Motohiko Nakajima", "Kei Koizumi",
  "Shoya Nakajima", "Koki Anzai", "Tomoki Hayakawa",
  "Yosuke Ideguchi", "Yuto Iwasaki", "Ryosuke Kojima",
  "Daiju Sasaki", "Daniel Schmidt", "Valère Germain",
  "Taishi Matsumoto", "Yuya Yamagishi", "Yuya Asano"
)

high_scoring_players <- j1l_lineups |>
  group_by(Player_Name) |>
  summarise(games = n(), goals = sum(Gls), goals_per_game = sum(Gls) / n()) |>
  filter(goals_per_game >= 0.15) |>
  pull(Player_Name)

j1l_transformed |>
  filter((name_player %in% high_market_value_players) | (name_player %in% high_scoring_players)) |>
  write_csv(here("data", "j1l_transformed_small.csv"))
