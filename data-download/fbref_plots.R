library(ggplot2)

source("fbref_transform.R")

# Show which seasons the elite players played in
j1l_transformed_filtered |>
  filter(name_player %in% elite_players) |>
  group_by(season, name_player) |>
  summarise(season = first(season), name_player = first(name_player)) |>
  ungroup() |>
  mutate(name_player = factor(name_player,
                              levels = sort(unique(name_player),
                                            decreasing = TRUE))) |>
  ggplot(aes(x = season, y = name_player)) +
  geom_line() +
  geom_point() +
  labs(x = "Season played", y = "Player") +
  theme_minimal()

# Show the difference between the elite level players and replacement level players in the data
goals_plot_df <- j1l_transformed_filtered |>
  mutate(group = ifelse(name_player %in% elite_players, "Elite", "RLP"),
         goals_in_match = ifelse(goals_in_match >= 3, 3, goals_in_match),
         goals_in_match = factor(goals_in_match, levels = c(0, 1, 2, 3),
                                 labels = c("0", "1", "2", "3+"), ordered = TRUE)) |>
  group_by(group, goals_in_match) |>
  summarise(count = n())

goals_plot_df <- goals_plot_df |>
  left_join(goals_plot_df |>
              group_by(group) |>
              summarise(total_count = sum(count)),
            by = join_by("group")) |>
  mutate(goals_frac = count / total_count)

goals_plot_df |>
  mutate() |>
  ggplot(aes(x = goals_in_match, y = goals_frac, fill = group)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(x = "Goals per player", y = "Share of outcome per group", fill = "Group") +
  theme_minimal()
