library(tidyverse)

chart_data <- billboard |>
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    names_prefix = "wk",
    values_to = "rank"
  ) |>
  filter(!is.na(rank)) |>
  mutate(week = as.integer(week))

featured_tracks <- chart_data |>
  group_by(track, artist) |>
  summarise(
    peak_rank = min(rank),
    weeks_on_chart = n(),
    .groups = "drop"
  ) |>
  slice_min(peak_rank, n = 12, with_ties = FALSE)

label_data <- chart_data |>
  inner_join(featured_tracks, by = c("track", "artist")) |>
  group_by(track, artist) |>
  slice_max(week, n = 1, with_ties = FALSE) |>
  ungroup() |>
  arrange(rank) |>
  mutate(label_y = seq(4, 92, length.out = n()))

top_songs <- chart_data |>
  inner_join(featured_tracks, by = c("track", "artist"))

write_rds(top_songs, file = "clean_data.rds")
