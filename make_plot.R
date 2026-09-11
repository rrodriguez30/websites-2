library(tidyverse)

top_songs <- read_rds("clean_data.rds")

label_data <- top_songs |>
  group_by(track, artist) |>
  slice_max(week, n = 1, with_ties = FALSE) |>
  ungroup() |>
  arrange(rank) |>
  mutate(label_y = seq(4, 92, length.out = n()))

billboard_plot <- top_songs |>
  ggplot(aes(week, rank, group = track, color = peak_rank)) +
  geom_line(linewidth = 1.15, alpha = 0.9) +
  geom_point(size = 1.8, alpha = 0.9) +
  geom_segment(
    data = label_data,
    aes(x = week, y = rank, xend = 78, yend = label_y),
    inherit.aes = FALSE,
    color = "#F4F1DE",
    alpha = 0.35,
    linewidth = 0.4
  ) +
  geom_text(
    data = label_data,
    aes(x = 79, y = label_y, label = str_trunc(track, 24)),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3.2,
    color = "#F4F1DE"
  ) +
  scale_x_continuous(
    breaks = seq(1, 76, by = 10),
    expand = expansion(mult = c(0, 0.35))
  ) +
  scale_y_reverse(breaks = c(1, 10, 20, 40, 60, 80, 100)) +
  scale_color_viridis_c(
    option = "magma",
    direction = -1,
    guide = "none"
  ) +
  labs(
    title = "The long climb to the top",
    subtitle = "Weekly Billboard rank for 12 tracks with the strongest peak positions",
    x = "Week on the chart",
    y = "Billboard rank (1 is highest)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#101820", color = NA),
    panel.background = element_rect(fill = "#101820", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#34424D", linewidth = 0.3),
    plot.title = element_text(color = "#F4F1DE", face = "bold", size = 22),
    plot.subtitle = element_text(color = "#B8C4CC", size = 12),
    axis.title = element_text(color = "#B8C4CC"),
    axis.text = element_text(color = "#B8C4CC"),
    plot.margin = margin(18, 130, 18, 18)
  )

ggsave(
  "billboard_plot.png",
  plot = billboard_plot,
  width = 10,
  height = 7,
  dpi = 300
)
