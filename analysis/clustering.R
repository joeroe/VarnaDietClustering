library(cowplot)
library(dbscan)
library(dplyr)
library(ggforce)
library(ggplot2)
library(ggrepel)
library(purrr)
library(readxl)
library(stringr)
library(tidyr)

varna <- read_xlsx("data/varna_human_isotopes.xlsx",
                   sheet = 1,
                   range = "B2:T82",
                   col_names = c("c14_lab_id", "grave", "c14_age", "c14_error",
                                 "c14_note", "age", "sex", "d13c", "d13c_error",
                                 "d15n", "d15n_error", "ratio",  "fruits1",
                                 "fruits1_sd", "fruits2", "fruits2_sd", "fruits3",
                                 "fruits3_sd", "reference")) |>
  fill(d13c:reference, .direction = "up") |>
  drop_na(grave) |>
  mutate(
    site = if_else(str_starts(grave, "G"), "Varna 3", "Varna 1"),
    .before = c14_lab_id
  ) |>
  distinct(grave, .keep_all = TRUE) # remove replicates

# Scatter plot
ggplot(varna, aes(d13c, d15n, label = grave)) +
  geom_point() +
  geom_text_repel(colour = "darkgrey") +
  scale_x_reverse() +
  labs(x = "δ13C", y = "δ15N") +
  theme_cowplot()

# Scatter plot with lab error envelopes
varna |>
  mutate(
    d13c_min = d13c - 0.2,
    d13c_max = d13c + 0.2,
    d15n_min = d15n - 0.3,
    d15n_max = d15n + 0.3
  ) |>
  pivot_longer(d13c_min:d13c_max,
               names_to = "d13c_range_type", values_to = "d13c_range") |>
  pivot_longer(d15n_min:d15n_max,
               names_to = "d15n_range_type", values_to = "d15n_range") |>
  group_by(grave) |>
  mutate(
    range_order = factor(row_number(), levels = c(1, 2, 4, 3))
  ) |>
  arrange(range_order) |>
  ggplot(aes(group = grave)) +
  geom_polygon(aes(d13c_range, d15n_range), alpha = 0.1) +
  geom_point(aes(d13c, d15n)) +
  scale_x_reverse() +
  labs(x = "δ13C", y = "δ15N") +
  theme_cowplot()

# DBSCAN clustering
dbscan_min_points <- 3 # Three's a crowd

# Select optimal eps from KNN plot
kNNdistplot(select(varna, d13c, d15n), minPts = dbscan_min_points)
dbscan_eps <- 0.3

# Determine clusters
varna |>
  nest() |>
  mutate(
    cluster_data = map(data, select, d13c, d15n),
    dbscan = map(cluster_data, dbscan,
                 eps = dbscan_eps, minPts = dbscan_min_points),
    cluster = map(dbscan, "cluster")
  ) |>
  select(data, cluster) |>
  unnest(c(data, cluster)) |>
  mutate(
    cluster = na_if(cluster, 0),
    cluster = factor(cluster)
  ) ->
  varna_cluster

# Plot clusters
ggplot(varna_cluster, aes(d13c, d15n, shape = site, group = cluster)) +
  geom_point() +
  geom_mark_hull(aes(fill = cluster, colour = cluster), na.rm = TRUE) +
  #geom_text_repel(aes(label = grave), colour = "darkgrey") +
  scale_x_reverse() +
  scale_colour_brewer(palette = "Set1", guide = guide_none()) +
  scale_fill_brewer(palette = "Set1", guide = guide_none()) +
  labs(x = "δ13C", y = "δ15N", shape = NULL) +
  theme_cowplot()
