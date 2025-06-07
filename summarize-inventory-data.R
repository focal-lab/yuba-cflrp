# Plot size is 14 m radius
plot_area_ha = pi * (14 / 100)^2
plot_area_ac = plot_area_ha * 2.47105  # convert to acres

library(tidyverse)
library(readxl)
library(sf)
library(elevatr)

source("constants.R")

plots = read_excel(RAW_INEVNTORY_DATA_FILEPATH, sheet = "plot")
trees = read_excel(RAW_INEVNTORY_DATA_FILEPATH, sheet = "tree")
fuels = read_excel(RAW_INEVNTORY_DATA_FILEPATH, sheet = "fuels")

# Make plots spatial and extract elev

names(plots)

plots = plots |>
  filter()

plots_sf = st_as_sf(plots, coords = c("Easting", "Northing"), crs = 32610) # UTM 10N

plots_sf = elevatr::get_elev_point(plots_sf, src = "aws", z = 12) # 13 is prob better

# Remove spatial
plots = plots_sf |>
  st_drop_geometry() |>
  mutate(cwhr_type = recode(cwhr_type,
                          "Douglas fir" = "DFR",
                          "Douglas Fir" = "DFR",
                          "Ponderosa Pine" = "PPN",
                          "Sierran mixed conifer" = "SMC"),
         cwhr_cover = recode(cwhr_cover,
                             "60+" = "D")) |>
  mutate(cwhr_cover = factor(cwhr_cover, 
                             levels = c("S", "P", "M", "D"),
                             ordered = TRUE))
  
# Simplify to only plot_id and elev
elev_median = plots$elevation %>%
  median(na.rm = TRUE)
plot_elev = plots |>
  select(plot_id, elev_m = elevation) |>
  # Compute low and high elev
  mutate(elev_class = case_when(
    elev_m < (elev_median) ~ "Low elev",
    elev_m > (elev_median) ~ "High elev"
  )) |>
  mutate(elev_class = factor(elev_class, 
                             levels = c("Low elev", "High elev"),
                             ordered = TRUE))




# Recode tree species
trees = trees |>
  mutate(species_code = toupper(species_code))


table(trees$species_code)

trees = trees |>
  filter(dbh_cm >= 10) |>
  mutate(species_group = case_when(
    species_code %in% c("ABCO", "ABMA", "CADE", "PSME", "TABR") ~ "shade_tolerant",
    species_code %in% c("PILA", "PINUS", "PIPO") ~ "pine",
    species_code %in% c("ACMA", "ARME", "CONU", "QUCH", "QUKE") ~ "hardwood",
    species_code %in% c("LIDE") ~ "tanoak",
    TRUE ~ "unknown")) |>
  mutate(species_group = factor(species_group, 
                               levels = c("unknown", "tanoak", "hardwood", "pine", "shade_tolerant"),
                               ordered = TRUE)) |>
  mutate(dbh_in = dbh_cm / 2.54) |>
  mutate(ba_m = pi * (dbh_cm / 200)^2) |>  # basal area in square meters
  mutate(ba_ft = ba_m * 10.7639) |>  # basal area in square feet
  # compute size class in 10 in bins but starting with 10 cm
  mutate(size_class = cut(dbh_in,
                          breaks = seq(0, 70, by = 10),
                          labels = c("04-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70"),
                          include.lowest = TRUE))

# Summarize trees into plot-level metrics
trees_plt = trees |>
  group_by(plot_id) |>
  summarize(
    n_live_gt10cm = sum(status == "L", na.rm = TRUE),
    n_live_gt10in = sum(dbh_cm > 10 & status == "L", na.rm = TRUE),
    n_dead_gt10in = sum(dbh_cm > 10 & status == "D", na.rm = TRUE),
    ba_live_ft = sum(ba_ft[status == "L"], na.rm = TRUE),
    ba_dead_ft = sum(ba_ft[status == "D"], na.rm = TRUE),
    qmd_live_in = sqrt(sum(dbh_in^2 * (status == "L"), na.rm = TRUE) / 
                sum(n_live_gt10cm, na.rm = TRUE)),
  ) |>
  ungroup() |>
  mutate(
    ba_live_sqfac = ba_live_ft / plot_area_ac,
    ba_dead_sqfac = ba_dead_ft / plot_area_ac,
    tpa_live_gt10cm = n_live_gt10cm / plot_area_ac,
    tpa_live_gt10in = n_live_gt10in / plot_area_ac,
    tpa_dead_gt10cm = n_dead_gt10in / plot_area_ac,
    tpa_dead_gt10in = n_dead_gt10in / plot_area_ac
  ) |>
  # Pull in plot elev
  left_join(plot_elev, by = "plot_id")

# Summarize trees into species_group X plot-level metrics
trees_plt_sp = trees |>
  group_by(plot_id, species_group) |>
  summarize(
    n_live_gt10cm = sum(status == "L", na.rm = TRUE),
    n_live_gt10in = sum(dbh_cm > 10 & status == "L", na.rm = TRUE),
    n_dead_gt10in = sum(dbh_cm > 10 & status == "D", na.rm = TRUE),
    ba_live_ft = sum(ba_ft[status == "L"], na.rm = TRUE),
    qmd_live_in = sqrt(sum(dbh_in^2 * (status == "L"), na.rm = TRUE) / 
                sum(n_live_gt10cm, na.rm = TRUE)),
  ) |>
  ungroup() |>
  mutate(
    ba_live_sqfac = ba_live_ft / plot_area_ac,
    tpa_live_gt10cm = n_live_gt10cm / plot_area_ac,
    tpa_live_gt10in = n_live_gt10in / plot_area_ac,
    tpa_snags_gt10in = n_dead_gt10in / plot_area_ac
  ) |>
  # Add zeros for missing species groups
  complete(plot_id, species_group) |>
  mutate(across(everything(), ~replace_na(.x, 0))) |>
  # Pull in plot elev
  left_join(plot_elev, by = "plot_id")


# Summarize trees into sp_group X size_class X plot-level metrics
trees_plt_sp_size = trees |>
  group_by(plot_id, species_group, size_class) |>
  summarize(
    n_live_gt10cm = sum(status == "L", na.rm = TRUE),
    n_live_gt10in = sum(dbh_cm > 10 & status == "L", na.rm = TRUE),
    n_dead_gt10in = sum(dbh_cm > 10 & status == "D", na.rm = TRUE),
    ba_live_ft = sum(ba_ft[status == "L"], na.rm = TRUE),
    qmd_live_in = sqrt(sum(dbh_in^2 * (status == "L"), na.rm = TRUE) /
                sum(n_live_gt10cm, na.rm = TRUE))
  ) |>
  ungroup() |>
  mutate(
    ba_live_sqfac = ba_live_ft / plot_area_ac,
    tpa_live_gt10cm = n_live_gt10cm / plot_area_ac,
    tpa_live_gt10in = n_live_gt10in / plot_area_ac,
    tpa_snags_gt10in = n_dead_gt10in / plot_area_ac
  ) |>
  # Add zeros for missing species groups x size_class
  complete(plot_id, species_group, size_class) |>
  mutate(across(everything(), ~replace_na(.x, 0))) |>
  # Pull in plot elev
  left_join(plot_elev, by = "plot_id")

# Get mean density across all plots (for sp_grp * elev_class)
# TODO: Account for any plots that had 0 tally trees and therefore did not contribute 0s to the mean
trees_sp_elev = trees_plt_sp |>
  group_by(species_group, elev_class) |>
  summarize(
    n_live_gt10cm = mean(n_live_gt10cm, na.rm = TRUE),
    n_live_gt10in = mean(n_live_gt10in, na.rm = TRUE),
    n_dead_gt10in = mean(n_dead_gt10in, na.rm = TRUE),
    ba_live_ft = mean(ba_live_ft, na.rm = TRUE),
    qmd_live_in = mean(qmd_live_in, na.rm = TRUE),
    tpa_live_gt10cm = mean(tpa_live_gt10cm, na.rm = TRUE),
    tpa_live_gt10in = mean(tpa_live_gt10in, na.rm = TRUE),
    tpa_snags_gt10in = mean(tpa_snags_gt10in, na.rm = TRUE)
  ) |>
  ungroup()
  
# Get mean density across all plots (for sp_grp * size_class)
# TODO: Account for any plots that had 0 tally trees and therefore did not contribute 0s to the mean
trees_sp_size = trees_plt_sp_size |>
  group_by(species_group, size_class) |>
  summarize(
    n_live_gt10cm = mean(n_live_gt10cm, na.rm = TRUE),
    n_live_gt10in = mean(n_live_gt10in, na.rm = TRUE),
    n_dead_gt10in = mean(n_dead_gt10in, na.rm = TRUE),
    ba_live_ft = mean(ba_live_ft, na.rm = TRUE),
    qmd_live_in = mean(qmd_live_in, na.rm = TRUE),
    tpa_live_gt10cm = mean(tpa_live_gt10cm, na.rm = TRUE),
    tpa_live_gt10in = mean(tpa_live_gt10in, na.rm = TRUE),
    tpa_snags_gt10in = mean(tpa_snags_gt10in, na.rm = TRUE)
  ) |>
  ungroup()

# Same as above but broken out by elev_class
# TODO: Account for any plots that had 0 tally trees and therefore did not contribute 0s to the mean
trees_sp_size_elev = trees_plt_sp_size |>
  group_by(species_group, size_class, elev_class) |>
  summarize(
    n_live_gt10cm = mean(n_live_gt10cm, na.rm = TRUE),
    n_live_gt10in = mean(n_live_gt10in, na.rm = TRUE),
    n_dead_gt10in = mean(n_dead_gt10in, na.rm = TRUE),
    ba_live_ft = mean(ba_live_ft, na.rm = TRUE),
    qmd_live_in = mean(qmd_live_in, na.rm = TRUE),
    tpa_live_gt10cm = mean(tpa_live_gt10cm, na.rm = TRUE),
    tpa_live_gt10in = mean(tpa_live_gt10in, na.rm = TRUE),
    tpa_snags_gt10in = mean(tpa_snags_gt10in, na.rm = TRUE)
  ) |>
  ungroup()



## Figures

# Size class distribution with species groups colored as stacked bars

ggplot(trees_sp_size, aes(x = size_class, y = tpa_live_gt10cm, fill = species_group)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis_d(option = "D", begin = 0, end = 1, direction = -1) +
  labs(title = "Live tree density by size class and species group",
       x = "Size class (inches)",
       y = "TPA") +
  theme_minimal()

# Redo but broken out by elev_class

ggplot(trees_sp_size_elev, aes(x = size_class, y = tpa_live_gt10cm, fill = species_group)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis_d(option = "D", begin = 0, end = 1, direction = -1) +
  labs(title = "Live tree density by size class and species group",
       x = "Size class (inches)",
       y = "TPA") +
  theme_minimal() +
  facet_wrap(~ elev_class)

# TPA, with bars by species group (stacked), with elev_class as facets, using trees_sp_size
ggplot(trees_sp_elev, aes(x = elev_class, y = tpa_live_gt10cm, fill = species_group)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis_d(option = "D", begin = 0, end = 1, direction = -1) +
  labs(title = "Live tree density (trees > 4\" DBH)",
       x = "",
       y = "TPA") +
  theme_minimal()

# BA, with bars by species group (stacked), with elev_class as facets, using trees_sp_elev
ggplot(trees_sp_elev, aes(x = elev_class, y = ba_live_ft, fill = species_group)) + 
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis_d(option = "D", begin = 0, end = 1, direction = -1) +
  labs(title = "Live basal area",
       x = "",
       y = "BA (sq ft/acre)") +
  theme_minimal()

# BA as violin plot, with boxplot overlay (no whiskers), by elev_class (not species)
ggplot(trees_plt, aes(x = elev_class, y = ba_live_ft)) + 
  geom_violin(fill = "lightblue", alpha = 0.5) +
  geom_boxplot(width = 0.1, outliers = FALSE, fill = "white", color = "black", coef = 0) +
  labs(title = "Live basal area",
       x = "",
       y = "BA (sq ft/acre)") +
  theme_minimal() +
  scale_x_discrete(limits = c("Low elev", "High elev"))

# TPA as violin plot, with boxplot overlay (no whiskers), by elev_class (not species)
ggplot(trees_plt, aes(x = elev_class, y = tpa_live_gt10cm)) + 
  geom_violin(fill = "lightblue", alpha = 0.5) +
  geom_boxplot(width = 0.1, outliers = FALSE, fill = "white", color = "black", coef = 0) +
  labs(title = "Live tree density (trees > 4\" DBH)",
       x = "",
       y = "Trees per acre") +
  theme_minimal() +
  scale_x_discrete(limits = c("Low elev", "High elev"))

# Repeat for snags
# BA as violin plot, with boxplot overlay (no whiskers), by elev_class (not species)
ggplot(trees_plt, aes(x = elev_class, y = ba_dead_ft)) + 
  geom_violin(fill = "lightblue", alpha = 0.5) +
  geom_boxplot(width = 0.1, outliers = FALSE, fill = "white", color = "black", coef = 0) +
  labs(title = "Snag basal area",
       x = "",
       y = "Snag BA (sq ft/acre)") +
  theme_minimal() +
  scale_x_discrete(limits = c("Low elev", "High elev"))

# TPA as violin plot, with boxplot overlay (no whiskers), by elev_class (not species)
ggplot(trees_plt, aes(x = elev_class, y = tpa_dead_gt10cm)) + 
  geom_violin(fill = "lightblue", alpha = 0.5) +
  geom_boxplot(width = 0.1, outliers = FALSE, fill = "white", color = "black", coef = 0) +
  labs(title = "Snag density (snags > 4\" DBH)",
       x = "",
       y = "Snags per acre") +
  theme_minimal() +
  scale_x_discrete(limits = c("Low elev", "High elev"))





## Plot-level CWHR
d_plot = plots |>
  filter(!is.na(cwhr_size) & !is.na(cwhr_cover) & !is.na(cwhr_type)) |>
  filter(!(cwhr_cover %in% c("25-40 percent open", "24-40% open")))

ggplot(d_plot, aes(x = as.factor(cwhr_size), y = cwhr_cover, color = cwhr_type)) +
  geom_jitter(width = .2, height =.2) +
  theme_minimal() +
  scale_color_viridis_d(option = "D", begin = 0, end = 1, direction = -1) +
  labs(title = "CWHR clasification",
       x = "CWHR size class",
       y = "CWHR cover class")
