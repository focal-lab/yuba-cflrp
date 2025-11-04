# Plot size is 14 m radius
plot_area_ha = pi * (14 / 100)^2
plot_area_ac = plot_area_ha * 2.47105  # convert to acres

library(tidyverse)
library(readxl)
library(sf)
library(elevatr)
library(patchwork)

source("constants.R")

plots_24 = read_excel(RAW_INVENTORY_DATA_2024_FILEPATH, sheet = "plot") |> mutate(year = "2024")
trees_24 = read_excel(RAW_INVENTORY_DATA_2024_FILEPATH, sheet = "tree") |> mutate(year = "2024")
fuels_24 = read_excel(RAW_INVENTORY_DATA_2024_FILEPATH, sheet = "fuels") |> mutate(year = "2024")

plots_25 = read_excel(RAW_INVENTORY_DATA_2025_FILEPATH, sheet = "plot") |> mutate(year = "2025")
trees_25 = read_excel(RAW_INVENTORY_DATA_2025_FILEPATH, sheet = "tree") |> mutate(year = "2025")
fuels_25 = read_excel(RAW_INVENTORY_DATA_2025_FILEPATH, sheet = "fuels") |> mutate(year = "2025")

# Added a treated/untreted column to 2025 plot table based on plot comments

# In 2024 plots, TR_GY_R001 and TR_GY_R002: GY should be GR
plots_24 = plots_24 |>
  mutate(plot_id = recode(plot_id,
                         "TR_GY_R001" = "TR_GV_R001",
                         "TR_GY_R002" = "TR_GV_R002"))
trees_24 = trees_24 |>
  mutate(plot_id = recode(plot_id,
                         "TR_GY_R001" = "TR_GV_R001",
                         "TR_GY_R002" = "TR_GV_R002"))
fuels_24 = fuels_24 |>
  mutate(plot_id = recode(plot_id,
                         "TR_GY_R001" = "TR_GV_R001",
                         "TR_GY_R002" = "TR_GV_R002"))



# Merge the tables

plots_25 = plots_25 |>
  rename(cavities_large = "cavities_l",
                cavities_small = "cavities_s",
                Observer_other = "Observer_o") |>
  # Remove the unsurveyed plots
  filter(!is.na(Easting))

plots = bind_rows(plots_24, plots_25)
trees = bind_rows(trees_24, trees_25)
fuels = bind_rows(fuels_24, fuels_25)

# TR_TP_103 in 2025 has a PSME with a dbh_cm of 168. In 2024, there was a PSME with dbh_cm of 16.8
# that does not have a matching tree in 2025. Change the 2025 dbh to 16.8 to match.
trees[trees$year == "2025" & trees$plot_id == "TR_TP_103" & trees$species_code == "PSME" & trees$dbh_cm == 168, "dbh_cm"] = 16.8


# TR_TP_121 has two trees with tree_num 23. Set the second one to a placeholder tree_num (9000) and
# keep it in the plot. It is a live CADE with DBH 15 cm.
trees = trees |>
  mutate(tree_num = ifelse(year == "2025" & plot_id == "TR_TP_121" & tree_num == 23 & dbh_cm == 15,
                           9000,
                           tree_num))


# There are trees pasted from two 2024 plots TR_TP_R002. Different sets of trees. Plot the BA by
# species for both of them, plus for the 2024 plots TR_TP_R002 to determine which they match. The
# 2024 plot with more trees comes first, so we can rely on duplicated() to isolate the second plot.
trees_foc = trees |>
  filter(plot_id == "TR_TP_R002")

duplicated = which(duplicated(trees_foc[,c("year","plot_id","tree_num")]))
trees_foc$plot_year = ifelse(trees_foc$year == "2025", "2025", 
                             ifelse(1:nrow(trees_foc) %in% duplicated, "2024b", "2024a"))

# Summarize it for bar plot
trees_foc_sum = trees_foc |>
  group_by(plot_year, species_code) |>
  summarize(ba = sum(pi * (dbh_cm / 20)^2)) |>
  ungroup() |>
  arrange(plot_year, -ba, species_code)

# It is clear that the second 2024 plot is the one that matches the 2025 plot. What 2024 plot does
# not have any trees recorded?

plots_plot_2024 = unique(trees_24$plot_id)
trees_plot_2024 = unique(trees_24$plot_id)
setdiff(plots_plot_2024, trees_plot_2024)
setdiff(trees_plot_2024, plots_plot_2024)
# There are no 2024 plots missing tree data.

# Check 2025
plots_plot_2025 = unique(trees_25$plot_id)
trees_plot_2025 = unique(trees_25$plot_id)
setdiff(plots_plot_2025, trees_plot_2025)
setdiff(trees_plot_2025, plots_plot_2025)
# There are no 2025 plots missing tree data.

# Simply remove the duplicate 2024 plot TR_TP_R002 tree survey. Remove the first instance of trees (the one
# that is not labeled as duplicated). This works because the first instance has more trees, so only
# (and all of) the trees in the second instance are marked as duplicated, while none in the first are.
duplicated = duplicated(trees[, c("year", "plot_id", "tree_num")])
trees = trees[!(!duplicated & ((trees$plot_id == "TR_TP_R002") & (trees$year == 2024))), ]

# Remove the first instance of all the trees in the duplicated 2025 plot TR_TP_112
duplicated = duplicated(trees[, c("year", "plot_id", "tree_num")])
trees = trees[!(!duplicated & ((trees$plot_id == "TR_TP_112") & (trees$year == 2025))), ]



# Confirm no dupliated trees
dup_index = which(duplicated(trees[,c("year","plot_id","tree_num")]))
length(dup_index)
trees_dup = trees[dup_index, ]
trees_dup

# Get treatment project from plot ID (second item in item_item_item name)
plots = plots |>
  mutate(trt_project = str_split_fixed(plot_id, "_", 3)[,2])



# Make plots spatial and extract elev

names(plots)

plots_sf = st_as_sf(plots, coords = c("Easting", "Northing"), crs = 32610) # UTM 10N

plots_sf = elevatr::get_elev_point(plots_sf, src = "aws", z = 12) |> # 13 is prob better
  rename(elev_m = elevation)
  
plots = plots_sf |>
  st_drop_geometry()
  
elev_median = plots$elev_m |>
  median(na.rm = TRUE)

# Hard-code an override
elev_median = 1305

# Recode values
plots = plots |>
  mutate(cwhr_type = recode(cwhr_type,
                          "Douglas fir" = "DFR",
                          "Douglas Fir" = "DFR",
                          "Ponderosa Pine" = "PPN",
                          "Sierran mixed conifer" = "SMC"),
         cwhr_cover = recode(cwhr_cover,
                             "60+" = "D")) |>
  mutate(cwhr_cover = factor(cwhr_cover,
                             levels = c("S", "P", "M", "D"),
                             ordered = TRUE)) |>
  # Compute low and high elev
  mutate(elev_class = case_when(
    elev_m < (elev_median) ~ "Low elev",
    elev_m > (elev_median) ~ "High elev"
  )) |>
  mutate(elev_class = factor(elev_class,
                             levels = c("Low elev", "High elev"),
                             ordered = TRUE)) |>
  mutate(treated_simp = treated %in% c("y", "inferred"))

# Simplify to only plot_id and elev and sample (trt vs control) and treated_simp (for merging onto
# summarized tree data))
plot_type_elev = plots |>
  select(year, plot_id, elev_m, elev_class, trt_ctl = Sample, treated_simp, trt_project)
# Add a flag for whether it was a revisited plot that in 2025 was treated. First get the plot IDs that
# treated in 2025
plot_ids_treated_2025 = plots |>
  filter(year == "2025" & treated_simp == TRUE) |>
  pull(plot_id) |>
  unique()
plots_ids_y2024_untrt_y2025_trt = plots |>
  filter(year == "2024" & treated_simp == FALSE & (plot_id %in% plot_ids_treated_2025)) |>
  pull(plot_id) |>
  unique()
plot_type_elev = plot_type_elev |>
  mutate(y2024_untrt_y2025_trt = plot_id %in% plots_ids_y2024_untrt_y2025_trt)


# Recode tree species
trees = trees |>
  mutate(species_code = toupper(species_code))


table(trees$species_code)

trees = trees |>
  filter(dbh_cm >= 10) |>
  mutate(species_group = case_when(
    species_code %in% c("ABCO", "ABMA", "CADE", "PSME", "TABR") ~ "shade_tolerant",
    species_code %in% c("PILA", "PINUS", "PIPO", "PIJE") ~ "pine",
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
  group_by(year, plot_id) |>
  summarize(
    n_live_gt10cm = sum(status == "L", na.rm = TRUE),
    n_live_gt10in = sum(dbh_cm > 25.4 & status == "L", na.rm = TRUE),
    n_dead_gt10in = sum(dbh_cm > 25.4 & status == "D", na.rm = TRUE),
    n_dead_gt10cm = sum(status == "D", na.rm = TRUE),
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
    tpa_dead_gt10cm = n_dead_gt10cm / plot_area_ac,
    tpa_dead_gt10in = n_dead_gt10in / plot_area_ac
  ) |>
  # Pull in plot elev
  left_join(plot_type_elev, by = c("year", "plot_id"))

# Summarize trees into species_group X plot-level metrics
trees_plt_sp = trees |>
  group_by(year, plot_id, species_group) |>
  summarize(
    n_live_gt10cm = sum(status == "L", na.rm = TRUE),
    n_live_gt10in = sum(dbh_cm > 25.4 & status == "L", na.rm = TRUE),
    n_dead_gt10in = sum(dbh_cm > 25.4 & status == "D", na.rm = TRUE),
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
  complete(nesting(year, plot_id), species_group) |>
  mutate(across(everything(), ~replace_na(.x, 0))) |>
  # Pull in plot elev
  left_join(plot_type_elev, by = c("year","plot_id"))


# Summarize trees into sp_group X size_class X plot-level metrics
trees_plt_sp_size = trees |>
  group_by(year, plot_id, species_group, size_class) |>
  summarize(
    n_live_gt10cm = sum(status == "L", na.rm = TRUE),
    n_live_gt10in = sum(dbh_cm > 25.4 & status == "L", na.rm = TRUE),
    n_dead_gt10in = sum(dbh_cm > 25.4 & status == "D", na.rm = TRUE),
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
  complete(nesting(year, plot_id), species_group, size_class) |>
  mutate(across(everything(), ~replace_na(.x, 0))) |>
  # Pull in plot elev
  left_join(plot_type_elev, by = c("year","plot_id"))


# Get mean density across all plots for 2024 (for sp_grp * elev_class)
# TODO: Account for any plots that had 0 tally trees and therefore did not contribute 0s to the mean
trees_sp_elev = trees_plt_sp |>
  filter(year == "2024") |>
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


# Get mean density across all plots for 2024 (for sp_grp * trt_ctl)
# TODO: Account for any plots that had 0 tally trees and therefore did not contribute 0s to the mean
trees_sp_trtctl = trees_plt_sp |>
  filter(year == "2024") |>
  group_by(species_group, trt_ctl) |>
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


# Get mean density across plots that were untreated in 2024 but treated in 2025 and sampled both
# years, broken out by year/treated (which should be synonymous) and treatment project
trees_sp_trtuntrt = trees_plt_sp |>
  filter(y2024_untrt_y2025_trt == TRUE) |>
  group_by(species_group, year, trt_project) |>
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




# Get mean density across all plots for 2024 (for sp_grp * size_class)
# TODO: Account for any plots that had 0 tally trees and therefore did not contribute 0s to the mean
trees_sp_size = trees_plt_sp_size |>
  filter(year == "2024") |>
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
  filter(year == "2024") |>
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

# Same as above but broken out by trt_ctl
# TODO: Account for any plots that had 0 tally trees and therefore did not contribute 0s to the mean
trees_sp_size_trtctl = trees_plt_sp_size |>
  filter(year == "2024") |>
  group_by(species_group, size_class, trt_ctl) |>
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

# Similar as above but  for plots that were untreated in 2024 but treated in 2025 and sampled both
# years, and broken out by year/treated (which should be synonymous) and treatment project
trees_sp_size_y2024untrt_y2025trt = trees_plt_sp_size |>
  filter(plot_id %in% plots_ids_y2024_untrt_y2025_trt) |>
  group_by(species_group, size_class, treated_simp, year, trt_project) |>
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


## Prep fuels

# Sum the counts from the two transects per plot
fuels_agg = fuels |>
  group_by(year, plot_id) |>
  summarize(
    count_1h = sum(count_1hr, na.rm = TRUE),
    count_10h = sum(count_10h, na.rm = TRUE),
    count_100h = sum(count_100h, na.rm = TRUE),
    count_1000h = sum(count_1000h, na.rm = TRUE)
  )



#Determine slope correction factor by averaging slopes for all 4 transects, assuming NA (which is <20%) is 10%
# Correction not needed because transect length was measured on the ground.
correction = 1

#Brown's calculations (code borrowed from Derek Young)
##divisor is transect length (m) * num of transects * conversion to feet
##NOT using qmd-sq, s, or a (angle correction) from Brown - using Van Wagtendonk et al 1996, average of values for PIPO, ABCO, and CADE
##final 2.2417 conversion is from tons/ac to Mg/ha
fuels_mass <- fuels_agg |>
  mutate(mass_1hr = (11.64 * `count_1h` * 0.021 * 0.56 * 1.023 * correction) /
           (2*2*3.28),
         mass_10hr = (11.64 * `count_10h` * 0.212 * 0.55 * 1.023 * correction) /
           (2*2*3.28),
         mass_100hr = (11.64 * `count_100h` * 2.672 * 0.53 * 1.023 * correction) /
           (3*2*3.28)#,
        # We would need sum of squared diameter for CWD to use the following:
        #  mass_cwd = (11.64 * cwd_sum_sq_diam * 0.155 * 0.37 * 1.027 * correction) / # using 0.37 as mean of sound (0.38) and rotten (0.36) values.
        #    (11.3*2*3.28)
           ) |>
  mutate(#mass_total = mass_1hr + mass_10hr + mass_100hr + mass_cwd,
         mass_fine = mass_1hr + mass_10hr + mass_100hr) # |>
  # mutate(across((starts_with("mass_")), ~ . * 2.2417))

# Pull in plot elev and type to fuels_mass
fuels_mass = fuels_mass |>
  left_join(plot_type_elev, by = c("year", "plot_id"))






### Figures

# Size class distribution with species groups colored as stacked bars

p = ggplot(trees_sp_size, aes(x = size_class, y = tpa_live_gt10cm, fill = species_group)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis_d(option = "D", begin = 0, end = 1, direction = -1) +
  labs(title = "Live tree size distribution & species composition",
       x = "Size class (inches)",
       y = "TPA") +
  theme_bw()
  
png("~/Documents/temp/nyfp-figs/size_class_comp.png", width = 800, height = 600, res = 150)
print(p)
dev.off()


## FACETED BY ELEVATION:

# Same as above but broken out by elev_class

p = ggplot(trees_sp_size_elev, aes(x = size_class, y = tpa_live_gt10cm, fill = species_group)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis_d(option = "D", begin = 0, end = 1, direction = -1) +
  labs(title = "Live tree size distribution & species composition",
       x = "Size class (inches)",
       y = "TPA") +
  theme_bw() +
  facet_wrap(~ elev_class)
  
png("~/Documents/temp/nyfp-figs/elev_size_class_comp.png", width = 1200, height = 600, res = 150)
print(p)
dev.off()

# TPA, with bars by species group (stacked), with elev_class as facets, using trees_sp_size
p1 = ggplot(trees_sp_elev, aes(x = elev_class, y = tpa_live_gt10cm, fill = species_group)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis_d(option = "D", begin = 0, end = 1, direction = -1) +
  labs(title = "Live tree density", # \n(trees > 4\" DBH)
       x = "",
       y = "TPA") +
  theme_bw()
  
# png("~/Documents/temp/nyfp-figs/elev_tpa_comp.png", width = 600, height = 600, res = 150)
# print(p1)
# dev.off()


# BA, with bars by species group (stacked), with elev_class as facets, using trees_sp_elev
p2 = ggplot(trees_sp_elev, aes(x = elev_class, y = ba_live_ft, fill = species_group)) + 
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis_d(option = "D", begin = 0, end = 1, direction = -1) +
  labs(title = "Live basal area",
       x = "",
       y = "BA (sq ft/acre)") +
  theme_bw()
  
# png("~/Documents/temp/nyfp-figs/elev_ba_comp.png", width = 600, height = 600, res = 150)
# print(p2)
# dev.off()

png("~/Documents/temp/nyfp-figs/elev_tph+ba_comp.png", width = 1000, height = 600, res = 150)
(p1 | p2) + plot_layout(guides = "collect")
dev.off()

## TODO: NEED TO FILTER TO 2024 ONLY

# BA as violin plot, with boxplot overlay (no whiskers), by elev_class (not species)
p1 = ggplot(trees_plt, aes(x = elev_class, y = ba_live_ft)) + 
  geom_violin(fill = "lightblue", alpha = 0.5) +
  geom_boxplot(width = 0.1, outliers = FALSE, fill = "white", color = "black", coef = 0) +
  labs(title = "Live basal area",
       x = "",
       y = "BA (sq ft/acre)") +
  theme_bw() +
  scale_x_discrete(limits = c("Low elev", "High elev"))

# png("~/Documents/temp/nyfp-figs/elev_ba_live_violin.png", width = 600, height = 600, res = 150)
# print(p1)
# dev.off()

# TPA as violin plot, with boxplot overlay (no whiskers), by elev_class (not species)
p2 = ggplot(trees_plt, aes(x = elev_class, y = tpa_live_gt10cm)) + 
  geom_violin(fill = "lightblue", alpha = 0.5) +
  geom_boxplot(width = 0.1, outliers = FALSE, fill = "white", color = "black", coef = 0) +
  labs(title = "Live tree density (trees > 4\" DBH)",
       x = "",
       y = "Trees per acre") +
  theme_bw() +
  scale_x_discrete(limits = c("Low elev", "High elev"))

# png("~/Documents/temp/nyfp-figs/elev_tpa_live_violin.png", width = 600, height = 600, res = 150)
# print(p2)
# dev.off()


# Repeat for snags
# BA as violin plot, with boxplot overlay (no whiskers), by elev_class (not species)
p3 = ggplot(trees_plt, aes(x = elev_class, y = ba_dead_ft)) + 
  geom_violin(fill = "lightblue", alpha = 0.5) +
  geom_boxplot(width = 0.1, outliers = FALSE, fill = "white", color = "black", coef = 0) +
  labs(title = "Snag basal area",
       x = "",
       y = "Snag BA (sq ft/acre)") +
  theme_bw() +
  scale_x_discrete(limits = c("Low elev", "High elev"))

# png("~/Documents/temp/nyfp-figs/elev_ba_dead_violin.png", width = 600, height = 600, res = 150)
# print(p3)
# dev.off()


# TPA as violin plot, with boxplot overlay (no whiskers), by elev_class (not species)
p4 = ggplot(trees_plt, aes(x = elev_class, y = tpa_dead_gt10cm)) + 
  geom_violin(fill = "lightblue", alpha = 0.5) +
  geom_boxplot(width = 0.1, outliers = FALSE, fill = "white", color = "black", coef = 0) +
  labs(title = "Snag density (snags > 4\" DBH)",
       x = "",
       y = "Snags per acre") +
  theme_bw() +
  scale_x_discrete(limits = c("Low elev", "High elev"))

# png("~/Documents/temp/nyfp-figs/elev_tpa_dead_violin.png", width = 600, height = 600, res = 150)
# print(p4)
# dev.off()


## Plot-level fuels

# Make the fuels df long-form
d_fig = fuels_mass |>
  select(plot_id, elev_class, trt_ctl, mass_fine) |> #, mass_cwd
  pivot_longer(cols = starts_with("mass_"),
               names_to = "fuel_class",
               values_to = "mass_tons_ac")

# Violoin with boxplot overlay (no whiskers), by fuel_class
p5 = ggplot(d_fig, aes(x = elev_class, y = mass_tons_ac)) +
  geom_violin(fill = "lightblue", alpha = 0.5) +
  geom_boxplot(width = 0.1, outliers = FALSE, fill = "white", color = "black", coef = 0) +
  labs(title = "Fine fuel mass",
       x = "",
       y = "Mass (tons/acre)") +
  theme_bw()

# png("~/Documents/temp/nyfp-figs/elev_fuels_violin.png", width = 600, height = 600, res = 150)
# print(p5)
# dev.off()

png("~/Documents/temp/nyfp-figs/elev_tpa+ba+fuels_violin.png", width = 1200, height = 1800, res = 150)
((p1 | p2) / (p3 | p4) / (p5 | plot_spacer()))
dev.off()


## Plot-level CWHR
d_fig = plots |>
  filter(!is.na(cwhr_size) & !is.na(cwhr_cover) & !is.na(cwhr_type)) |>
  filter(!(cwhr_cover %in% c("25-40 percent open", "24-40% open"))) |>
  filter(cwhr_size %in% 0:6)

p = ggplot(d_fig, aes(x = as.factor(cwhr_size), y = cwhr_cover, color = cwhr_type)) +
  geom_jitter(width = .2, height =.2, size = 2) +
  theme_bw() +
  scale_color_brewer(palette = "Dark2") +
  #scale_color_viridis_d(option = "D", begin = 0, end = 1, direction = -1) +
  labs(title = "CWHR clasification",
       x = "CWHR size class",
       y = "CWHR cover class") +
  facet_wrap(~ elev_class)

png("~/Documents/temp/nyfp-figs/elev_cwhr.png", width = 1200, height = 600, res = 150)
print(p)
dev.off()









## FACETED BY CONTROL/TREATMENT:

p = ggplot(trees_sp_size_trtctl, aes(x = size_class, y = tpa_live_gt10cm, fill = species_group)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis_d(option = "D", begin = 0, end = 1, direction = -1) +
  labs(title = "Live tree size distribution & species composition",
       x = "Size class (inches)",
       y = "TPA") +
  theme_bw() +
  facet_wrap(~ trt_ctl)
p

png("~/Documents/temp/nyfp-figs/trtctl_size_class_comp.png", width = 1200, height = 600, res = 150)
print(p)
dev.off()

# TPA, with bars by species group (stacked), with elev_class as facets, using trees_sp_size
p1 = ggplot(trees_sp_trtctl, aes(x = trt_ctl, y = tpa_live_gt10cm, fill = species_group)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis_d(option = "D", begin = 0, end = 1, direction = -1) +
  labs(title = "Live tree density", # (trees > 4\" DBH)
       x = "",
       y = "TPA") +
  theme_bw()
  
# png("~/Documents/temp/nyfp-figs/trtctl_tpa_comp.png", width = 600, height = 600, res = 150)
# print(p1)
# dev.off()



# BA, with bars by species group (stacked), with elev_class as facets, using trees_sp_elev
p2 = ggplot(trees_sp_trtctl, aes(x = trt_ctl, y = ba_live_ft, fill = species_group)) + 
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis_d(option = "D", begin = 0, end = 1, direction = -1) +
  labs(title = "Live basal area",
       x = "",
       y = "BA (sq ft/acre)") +
  theme_bw()
  
# png("~/Documents/temp/nyfp-figs/trtctl_ba_comp.png", width = 600, height = 600, res = 150)
# print(p2)
# dev.off()

png("~/Documents/temp/nyfp-figs/trtctl_tph+ba_comp.png", width = 1000, height = 600, res = 150)
(p1 | p2) + plot_layout(guides = "collect")
dev.off()


# TOODO: NEED TO FILTER TO 2024 ONLY

# # BA as violin plot, with boxplot overlay (no whiskers), by elev_class (not species)
# p1 = ggplot(trees_plt, aes(x = trt_ctl, y = ba_live_ft)) + 
#   geom_violin(fill = "lightblue", alpha = 0.5) +
#   geom_boxplot(width = 0.1, outliers = FALSE, fill = "white", color = "black", coef = 0) +
#   labs(title = "Live basal area",
#        x = "",
#        y = "BA (sq ft/acre)") +
#   theme_bw()
  
# # png("~/Documents/temp/nyfp-figs/trtctl_ba_live_violin.png", width = 600, height = 600, res = 150)
# # print(p1)
# # dev.off()

# # TPA as violin plot, with boxplot overlay (no whiskers), by elev_class (not species)
# p2 = ggplot(trees_plt, aes(x = trt_ctl, y = tpa_live_gt10cm)) + 
#   geom_violin(fill = "lightblue", alpha = 0.5) +
#   geom_boxplot(width = 0.1, outliers = FALSE, fill = "white", color = "black", coef = 0) +
#   labs(title = "Live tree density (trees > 4\" DBH)",
#        x = "",
#        y = "Trees per acre") +
#   theme_bw()
  
# # png("~/Documents/temp/nyfp-figs/trtctl_tpa_live_violin.png", width = 600, height = 600, res = 150)
# # print(p2)
# # dev.off()


# # Repeat for snags
# # BA as violin plot, with boxplot overlay (no whiskers), by elev_class (not species)
# p3 = ggplot(trees_plt, aes(x = trt_ctl, y = ba_dead_ft)) + 
#   geom_violin(fill = "lightblue", alpha = 0.5) +
#   geom_boxplot(width = 0.1, outliers = FALSE, fill = "white", color = "black", coef = 0) +
#   labs(title = "Snag basal area",
#        x = "",
#        y = "Snag BA (sq ft/acre)") +
#   theme_bw()
  
# # png("~/Documents/temp/nyfp-figs/trtctl_ba_dead_violin.png", width = 600, height = 600, res = 150)
# # print(p3)
# # dev.off()


# # TPA as violin plot, with boxplot overlay (no whiskers), by elev_class (not species)
# p4 = ggplot(trees_plt, aes(x = trt_ctl, y = tpa_dead_gt10cm)) + 
#   geom_violin(fill = "lightblue", alpha = 0.5) +
#   geom_boxplot(width = 0.1, outliers = FALSE, fill = "white", color = "black", coef = 0) +
#   labs(title = "Snag density (snags > 4\" DBH)",
#        x = "",
#        y = "Snags per acre") +
#   theme_bw()

# # png("~/Documents/temp/nyfp-figs/trtctl_tpa_dead_violin.png", width = 600, height = 600, res = 150)
# # print(p4)
# # dev.off()


# ## Plot-level fuels

# # Make the fuels df long-form
# d_fig = fuels_mass |>
#   select(plot_id, elev_class, trt_ctl, mass_fine) |> #, mass_cwd
#   pivot_longer(cols = starts_with("mass_"),
#                names_to = "fuel_class",
#                values_to = "mass_tons_ac")

# # Violoin with boxplot overlay (no whiskers), by fuel_class
# p5 = ggplot(d_fig, aes(x = trt_ctl, y = mass_tons_ac)) +
#   geom_violin(fill = "lightblue", alpha = 0.5) +
#   geom_boxplot(width = 0.1, outliers = FALSE, fill = "white", color = "black", coef = 0) +
#   labs(title = "Fine fuel mass",
#        x = "",
#        y = "Mass (tons/acre)") +
#   theme_bw()

# # png("~/Documents/temp/nyfp-figs/trtctl_fuels_violin.png", width = 600, height = 600, res = 150)
# # print(p5)
# # dev.off()

# png("~/Documents/temp/nyfp-figs/trtctl_tpa+ba+fuels_violin.png", width = 1200, height = 1800, res = 150)
# ((p1 | p2) / (p3 | p4) / (p5 | plot_spacer()))
# dev.off()

# ## Plot-level CWHR
# d_fig = plots |>
#   rename(trt_ctl = Sample) |>
#   filter(!is.na(cwhr_size) & !is.na(cwhr_cover) & !is.na(cwhr_type)) |>
#   filter(!(cwhr_cover %in% c("25-40 percent open", "24-40% open"))) |>
#   filter(cwhr_size %in% 0:6)

# p = ggplot(d_fig, aes(x = as.factor(cwhr_size), y = cwhr_cover, color = cwhr_type)) +
#   geom_jitter(width = .2, height =.2, size = 2) +
#   theme_bw() +
#   scale_color_brewer(palette = "Dark2") +
#   #scale_color_viridis_d(option = "D", begin = 0, end = 1, direction = -1) +
#   labs(title = "CWHR clasification",
#        x = "CWHR size class",
#        y = "CWHR cover class") +
#   facet_wrap(~ trt_ctl)
    
# png("~/Documents/temp/nyfp-figs/trtctl_cwhr.png", width = 1200, height = 600, res = 150)
# print(p)
# dev.off()












## FACETED BY 2024/2025 and trt_project (for plots that were measured in both years and were untreated in 2024 but treated in 2025):

# Calculate totals for labels
trees_sp_size_y2024untrt_y2025trt_totals = trees_sp_size_y2024untrt_y2025trt |>
  group_by(trt_project, year, size_class) |>
  summarize(total_tpa = sum(tpa_live_gt10cm, na.rm = TRUE), .groups = "drop")

p = ggplot(trees_sp_size_y2024untrt_y2025trt, aes(x = size_class, y = tpa_live_gt10cm, fill = species_group)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(data = trees_sp_size_y2024untrt_y2025trt_totals,
            aes(x = size_class, y = total_tpa, label = round(total_tpa, 0), fill = NULL),
            vjust = -0.5, size = 3) +
  scale_fill_viridis_d(option = "D", begin = 0, end = 1, direction = -1) +
  labs(title = "Live tree size distribution & species composition",
       x = "Size class (inches)",
       y = "TPA") +
  theme_bw() +
  facet_grid(trt_project ~ year)
p

png("~/temp/nyfp-figs/trtuntrt_size_class_comp.png", width = 1200, height = 1200, res = 150)
print(p)
dev.off()

# Calculate totals for TPA labels
trees_sp_trtuntrt_tpa_totals = trees_sp_trtuntrt |>
  group_by(year, trt_project) |>
  summarize(total_tpa = sum(tpa_live_gt10cm, na.rm = TRUE), .groups = "drop")

# TPA, with bars by species group (stacked), with elev_class as facets, using trees_sp_size
p1 = ggplot(trees_sp_trtuntrt, aes(x = year, y = tpa_live_gt10cm, fill = species_group)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(data = trees_sp_trtuntrt_tpa_totals,
            aes(x = year, y = total_tpa, label = round(total_tpa, 0), fill = NULL),
            vjust = -0.5, size = 3) +
  scale_fill_viridis_d(option = "D", begin = 0, end = 1, direction = -1) +
  labs(
    title = "Live tree density", # (trees > 4\" DBH)
    x = "",
    y = "TPA"
  ) +
  facet_wrap(~ forcats::fct_rev(trt_project), ncol = 1) +
  theme_bw()
  
# png("~/Documents/temp/nyfp-figs/trtctl_tpa_comp.png", width = 600, height = 600, res = 150)
# print(p1)
# dev.off()

# Calculate totals for BA labels
trees_sp_trtuntrt_ba_totals = trees_sp_trtuntrt |>
  group_by(year, trt_project) |>
  summarize(total_ba = sum(ba_live_ft, na.rm = TRUE), .groups = "drop")

# BA, with bars by species group (stacked), with elev_class as facets, using trees_sp_elev
p2 = ggplot(trees_sp_trtuntrt, aes(x = year, y = ba_live_ft, fill = species_group)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(data = trees_sp_trtuntrt_ba_totals,
            aes(x = year, y = total_ba, label = round(total_ba, 0), fill = NULL),
            vjust = -0.5, size = 3) +
  scale_fill_viridis_d(option = "D", begin = 0, end = 1, direction = -1) +
  labs(title = "Live basal area",
       x = "",
       y = "BA (sq ft/acre)") +
  facet_wrap(~ forcats::fct_rev(trt_project), ncol = 1) +
  theme_bw()

png("~/temp/nyfp-figs/trtctl_tph+ba_comp.png", width = 1000, height = 1200, res = 150)
(p1 | p2) + plot_layout(guides = "collect")
dev.off()


trees_plt_y2024untrt_y2025trt = trees_plt |>
  filter(y2024_untrt_y2025_trt == TRUE)

# BA as violin plot, with boxplot overlay (no whiskers), by elev_class (not species)
p1 = ggplot(trees_plt_y2024untrt_y2025trt, aes(x = year, y = ba_live_ft)) +
  geom_violin(fill = "lightblue", alpha = 0.5) +
  geom_boxplot(width = 0.1, outliers = FALSE, fill = "white", color = "black", coef = 0) +
  geom_jitter(width = 0.1, height = 0, size = 1, alpha = 0.5) +
  facet_grid(. ~ trt_project) +
  labs(title = "Live basal area",
       x = "",
       y = "BA (sq ft/acre)") +
  theme_bw()
  
# png("~/Documents/temp/nyfp-figs/trtctl_ba_live_violin.png", width = 600, height = 600, res = 150)
# print(p1)
# dev.off()

# Second version: BA with connected lines for each plot
p1_lines = ggplot(trees_plt_y2024untrt_y2025trt, aes(x = year, y = ba_live_ft)) +
  geom_violin(fill = "lightblue", alpha = 0.5) +
  geom_boxplot(width = 0.1, outliers = FALSE, fill = "white", color = "black", coef = 0) +
  geom_line(aes(group = plot_id), alpha = 0.5) +
  geom_point(size = 2) +
  geom_text(data = trees_plt_y2024untrt_y2025trt |> filter(year == "2025"),
            aes(label = plot_id), hjust = -0.1, size = 2.5) +
  facet_grid(. ~ trt_project) +
  labs(title = "Live basal area (individual plots)",
       x = "",
       y = "BA (sq ft/acre)") +
  theme_bw()

png("~/temp/nyfp-figs/trtctl_ba_live_lines.png", width = 1200, height = 1000, res = 150)
print(p1_lines)
dev.off()

# TPA as violin plot, with boxplot overlay (no whiskers), by elev_class (not species)
p2 = ggplot(trees_plt_y2024untrt_y2025trt, aes(x = year, y = tpa_live_gt10cm)) + 
  geom_violin(fill = "lightblue", alpha = 0.5) +
  geom_boxplot(width = 0.1, outliers = FALSE, fill = "white", color = "black", coef = 0) +
  geom_jitter(width = 0.1, height = 0, size = 1, alpha = 0.5) +
  facet_grid(. ~ trt_project) +
  labs(title = "Live tree density (trees > 4\" DBH)",
       x = "",
       y = "Trees per acre") +
  theme_bw()
  
# png("~/Documents/temp/nyfp-figs/trtctl_tpa_live_violin.png", width = 600, height = 600, res = 150)
# print(p2)
# dev.off()


# Repeat for snags
# BA as violin plot, with boxplot overlay (no whiskers), by elev_class (not species)
p3 = ggplot(trees_plt_y2024untrt_y2025trt, aes(x = year, y = ba_dead_ft)) + 
  geom_violin(fill = "lightblue", alpha = 0.5) +
  geom_boxplot(width = 0.1, outliers = FALSE, fill = "white", color = "black", coef = 0) +
  geom_jitter(width = 0.1, height = 0, size = 1, alpha = 0.5) +
  facet_grid(. ~ trt_project) +
  labs(title = "Snag basal area",
       x = "",
       y = "Snag BA (sq ft/acre)") +
  theme_bw()
  
# png("~/Documents/temp/nyfp-figs/trtctl_ba_dead_violin.png", width = 600, height = 600, res = 150)
# print(p3)
# dev.off()


# TPA as violin plot, with boxplot overlay (no whiskers), by elev_class (not species)
p4 = ggplot(trees_plt_y2024untrt_y2025trt, aes(x = year, y = tpa_dead_gt10cm)) + 
  geom_violin(fill = "lightblue", alpha = 0.5) +
  geom_boxplot(width = 0.1, outliers = FALSE, fill = "white", color = "black", coef = 0) +
  geom_jitter(width = 0.1, height = 0, size = 1, alpha = 0.5) +
  facet_grid(. ~ trt_project) +
  labs(title = "Snag density (snags > 4\" DBH)",
       x = "",
       y = "Snags per acre") +
  theme_bw()

# png("~/Documents/temp/nyfp-figs/trtctl_tpa_dead_violin.png", width = 600, height = 600, res = 150)
# print(p4)
# dev.off()


## Plot-level fuels

# Make the fuels df long-form
d_fig = fuels_mass |>
  filter(y2024_untrt_y2025_trt == TRUE) |>
  select(year, plot_id, elev_class, trt_ctl, mass_fine, trt_project) |> #, mass_cwd
  pivot_longer(cols = starts_with("mass_"),
               names_to = "fuel_class",
               values_to = "mass_tons_ac")

# Violoin with boxplot overlay (no whiskers), by fuel_class
p5 = ggplot(d_fig, aes(x = year, y = mass_tons_ac)) +
  geom_violin(fill = "lightblue", alpha = 0.5) +
  geom_boxplot(width = 0.1, outliers = FALSE, fill = "white", color = "black", coef = 0) +
  geom_jitter(width = 0.1, height = 0, size = 1, alpha = 0.5) +
  facet_grid(. ~ trt_project) +
  labs(title = "Fine fuel mass",
       x = "",
       y = "Mass (tons/acre)") +
  theme_bw()

# png("~/Documents/temp/nyfp-figs/trtctl_fuels_violin.png", width = 600, height = 600, res = 150)
# print(p5)
# dev.off()

png("~/temp/nyfp-figs/trtctl_tpa+ba+fuels_violin.png", width = 1200, height = 1800, res = 150)
((p1 | p2) / (p3 | p4) / (p5 | plot_spacer()))
dev.off()


# STILL MUST ADAPT TO THE PRE/POST TREATMENT SCENARIO

# ## Plot-level CWHR
# d_fig = plots |>
#   rename(trt_ctl = Sample) |>
#   filter(!is.na(cwhr_size) & !is.na(cwhr_cover) & !is.na(cwhr_type)) |>
#   filter(!(cwhr_cover %in% c("25-40 percent open", "24-40% open"))) |>
#   filter(cwhr_size %in% 0:6)

# p = ggplot(d_fig, aes(x = as.factor(cwhr_size), y = cwhr_cover, color = cwhr_type)) +
#   geom_jitter(width = .2, height =.2, size = 2) +
#   theme_bw() +
#   scale_color_brewer(palette = "Dark2") +
#   #scale_color_viridis_d(option = "D", begin = 0, end = 1, direction = -1) +
#   labs(title = "CWHR clasification",
#        x = "CWHR size class",
#        y = "CWHR cover class") +
#   facet_wrap(~ trt_ctl)
    
# png("~/Documents/temp/nyfp-figs/trtctl_cwhr.png", width = 1200, height = 600, res = 150)
# print(p)
# dev.off()