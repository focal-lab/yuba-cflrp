# Plot size is 14 m radius
plot_area_ha = pi * (14 / 100)^2
plot_area_ac = plot_area_ha * 2.47105  # convert to acres

library(tidyverse)
library(readxl)
library(sf)
library(elevatr)
library(patchwork)

source("constants.R")

plots = read_excel(RAW_INVENTORY_DATA_FILEPATH, sheet = "plot")
trees = read_excel(RAW_INVENTORY_DATA_FILEPATH, sheet = "tree")
fuels = read_excel(RAW_INVENTORY_DATA_FILEPATH, sheet = "fuels")

# Make plots spatial and extract elev

names(plots)

plots = plots |>
  filter()

plots_sf = st_as_sf(plots, coords = c("Easting", "Northing"), crs = 32610) # UTM 10N

plots_sf = elevatr::get_elev_point(plots_sf, src = "aws", z = 12) |> # 13 is prob better
  rename(elev_m = elevation)
  
plots = plots_sf |>
  st_drop_geometry()
  
elev_median = plots$elev_m |>
  median(na.rm = TRUE)

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
                             ordered = TRUE))

# Simplify to only plot_id and elev and sample
plot_type_elev = plots |>
  select(plot_id, elev_m, elev_class, trt_ctl = Sample)




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
  left_join(plot_type_elev, by = "plot_id")

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
  left_join(plot_type_elev, by = "plot_id")


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
  left_join(plot_type_elev, by = "plot_id")

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


# Get mean density across all plots (for sp_grp * trt_ctl)
# TODO: Account for any plots that had 0 tally trees and therefore did not contribute 0s to the mean
trees_sp_trtctl = trees_plt_sp |>
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



# Same as above but broken out by trt_ctl
# TODO: Account for any plots that had 0 tally trees and therefore did not contribute 0s to the mean
trees_sp_size_trtctl = trees_plt_sp_size |>
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



## Prep fuels

# Sum the counts from the two transects per plot
fuels_agg = fuels |>
  group_by(plot_id) |>
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
  left_join(plot_type_elev, by = "plot_id")






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

# BA as violin plot, with boxplot overlay (no whiskers), by elev_class (not species)
p1 = ggplot(trees_plt, aes(x = trt_ctl, y = ba_live_ft)) + 
  geom_violin(fill = "lightblue", alpha = 0.5) +
  geom_boxplot(width = 0.1, outliers = FALSE, fill = "white", color = "black", coef = 0) +
  labs(title = "Live basal area",
       x = "",
       y = "BA (sq ft/acre)") +
  theme_bw()
  
# png("~/Documents/temp/nyfp-figs/trtctl_ba_live_violin.png", width = 600, height = 600, res = 150)
# print(p1)
# dev.off()

# TPA as violin plot, with boxplot overlay (no whiskers), by elev_class (not species)
p2 = ggplot(trees_plt, aes(x = trt_ctl, y = tpa_live_gt10cm)) + 
  geom_violin(fill = "lightblue", alpha = 0.5) +
  geom_boxplot(width = 0.1, outliers = FALSE, fill = "white", color = "black", coef = 0) +
  labs(title = "Live tree density (trees > 4\" DBH)",
       x = "",
       y = "Trees per acre") +
  theme_bw()
  
# png("~/Documents/temp/nyfp-figs/trtctl_tpa_live_violin.png", width = 600, height = 600, res = 150)
# print(p2)
# dev.off()


# Repeat for snags
# BA as violin plot, with boxplot overlay (no whiskers), by elev_class (not species)
p3 = ggplot(trees_plt, aes(x = trt_ctl, y = ba_dead_ft)) + 
  geom_violin(fill = "lightblue", alpha = 0.5) +
  geom_boxplot(width = 0.1, outliers = FALSE, fill = "white", color = "black", coef = 0) +
  labs(title = "Snag basal area",
       x = "",
       y = "Snag BA (sq ft/acre)") +
  theme_bw()
  
# png("~/Documents/temp/nyfp-figs/trtctl_ba_dead_violin.png", width = 600, height = 600, res = 150)
# print(p3)
# dev.off()


# TPA as violin plot, with boxplot overlay (no whiskers), by elev_class (not species)
p4 = ggplot(trees_plt, aes(x = trt_ctl, y = tpa_dead_gt10cm)) + 
  geom_violin(fill = "lightblue", alpha = 0.5) +
  geom_boxplot(width = 0.1, outliers = FALSE, fill = "white", color = "black", coef = 0) +
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
  select(plot_id, elev_class, trt_ctl, mass_fine) |> #, mass_cwd
  pivot_longer(cols = starts_with("mass_"),
               names_to = "fuel_class",
               values_to = "mass_tons_ac")

# Violoin with boxplot overlay (no whiskers), by fuel_class
p5 = ggplot(d_fig, aes(x = trt_ctl, y = mass_tons_ac)) +
  geom_violin(fill = "lightblue", alpha = 0.5) +
  geom_boxplot(width = 0.1, outliers = FALSE, fill = "white", color = "black", coef = 0) +
  labs(title = "Fine fuel mass",
       x = "",
       y = "Mass (tons/acre)") +
  theme_bw()

# png("~/Documents/temp/nyfp-figs/trtctl_fuels_violin.png", width = 600, height = 600, res = 150)
# print(p5)
# dev.off()

png("~/Documents/temp/nyfp-figs/trtctl_tpa+ba+fuels_violin.png", width = 1200, height = 1800, res = 150)
((p1 | p2) / (p3 | p4) / (p5 | plot_spacer()))
dev.off()

## Plot-level CWHR
d_fig = plots |>
  rename(trt_ctl = Sample) |>
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
  facet_wrap(~ trt_ctl)
    
png("~/Documents/temp/nyfp-figs/trtctl_cwhr.png", width = 1200, height = 600, res = 150)
print(p)
dev.off()