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

plots_26 = read_excel(RAW_INVENTORY_DATA_2026_FILEPATH, sheet = "plot") |> mutate(year = "2026")
trees_26 = read_excel(RAW_INVENTORY_DATA_2026_FILEPATH, sheet = "tree") |> mutate(year = "2026")
fuels_26 = read_excel(RAW_INVENTORY_DATA_2026_FILEPATH, sheet = "fuels") |> mutate(year = "2026")



# Manually added a treated/untreted column to 2025 & 2026 plot tables based on plot comments

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
  rename(
    cavities_large = "cavities_l",
    cavities_small = "cavities_s",
    Observer_other = "Observer_o"
  ) |>
  # Convert piles_number to numeric, handling missing values
  mutate(piles_number = ifelse(piles_number %in% c("", "N/A", "NA"), NA, piles_number)) |>
  mutate(piles_number = as.numeric(piles_number)) |>
  # Remove the unsurveyed plots
  filter(!is.na(Easting))

plots_26 = plots_26 |>
  rename(
    cavities_large = "cavities_l",
    cavities_small = "cavities_s",
    Observer_other = "Observer_o"
  ) |>
  # Convert piles_number to numeric, handling missing values
  mutate(piles_number = ifelse(piles_number %in% c("", "N/A", "NA"), NA, piles_number)) |>
  mutate(piles_number = as.numeric(piles_number)) |>
  # Remove the unsurveyed plots
  filter(!is.na(cover_tos))

fuels_26 = fuels_26 |>
  # Remove the leading "T" (in e.g. T1) from the transect_id, convert 'X' to NA, and convert to numeric
  mutate(transect_id = str_remove(transect_id, "^T")) |>
  mutate(transect_id = na_if(transect_id, "X")) |>
  mutate(transect_id = as.numeric(transect_id)) |>
  filter(!is.na(plot_id))


trees_26 = trees_26 |>
  # In 2026, DBHs were recorded in inches (even though in column dbh_cm), so convert to cm
  mutate(dbh_cm = dbh_cm * 2.54) |>
  # Remove empty placeholder rows
  filter(!is.na(tree_num))

# Remove an empty placeholder tree with a number ( SPAULDER_012 tree 32 )
trees_26 = trees_26 |>
  filter(!(plot_id == "SPAULDER_012" & tree_num == 32))


# Confirm all 2026 plots have trees listed and vice versa
setdiff(plots_26$plot_id, trees_26$plot_id)
setdiff(trees_26$plot_id, plots_26$plot_id)

# Same for fuels
setdiff(plots_26$plot_id, fuels_26$plot_id)
setdiff(fuels_26$plot_id, plots_26$plot_id)


plots = bind_rows(plots_24, plots_25, plots_26)
trees = bind_rows(trees_24, trees_25, trees_26)
fuels = bind_rows(fuels_24, fuels_25, fuels_26)


# If treated is NA, set to "NA" for plotting purpose


## DATA CLEANING, DETERMINED BASED ON EXAMINATION OF THE DATA

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

# Summarize BA by species to determine which is the match to 2025
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

# Get treatment project from plot ID (second item if format is item_item_item, first item if format is item_item)
plots = plots |>
  mutate(trt_project = if_else(str_count(plot_id, "_") >= 2,
    str_split_fixed(plot_id, "_", 3)[, 2],
    str_split_fixed(plot_id, "_", 3)[, 1]
  )) |>
  # Get plot type & treatment project merged (everything before the final "_": first and second
  # items in item_item_item name, first item in item_item name)
  mutate(plot_id_num = str_extract(plot_id, "[^_]*$")) |>
  mutate(type_trt_project = str_remove(plot_id, "_[^_]*$"))



# # Make plots spatial and extract elev (re-enable once we have the missing 2026 coords)

# names(plots)

# plots_sf = st_as_sf(plots, coords = c("Easting", "Northing"), crs = 32610) # UTM 10N

# plots_sf = elevatr::get_elev_point(plots_sf, src = "aws", z = 12) |> # 13 is prob better
#   rename(elev_m = elevation)
  
# plots = plots_sf |>
#   st_drop_geometry()
  
# elev_median = plots$elev_m |>
#   median(na.rm = TRUE)

# # Hard-code an override
# elev_median = 1305

# Recode values
plots = plots |>
  mutate(cwhr_type = recode(cwhr_type,
                          "Douglas fir" = "DFR",
                          "Douglas Fir" = "DFR",
                          "Ponderosa Pine" = "PPN",
                          "Sierran mixed conifer" = "SMC",
                          "Sierras mixed conifer" = "SMC",
                          "Smc" = "SMC",
                          "PILA need habitat code" = "SMC"),
         cwhr_cover = recode(cwhr_cover,
                             "24-40% open" = "P",
                             "25-40 percent open" = "P",
                             "60+" = "D"))
                             

# Check for any other values that need to be recoded
table(plots$cwhr_type)
table(plots$cwhr_cover)

plots = plots |>
  mutate(cwhr_cover = factor(cwhr_cover,
                             levels = c("S", "P", "M", "D"),
                             ordered = TRUE)) |>
  # # Compute low and high elev # RE-ENABLE ONCE WE HAVE THE MISSING 2026 COORDS
  # mutate(elev_class = case_when(
  #   elev_m < (elev_median) ~ "Low elev",
  #   elev_m > (elev_median) ~ "High elev"
  # )) |>
  # mutate(elev_class = factor(elev_class,
  #                            levels = c("Low elev", "High elev"),
  #                            ordered = TRUE)) |>
  mutate(treated_simp = treated %in% c("y", "inferred"))

# Recode tree species
trees = trees |>
  mutate(species_code = toupper(species_code))

table(trees$species_code)
table(trees_26$species_code)



## For each plot/year, compute plot-level tree metrics

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
  ) #|>
  # # Pull in plot elev
  # left_join(plot_type_elev, by = c("year", "plot_id"))

# # Summarize trees into species_group X plot-level metrics
# trees_plt_sp = trees |>
#   group_by(year, plot_id, species_group) |>
#   summarize(
#     n_live_gt10cm = sum(status == "L", na.rm = TRUE),
#     n_live_gt10in = sum(dbh_cm > 25.4 & status == "L", na.rm = TRUE),
#     n_dead_gt10in = sum(dbh_cm > 25.4 & status == "D", na.rm = TRUE),
#     ba_live_ft = sum(ba_ft[status == "L"], na.rm = TRUE),
#     qmd_live_in = sqrt(sum(dbh_in^2 * (status == "L"), na.rm = TRUE) / 
#                 sum(n_live_gt10cm, na.rm = TRUE)),
#   ) |>
#   ungroup() |>
#   mutate(
#     ba_live_sqfac = ba_live_ft / plot_area_ac,
#     tpa_live_gt10cm = n_live_gt10cm / plot_area_ac,
#     tpa_live_gt10in = n_live_gt10in / plot_area_ac,
#     tpa_snags_gt10in = n_dead_gt10in / plot_area_ac
#   ) |>
#   # Add zeros for missing species groups
#   complete(nesting(year, plot_id), species_group) |>
#   mutate(across(everything(), ~replace_na(.x, 0))) #|>
#   # # Pull in plot elev
#   # left_join(plot_type_elev, by = c("year","plot_id"))


# # Summarize trees into sp_group X size_class X plot-level metrics
# trees_plt_sp_size = trees |>
#   group_by(year, plot_id, species_group, size_class) |>
#   summarize(
#     n_live_gt10cm = sum(status == "L", na.rm = TRUE),
#     n_live_gt10in = sum(dbh_cm > 25.4 & status == "L", na.rm = TRUE),
#     n_dead_gt10in = sum(dbh_cm > 25.4 & status == "D", na.rm = TRUE),
#     ba_live_ft = sum(ba_ft[status == "L"], na.rm = TRUE),
#     qmd_live_in = sqrt(sum(dbh_in^2 * (status == "L"), na.rm = TRUE) /
#                 sum(n_live_gt10cm, na.rm = TRUE))
#   ) |>
#   ungroup() |>
#   mutate(
#     ba_live_sqfac = ba_live_ft / plot_area_ac,
#     tpa_live_gt10cm = n_live_gt10cm / plot_area_ac,
#     tpa_live_gt10in = n_live_gt10in / plot_area_ac,
#     tpa_snags_gt10in = n_dead_gt10in / plot_area_ac
#   ) |>
#   # Add zeros for missing species groups x size_class
#   complete(nesting(year, plot_id), species_group, size_class) |>
#   mutate(across(everything(), ~replace_na(.x, 0))) #|>
#   # # Pull in plot elev
#   # left_join(plot_type_elev, by = c("year","plot_id"))



## For each plot, compute plot-level fuel loads


# Sum the counts from the two transects per plot
fuels_agg = fuels |>
  group_by(year, plot_id) |>
  summarize(
    count_1h = sum(count_1hr, na.rm = TRUE),
    count_10h = sum(count_10h, na.rm = TRUE),
    count_100h = sum(count_100h, na.rm = TRUE),
    count_1000h = sum(count_1000h, na.rm = TRUE)
  )



# Slope correction not needed because transect length was measured on the ground.
correction = 1

#Brown's calculations
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





# Create a full plot-level table with plot-level and aggregated tree and fuel metrics

plot = plots |>
  left_join(trees_plt, by = c("year", "plot_id")) |>
  left_join(fuels_mass, by = c("year", "plot_id"))


# Make a figure where x-axis is year, y-axis is BA, and vertical facet is type_trt_project, with
# each resurveyed plot connected by lines

# TEMPORARILY exclude SPAULDER_016 because it has a DBH that needs fixing
plot_for_fig = plot |>
  filter(plot_id != "SPAULDER_016")

# Color lines by the treated status at each plot's last (rightmost) observation; points and labels
# are colored by their own year's status
add_treated_last = function(d) {
  d |>
    group_by(plot_id) |>
    arrange(year, .by_group = TRUE) |>
    mutate(treated_last = last(treated)) |>
    ungroup()
}

p = ggplot(add_treated_last(plot_for_fig), aes(x = year, y = ba_live_sqfac, group = plot_id)) +
  geom_line(aes(color = treated_last), linewidth = 1) +
  geom_point(aes(color = treated), size = 3) +
  geom_text(aes(label = plot_id_num, color = treated), hjust = -0.3, size = 3) +
  scale_color_viridis_d(begin = 0.2, end = 0.8, na.value = "black") +
  facet_wrap(~type_trt_project) +
  theme_bw() +
  labs(x = "Year", y = "Basal area (sq ft/ac)", color = "Treated", title = "Basal area over time by treatment project")
p

# Redo for TR_GV only
p = ggplot(plot_for_fig |>
             filter(type_trt_project == "TR_GV") |>
             add_treated_last(), aes(x = year, y = ba_live_sqfac, group = plot_id)) +
  geom_line(aes(color = treated_last), linewidth = 1) +
  geom_point(aes(color = treated), size = 3) +
  geom_text(aes(label = plot_id_num, color = treated), hjust = -0.3, size = 3) +
  scale_color_viridis_d(begin = 0.2, end = 0.8, na.value = "black") +
  facet_wrap(~type_trt_project) +
  theme_bw() +
  labs(x = "Year", y = "Basal area (sq ft/ac)", color = "Treated", title = "Basal area over time by treatment project")
p


# Redo without the 2025 obs (treated_last is recomputed after the filter)
p = ggplot(plot_for_fig |>
             filter(type_trt_project == "TR_GV" & year != "2025") |>
             add_treated_last(), aes(x = year, y = ba_live_sqfac, group = plot_id)) +
  geom_line(aes(color = treated_last), linewidth = 1) +
  geom_point(aes(color = treated), size = 3) +
  geom_text(aes(label = plot_id_num, color = treated), hjust = -0.3, size = 3) +
  scale_color_viridis_d(begin = 0.2, end = 0.8, na.value = "black") +
  facet_wrap(~type_trt_project) +
  theme_bw() +
  labs(x = "Year", y = "Basal area (sq ft/ac)", color = "Treated", title = "Basal area over time by treatment project")
p


# Repeat this for fuels (total fine fuel load; the Mg/ha conversion above is disabled, so units are tons/ac)

p = ggplot(add_treated_last(plot_for_fig), aes(x = year, y = mass_fine, group = plot_id)) +
  geom_line(aes(color = treated_last), linewidth = 1) +
  geom_point(aes(color = treated), size = 3) +
  geom_text(aes(label = plot_id_num, color = treated), hjust = -0.3, size = 3) +
  scale_color_viridis_d(begin = 0.2, end = 0.8, na.value = "black") +
  facet_wrap(~type_trt_project) +
  theme_bw() +
  labs(x = "Year", y = "Fine fuel load (tons/ac)", color = "Treated", title = "Fine fuel load over time by treatment project")
p













