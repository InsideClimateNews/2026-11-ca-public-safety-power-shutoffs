# set working directory to the folder containing this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# required packages
library(tidyverse)
library(janitor)
library(sf)
library(esri2sf)
library(scales)

# convert ESRI epoch-millisecond date fields to Pacific calendar dates.
# PSPS date-only values are stored as midnight Pacific, so converting in Pacific time
# returns the intended date and matches the Red Flag Warning dates below.
esri_ms_to_pt_date <- function(x) {
  as.Date(as.POSIXct(x / 1000, origin = "1970-01-01", tz = "UTC"),
          tz = "America/Los_Angeles")
}

# =============================================================================
# Data underlying California Public Utilities Commission Public Power Safety Shutoff Dashboard
# Accessed Nov. 9, 2026
# https://www.arcgis.com/apps/dashboards/ecd21b1c204f47da8b1fcc4c5c3b7d3a#.
# =============================================================================

# full data, not mapped, from 2018 onward
psps_not_mapped <- esri2sf("https://services2.arcgis.com/VofPZYDe2pLxSP5G/ArcGIS/rest/services/survey123_d4f10a2bbdba428283473319f537e4c1/FeatureServer/0") %>%
  clean_names() %>%
  mutate(across(c(first_date_of_poc, de_energization_starting_date, full_restoration_date),
                esri_ms_to_pt_date)) %>%
  unique() %>%
  st_drop_geometry() # location data are all points at zero lat and lon

glimpse(psps_not_mapped)

range(psps_not_mapped$de_energization_starting_date, na.rm = TRUE)
# [1] "2018-01-27" "2026-10-10"

unique(psps_not_mapped$iou)
# [1] "PGE"               "SCE"               "SDGE"              "Liberty Utilities" "PacifiCorp"        "BVES"

# process utility names
psps_not_mapped <- psps_not_mapped %>%
  mutate(iou = case_when(iou == "Liberty Utilities" ~ "Liberty",
                         TRUE ~ iou))

# total customers de-energized
psps_not_mapped %>%
  filter(de_energization_status == "Yes") %>%
  summarize(events = n(),
            customers_de_energized = sum(customers_de_energized,na.rm = TRUE),
            .groups = "drop")
# events customers_de_energized
# 1    128                4561198

# total customers de-energized by investor-owned utility
psps_not_mapped %>%
  filter(de_energization_status == "Yes") %>%
  group_by(iou) %>%
  summarize(events = n(),
            customers_de_energized = sum(customers_de_energized,na.rm = TRUE),
            .groups = "drop") %>%
  arrange(-customers_de_energized)
# # A tibble: 6 × 3
# iou        events customers_de_energized
# <chr>       <int>                  <int>
# 1 PGE            35                2904225
# 2 SCE            67                1346860
# 3 SDGE           17                 250907
# 4 BVES            2                  49954
# 5 Liberty         5                   4740
# 6 PacifiCorp      2                   4512

# geodata, from October 2021 onward (one row per event; PacifiCorp is not in this layer)
psps_mapped <- esri2sf("https://services2.arcgis.com/VofPZYDe2pLxSP5G/arcgis/rest/services/Consolidated_PSPS_Map20221231_gdb/FeatureServer/0") %>%
  clean_names() %>%
  rename(first_date_of_poc = first_dateof_poc,
         de_energization_starting_date = de_energization_start_date,
         customers_de_energized = customer_de_energized,
         de_energization_status = de_energization) %>%
  mutate(across(c(first_date_of_poc, de_energization_starting_date, full_restoration_date),
                esri_ms_to_pt_date)) %>%
  unique()

range(psps_mapped$de_energization_starting_date)
# [1] "2021-10-11" "2026-10-02"

unique(psps_mapped$iou)
# [1] "SCE"     "PGE"     "SDGE"    "Liberty" "BVES"    "PG&E"

# clean utility names
psps_mapped <- psps_mapped %>%
  mutate(iou = case_when(iou == "PG&E" ~ "PGE",
                         iou == "SDG&E" ~ "SDGE",
                         TRUE ~ iou))

# total customers de-energized since Oct. 2021
psps_mapped %>%
  st_drop_geometry() %>%
  filter(de_energization_status == "Yes") %>%
  summarize(events = n(),
            customers_de_energized = sum(customers_de_energized,na.rm = TRUE))
# events customers_de_energized
# 1     73                1081911

# total customers de-energized by company since Oct. 2021
psps_mapped %>%
  st_drop_geometry() %>%
  filter(de_energization_status == "Yes") %>%
  group_by(iou) %>%
  summarize(events = n(),
            customers_de_energized = sum(customers_de_energized,na.rm = TRUE),
            .groups = "drop") %>%
  arrange(-customers_de_energized)
# # A tibble: 5 × 3
# iou     events customers_de_energized
# <chr>    <int>                  <int>
# 1 SCE         43                 826912
# 2 PGE         18                 123805
# 3 SDGE         6                 101179
# 4 BVES         1                  25275
# 5 Liberty      5                   4740

# convert utilities to ordered factor
psps_mapped <- psps_mapped %>%
  mutate(iou = factor(iou, levels = c("SCE","PGE","SDGE","BVES","Liberty"),
                      ordered = TRUE))

# check coordinate reference system
st_crs(psps_mapped)
# 4326

# =============================================================================
# Red Flag Warnings issued for California from 2021-10-01
# from the Iowa Environmental Mesonet (IEM) VTEC warnings archive.
#
#   https://mesonet.agron.iastate.edu/request/gis/watchwarn.phtml
#   https://mesonet.agron.iastate.edu/cgi-bin/request/gis/watchwarn.py?help=
# =============================================================================

# download Red Flag Warning shapefile
base_url <- "https://mesonet.agron.iastate.edu/cgi-bin/request/gis/watchwarn.py"

start_time <- "2021-10-01T00:00Z"
end_time   <- format(Sys.time(), "%Y-%m-%dT%H:%MZ", tz = "UTC")

params <- list(
  accept          = "shapefile",
  sts             = start_time,
  ets             = end_time,
  location_group  = "states",
  states          = "CA",
  phenomena       = "FW",         # fire weather
  significance    = "W",          # warning (Red Flag Warning)
  limitps         = "1"           # restrict output to the phenomena/significance above
)

query_string <- paste(
  mapply(function(n, v) paste0(n, "=", utils::URLencode(v, reserved = TRUE)),
         names(params), params),
  collapse = "&"
)

full_url <- paste0(base_url, "?", query_string)

zip_path <- "ca_red_flag.zip"
unzip_dir <- "ca_red_flag"

download.file(full_url, destfile = zip_path, mode = "wb")
unzip(zip_path, exdir = unzip_dir)

# find the .shp file and import
shp_file <- list.files(unzip_dir, pattern = "\\.shp$", full.names = TRUE)[1]

red_flag <- st_read(shp_file, quiet = TRUE)

# check coordinate reference system
st_crs(red_flag)
# 4326

# =============================================================================
# Prepare data for the overlap test
# =============================================================================

# filter PSPS data to cases where customers were de-energized,
# give every event a unique ID, and flag events missing the dates the overlap test needs
psps_deenergized <- psps_mapped %>%
  filter(de_energization_status == "Yes") %>%
  st_make_valid() %>%
  mutate(event_id = row_number(),
         dates_missing = is.na(de_energization_starting_date) | is.na(full_restoration_date))

# any events with missing dates? (these get NA, not FALSE)
psps_deenergized %>%
  st_drop_geometry() %>%
  count(dates_missing)
# dates_missing  n
# 1         FALSE 73
# none with missing dates

# Red Flag Warnings: IEM writes one row per forecast zone per warning.
# convert issued/expired (UTC timestamps) to Pacific calendar dates, so they're comparable
# to the PSPS date-only fields. A warning is treated as "active" on every Pacific calendar
# date it touches, from its issued date through its expired date.
# ETN is only unique within a forecast office (WFO) and year, so build a warning ID from all three.
red_flag <- red_flag %>%
  st_make_valid() %>%  # guards against topology errors in spatial join
  mutate(rf_row          = row_number(),
         issued_utc      = ymd_hm(ISSUED,  tz = "UTC"),
         expired_utc     = ymd_hm(EXPIRED, tz = "UTC"),
         issued_date_pt  = as.Date(issued_utc,  tz = "America/Los_Angeles"),
         expired_date_pt = as.Date(expired_utc, tz = "America/Los_Angeles"),
         warning_id      = paste(WFO, ETN, year(issued_utc), sep = "-"))

# stop if any timestamp failed to parse, since those warnings would silently drop out of the test
stopifnot(!anyNA(red_flag$issued_utc), !anyNA(red_flag$expired_utc))

# number of distinct Red Flag Warnings (vs. zone rows)
nrow(red_flag)
# [1] 1296
n_distinct(red_flag$warning_id)
# [1] 298

# =============================================================================
# Overlap test
#
# Two definitions:
#   1. full window: a spatially intersecting warning was active on any date from
#                   de-energization start through full restoration
#   2. lead window: a spatially intersecting warning was active on the de-energization
#                   start date, or took effect within `lead_days` after it (and before
#                   restoration of power).
# =============================================================================

lead_days <- 3

# spatial join: one row per event x intersecting warning-zone row
joined <- st_join(psps_deenergized,
                  red_flag %>% select(rf_row, warning_id, issued_date_pt, expired_date_pt),
                  join = st_intersects, left = TRUE) %>%
  st_drop_geometry() %>%
  mutate(
    spatial_match = !is.na(rf_row),
    overlap_full  = spatial_match &
      issued_date_pt  <= full_restoration_date &
      expired_date_pt >= de_energization_starting_date,
    overlap_lead  = spatial_match &
      issued_date_pt  <= pmin(de_energization_starting_date + lead_days, full_restoration_date) &
      expired_date_pt >= de_energization_starting_date
  )

# collapse to one row per event.
# events with no intersecting warning are FALSE; events that do intersect a warning
# but are missing dates can't be tested and are NA
event_overlap_summary <- joined %>%
  group_by(event_id) %>%
  summarise(
    overlapped_red_flag_warning = case_when(!any(spatial_match) ~ FALSE,
                                            first(dates_missing) ~ NA,
                                            TRUE ~ any(overlap_full)),
    overlapped_within_lead      = case_when(!any(spatial_match) ~ FALSE,
                                            first(dates_missing) ~ NA,
                                            TRUE ~ any(overlap_lead)),
    n_warnings_full             = n_distinct(warning_id[overlap_full %in% TRUE]),
    n_warnings_lead             = n_distinct(warning_id[overlap_lead %in% TRUE]),
    .groups = "drop"
  )

# join back onto the PSPS de-energized data
psps_deenergized <- psps_deenergized %>%
  left_join(event_overlap_summary, by = "event_id")

events_df <- st_drop_geometry(psps_deenergized)

# =============================================================================
# Summaries
# =============================================================================

overlap_definitions <- c("full_window", paste0("lead_", lead_days, "_days"))
names(overlap_definitions) <- c("overlapped_red_flag_warning", "overlapped_within_lead")

summarize_definition <- function(flag_col) {
  flag <- events_df[[flag_col]]
  tibble(definition           = overlap_definitions[[flag_col]],
         events_overlap       = sum(flag %in% TRUE),
         events_no_overlap    = sum(flag %in% FALSE),
         events_untestable    = sum(is.na(flag)),
         customers_overlap    = sum(events_df$customers_de_energized[flag %in% TRUE],  na.rm = TRUE),
         customers_no_overlap = sum(events_df$customers_de_energized[flag %in% FALSE], na.rm = TRUE)) %>%
    mutate(pct_customers_overlap = customers_overlap / (customers_overlap + customers_no_overlap))
}

# totals under both definitions
map_dfr(names(overlap_definitions), summarize_definition)
# A tibble: 2 × 7
# definition  events_overlap events_no_overlap events_untestable customers_overlap customers_no_overlap pct_customers_overlap
# <chr>                <int>             <int>             <int>             <int>                <int>                 <dbl>
# 1 full_window             44                29                 0           1019910                62001                 0.943
# 2 lead_3_days             42                31                 0           1005545                76366                 0.929

# totals by utility under both definitions
events_df %>%
  group_by(iou) %>%
  summarise(events = n(),
            events_overlap_full = sum(overlapped_red_flag_warning %in% TRUE),
            events_overlap_lead = sum(overlapped_within_lead %in% TRUE),
            customers_overlap_full = sum(customers_de_energized[overlapped_red_flag_warning %in% TRUE], na.rm = TRUE),
            customers_overlap_lead = sum(customers_de_energized[overlapped_within_lead %in% TRUE], na.rm = TRUE),
            .groups = "drop")
# # A tibble: 5 × 6
# iou     events events_overlap_full events_overlap_lead customers_overlap_full customers_overlap_lead
# <ord>    <int>               <int>               <int>                  <int>                  <int>
# 1 SCE         43                  23                  21                 804330                 789965
# 2 PGE         18                  16                  16                 114419                 114419
# 3 SDGE         6                   5                   5                 101161                 101161
# 4 BVES         1                   0                   0                      0                      0
# 5 Liberty      5                   0                   0                      0                      0

# events that count under the full window but not the lead window
events_df %>%
  filter(overlapped_red_flag_warning %in% TRUE, overlapped_within_lead %in% FALSE) %>%
  select(event_id, iou, event_name, de_energization_starting_date, full_restoration_date,
         customers_de_energized) %>%
  arrange(desc(customers_de_energized))
# event_id iou              event_name de_energization_starting_date full_restoration_date customers_de_energized
# 1       26 SCE SCE PSPS Event 06/10/25                    2025-06-10            2025-06-22                   7513
# 2       31 SCE SCE PSPS Event 08/11/25                    2025-08-10            2025-08-24                   6852

# =============================================================================
# Charts
# choose which definition to chart: "overlapped_within_lead" or "overlapped_red_flag_warning"
# =============================================================================

chart_flag <- "overlapped_within_lead"
# chart_flag <- "overlapped_red_flag_warning"

chart_subtitle <- if (chart_flag == "overlapped_within_lead") {
  paste0("Warning active on shutoff start date or within ", lead_days,
         " days after.")
} else {
  "Warning active at any time from shutoff to full restoration."
}

# summarize data by month (bars centered mid-month)
de_energize_redflag <- events_df %>%
  filter(!is.na(de_energization_starting_date)) %>%
  mutate(overlap_flag = .data[[chart_flag]],
         de_energization_my = floor_date(de_energization_starting_date, "month") + days(14),
         overlap_label = factor(case_when(overlap_flag ~ "Red Flag Warning",
                                          TRUE ~ "No Red Flag Warning"),
                                levels = c("No Red Flag Warning", "Red Flag Warning"))) %>%
  group_by(de_energization_my, overlap_label, iou) %>%
  summarize(events = n(),
            customers_de_energized = sum(customers_de_energized, na.rm = TRUE),
            .groups = "drop")

overlap_colors <- c("No Red Flag Warning" = "#ccc",
                    "Red Flag Warning"    = "red")

# whole-number y-axis breaks for event counts
integer_breaks <- function(lims) {
  top <- ceiling(lims[2])
  seq(0, top, by = max(1, ceiling(top / 5)))
}

ggplot(de_energize_redflag, aes(x = de_energization_my, y = events, fill = overlap_label)) +
  geom_col(width = 28) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  scale_y_continuous(breaks = integer_breaks) +
  scale_x_date(date_breaks = "6 months", labels = label_date_short()) +
  scale_fill_manual(values = overlap_colors, name = "") +
  theme_minimal() +
  facet_wrap(~iou, ncol = 1) +
  labs(title = "PSPS Events, by Month and Overlap With Red Flag Warning", subtitle = chart_subtitle) +
  xlab("") +
  ylab("") +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank())

ggplot(de_energize_redflag, aes(x = de_energization_my, y = customers_de_energized, fill = overlap_label)) +
  geom_col(width = 28) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "6 months", labels = label_date_short()) +
  scale_fill_manual(values = overlap_colors, name = "") +
  facet_wrap(~iou, ncol = 1, scales = "free_y") +
  theme_minimal() +
  labs(title = "Customers De-energized, by Month and Overlap With Red Flag Warning", subtitle = chart_subtitle) +
  xlab("") +
  ylab("") +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank())

