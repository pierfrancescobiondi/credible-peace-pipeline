# =============================================================================
# THESIS EMPIRICAL IMPLEMENTATION
# Pre-conflict institutional quality and post-ceasefire economic recovery


#Author: Pierfrancesco Biondi
# Date: 12/06/2026
# R version: 4.5.1
# =============================================================================




# =============================================================================
# function to clear the console, plots and environment

clear_all <- function() {
  # clear console
  cat("\014")
  
  # clear plots
  if (dev.cur() > 1) dev.off()
  while (dev.cur() > 1) dev.off()
  
  # clear environment (global environment)
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  
  message("Console, Plots, and Environment cleared!")
}


clear_all()
# =============================================================================







# --- STAGE 1: SETUP -----------------------------------------------------------

# 1.1 Load packages ------------------------------------------------------------

install_and_load_packages <- function(packages) {
  # check for packages that are not installed
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  
  # install missing packages
  if (length(new_packages)) {
    install.packages(new_packages)
    message("The following packages were installed: ", paste(new_packages, collapse = ", "))
  } else {
    message("All packages are already installed.")
  }
  
  # load all the packages
  sapply(packages, require, character.only = TRUE)
}


#packages to install
packages_to_install <- c("tidyverse", "lubridate", "data.table", "haven", "countrycode", "fixest", "modelsummary", "pandoc")
install_and_load_packages(packages_to_install)

library(tidyverse)    # Core data manipulation (dplyr, tidyr, ggplot2, readr)
library(lubridate)    # Date handling 
library(data.table)   # Fast reading of large CSVs 
library(countrycode) 
library(fixest)       # Fixed effects estimation
library(modelsummary) # Regression table output

# 1.2 File paths -----------------------------------------------------------

base_path <- "Desktop/Thesis/" 

# Input paths
path_ceasefire  <- file.path(base_path, "data", "raw", "CFD_oct_2022_id-1.csv")
path_ged        <- file.path(base_path, "data", "raw", "GEDEvent_v25_1.rds")
path_ntl        <- file.path(base_path, "data", "raw", "prio_cells_nightlights_daily_clean_all.csv")
path_vdem     <- file.path(base_path, "data", "raw", "V-Dem-CY-Full+Others-v16.rds")  
path_geopko   <- file.path(base_path, "data", "raw", "geo-pko-v2.1.csv")
path_gdp     <- file.path(base_path, "data", "raw", "0b8580e1-b6f8-44b3-b1da-ebc7fb81a0d2_Data.csv") 

# Output paths
path_output     <- file.path(base_path, "data", "processed")
path_figures    <- file.path(base_path, "figures")
path_tables     <- file.path(base_path, "tables")

dir.create(path_output, recursive = TRUE, showWarnings = FALSE)
dir.create(path_figures, recursive = TRUE, showWarnings = FALSE)
dir.create(path_tables,  recursive = TRUE, showWarnings = FALSE)


# 1.3 Reproducibility --------------------------------------------------

set.seed(42)  
options(scipen = 999)

# 1.4 Attrition tracking ------------------------------------------------------
# Tibble to record every sample restriction step.

attrition_log <- tibble(
  step        = character(),
  description = character(),
  n_ceasefires = integer(),
  n_dropped   = integer(),
  note        = character()
)

log_attrition <- function(log, step_name, description, current_n, note = "") {
  previous_n <- if (nrow(log) == 0) NA_integer_ else tail(log$n_ceasefires, 1)
  dropped    <- if (is.na(previous_n)) 0L else previous_n - current_n
  
  bind_rows(log, tibble(
    step         = step_name,
    description  = description,
    n_ceasefires = current_n,
    n_dropped    = dropped,
    note         = note
  ))
}







# --- STAGE 2: ETH/PRIO CEASEFIRE DATA ----------------------------------------


# 2.1 Loading the data ------------------------------------------------------------
library(readr)

cf_raw <- read_delim(
  "Desktop/Thesis/data/raw/CFD_oct_2022_id-1.csv",
  delim = ";"
)

cf_raw <- cf_raw |>
  rename(ucdp_acd = ucdp_acd_id)

cat("Rows (directed declarations):", nrow(cf_raw), "\n")
cat("Unique cf_id values:", n_distinct(cf_raw$cf_id), "\n")
cat("Unique uniq_id values:", n_distinct(cf_raw$uniq_id), "\n")


# 2.2 Inspecting key variable ---------------------------------------------------

# Ceasefire effect year distribution
cat("\nCeasefire effect year distribution:\n")
print(table(cf_raw$cf_effect_yr, useNA = "always"))

# The 'ended' variable — how ceasefires ended
cat("\n'ended' variable distribution:\n")
print(table(cf_raw$ended, useNA = "always"))

# The 'link' variable — renewals and extensions
cat("\n'link' variable distribution:\n")
print(table(cf_raw$link, useNA = "always"))

# Ceasefire class
cat("\nCeasefire class distribution:\n")
print(table(cf_raw$ceasefire_class, useNA = "always"))

# ucdp_acd — link to UCDP conflicts (-1 means no link)
cat("\nucdp_acd: proportion with no UCDP link (ucdp_acd == -1):\n")
cat(sum(cf_raw$ucdp_acd == -1, na.rm = TRUE), "out of", nrow(cf_raw), "\n")


# 2.3 Collapsing to ceasefire level for counting ---------------------------------

ended_varies <- cf_raw |>
  group_by(uniq_id) |>
  summarise(n_unique_ended = n_distinct(ended, na.rm = FALSE)) |>
  filter(n_unique_ended > 1)

link_varies <- cf_raw |>
  group_by(uniq_id) |>
  summarise(n_unique_link = n_distinct(link, na.rm = FALSE)) |>
  filter(n_unique_link > 1)

cat("Ceasefires where 'ended' varies across actors:", nrow(ended_varies), "\n")
cat("Ceasefires where 'link' varies across actors:", nrow(link_varies), "\n")


# 2.4 Constructing ceasefire effect date ------------------------------------------

cf_raw <- cf_raw |>
  mutate(
    
    date_problem = case_when(
      cf_effect_yr == 0  ~ "never_entered_effect",
      cf_effect_yr == -1 ~ "year_unclear",
      cf_effect_month == 0  ~ "never_entered_effect",
      cf_effect_month == -1 ~ "month_unclear",
      cf_effect_day == 0  ~ "never_entered_effect",
      cf_effect_day == -1 ~ "day_unclear",
      TRUE ~ "ok"
    ),
   
    cf_effect_date = if_else(
      date_problem == "ok",
      make_date(cf_effect_yr, cf_effect_month, cf_effect_day),
      NA_Date_
    )
  )


print(table(cf_raw$date_problem, useNA = "always"))


# 2.5 Constructing end date and duration for 14-day filter ------------------------

cf_raw <- cf_raw |>
  mutate(
    end_date_problem = case_when(
      ended == 3 ~ "never_started",      
      end_yr == 0  ~ "ongoing_or_never", 
      end_yr == -1 ~ "end_unclear",
      end_month == -1 ~ "end_month_unclear",
      end_day == -1 ~ "end_day_unclear",
      TRUE ~ "ok"
    ),
    cf_end_date = if_else(
      end_date_problem == "ok",
      make_date(end_yr, end_month, end_day),
      NA_Date_
    ),
    
    cf_duration_days = as.integer(cf_end_date - cf_effect_date)
  )


print(table(cf_raw$end_date_problem, useNA = "always"))


# 2.6 Applying filters ---------------------------------------------------------------

cf_ceasefire_level <- cf_raw |>
  group_by(uniq_id) |>
  summarise(
    
    cf_id          = first(cf_id),
    cc             = first(cc),
    location       = first(location),
    cf_effect_yr   = first(cf_effect_yr),
    cf_effect_date = first(cf_effect_date),
    date_problem   = first(date_problem),
    ceasefire_class = first(ceasefire_class),
    p_peaceprocess = first(p_peaceprocess),
    p_humanitarian = first(p_humanitarian),
    p_holiday      = first(p_holiday),
    p_election     = first(p_election),
    side           = first(side),
    partial        = first(partial),
    implement      = first(implement),
    enforcement    = first(enforcement),
    
   
    any_never_started = any(ended == 3, na.rm = TRUE),
    any_failed        = any(ended == 2, na.rm = TRUE),
   
    min_duration_days = min(cf_duration_days, na.rm = TRUE),
    
    all_end_unknown   = all(is.na(cf_duration_days)),
    
    any_renewal       = any(link %in% c(1, 3), na.rm = TRUE),
    
    ucdp_acd_values   = list(unique(ucdp_acd[ucdp_acd != -1])),
    n_ucdp_conflicts  = n_distinct(ucdp_acd[ucdp_acd != -1]),
    has_ucdp_link     = any(ucdp_acd != -1, na.rm = TRUE),
    
    n_declarations     = n(),
    
    .groups = "drop"
  ) |>
  mutate(
    min_duration_days = if_else(is.infinite(min_duration_days), 
                                NA_integer_, 
                                as.integer(min_duration_days))
  )


attrition_log <- log_attrition(
  attrition_log,
  "0", "All ETH/PRIO ceasefires (unique uniq_id)",
  nrow(cf_ceasefire_level)
)


# --- FILTER 1: cf_effect_yr >= 2012) --------------------------
# Rationale: Black Marble VNP46A2 available from 2012-01-19.

cf_filtered <- cf_ceasefire_level |>
  filter(
    date_problem == "ok",        # Valid ceasefire effect date
    cf_effect_yr >= 2012         # Black Marble availability
  )

attrition_log <- log_attrition(
  attrition_log,
  "1", "Post-2012 with valid effect date",
  nrow(cf_filtered),
  "Black Marble available from 2012-01-19"
)

cat("\nFilter 1 applied: post-2012, valid dates\n")
cat("Remaining:", nrow(cf_filtered), "\n")


# --- FILTER 2: UCDP conflict link ---------------------------------------------
# Rationale: Only state-based intrastate conflicts (type_of_violence == 1).


cf_filtered <- cf_filtered |>
  filter(has_ucdp_link == TRUE)

attrition_log <- log_attrition(
  attrition_log,
  "2a", "Has UCDP conflict link (ucdp_acd != -1)",
  nrow(cf_filtered),
  "Full type_of_violence filter applied in Stage 4 after GED merge"
)

cat("\nFilter 2a applied: has UCDP link\n")
cat("Remaining:", nrow(cf_filtered), "\n")

# --- FILTER 3: Exclude never-started (ended == 3) -----------------------------
# Rationale: The ceasefire effect date is coded even for ceasefires that never started. 
# Including them means measuring NTL around a non-event.

cf_filtered <- cf_filtered |>
  filter(any_never_started == FALSE)

attrition_log <- log_attrition(
  attrition_log,
  "3", "Exclude never-started (ended == 3)",
  nrow(cf_filtered)
)

cat("\nFilter 3 applied: exclude never-started\n")
cat("Remaining:", nrow(cf_filtered), "\n")

# --- FILTER 4: 14-day holding filter ------------------------------------------
# Rationale: A ceasefire collapsing within 14 days is a negotiation failure,
# not a signal economic actors can observe and respond to.


cf_filtered <- cf_filtered |>
  filter(
    all_end_unknown == TRUE | 
      is.na(min_duration_days) | 
      min_duration_days >= 14
  )

attrition_log <- log_attrition(
  attrition_log,
  "4", "14-day minimum holding filter",
  nrow(cf_filtered),
  "Unknown end dates treated as holding (conservative)"
)

cat("\nFilter 4 applied: 14-day holding filter\n")
cat("Remaining:", nrow(cf_filtered), "\n")

# --- FILTER 5: Exclude renewals (link in {1, 3}) -----------------------------
# Rationale: Renewals and reaffirmations create dependent observations — the
# NTL response to a renewed ceasefire is not independent of the previous one.
# Upgrades (link == 2) are retained as new credibility signals.

# Excluding the ceasefire entirely if any actor-row is a renewal.

cf_filtered <- cf_filtered |>
  filter(any_renewal == FALSE)

attrition_log <- log_attrition(
  attrition_log,
  "5", "Exclude renewals and reaffirmations (link in {1,3})",
  nrow(cf_filtered),
  "Upgrades (link=2) retained"
)

cat("\nFilter 5 applied: exclude renewals\n")
cat("Remaining:", nrow(cf_filtered), "\n")


# --- FILTER 6: Complete ±180 day NTL window -----------------------------------
# Rationale: Both pre and post windows must fall within Black Marble availability.


ntl_start_date <- as.Date("2012-01-19")
ntl_end_date   <- as.Date("2021-01-01")  

cf_filtered <- cf_filtered |>
  mutate(
    window_start = cf_effect_date - days(180),
    window_end   = cf_effect_date + days(180)
  ) |>
  filter(
    window_start >= ntl_start_date,
    window_end   <= ntl_end_date
  )

attrition_log <- log_attrition(
  attrition_log,
  "6", "Complete ±180 day NTL window within Black Marble availability",
  nrow(cf_filtered),
  paste("NTL range:", ntl_start_date, "to", ntl_end_date)
)

cat("\nFilter 6 applied: complete ±180 day window\n")
cat("Remaining:", nrow(cf_filtered), "\n")

# --- FILTER 7: V-Dem coverage -------------------------------------------------
# Requires the UCDP GED merge. Run in Stage 4.


cat("\nFilter 7 (V-Dem coverage) Ran in Stage 4 (requires UCDP onset year)\n")



# 2.7 Post-Filter Diagnosis ------------------------------------------------------
cat("\n\n========================================\n")
cat("ATTRITION TABLE — STAGE 2\n")
cat("========================================\n\n")
print(attrition_log)


cat("\n--- Descriptive statistics of surviving ceasefires ---\n")

# Year distribution
cat("\nCeasefire effect year (surviving sample):\n")
print(table(cf_filtered$cf_effect_yr))

# Ceasefire class
cat("\nCeasefire class:\n")
cat("1 = CoH, 2 = CoHm, 3 = Definitive\n")
print(table(cf_filtered$ceasefire_class, useNA = "always"))

# Peace process purpose
cat("\nPeace process purpose:\n")
print(table(cf_filtered$p_peaceprocess, useNA = "always"))

# Coverage (partial vs whole state)
cat("\nGeographic coverage (0=whole, 1=partial):\n")
print(table(cf_filtered$partial, useNA = "always"))

# Regional distribution
cat("\nLocation (country):\n")
print(sort(table(cf_filtered$location), decreasing = TRUE))

# Number of UCDP conflicts per ceasefire
cat("\nNumber of linked UCDP conflicts per ceasefire:\n")
print(table(cf_filtered$n_ucdp_conflicts))



# 2.9 Saving output-------------------------------------------------


saveRDS(cf_filtered, file.path(path_output, "cf_filtered_stage2.rds"))


cf_actors_surviving <- cf_raw |>
  filter(uniq_id %in% cf_filtered$uniq_id) |>
  select(uniq_id, cf_id, ucdp_actor_id, actor_name, ucdp_acd, ucdp_dyad)

saveRDS(cf_actors_surviving, file.path(path_output, "cf_actors_stage2.rds"))










# --- STAGE 3: UCDP GED LOADING AND PREPARATION -------------------------------------
# --- 3.0 Loading Data 

cf_filtered      <- readRDS(file.path(path_output, "cf_filtered_stage2.rds"))
cf_actors_surviving <- readRDS(file.path(path_output, "cf_actors_stage2.rds"))

# --- 3.1 Extracting all unique conflict IDs in the sample
all_conflict_ids <- cf_filtered$ucdp_acd_values |>
  unlist() |>
  unique()

cat("\nUnique conflict IDs in ceasefire sample:", length(all_conflict_ids), "\n")
cat("Range:", min(all_conflict_ids), "to", max(all_conflict_ids), "\n")


# --- 3.2 Loading UCDP GED data --------------------------------------------------

ged_raw <- readRDS(path_ged)


# --- 3.3 conflict_new_id to ucdp_acd linkage -----------------------

ids_in_ged <- all_conflict_ids[all_conflict_ids %in% ged_raw$conflict_new_id]
ids_missing_from_ged <- all_conflict_ids[!all_conflict_ids %in% ged_raw$conflict_new_id]

# --- 3.6 Applying filters to GED -------------------------------------------------
# Filter sequence:
# 1. Restricting to conflicts in sample 
# 2. Filtering to state-based violence (type_of_violence == 1)
# 3. Applying geo-precision filter (where_prec <= 4)


ged_filtered <- ged_raw |>
  filter(conflict_new_id %in% all_conflict_ids)

ged_filtered <- ged_filtered |>
  filter(type_of_violence == 1)

ged_filtered <- ged_filtered |>
  filter(where_prec <= 4)

# --- 3.7 Selecting relevant columns ----------------------------------

ged_clean <- ged_filtered |>
  select(
    # Identifiers
    id,                 # Event ID 
    conflict_new_id,    # Links to ucdp_acd in ETH/PRIO — primary merge key
    dyad_new_id,        # Links to ucdp_dyad — secondary merge key
    side_a_new_id,      # Actor A ID 
    side_b_new_id,      # Actor B ID 
    side_a,             # Actor A name 
    side_b,             # Actor B name 
    
    # Geography
    priogrid_gid,       # PRIO-GRID cell
    country,            # Country name
    country_id,         # Gleditsch-Ward country code — for country FE
    where_prec,         
    
    # Time
    year,               # Year of event — for year FE and aggregation
    date_start,         # Start date — for salient/other classification
    date_end,           # End date — for events spanning multiple days
    
    # Fatalities — for battle deaths control variable
    best,               # Best estimate of total deaths
    deaths_a,           # Deaths on side A
    deaths_b,           # Deaths on side B
    
    # Conflict type (already filtered to 1)
    type_of_violence
  )

cat("\n--- GED cleaned dataset ---\n")
cat("Rows:", nrow(ged_clean), "\n")
cat("Unique conflicts:", n_distinct(ged_clean$conflict_new_id), "\n")
cat("Unique cells (priogrid_gid):", n_distinct(ged_clean$priogrid_gid), "\n")
cat("Unique country IDs:", n_distinct(ged_clean$country_id), "\n")
cat("Year range:", min(ged_clean$year), "to", max(ged_clean$year), "\n")


# --- 3.8 Parsing ----------------------------------------------------------

cat("\nClass of date_start:", class(ged_clean$date_start), "\n")


if (!inherits(ged_clean$date_start, "Date")) {
  ged_clean <- ged_clean |>
    mutate(date_start = as.Date(date_start))
  cat("date_start converted to Date class.\n")
} else {
  cat("date_start is already Date class\n")
}


# --- 3.9 Diagnostics -----------------------------------------------------------

# Distribution of geo-precision in filtered data
cat("\nwhere_prec distribution (filtered):\n")
print(table(ged_clean$where_prec, useNA = "always"))

# Fatality distribution
cat("\nBattle death statistics (best estimate):\n")
print(summary(ged_clean$best))

# Checking for any events where priogrid_gid is missing
n_missing_pgid <- sum(is.na(ged_clean$priogrid_gid))
cat("\nEvents with missing priogrid_gid:", n_missing_pgid, "\n")
if (n_missing_pgid > 0) {
  cat("\Events without priogrid_gid cannot be assigned to cells.\n")
  cat("Removing these", n_missing_pgid, "events.\n")
  ged_clean <- ged_clean |> filter(!is.na(priogrid_gid)) 
  }


# Events per conflict
cat("\nEvents per conflict (top 10 conflicts):\n")
print(
  ged_clean |>
    count(conflict_new_id, side_a, side_b) |>
    arrange(desc(n)) |>
    head(10)
)

# Year distribution of events
cat("\nEvents by year:\n")
print(table(ged_clean$year))

# --- 3.10 Freeing memory ---------------------------------------------------------

rm(ged_raw)
gc() 

cat("\nRaw GED removed from memory to free space.\n")

# --- 3.11 Saving filtered GED ---------------------------------------------------

saveRDS(ged_clean, file.path(path_output, "ged_filtered_stage3.rds"))


cf_filtered |>
  filter(uniq_id == 775051) |>
  pull(ucdp_acd_values)


ged_clean |>
  filter(conflict_new_id %in% unlist(
    cf_filtered |> filter(uniq_id == 775051) |> pull(ucdp_acd_values)
  )) |>
  distinct(conflict_new_id, conflict_name = side_a)












# --- STAGE 4: CELL CLASSIFICATION — SALIENT / OTHER / ALL -------------------------
library(data.table)

# --- 4.0 Loading data ------------------------------------

cf_filtered      <- readRDS(file.path(path_output, "cf_filtered_stage2.rds"))
cf_actors_surviving <- readRDS(file.path(path_output, "cf_actors_stage2.rds"))
ged_clean        <- readRDS(file.path(path_output, "ged_filtered_stage3.rds"))

cat("cf_filtered:", nrow(cf_filtered), "ceasefires\n")
cat("ged_clean:", nrow(ged_clean), "events\n")



# --- 4.1 Resolving Myanmar conflict ID 440 issue

myanmar_cf_ids <- cf_filtered |>
  filter(uniq_id == 775051) |>
  pull(ucdp_acd_values) |>
  unlist()

cat("\nMyanmar ceasefire 775051 — conflict IDs:", myanmar_cf_ids, "\n")
cat("Conflict ID 440 not in GED. Cell classification will use remaining ID(s) only.\n")

known_myanmar_id <- myanmar_cf_ids[myanmar_cf_ids != 440]
cat("Known conflict ID (in GED):", known_myanmar_id, "\n")





# --- 4.2 Building the ceasefire-conflict link table ------------------------------

#


cf_conflict_link <- cf_actors_surviving |>
  filter(ucdp_acd != -1) |>                    # Removing rows with no UCDP link
  filter(ucdp_acd != 440) |>                    # Removing missing conflict ID
  select(uniq_id, conflict_new_id = ucdp_acd) |>  # Renaming for consistency with GED
  distinct()                                     

cat("\ncf_conflict_link rows:", nrow(cf_conflict_link), "\n")
cat("Unique ceasefires:", n_distinct(cf_conflict_link$uniq_id), "\n")
cat("Unique conflict IDs:", n_distinct(cf_conflict_link$conflict_new_id), "\n")


missing_from_link <- setdiff(cf_filtered$uniq_id, cf_conflict_link$uniq_id)
if (length(missing_from_link) > 0) {
  cat("WARNING:", length(missing_from_link), 
      "ceasefires have no conflict link after removing ID 440:\n")
  print(missing_from_link)
} else {
  cat("All ceasefires have at least one conflict link.\n")
}

# --- 4.3 Adding ceasefire effect dates to the link table -------------------------

cf_conflict_dates <- cf_conflict_link |>
  left_join(
    cf_filtered |> select(uniq_id, cf_effect_date),
    by = "uniq_id"
  )

cat("\ncf_conflict_dates rows:", nrow(cf_conflict_dates), "\n")



# --- 4.4 Matching ceasefires to their GED events -----------------



cf_dt  <- as.data.table(cf_conflict_dates)
ged_dt <- as.data.table(ged_clean |> select(conflict_new_id, priogrid_gid, date_start))

# Joining on conflict_new_id
joined_dt <- ged_dt[cf_dt, on = "conflict_new_id", allow.cartesian = TRUE]

cat("After join (before date filter):", nrow(joined_dt), "rows\n")

# Filtering to only pre-ceasefire events
joined_dt <- joined_dt[date_start < cf_effect_date]

cat("After pre-ceasefire filter:", nrow(joined_dt), "rows\n")



# --- 4.5 Classifying cells as salient or other ------------------------------------
# For each (ceasefire × cell) combination:
# 1. Was there any event in the 180 days before the ceasefire? → salient
# 2. Were there events only before those 180 days? → other
#
# The 180-day window is: cf_effect_date - 180 days to cf_effect_date (exclusive)


cell_class_dt <- joined_dt[, .(
  has_event_last_180d = any(date_start >= (cf_effect_date[1] - 180L)),
  has_event_before_180d = any(date_start < (cf_effect_date[1] - 180L)),
  n_events_pre = .N,
  last_event_date = max(date_start)
), by = .(uniq_id, priogrid_gid, cf_effect_date)]

cell_class_dt[, cell_type := fcase(
  has_event_last_180d == TRUE,   "salient",   # Any event in last 6 months → salient
  has_event_last_180d == FALSE,  "other"      # Events exist but not in last 6 months → other
)]

cell_classification <- as_tibble(cell_class_dt)

cat("\n--- Cell classification results ---\n")
cat("Rows (ceasefire × cell):", nrow(cell_classification), "\n")
cat("Unique ceasefires:", n_distinct(cell_classification$uniq_id), "\n")
cat("Unique cells:", n_distinct(cell_classification$priogrid_gid), "\n")

# Classification distribution
cat("\nCell type distribution:\n")
print(table(cell_classification$cell_type))

# Average cells per ceasefire
avg_cells <- cell_classification |>
  count(uniq_id) |>
  summarise(
    mean_cells = mean(n),
    median_cells = median(n),
    min_cells = min(n),
    max_cells = max(n)
  )
cat("\nCells per ceasefire:\n")
print(avg_cells)

# --- 4.6 Ceasefires with zero cells -------------------------------------------

ceasefires_with_cells <- unique(cell_classification$uniq_id)
ceasefires_no_cells   <- setdiff(cf_filtered$uniq_id, ceasefires_with_cells)

cat("\nCeasefires with at least one cell:", length(ceasefires_with_cells), "\n")
cat("Ceasefires with NO cells (cannot be analysed):", length(ceasefires_no_cells), "\n")

if (length(ceasefires_no_cells) > 0) {
  cat("\nCeasefires with no cells:\n")
  cf_filtered |>
    filter(uniq_id %in% ceasefires_no_cells) |>
    select(uniq_id, location, cf_effect_yr) |>
    print()
  
  cat("\nThese ceasefires will be excluded from the analytical sample.\n")
  cat("Adding to attrition log in §3.3.\n")
}

# Updating attrition log
attrition_log <- log_attrition(
  attrition_log,
  "7a", "Ceasefires with at least one active PRIO-GRID cell in GED",
  length(ceasefires_with_cells),
  paste("Ceasefires with no cells:", length(ceasefires_no_cells))
)

# --- 4.7 Ceasefires with zero "other" cells -----------------------------------

ceasefires_with_other_cells <- cell_classification |>
  filter(cell_type == "other") |>
  distinct(uniq_id) |>
  pull(uniq_id)

ceasefires_no_other_cells <- setdiff(ceasefires_with_cells, ceasefires_with_other_cells)

cat("\nCeasefires with at least one 'other' cell:", length(ceasefires_with_other_cells), "\n")
cat("Ceasefires with NO 'other' cells:", length(ceasefires_no_other_cells), "\n")

if (length(ceasefires_no_other_cells) > 0) {
  cat("\nCeasefires with no 'other' cells (salient-only):\n")
  cf_filtered |>
    filter(uniq_id %in% ceasefires_no_other_cells) |>
    select(uniq_id, location, cf_effect_yr) |>
    print()
  cat("\nThese ceasefires contribute to salient and all-cells analyses\n")
  cat("but are absent from the primary 'other' cells analysis.\n")
}

attrition_log <- log_attrition(
  attrition_log,
  "7b", "Ceasefires with at least one 'other' cell (primary analysis population)",
  length(ceasefires_with_other_cells),
  "Salient-only ceasefires retained for comparison analyses"
)


# --- 4.8 Detailed diagnostics by sub-sample -----------------------------------

cat("\n--- Cells per ceasefire by sub-sample ---\n")

cell_summary_by_type <- cell_classification |>
  group_by(uniq_id, cell_type) |>
  summarise(n_cells = n(), .groups = "drop") |>
  group_by(cell_type) |>
  summarise(
    n_ceasefires = n(),
    mean_cells   = round(mean(n_cells), 1),
    median_cells = median(n_cells),
    min_cells    = min(n_cells),
    max_cells    = max(n_cells),
    .groups = "drop"
  )

print(cell_summary_by_type)

# Distribution of other-cell counts per ceasefire
cat("\n--- Distribution of 'other' cells per ceasefire ---\n")
other_cells_per_cf <- cell_classification |>
  filter(cell_type == "other") |>
  count(uniq_id) |>
  pull(n)

print(summary(other_cells_per_cf))
cat("Ceasefires with only 1 'other' cell:", sum(other_cells_per_cf == 1), "\n")
cat("Ceasefires with 10+ 'other' cells:", sum(other_cells_per_cf >= 10), "\n")

# --- 4.9 Adding country_id to cell classification (for country FE) -------------
cf_country <- ged_clean |>
  filter(conflict_new_id %in% cf_conflict_link$conflict_new_id) |>
  inner_join(cf_conflict_link, by = "conflict_new_id") |>
  count(uniq_id, country_id, country) |>
  group_by(uniq_id) |>
  slice_max(n, n = 1, with_ties = FALSE) |>  
  ungroup() |>
  select(uniq_id, country_id, country)


cat("\n--- Country assignment ---\n")
cat("Ceasefires with country assigned:", nrow(cf_country), "\n")
cat("Ceasefires missing country:", 
    length(setdiff(cf_filtered$uniq_id, cf_country$uniq_id)), "\n")


# Checking for cross-border conflicts
multi_country <- ged_clean |>
  inner_join(cf_conflict_link, by = "conflict_new_id") |>
  group_by(uniq_id) |>
  summarise(n_countries = n_distinct(country_id), .groups = "drop") |>
  filter(n_countries > 1)

cat("Ceasefires with events in multiple countries:", nrow(multi_country), "\n")
if (nrow(multi_country) > 0) {
  cat("These receive the modal country for the country FE.\n")
 
}

# Merging country info into cell classification
cell_classification <- cell_classification |>
  left_join(cf_country, by = "uniq_id")


# --- 4.10 Final attrition log update ------------------------------------------
print(attrition_log)


# --- 4.11 Memory cleanup and save ---------------------------------------------


rm(joined_dt, cell_class_dt, cf_dt, ged_dt)
gc()

saveRDS(cell_classification, file.path(path_output, "cell_classification_stage4.rds"))

saveRDS(cf_conflict_link, file.path(path_output, "cf_conflict_link_stage4.rds"))


cell_classification |>
  filter(cell_type == "other") |>
  count(uniq_id) |>
  filter(n == 1) |>
  left_join(cf_filtered |> select(uniq_id, location, cf_effect_yr), by = "uniq_id") |>
  print()










# --- STAGE 5: PANEL SKELETON CONSTRUCTION-------------------------------------
library(tidyverse)
library(lubridate)

# --- 5.1 Loading Stage 2 and Stage 4 outputs ------------------------------------

cf_filtered        <- readRDS(file.path(path_output, "cf_filtered_stage2.rds"))
cell_classification <- readRDS(file.path(path_output, "cell_classification_stage4.rds"))

cat("cf_filtered:", nrow(cf_filtered), "ceasefires\n")
cat("cell_classification:", nrow(cell_classification), "ceasefire × cell pairs\n")

# --- 5.2 Fixing country FE:

cf_country_fe <- cf_filtered |>
  select(uniq_id, country_code_fe = cc, location)

cat("\nCountry FE source: ETH/PRIO cc variable\n")
cat("Unique countries in sample:", n_distinct(cf_country_fe$country_code_fe), "\n")


# --- 5.3 Preparing the cell classification base --------------------------------
cell_base <- cell_classification |>
  select(uniq_id, priogrid_gid, cell_type, cf_effect_date) |>
  left_join(cf_country_fe, by = "uniq_id")

# --- 5.4 Define the period structure -----------------------------------------
# PERIOD DEFINITION (following Armand et al. 2023):
# For t > 0 (post-ceasefire):
#   period t covers days (t-1)*10 + 1 to t*10 after the ceasefire date
#   t = 1: days 1–10,  t = 2: days 11–20, ..., t = 18: days 171–180
#
# For t < 0 (pre-ceasefire):
#   period t covers days t*10 to t*10 + 9 before/around the ceasefire date
#   t = -1: days -10 to -1, t = -2: days -20 to -11, ..., t = -18: days -180 to -171
#
# t = 0 is EXCLUDED (the ceasefire day itself)
periods <- tibble(period = c(-18:-1, 1:18))  # 36 periods

cat("\nPeriod structure:\n")
cat("Total periods:", nrow(periods), "\n")
cat("Range:", min(periods$period), "to", max(periods$period), "(t=0 excluded)\n")


# --- 5.5 Build the panel skeleton --------------------------------------------

panel_skeleton <- cell_base |>
  crossing(periods) |>   
  mutate(
    
    
    post = as.integer(period > 0),
    
    
    period_start_date = if_else(
      period < 0,
      cf_effect_date + days(period * 10),          # e.g., t=-1: -10 days
      cf_effect_date + days((period - 1) * 10 + 1)  # e.g., t=+1: +1 day
    ),
    period_end_date = if_else(
      period < 0,
      cf_effect_date + days(period * 10 + 9),       # e.g., t=-1: -1 day
      cf_effect_date + days(period * 10)             # e.g., t=+1: +10 days
    ),
    
    
    cf_year = year(cf_effect_date)
  )

cat("Panel skeleton rows:", nrow(panel_skeleton), "\n")

# --- 5.6 Sanity checks -------------------------------------------


cat("\n--- Period date verification ---\n")
example_check <- panel_skeleton |>
  filter(uniq_id == first(uniq_id), priogrid_gid == first(priogrid_gid)) |>
  filter(period %in% c(-18, -1, 1, 18)) |>
  select(uniq_id, cf_effect_date, period, period_start_date, period_end_date, post)

print(example_check)

bad_dates <- panel_skeleton |>
  filter(period_start_date > period_end_date) |>
  nrow()

if (bad_dates > 0) {
  cat("ERROR:", bad_dates, "rows where period_start > period_end!\n")
  stop("Date arithmetic error.")
} else {
  cat("All period dates are correctly ordered (start ≤ end).\n")
}

bad_post <- panel_skeleton |>
  filter(post == 1, period_start_date < cf_effect_date) |>
  nrow()

if (bad_post > 0) {
  cat("ERROR:", bad_post, "post-period rows with dates before ceasefire!\n")
  stop("Date logic error.")
} else {
  cat("All post-period dates are after ceasefire date. OK.\n")
}

bad_pre <- panel_skeleton |>
  filter(post == 0, period_end_date >= cf_effect_date) |>
  nrow()

if (bad_pre > 0) {
  cat("ERROR:", bad_pre, "pre-period rows with end dates on/after ceasefire!\n")
  stop("Date logic error.")
} else {
  cat("All pre-period end dates are before ceasefire date.\n")
} 

# --- 5.7 Panel structure diagnostics ------------------------------------------

cat("\n--- Panel structure ---\n")
cat("Total rows:", nrow(panel_skeleton), "\n")
cat("Unique ceasefires:", n_distinct(panel_skeleton$uniq_id), "\n")
cat("Unique cells:", n_distinct(panel_skeleton$priogrid_gid), "\n")
cat("Unique country codes (FE):", n_distinct(panel_skeleton$country_code_fe), "\n")
cat("Unique ceasefire years:", n_distinct(panel_skeleton$cf_year), "\n")

cat("\nRows by cell type:\n")
print(panel_skeleton |> count(cell_type))

cat("\nRows by post indicator:\n")
print(panel_skeleton |> count(post))

cat("\nCeasefire year distribution:\n")
print(table(panel_skeleton |> distinct(uniq_id, cf_year) |> pull(cf_year)))


# --- 5.8 Checking date range of all period windows ------------------------------

cat("\n--- NTL date range required ---\n")
cat("Earliest period_start_date:", as.character(min(panel_skeleton$period_start_date)), "\n")
cat("Latest period_end_date:", as.character(max(panel_skeleton$period_end_date)), "\n")
cat("NTL data available from: 2012-01-19\n")

before_ntl <- panel_skeleton |>
  filter(period_start_date < as.Date("2012-01-19")) |>
  nrow()

if (before_ntl > 0) {
  cat("WARNING:", before_ntl, 
      "period rows fall before 2012-01-19 NTL start.\n")
  cat("These rows will have missing NTL values in Stage 6.\n")
  cat("They should have been caught by Filter 6 in Stage 2.\n")
} else {
  cat("All period windows fall within NTL availability.\n")
}


# --- 5.9 Adding identifier columns needed for FE estimation ---------------------

# - country_fe: factor of ETH/PRIO cc values
# - year_fe: factor of ceasefire effect year

panel_skeleton <- panel_skeleton |>
  mutate(
    country_fe = factor(country_code_fe),
    year_fe    = factor(cf_year),
  )

cat("\nFixed effect variables created:\n")
cat("country_fe levels:", nlevels(panel_skeleton$country_fe), "\n")
cat("year_fe levels:", nlevels(panel_skeleton$year_fe), "\n")

# --- 5.10 Saving ----------------------------------------------------------------

saveRDS(panel_skeleton, file.path(path_output, "panel_skeleton_stage5.rds"))












# --- STAGE 6: NTL DATA LOADING, QUALITY MASKING, AND PANEL MERGE--------------------

library(data.table)
library(tidyverse)
library(lubridate)

# --- 6.1 Loading Stage 5 output -------------------------------------------------

panel_skeleton <- readRDS(file.path(path_output, "panel_skeleton_stage5.rds"))
cat("Panel skeleton loaded:", nrow(panel_skeleton), "rows\n")

# --- 6.2 Loading and pre-filtering NTL data ----------------------------------------
ntl_raw <- fread(
  path_ntl,
  select = c("priogrid_gid", "date", 
             "DNB_BRDF_Corrected_NTL", 
             "Mandatory_Quality_Flag", 
             "Snow_Flag")
)

cat("NTL loaded. Rows:", nrow(ntl_raw), "\n")
cat("Columns:", names(ntl_raw), "\n")


# --- 6.3 Filtering to relevant cells and date range ----------------------------


cells_ordine     <- unique(panel_skeleton$priogrid_gid)
ntl_start     <- as.IDate("2012-02-09")   # = min(panel_skeleton$period_start_date)
ntl_end       <- as.IDate("2021-06-21")   # = max(panel_skeleton$period_end_date)

cat("\nFiltering NTL to relevant cells and dates...\n")
cat("Relevant cells:", length(cells_ordine), "\n")
cat("Date range:", as.character(ntl_start), "to", as.character(ntl_end), "\n")

ntl_filtered <- ntl_raw[
  priogrid_gid %in% cells_ordine &
    date >= ntl_start &
    date <= ntl_end
]

ntl_filtered <- ntl_raw[
  priogrid_gid %in% cells_ordine &
    date >= ntl_start &
    date <= ntl_end
]

cat("After cell and date filter:", nrow(ntl_filtered), "rows\n")
cat("Unique cells after filter:", uniqueN(ntl_filtered$priogrid_gid), "\n")


cells_in_ntl     <- unique(ntl_filtered$priogrid_gid)
cells_missing_ntl <- setdiff(cells_ordine, cells_in_ntl)

cat("\nCells in panel:", length(cells_ordine), "\n")
cat("Cells with NTL data:", length(cells_in_ntl), "\n")
cat("Panel cells with NO NTL data:", length(cells_missing_ntl), "\n")

if (length(cells_missing_ntl) > 0) {
  cat("\nWARNING: The following cells have no NTL data:\n")
  print(cells_missing_ntl)
  cat("\nThese cells will have NA NTL for all periods.\n")
  cat("If many cells are missing, investigate the GEE extraction coverage.\n")
  cat("These cells' ceasefire-period rows will be dropped when the model\n")
  cat("is estimated on non-missing outcomes.\n")


# Freeing raw NTL from memory
rm(ntl_raw)
gc()


# --- 6.4 Inspect NTL value distributions BEFORE masking ---------------------

cat("\n--- NTL distribution BEFORE quality masking ---\n")

cat("\nDNB_BRDF_Corrected_NTL summary:\n")
print(summary(ntl_filtered$DNB_BRDF_Corrected_NTL))

cat("\nMandatory_Quality_Flag summary:\n")
print(summary(ntl_filtered$Mandatory_Quality_Flag))
cat("Proportion > 1 (will be masked):", 
    mean(ntl_filtered$Mandatory_Quality_Flag > 1, na.rm = TRUE), "\n")
cat("Proportion > 0.5:", 
    mean(ntl_filtered$Mandatory_Quality_Flag > 0.5, na.rm = TRUE), "\n")

cat("\nSnow_Flag summary:\n")
print(summary(ntl_filtered$Snow_Flag))
cat("Proportion > 0.5 (majority snow):", 
    mean(ntl_filtered$Snow_Flag > 0.5, na.rm = TRUE), "\n")

cat("\nProportion of zero NTL values:", 
    mean(ntl_filtered$DNB_BRDF_Corrected_NTL == 0, na.rm = TRUE), "\n")


# --- 6.5 Applying quality masking -----------------------------------------------
# MASKING RULE:
# If Mandatory_Quality_Flag > 1, set DNB_BRDF_Corrected_NTL to NA.




ntl_filtered[
  is.na(Mandatory_Quality_Flag) | Mandatory_Quality_Flag > 1,
  DNB_BRDF_Corrected_NTL := NA_real_
]

cat("Rows masked (NTL set to NA):", 
    sum(is.na(ntl_filtered$DNB_BRDF_Corrected_NTL)), "\n")
cat("Rows with valid NTL:", 
    sum(!is.na(ntl_filtered$DNB_BRDF_Corrected_NTL)), "\n")
cat("Masking rate:", 
    round(mean(is.na(ntl_filtered$DNB_BRDF_Corrected_NTL)), 4), "\n")




# --- 6.6 Range join: matching daily NTL to panel periods -----------------------


panel_dt <- as.data.table(panel_skeleton)[, .(
  uniq_id,
  priogrid_gid,
  period,
  post,
  cf_effect_date,
  cf_year,
  country_code_fe,
  cell_type,
  cell_cf_id,
  period_start_date,
  period_end_date
)]

panel_dt[, period_start_date := as.IDate(period_start_date)]
panel_dt[, period_end_date   := as.IDate(period_end_date)]
panel_dt[, cf_effect_date    := as.IDate(cf_effect_date)]

ntl_filtered[, date_end := date]

setkey(panel_dt,    priogrid_gid, period_start_date, period_end_date)
setkey(ntl_filtered, priogrid_gid, date, date_end)





merged_dt <- foverlaps(
  ntl_filtered,
  panel_dt,
  by.x = c("priogrid_gid", "date", "date_end"),
  by.y = c("priogrid_gid", "period_start_date", "period_end_date"),
  type    = "within",
  nomatch = NA
)

cat("Merge complete. Rows after join:", nrow(merged_dt), "\n")

# --- 6.7 Aggregating daily NTL to ten-day period means -------------------------

panel_ntl <- merged_dt[, .(
  ntl_mean_raw   = mean(DNB_BRDF_Corrected_NTL, na.rm = TRUE),  # Mean of valid days
  n_days_total   = sum(!is.na(date)),                             # Days with any NTL data
  n_days_valid   = sum(!is.na(DNB_BRDF_Corrected_NTL)),          # Valid (non-masked) days
  snow_mean      = mean(Snow_Flag, na.rm = TRUE)                  # Mean snow fraction 
), by = .(uniq_id, priogrid_gid, period, post, cf_effect_date,
          cf_year, country_code_fe, cell_type, cell_cf_id,
          period_start_date, period_end_date)]

cat("Rows after aggregation:", nrow(panel_ntl), "\n")

# If fewer than 3 valid days in the 10-day period, NTL is too uncertain → NA
MIN_VALID_DAYS <- 3

panel_ntl[n_days_valid < MIN_VALID_DAYS, ntl_mean_raw := NA_real_]

cat("Periods masked for < 3 valid days:", 
    sum(is.na(panel_ntl$ntl_mean_raw) & panel_ntl$n_days_valid < MIN_VALID_DAYS,
        na.rm = TRUE), "\n")

# --- 6.8 Applying ln(NTL + 1) transformation -----------------------------------

panel_ntl[, ln_ntl := log(ntl_mean_raw + 1)]

cat("\n--- NTL outcome variable distribution ---\n")
cat("ln_ntl summary:\n")
print(summary(panel_ntl$ln_ntl))

cat("\nProportion of NA ln_ntl:", mean(is.na(panel_ntl$ln_ntl)), "\n")
cat("Proportion of zero ln_ntl (zero raw NTL):", 
    mean(panel_ntl$ln_ntl == 0, na.rm = TRUE), "\n")



# --- 6.9 Save ---------------------------------------------------


rm(ntl_filtered, merged_dt)
gc()

panel_with_ntl <- as_tibble(panel_ntl)

cat("\nFinal panel columns:\n")
print(names(panel_with_ntl))

saveRDS(panel_with_ntl, file.path(path_output, "panel_with_ntl_stage6.rds"))


# --- 6.10 Cleanup ---------------------------------------------------
panel_with_ntl  <- readRDS(file.path(path_output, "panel_with_ntl_stage6.rds"))
panel_skeleton  <- readRDS(file.path(path_output, "panel_skeleton_stage5.rds"))

cat("Before fix:", nrow(panel_with_ntl), "rows\n")

panel_ntl_clean <- panel_with_ntl |>
  filter(!is.na(cell_type))

cat("After removing NA cell_type rows:", nrow(panel_ntl_clean), "\n")


panel_final <- panel_skeleton |>
  select(uniq_id, priogrid_gid, period, post, cf_effect_date, cf_year,
         country_code_fe, cell_type, cell_cf_id, 
         period_start_date, period_end_date) |>
  left_join(
    panel_ntl_clean |>
      select(uniq_id, priogrid_gid, period,
             ntl_mean_raw, n_days_total, n_days_valid, snow_mean, ln_ntl),
    by = c("uniq_id", "priogrid_gid", "period")
  )

cat("After left join with skeleton:", nrow(panel_final), "\n")

cat("\n--- Final panel missingness after fix ---\n")
cat("Total rows:", nrow(panel_final), "\n")
cat("ln_ntl NA rate:", round(mean(is.na(panel_final$ln_ntl)), 4), "\n")

panel_final |>
  group_by(cell_type) |>
  summarise(
    n_rows      = n(),
    pct_na_ntl  = round(100 * mean(is.na(ln_ntl)), 2),
    mean_valid_days = round(mean(n_days_valid, na.rm = TRUE), 1),
    .groups = "drop"
  ) |>
  print()

# Overwriting with corrected panel
saveRDS(panel_final, file.path(path_output, "panel_with_ntl_stage6.rds"))
saveRDS(attrition_log, file.path(path_output, "attrition_log.rds"))
write_csv(attrition_log, file.path(path_output, "attrition_log.csv"))


saveRDS(attrition_log, file.path(path_output, "attrition_log.rds"))
write_csv(attrition_log, file.path(path_output, "attrition_log.csv"))
cat("Attrition log saved.\n")
print(attrition_log)


#re-start
library(tidyverse)
library(lubridate)
library(data.table)
library(fixest)
library(countrycode)
library(modelsummary)


log_attrition <- function(log, step_name, description, current_n, note = "") {
  previous_n <- if (nrow(log) == 0) NA_integer_ else tail(log$n_ceasefires, 1)
  dropped    <- if (is.na(previous_n)) 0L else previous_n - current_n
  bind_rows(log, tibble(
    step = step_name, description = description,
    n_ceasefires = current_n, n_dropped = dropped, note = note
  ))
}


panel_final      <- readRDS(file.path(path_output, "panel_with_ntl_stage6.rds"))
cf_filtered      <- readRDS(file.path(path_output, "cf_filtered_stage2.rds"))
cf_conflict_link <- readRDS(file.path(path_output, "cf_conflict_link_stage4.rds"))
ged_clean        <- readRDS(file.path(path_output, "ged_filtered_stage3.rds"))
attrition_log    <- readRDS(file.path(path_output, "attrition_log.rds"))

cat("panel_final:", nrow(panel_final), "rows\n")
cat("cf_filtered:", nrow(cf_filtered), "ceasefires\n")
cat("cf_conflict_link:", nrow(cf_conflict_link), "rows\n")
cat("ged_clean:", nrow(ged_clean), "events\n")
cat("attrition_log steps:", nrow(attrition_log), "\n")
cat("\nAttrition log:\n")
print(attrition_log)


vdem_raw <- readRDS("Desktop/Thesis/data/raw/V-Dem-CY-Full+Others-v16.rds")  
path_vdem <- "Desktop/Thesis/data/raw/V-Dem-CY-Full+Others-v16.rds"
path_wdi  <- "Desktop/Thesis/data/raw/0b8580e1-b6f8-44b3-b1da-ebc7fb81a0d2_Data.csv"






# --- STAGE 7: V-DEM, CONTROL VARIABLES, AND FINAL PANEL-------------------

episode_check <- ged_clean |>
  group_by(conflict_new_id) |>
  summarise(
    first_year = min(year),
    last_year  = max(year),
    n_years_with_events = n_distinct(year),
    total_span = max(year) - min(year) + 1,
    .groups = "drop"
  ) |>
  mutate(
    # If n_years_with_events << total_span, there are gaps suggesting episodes
    coverage_ratio = n_years_with_events / total_span,
    likely_multi_episode = coverage_ratio < 0.7 & total_span > 5
  ) |>
  left_join(
    ged_clean |> distinct(conflict_new_id, side_a, side_b),
    by = "conflict_new_id"
  )


episode_check |>
  select(conflict_new_id, side_a, first_year, last_year, 
         n_years_with_events, coverage_ratio) |>
  arrange(first_year) |>
  print(n = 50)


acd <- readRDS("Desktop/Thesis/data/raw/Dyadic_v25_1.rds") 

cat("ACD rows:", nrow(acd), "\n")
cat("ACD columns:", ncol(acd), "\n")
cat("\nColumn names:\n")
print(names(acd))
cat("\nFirst 3 rows (key columns only):\n")

conflict_ids_ordine <- unique(ged_clean$conflict_new_id)
id_cols <- names(acd)[grepl("conflict|id", names(acd), ignore.case = TRUE)]
date_cols <- names(acd)[grepl("start|date|onset", names(acd), ignore.case = TRUE)]

ids_ordine <- unique(ged_clean$conflict_new_id)
acd_ids <- unique(acd$conflict_id)
cat("NOT found in ACD:", sum(!ids_ordine %in% acd_ids), "\n")


cat("\nMozambique (conflict 332) episode starts:\n")
acd |>
  filter(conflict_id == 332) |>
  distinct(conflict_id, start_date, start_date2, start_prec2) |>
  arrange(start_date2) |>
  print()

cat("\nAfghanistan (conflict 333) episode starts:\n")
acd |>
  filter(conflict_id == 333) |>
  distinct(conflict_id, start_date, start_date2, start_prec2) |>
  arrange(start_date2) |>
  print()

# --- 7.1 EPISODE ONSET YEAR USING UCDP ACD ----------------------------------------
 

cf_actors_surviving <- readRDS(
  file.path(path_output, "cf_actors_stage2.rds")
)


cf_dyad_link <- cf_actors_surviving |>
  filter(
    uniq_id %in% cf_filtered$uniq_id,  # Only ceasefires surviving all filters
    ucdp_dyad > 0                        # Valid dyad IDs only
  ) |>
  select(uniq_id, dyad_id = ucdp_dyad) |>
  distinct()

cat("Ceasefire-dyad pairs with valid dyad ID:", nrow(cf_dyad_link), "\n")
cat("Ceasefires with at least one valid dyad:",
    n_distinct(cf_dyad_link$uniq_id), "\n")
cat("Ceasefires with NO valid dyad ID:",
    n_distinct(cf_filtered$uniq_id) - n_distinct(cf_dyad_link$uniq_id), "\n")


# start_date2 = date each episode reached 25 battle deaths
# A new start_date2 is coded whenever a dyad restarts after 1+ years inactive
acd_episodes <- acd |>
  select(dyad_id, conflict_id, start_date2) |>
  filter(!is.na(start_date2)) |>
  distinct()


cf_with_dates <- cf_dyad_link |>
  left_join(
    cf_filtered |> select(uniq_id, cf_effect_date),
    by = "uniq_id"
  ) |>
  left_join(acd_episodes, by = "dyad_id") |>
  filter(!is.na(start_date2), start_date2 < cf_effect_date)



cf_episode_onset <- cf_with_dates |>
  group_by(uniq_id, cf_effect_date) |>
  slice_max(start_date2, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    episode_onset_year = year(start_date2),
    vdem_year = episode_onset_year - 1
  ) |>
  select(uniq_id, episode_onset_date = start_date2,
         episode_onset_year, vdem_year)



ceasefires_no_episode <- setdiff(cf_filtered$uniq_id, cf_episode_onset$uniq_id)
cat("Ceasefires needing conflict-level fallback:", length(ceasefires_no_episode), "\n")

if (length(ceasefires_no_episode) > 0) {
  cat("Fallback ceasefires:\n")
  cf_filtered |>
    filter(uniq_id %in% ceasefires_no_episode) |>
    select(uniq_id, location, cf_effect_yr) |>
    print()
  
conflict_onset_fallback <- ged_clean |>
    group_by(conflict_new_id) |>
    summarise(conflict_onset_year = min(year), .groups = "drop")
  
  fallback_onset <- cf_conflict_link |>
    filter(uniq_id %in% ceasefires_no_episode) |>
    left_join(conflict_onset_fallback, by = "conflict_new_id") |>
    group_by(uniq_id) |>
    summarise(episode_onset_year = min(conflict_onset_year), .groups = "drop") |>
    left_join(cf_filtered |> select(uniq_id, cf_effect_date), by = "uniq_id") |>
    mutate(
      episode_onset_date = as.Date(paste0(episode_onset_year, "-01-01")),
      vdem_year = episode_onset_year - 1
    ) |>
    select(uniq_id, episode_onset_date, episode_onset_year, vdem_year)


 cf_onset <- bind_rows(cf_episode_onset, fallback_onset)
} else {
  cf_onset <- cf_episode_onset
}


acd_conflict_onset <- acd |>
  group_by(conflict_id) |>
  summarise(
    earliest_start_date2 = min(start_date2, na.rm = TRUE),
    .groups = "drop"
  )


acd_conflict_episodes <- acd |>
  select(conflict_id, start_date2) |>
  filter(!is.na(start_date2)) |>
  distinct()


fallback_onset <- cf_conflict_link |>
  filter(uniq_id %in% ceasefires_no_episode) |>
  left_join(
    cf_filtered |> select(uniq_id, cf_effect_date),
    by = "uniq_id"
  ) |>
  left_join(
    acd_conflict_episodes,
    by = c("conflict_new_id" = "conflict_id"),
    relationship = "many-to-many"
  ) |>
  filter(!is.na(start_date2), start_date2 < cf_effect_date) |>
  group_by(uniq_id, cf_effect_date) |>
  slice_max(start_date2, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    episode_onset_year = year(start_date2),
    vdem_year = episode_onset_year - 1
  ) |>
  select(uniq_id, episode_onset_date = start_date2,
         episode_onset_year, vdem_year)

cat("Fallback ceasefires resolved via ACD conflict_id:", nrow(fallback_onset), "\n")


  # Last resort fallback: min(GED year)
  last_resort <- cf_conflict_link |>
    filter(uniq_id %in% still_missing) |>
    left_join(
      ged_clean |>
        group_by(conflict_new_id) |>
        summarise(onset_yr = min(year), .groups = "drop"),
      by = "conflict_new_id"
    ) |>
    group_by(uniq_id) |>
    summarise(episode_onset_year = min(onset_yr), .groups = "drop") |>
    left_join(cf_filtered |> select(uniq_id, cf_effect_date), by = "uniq_id") |>
    mutate(
      episode_onset_date = as.Date(paste0(episode_onset_year, "-01-01")),
      vdem_year = episode_onset_year - 1
    ) |>
    select(uniq_id, episode_onset_date, episode_onset_year, vdem_year)
  
  fallback_onset <- bind_rows(fallback_onset, last_resort)
}


cf_onset <- bind_rows(cf_episode_onset, fallback_onset)




# --- 7.2 V-DEM MERGE ----------------------------------------
vdem_slim <- vdem_raw |>
  select(country_name, COWcode, year, v2x_liberal)

our_gw_codes <- unique(cf_filtered$cc)

gw_to_cow <- tibble(gw_code = our_gw_codes) |>
  mutate(
    cow_code = countrycode(gw_code,
                           origin      = "gwn",
                           destination = "cown",
                           warn        = FALSE)
  )



cf_vdem_key <- cf_filtered |>
  select(uniq_id, cc, location) |>
  left_join(gw_to_cow, by = c("cc" = "gw_code")) |>
  left_join(cf_onset |> select(uniq_id, vdem_year), by = "uniq_id")


cf_iq <- cf_vdem_key |>
  left_join(
    vdem_slim |> select(COWcode, year, v2x_liberal),
    by = c("cow_code" = "COWcode", "vdem_year" = "year")
  )


cat("\nCeasefires with v2x_liberal matched:", sum(!is.na(cf_iq$v2x_liberal)), "\n")
cat("Ceasefires MISSING v2x_liberal:", sum(is.na(cf_iq$v2x_liberal)), "\n")

if (any(is.na(cf_iq$v2x_liberal))) {
  cat("\nMissing cases:\n")
  cf_iq |>
    filter(is.na(v2x_liberal)) |>
    select(uniq_id, location, cc, cow_code, vdem_year) |>
    print()
  cat("\nPossible fix: check if location has a different COW code in V-Dem.\n")
  cat("Run: vdem_slim |> filter(grepl('COUNTRY', country_name)) |> distinct(country_name, COWcode)\n")
}



#Investigating Yemen
vdem_slim |>
  filter(grepl("Yemen", country_name, ignore.case = TRUE)) |>
  distinct(country_name, COWcode) |>
  print()

vdem_slim |>
  filter(COWcode == 678) |>
  filter(!is.na(v2x_liberal)) |>
  summarise(min_year = min(year), max_year = max(year)) |>
  print()

vdem_slim |>
  filter(COWcode == 679) |>
  filter(!is.na(v2x_liberal)) |>
  summarise(min_year = min(year), max_year = max(year)) |>
  print()

cf_iq |>
  filter(grepl("Yemen", location, ignore.case = TRUE)) |>
  select(uniq_id, location, cow_code, vdem_year) |>
  print()


# Manual COWcode fix for Yemen
cf_iq <- cf_vdem_key |>
  mutate(
    # Apply manual override: GW 678 → COW 679 for post-1990 data
    cow_code_adjusted = case_when(
      cow_code == 678 ~ 679L,   # Yemen: use unified Yemen code
      TRUE ~ as.integer(cow_code)
    )
  ) |>
  left_join(
    vdem_slim |> select(COWcode, year, v2x_liberal),
    by = c("cow_code_adjusted" = "COWcode", "vdem_year" = "year")
  )


cf_iq_complete <- cf_iq |> filter(!is.na(v2x_liberal))

cat("Ceasefires with V-Dem:", nrow(cf_iq_complete), "\n")
cat("Dropped:", nrow(cf_iq) - nrow(cf_iq_complete), "\n")

attrition_log <- log_attrition(
  attrition_log,
  "7c", "V-Dem v2x_liberal available at conflict onset year - 1",
  nrow(cf_iq_complete),
  "Final analytical sample — Filter 7 from methodology"
)

cat("\nFinal attrition log:\n")
print(attrition_log)



iq_mean <- mean(cf_iq_complete$v2x_liberal)
iq_sd   <- sd(cf_iq_complete$v2x_liberal)

cat("  Mean:", round(iq_mean, 4), "\n")
cat("  SD:", round(iq_sd, 4), "\n")
cat("  Min:", round(min(cf_iq_complete$v2x_liberal), 4), "\n")
cat("  Max:", round(max(cf_iq_complete$v2x_liberal), 4), "\n")

cf_iq_complete <- cf_iq_complete |>
  mutate(iq_std = (v2x_liberal - iq_mean) / iq_sd)

iq_params <- tibble(
  variable = "v2x_liberal", vdem_version = "v16",
  sample = "final ceasefire sample after all filters",
  mean = iq_mean, sd = iq_sd
)
write_csv(iq_params, file.path(path_output, "iq_standardisation_params.csv"))

#saving 
panel_final <- panel_final |>
  filter(uniq_id %in% cf_iq_complete$uniq_id) |>
  left_join(
    cf_iq_complete |> select(uniq_id, v2x_liberal, iq_std, vdem_year),
    by = "uniq_id"
  )

cat("\nPanel rows after V-Dem filter:", nrow(panel_final), "\n")
cat("IQ NA in panel:", sum(is.na(panel_final$iq_std)), "\n")

rm(vdem_raw, vdem_slim)
gc()


 #--- 7.3 GDP PER CAPITA (World Bank WDI) ----------------------------------------

wdi_raw <- read_csv(path_wdi, show_col_types = FALSE)
year_cols <- names(wdi_raw)[grepl("^[0-9]{4} \\[YR[0-9]{4}\\]$", names(wdi_raw))]
wdi_long <- wdi_raw |>
  select(`Country Code`, all_of(year_cols)) |>
  pivot_longer(
    cols      = all_of(year_cols),
    names_to  = "year_str",
    values_to = "gdp_pc"
  ) |>
  mutate(
    year   = as.integer(substr(year_str, 1, 4)),
    gdp_pc = suppressWarnings(as.numeric(gdp_pc))
  ) |>
  select(-year_str) |>
  filter(!is.na(gdp_pc))

wdi_long <- wdi_long |>
  mutate(
    gw_code = countrycode(`Country Code`,
                          origin      = "wb",
                          destination = "gwn",
                          warn        = FALSE)
  ) |>
  filter(!is.na(gw_code))



cf_gdp_key <- cf_filtered |>
  filter(uniq_id %in% cf_iq_complete$uniq_id) |>
  select(uniq_id, country_code_fe = cc, cf_effect_yr) |>
  mutate(gdp_year = cf_effect_yr - 1)


cf_gdp <- cf_gdp_key |>
  left_join(
    wdi_long |> select(gw_code, year, gdp_pc),
    by = c("country_code_fe" = "gw_code", "gdp_year" = "year")
  ) |>
  mutate(ln_gdp_pc = log(gdp_pc)) |>
  select(uniq_id, gdp_pc, ln_gdp_pc)

if (any(is.na(cf_gdp$gdp_pc))) {
  cat("Missing GDP cases:\n")
  cf_gdp |>
    filter(is.na(gdp_pc)) |>
    left_join(cf_filtered |> select(uniq_id, location), by = "uniq_id") |>
    print()
}

panel_final <- panel_final |>
  left_join(cf_gdp, by = "uniq_id")



#south sudan and yemen check
cf_gdp_key |>
  left_join(cf_filtered |> select(uniq_id, location), by = "uniq_id") |>
  filter(grepl("Yemen", location)) |>
  select(uniq_id, location, country_code_fe, gdp_year) |>
  print()

wdi_long |>
  filter(gw_code == 678) |>
  arrange(year) |>
  print()

wdi_long |>
  filter(gw_code == 679) |>
  arrange(year) |>
  print()

wdi_long |>
  filter(gw_code == 626) |>
  arrange(year) |>
  print()


cf_gdp_key |>
  filter(uniq_id %in% c(626010, 626013, 626017,
                        678036, 678037, 678043)) |>
  left_join(cf_filtered |> select(uniq_id, location), by = "uniq_id") |>
  select(uniq_id, location, country_code_fe, gdp_year) |>
  print()


wdi_long |>
  filter(`Country Code` %in% c("YEM", "YMN", "YMD")) |>
  arrange(year) |>
  print()

# Fix 1: Yemen — adding manual GW→WB code mapping before the GW conversion
wdi_yemen <- wdi_long |>
  filter(`Country Code` == "YEM") |>
  mutate(gw_code = 678)

# Fix 2: South Sudan 
wdi_ss_2015 <- wdi_long |>
  filter(gw_code == 626, year == 2015) |>
  pull(gdp_pc)

wdi_ss_extra <- tibble(
  `Country Code` = "SSD",
  gdp_pc = wdi_ss_2015,
  year = c(2016L, 2017L),
  gw_code = 626
)


wdi_long_fixed <- bind_rows(wdi_long, wdi_yemen, wdi_ss_extra)

# Re-running the GDP merge with fixed WDI

cf_gdp <- cf_gdp_key |>
  left_join(
    wdi_long_fixed |> select(gw_code, year, gdp_pc),
    by = c("country_code_fe" = "gw_code", "gdp_year" = "year")
  ) |>
  mutate(ln_gdp_pc = log(gdp_pc)) |>
  select(uniq_id, gdp_pc, ln_gdp_pc)


panel_final <- panel_final |>
  left_join(cf_gdp, by = "uniq_id")



if (any(is.na(cf_gdp$gdp_pc))) {
  cat("Remaining missing:\n")
  cf_gdp |>
    filter(is.na(gdp_pc)) |>
    left_join(cf_filtered |> select(uniq_id, location), by = "uniq_id") |>
    print()
}
cat("ln_gdp_pc NA in panel:", sum(is.na(panel_final$ln_gdp_pc)), "\n")

#fixing yemen again
# Reloading raw WDI

wdi_raw <- read_csv(path_wdi, show_col_types = FALSE)
year_cols <- names(wdi_raw)[grepl("^[0-9]{4} \\[YR[0-9]{4}\\]$", names(wdi_raw))]


wdi_long <- wdi_raw |>
  select(`Country Code`, all_of(year_cols)) |>
  pivot_longer(
    cols      = all_of(year_cols),
    names_to  = "year_str",
    values_to = "gdp_pc"
  ) |>
  mutate(
    year   = as.integer(substr(year_str, 1, 4)),
    gdp_pc = suppressWarnings(as.numeric(gdp_pc))
  ) |>
  select(-year_str) |>
  filter(!is.na(gdp_pc))



wdi_long <- wdi_long |>
  mutate(
    gw_code = case_when(
      `Country Code` == "YEM" ~ 678,   # Yemen, Rep. → GW 678
      `Country Code` == "SSD" ~ 626,   # South Sudan → GW 626
      `Country Code` == "SRB" ~ 340,   # Serbia
      `Country Code` == "XKX" ~ NA_real_, # Kosovo — not in GW
      TRUE ~ countrycode(`Country Code`,
                         origin      = "wb",
                         destination = "gwn",
                         warn        = FALSE)
    )
  ) |>
  filter(!is.na(gw_code))


# South Sudan carry-forward for 2016 and 2017
wdi_ss_2015 <- wdi_long |>
  filter(gw_code == 626, year == 2015) |>
  pull(gdp_pc)

cat("South Sudan 2015 GDP:", wdi_ss_2015, "\n")

wdi_long <- bind_rows(
  wdi_long,
  tibble(
    `Country Code` = "SSD",
    gdp_pc = wdi_ss_2015,
    year   = c(2016L, 2017L),
    gw_code = 626
  )
)


cf_gdp <- cf_gdp_key |>
  left_join(
    wdi_long |> select(gw_code, year, gdp_pc),
    by = c("country_code_fe" = "gw_code", "gdp_year" = "year")
  ) |>
  mutate(ln_gdp_pc = log(gdp_pc)) |>
  select(uniq_id, gdp_pc, ln_gdp_pc)


if (any(is.na(cf_gdp$gdp_pc))) {
  cat("Remaining missing:\n")
  cf_gdp |>
    filter(is.na(gdp_pc)) |>
    left_join(cf_filtered |> select(uniq_id, location), by = "uniq_id") |>
    print()
}



panel_final <- panel_final |>
  left_join(cf_gdp, by = "uniq_id")

cat("ln_gdp_pc NA in panel:", sum(is.na(panel_final$ln_gdp_pc)), "\n")


#more fixing
cf_gdp_key |>
  filter(uniq_id %in% c(678067, 678069)) |>
  left_join(cf_filtered |> select(uniq_id, location), by = "uniq_id") |>
  select(uniq_id, location, country_code_fe, gdp_year) |>
  print()


yemen_2018 <- wdi_long |>
  filter(gw_code == 678, year == 2018) |>
  pull(gdp_pc)
cat("Yemen 2018 GDP (carry-forward to 2019):", yemen_2018, "\n")


wdi_long <- bind_rows(
  wdi_long,
  tibble(
    `Country Code` = "YEM",
    gdp_pc  = yemen_2018,
    year    = 2019L,
    gw_code = 678
  )
)


cf_gdp <- cf_gdp_key |>
  left_join(
    wdi_long |> select(gw_code, year, gdp_pc),
    by = c("country_code_fe" = "gw_code", "gdp_year" = "year")
  ) |>
  mutate(ln_gdp_pc = log(gdp_pc)) |>
  select(uniq_id, gdp_pc, ln_gdp_pc)


panel_final <- panel_final |>
  select(-any_of(c("gdp_pc", "ln_gdp_pc"))) |>
  left_join(cf_gdp, by = "uniq_id")


cat("ln_gdp_pc NA in panel:", sum(is.na(panel_final$ln_gdp_pc)), "\n")

rm(wdi_raw, wdi_long)
gc()



#--- 7.4 BATTLE DEATHS PER CELL PER PERIOD ----------------------------------------
ged_dt <- as.data.table(ged_clean)[, .(
  priogrid_gid,
  date_start = as.IDate(date_start),
  best
)]
ged_dt[, date_end := date_start]


panel_dt <- as.data.table(
  panel_final |>
    select(uniq_id, priogrid_gid, period, period_start_date, period_end_date)
)[, `:=`(
  period_start_date = as.IDate(period_start_date),
  period_end_date   = as.IDate(period_end_date)
)]



setkey(panel_dt, priogrid_gid, period_start_date, period_end_date)
setkey(ged_dt,   priogrid_gid, date_start, date_end)


deaths_merged <- foverlaps(
  ged_dt, panel_dt,
  by.x    = c("priogrid_gid", "date_start", "date_end"),
  by.y    = c("priogrid_gid", "period_start_date", "period_end_date"),
  type    = "within",
  nomatch = NA
)

deaths_agg <- deaths_merged[!is.na(uniq_id), .(
  battle_deaths = sum(best, na.rm = TRUE)
), by = .(uniq_id, priogrid_gid, period)]


panel_final <- panel_final |>
  left_join(deaths_agg |> as_tibble(),
            by = c("uniq_id", "priogrid_gid", "period")) |>
  mutate(
    battle_deaths    = replace_na(battle_deaths, 0),
    ln_battle_deaths = log(battle_deaths + 1)
  )

cat("ln_battle_deaths NA:", sum(is.na(panel_final$ln_battle_deaths)), "\n")

rm(ged_dt, panel_dt, deaths_merged, deaths_agg)
gc()


#--- 7.5 CONFLICT DURATION ----------------------------------------

conflict_first_event <- ged_clean |>
  group_by(conflict_new_id) |>
  summarise(first_event_date = min(date_start), .groups = "drop")

cf_duration <- cf_conflict_link |>
  left_join(conflict_first_event, by = "conflict_new_id") |>
  group_by(uniq_id) |>
  summarise(conflict_start_date = min(first_event_date, na.rm = TRUE),
            .groups = "drop") |>
  left_join(cf_filtered |> select(uniq_id, cf_effect_date), by = "uniq_id") |>
  mutate(
    conflict_duration_months = as.numeric(
      difftime(cf_effect_date, conflict_start_date, units = "days")
    ) / 30.44,
    ln_conflict_duration = log(conflict_duration_months + 1)
  )

cat("Conflict duration (months):\n")
print(summary(cf_duration$conflict_duration_months))

panel_final <- panel_final |>
  left_join(
    cf_duration |> select(uniq_id, conflict_duration_months, ln_conflict_duration),
    by = "uniq_id"
  )


#--- 7.6 CEASEFIRE CLASS DUMMIES ----------------------------------------

cf_class <- cf_filtered |>
  filter(uniq_id %in% cf_iq_complete$uniq_id) |>
  select(uniq_id, ceasefire_class) |>
  mutate(
    cf_class_cohm       = as.integer(ceasefire_class == 2),
    cf_class_definitive = as.integer(ceasefire_class == 3)
  )

cat("Ceasefire class distribution:\n")
print(table(cf_class$ceasefire_class, dnn = "class"))
cat("1=CoH (reference), 2=CoHm, 3=Definitive\n")

panel_final <- panel_final |>
  left_join(cf_class |> select(uniq_id, cf_class_cohm, cf_class_definitive),
            by = "uniq_id")


#--- 7.7 PANEL FINAL - creating interaction terms ----------------------------------------

panel_final <- panel_final |>
  mutate(
    # Primary interaction — β₂ in Equation 2
    post_x_iq     = post * iq_std,
    
    # Differential trend control — γ term in Equation 2
    iq_x_period   = iq_std * period,
    
    # Armand et al. local polynomial terms — Equation 2
    period_post    = post * period,       # δ₂: linear post-period trend
    period_post_sq = post * period^2      # δ₃: quadratic post-period trend
  )

cat("Interaction terms created.\n")
cat("post_x_iq NA:", sum(is.na(panel_final$post_x_iq)), "\n")



#--- 7.8 FINAL PANEL CHECKS ----------------------------------------
cat("Total rows:", nrow(panel_final), "\n")
cat("Unique ceasefires:", n_distinct(panel_final$uniq_id), "\n")
cat("Unique cells:", n_distinct(panel_final$priogrid_gid), "\n")
cat("Countries (FE):", n_distinct(panel_final$country_code_fe), "\n")
cat("Ceasefire years:", paste(sort(unique(panel_final$cf_year)), collapse = ", "), "\n")

cat("\nRows by cell type:\n")
print(panel_final |> count(cell_type))
cat("\nRows by post indicator:\n")
print(panel_final |> count(post))



cat("\n--- Missingness by key variable ---\n")
key_vars <- c("ln_ntl", "iq_std", "post_x_iq", "ln_gdp_pc",
              "ln_battle_deaths", "ln_conflict_duration",
              "cf_class_cohm", "cf_class_definitive")

panel_final |>
  summarise(across(all_of(key_vars), ~round(100 * mean(is.na(.)), 2))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "pct_na") |>
  print()

cat("\n--- IQ distribution in final sample ---\n")
iq_summary <- panel_final |>
  distinct(uniq_id, v2x_liberal, iq_std)

cat("v2x_liberal (raw):\n")
print(summary(iq_summary$v2x_liberal))
cat("iq_std (standardised):\n")
print(summary(iq_summary$iq_std))

cat("\n--- Ceasefire-level descriptives ---\n")
cat("Ceasefires by location (top 10):\n")
panel_final |>
  distinct(uniq_id, cf_effect_date) |>
  left_join(cf_filtered |> select(uniq_id, location), by = "uniq_id") |>
  count(location, sort = TRUE) |>
  head(10) |>
  print()

cat("\n--- Final attrition log ---\n")
print(attrition_log)



# Corrected conflict duration using episode onset from cf_onset
cf_duration_corrected <- cf_onset |>
  left_join(cf_filtered |> select(uniq_id, cf_effect_date), by = "uniq_id") |>
  mutate(
    # Duration from episode onset to ceasefire — theoretically correct
    conflict_duration_months = as.numeric(
      difftime(cf_effect_date, episode_onset_date, units = "days")
    ) / 30.44,
    ln_conflict_duration = log(conflict_duration_months + 1)
  ) |>
  select(uniq_id, conflict_duration_months, ln_conflict_duration)

cat("Corrected conflict duration (months):\n")
print(summary(cf_duration_corrected$conflict_duration_months))


# Comparing with original
cat("\nOriginal duration median:", 
    median(cf_duration$conflict_duration_months), "months\n")
cat("Corrected duration median:", 
    median(cf_duration_corrected$conflict_duration_months), "months\n")


# Removing old duration columns and replace
panel_final <- panel_final |>
  select(-any_of(c("conflict_duration_months", "ln_conflict_duration"))) |>
  left_join(cf_duration_corrected, by = "uniq_id")

cat("ln_conflict_duration NA after correction:", 
    sum(is.na(panel_final$ln_conflict_duration)), "\n")

#more fixing

panel_final <- panel_final |>
  select(
    # Identifiers
    uniq_id, priogrid_gid, period, post,
    cf_effect_date, cf_year, country_code_fe,
    cell_type, cell_cf_id,
    period_start_date, period_end_date,
    
    # Outcome variable
    ln_ntl, ntl_mean_raw, n_days_total, n_days_valid, snow_mean,
    
    # Moderating variable
    v2x_liberal, iq_std, vdem_year,
    
    # Control variables
    ln_gdp_pc, ln_battle_deaths,
    ln_conflict_duration, conflict_duration_months,
    cf_class_cohm, cf_class_definitive,
    
    # Model terms
    post_x_iq, iq_x_period, period_post, period_post_sq
  )

cat("\nColumns after cleanup:", ncol(panel_final), "\n")
cat("Rows:", nrow(panel_final), "\n")
print(names(panel_final))


#  Full missingness check on clean panel
cat("\n--- Missingness check ---\n")
panel_final |>
  summarise(across(everything(), ~round(100 * mean(is.na(.)), 2))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "pct_na") |>
  filter(pct_na > 0) |>
  print()

# Verifying key variable distributions
cat("\n--- Key variable checks ---\n")
cat("ln_ntl range:", min(panel_final$ln_ntl, na.rm=TRUE), 
    "to", max(panel_final$ln_ntl, na.rm=TRUE), "\n")
cat("iq_std range:", min(panel_final$iq_std), 
    "to", max(panel_final$iq_std), "\n")
cat("post_x_iq NA:", sum(is.na(panel_final$post_x_iq)), "\n")
cat("ln_gdp_pc range:", min(panel_final$ln_gdp_pc, na.rm=TRUE),
    "to", max(panel_final$ln_gdp_pc, na.rm=TRUE), "\n")
cat("ln_battle_deaths range:", min(panel_final$ln_battle_deaths),
    "to", max(panel_final$ln_battle_deaths), "\n")
cat("ln_conflict_duration range:", min(panel_final$ln_conflict_duration),
    "to", max(panel_final$ln_conflict_duration), "\n")

#Check no duplicate ceasefire-cell-period rows
n_dup <- panel_final |>
  count(uniq_id, priogrid_gid, period) |>
  filter(n > 1) |>
  nrow()
cat("\nDuplicate rows:", n_dup, "\n")





#--- 7.9 SAVING FINAL ANALYTICAL PANELS ----------------------------------------
panel_other <- panel_final |> filter(cell_type == "other")
saveRDS(panel_other, file.path(path_output, "panel_analysis_other.rds"))
cat("Primary analysis panel (other cells):", nrow(panel_other), "rows\n")

saveRDS(panel_final, file.path(path_output, "panel_analysis_full.rds"))
cat("Full panel (all cells):", nrow(panel_final), "rows\n")

saveRDS(attrition_log, file.path(path_output, "attrition_log_final.rds"))
write_csv(attrition_log, file.path(path_output, "attrition_log_final.csv"))

saveRDS(cf_onset, file.path(path_output, "cf_onset_final.rds"))




#--- 7.10 Adding Geo-PKO control to full panel ----------------------------------------

geopko_raw <- readRDS(file.path(path_raw, "Geo_PKO_v.2.3_location_month.rds"))
setDT(geopko_raw)

cat("Geo-PKO raw dimensions:", nrow(geopko_raw), "x", ncol(geopko_raw), "\n")
cat("Years covered:", min(geopko_raw$year, na.rm = TRUE), "-",
    max(geopko_raw$year, na.rm = TRUE), "\n")
cat("Unique missions:", uniqueN(geopko_raw$mission), "\n")
cat("no.troops 'unknown' count:", sum(geopko_raw$no.troops == "unknown", na.rm = TRUE), "\n")


exclude_missions <- c("UNDOF", "UNIFIL", "UNTSO", "UNMOGIP", "UNSMIS")

geopko_clean <- geopko_raw[!mission %in% exclude_missions]

geopko_clean[, troops_numeric := suppressWarnings(as.numeric(no.troops))]


geopko_cell <- geopko_clean[
  !is.na(prioid),
  .(
    pko_present     = 1L,
    pko_troops_sum  = sum(troops_numeric, na.rm = TRUE),
    pko_any_unknown = as.integer(any(is.na(troops_numeric)))
  ),
  by = .(prioid = as.integer(prioid), year, month)
]


panel <- readRDS(file.path(path_output, "panel_analysis_full.rds"))
setDT(panel)

panel[, pko_merge_year  := year(period_start_date)]
panel[, pko_merge_month := month(period_start_date)]


panel <- merge(
  panel,
  geopko_cell[, .(prioid, year, month, 
                  pko_present, pko_troops_sum, pko_any_unknown)],
  by.x = c("priogrid_gid", "pko_merge_year", "pko_merge_month"),
  by.y = c("prioid",        "year",           "month"),
  all.x = TRUE
)

# NA → 0: no mission = zero troops
panel[is.na(pko_present),    pko_present    := 0L]
panel[is.na(pko_troops_sum), pko_troops_sum := 0]


panel[, ln_pko_troops := log(pko_troops_sum + 1)]
panel[, c("pko_merge_year", "pko_merge_month") := NULL]

#7.11 Savinf final panel with added PKO control
saveRDS(panel, file.path(path_output, "panel_analysis_full_pko.rds"))

panel_other <- panel[cell_type == "other"]
saveRDS(panel_other, file.path(path_output, "panel_analysis_other_pko.rds"))



panel_other <- readRDS(file.path(path_output, "panel_analysis_other_pko.rds"))
panel_final  <- readRDS(file.path(path_output, "panel_analysis_full_pko.rds"))









############################################################################################
############################################################################################
# --- Stage 8: REGRESSIONS AND RESULTS -------------------
library(fixest)

path_raw    <- "Desktop/Thesis/data/raw"
path_output <- "Desktop/Thesis/data/processed"

panel_other <- readRDS(file.path(path_output, "panel_analysis_other_pko.rds"))
panel_final  <- readRDS(file.path(path_output, "panel_analysis_full_pko.rds"))
panel_salient <- panel_final[cell_type == "salient"]
panel_all     <- panel_final   # "all" = every cell type pooled, no cell_type filter

# --- 8.0 Creating cluster variable and doing main regression -----------------------
# Cluster = ceasefire × period (following Armand et al.)
# Multiple cells within the same ceasefire-period are in the same cluster.

panel_other <- panel_other |>
  mutate(cf_period_id = paste0(uniq_id, "_", period))

panel_final <- panel_final |>
  mutate(cf_period_id = paste0(uniq_id, "_", period))


# --- 8.1 first, Parallel Pre trend tests -----------------------
panel_other_pre <- panel_other |> filter(post == 0)
panel_salient_pre <- panel_salient |> filter(post == 0)
panel_all_pre <- panel_all |> filter(post == 0)


pretrend_test_interest <- feols(
  ln_ntl ~ iq_x_period + period + 
    ln_gdp_pc + ln_battle_deaths + ln_conflict_duration +
    cf_class_cohm + cf_class_definitive + pko_present |
    country_code_fe + cf_year,
  data      = panel_other_pre,
  cluster   = ~cf_period_id,
)

summary(pretrend_test_interest)

pretrend_test_salient <- feols(
  ln_ntl ~ iq_x_period + period + 
    ln_gdp_pc + ln_battle_deaths + ln_conflict_duration +
    cf_class_cohm + cf_class_definitive + pko_present |
    country_code_fe + cf_year,
  data      = panel_salient_pre,
  cluster   = ~cf_period_id,
)

summary(pretrend_test_salient)

pretrend_test_all <- feols(
  ln_ntl ~ iq_x_period + period + 
    ln_gdp_pc + ln_battle_deaths + ln_conflict_duration +
    cf_class_cohm + cf_class_definitive + pko_present |
    country_code_fe + cf_year,
  data      = panel_all_pre,
  cluster   = ~cf_period_id,
)

summary(pretrend_test_all)


library(modelsummary)

pretrend_models <- list(
  "(1) Interest"         = pretrend_test_interest,
  "(2) Conflict Salient" = pretrend_test_salient,
  "(3) All"              = pretrend_test_all
)


coef_map_pre <- c(
  "iq_x_period" = "IQ × Date (γ, pre-period)",
  "period"      = "Date (pre-period trend)"
)

gof_pre <- tribble(
  ~raw,            ~clean,         ~fmt,
  "nobs",          "Observations", 0,
  "r.squared",     "R²",           3
)

msummary(
  pretrend_models,
  coef_map = coef_map_pre,
  stars    = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map  = gof_pre,
  fmt      = "%.4f",
  title    = "Table X: Parallel pre-ceasefire trend test (pre-period observations only)",
  notes    = c(
    "Estimated on pre-ceasefire observations only (t < 0).",
    "Coefficient on IQ × Date tests whether luminosity trends differ by institutional quality before the ceasefire.",
    "Controls (GDP, battle deaths, conflict duration, ceasefire class, peacekeeping) and country and year fixed effects included but not shown.",
    "Standard errors clustered at the period-ceasefire level."
  ),
  output   = file.path(path_output, "pretrend_test_table.docx")
)








#--- 8.2 Running main regression -----------------------
eq_main <- feols(
  ln_ntl ~ post + post_x_iq + iq_x_period +
    period + period_post + period_post_sq +
    ln_battle_deaths + ln_gdp_pc + ln_conflict_duration +
    cf_class_cohm + cf_class_definitive + pko_present |
    country_code_fe + cf_year,
  data    = panel_other,
  cluster = ~cf_period_id
)


summary(eq_main)

length(unique(panel_other$country_code_fe)) 

library(data.table)



f_main <- ln_ntl ~ post + post_x_iq + iq_x_period +
  period + period_post + period_post_sq +
  ln_battle_deaths + ln_gdp_pc + ln_conflict_duration +
  cf_class_cohm + cf_class_definitive + pko_present |
  country_code_fe + cf_year

eq_salient <- feols(f_main, data = panel_salient, cluster = ~cf_period_id)
eq_all     <- feols(f_main, data = panel_all,     cluster = ~cf_period_id)

summary(eq_salient)
summary(eq_all)


#--- 8.2.2 Creating table for main regression -----------------------
library(modelsummary)

# Define models for Table 2
table2_models <- list(
  "(1) Interest cells"   = eq_main,
  "(2) Conflict Salient cells" = eq_salient,
  "(3) All cells"     = eq_all
)

# Coefficient labels
coef_map_t2 <- c(
  "post"                 = "Post (β₁)",
  "post_x_iq"           = "Post × IQ_std (β₂)",
  "iq_x_period"         = "IQ × Date (γ)",
  "period"              = "Date (δ₁)",
  "period_post"         = "Post × Date (δ₂)",
  "period_post_sq"      = "Post × Date² (δ₃)",
  "ln_gdp_pc"           = "ln GDP per capita",
  "ln_battle_deaths"    = "ln Battle deaths",
  "ln_conflict_duration"= "ln Conflict duration",
  "cf_class_cohm"       = "CoHm dummy",
  "cf_class_definitive" = "Definitive ceasefire dummy",
  "pko_present" = "UN peacekeeping"
)

# Goodness of fit rows
gof_t2 <- tribble(
  ~raw,           ~clean,              ~fmt,
  "nobs",         "Observations",      0,
  "r.squared",    "R²",                3,
  "adj.r.squared","Adj. R²",           3
)

msummary(
  table2_models,
  coef_map  = coef_map_t2,
  stars     = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map   = gof_t2,
  title     = "Table 1: Institutional moderation of post-ceasefire NTL response",
  notes     = c(
    "Standard errors clustered at the period-ceasefire level.",
    "Fixed effects: country and ceasefire year.",
    "Interest cells: historically active, no signatory event in 6 months pre-ceasefire.",
    "Conflict Salient cells: at least one signatory event in 6 months pre-ceasefire.",
    "IQ_std = V-Dem v2x_liberal (Liberal Component Index), standardised mean 0 SD 1."
  ),
  output = file.path(path_output, "table1_main_results____.docx")
)




# Ceasefires = distinct uniq_id; Countries = distinct country_code_fe, per panel
n_cf_interest <- length(unique(panel_other$uniq_id))
n_co_interest <- length(unique(panel_other$country_code_fe))

n_cf_salient  <- length(unique(panel_salient$uniq_id))
n_co_salient  <- length(unique(panel_salient$country_code_fe))

n_cf_all      <- length(unique(panel_all$uniq_id))
n_co_all      <- length(unique(panel_all$country_code_fe))
n_co_interest <- length(unique(eq_main$fixef_id$country_code_fe))  
library(tibble)

extra_rows <- tribble(
  ~term,                  ~`(1) Interest cells`, ~`(2) Conflict Salient cells`, ~`(3) All cells`,
  "Number of ceasefires", as.character(n_cf_interest), as.character(n_cf_salient), as.character(n_cf_all),
  "Number of countries",  as.character(n_co_interest), as.character(n_co_salient), as.character(n_co_all)
)


attr(extra_rows, "position") <- c(30,31)


msummary(
  table2_models,
  coef_map  = coef_map_t2,
  stars     = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map   = gof_t2,
  add_rows  = extra_rows,            
  title     = "Table 1: Institutional moderation of post-ceasefire NTL response",
  notes     = c(
    "Standard errors clustered at the period-ceasefire level.",
    "Fixed effects: country and ceasefire year.",
    "Interest cells: historically active, no signatory event in 6 months pre-ceasefire.",
    "Conflict Salient cells: at least one signatory event in 6 months pre-ceasefire.",
    "IQ_std = V-Dem v2x_liberal (Liberal Component Index), standardised mean 0 SD 1."
  ),
  output = file.path(path_output, "table1_main_results____fixed.docx")
)



# --- 8.3 Robustness ---------------------------------------------

# initial step for R3: adding libdem to panel ----------------------------------------
library(dplyr)
library(data.table)

vdem_raw <- readRDS("Desktop/Thesis/data/raw/V-Dem-CY-Full+Others-v16.rds")
vdem_libdem <- vdem_raw |>
  select(COWcode, year, v2x_libdem) |>
  filter(!is.na(v2x_libdem))

vdem_libdem[COWcode == 679, COWcode := 678]

setDT(panel_other)
setDT(vdem_libdem)

panel_other <- merge(
  panel_other,
  vdem_libdem,
  by.x = c("country_code_fe", "vdem_year"),
  by.y = c("COWcode", "year"),
  all.x = TRUE   # keep all panel rows; libdem = NA where unmatched
)


setdiff(panel_other[!is.na(v2x_liberal), unique(country_code_fe)],
        panel_other[!is.na(v2x_libdem),  unique(country_code_fe)])


panel_other[, libdem_std := scale(v2x_libdem)[, 1]]
panel_other[, post_x_libdem   := post * libdem_std]
panel_other[, libdem_x_period := libdem_std * period]

setDT(panel_other)[, cf_period_id := paste0(uniq_id, "_", period)]

unique(panel_other$country_code_fe) 

# Main formula (PKO included) — reused everywhere except the PKO-out row
f_main <- ln_ntl ~ post + post_x_iq + iq_x_period +
  period + period_post + period_post_sq +
  ln_battle_deaths + ln_gdp_pc + ln_conflict_duration +
  cf_class_cohm + cf_class_definitive + pko_present |
  country_code_fe + cf_year


# --- R1: bandwidth ±90 days (±9 periods) ---
eq_bw90 <- feols(f_main, data = panel_other[abs(period) <= 9], cluster = ~cf_period_id)

# --- R2: bandwidth ±60 days (±6 periods) ---
eq_bw60 <- feols(f_main, data = panel_other[abs(period) <= 6], cluster = ~cf_period_id)

# --- R3: alternative IQ measure (v2x_libdem) ---
# R3: Alternative IQ — v2x_libdem (includes electoral dimension)

# R3 regression
eq_libdem <- feols(
  ln_ntl ~ post + post_x_libdem + libdem_x_period +
    period + period_post + period_post_sq +
    ln_battle_deaths + ln_gdp_pc + ln_conflict_duration +
    cf_class_cohm + cf_class_definitive + pko_present |
    country_code_fe + cf_year,
  data = panel_other, cluster = ~cf_period_id
)
summary(eq_libdem)

# --- R4: excluding Syria ---
eq_noSyria <- feols(f_main,
                    data = panel_other[country_code_fe != 652], cluster = ~cf_period_id)


# --- R5: Donut - dropping the two periods closest to cutoff (±1) ---
eq_donut <- feols(f_main, data = panel_other[abs(period) > 1], cluster = ~cf_period_id)


# --- R6: PKO-out: main spec WITHOUT peacekeeping  ---
f_nopko <- ln_ntl ~ post + post_x_iq + iq_x_period +
  period + period_post + period_post_sq +
  ln_battle_deaths + ln_gdp_pc + ln_conflict_duration +
  cf_class_cohm + cf_class_definitive |
  country_code_fe + cf_year
eq_nopko <- feols(f_nopko, data = panel_other, cluster = ~cf_period_id)

# --- R7: Continuous PKO: troop count instead of binary ---
f_pkocont <- ln_ntl ~ post + post_x_iq + iq_x_period +
  period + period_post + period_post_sq +
  ln_battle_deaths + ln_gdp_pc + ln_conflict_duration +
  cf_class_cohm + cf_class_definitive + ln_pko_troops |
  country_code_fe + cf_year
eq_pkocont <- feols(f_pkocont, data = panel_other, cluster = ~cf_period_id)

eq_libdem$nobs


grab <- function(m, label, term = "post_x_iq") {
  ct <- summary(m)$coeftable
  data.frame(
    Check   = label,
    beta2   = round(ct[term, "Estimate"], 4),
    p_value = round(ct[term, "Pr(>|t|)"], 4),
    N       = round(m$nobs, 0)
  )
}


battery <- rbind(
  grab(eq_main,    "Main (±180, PKO)"),
  grab(eq_bw90,    "R1: ±90 days"),
  grab(eq_bw60,    "R2: ±60 days"),
  grab(eq_libdem,  "R3: v2x_libdem", term = "post_x_libdem"),  
  grab(eq_noSyria, "R4: exclude Syria"),
  grab(eq_donut,   "R5:Donut - dropping ±1"),
  grab(eq_nopko,   "R6: PKO-out"),
  grab(eq_pkocont, "R7: PKO as troop count")
)
print(battery)


# --- 8.4 more Robustness to have ---------------------------------------------
setDT(panel_salient)[, cf_period_id := paste0(uniq_id, "_", period)]
setDT(panel_all)[,     cf_period_id := paste0(uniq_id, "_", period)]

panel_salient <- merge(panel_salient, vdem_libdem,
                       by.x = c("country_code_fe", "vdem_year"),
                       by.y = c("COWcode", "year"), all.x = TRUE)

panel_all <- merge(panel_all, vdem_libdem,
                   by.x = c("country_code_fe", "vdem_year"),
                   by.y = c("COWcode", "year"), all.x = TRUE)


setdiff(panel_salient[!is.na(v2x_liberal), unique(country_code_fe)],
        panel_salient[!is.na(v2x_libdem),  unique(country_code_fe)])
setdiff(panel_all[!is.na(v2x_liberal), unique(country_code_fe)],
        panel_all[!is.na(v2x_libdem),  unique(country_code_fe)])

# Building standardised libdem + interactions in each (mirror panel_other)
for (nm in c("panel_salient", "panel_all")) {
  dt <- get(nm)
  dt[, libdem_std      := scale(v2x_libdem)[, 1]]
  dt[, post_x_libdem   := post * libdem_std]
  dt[, libdem_x_period := libdem_std * period]
  assign(nm, dt)
}




run_battery <- function(dt, geography_label) {
  setDT(dt)
  if (!"cf_period_id" %in% names(dt)) dt[, cf_period_id := paste0(uniq_id, "_", period)]
  
  f_main <- ln_ntl ~ post + post_x_iq + iq_x_period +
    period + period_post + period_post_sq +
    ln_battle_deaths + ln_gdp_pc + ln_conflict_duration +
    cf_class_cohm + cf_class_definitive + pko_present |
    country_code_fe + cf_year
  
  f_nopko <- ln_ntl ~ post + post_x_iq + iq_x_period +
    period + period_post + period_post_sq +
    ln_battle_deaths + ln_gdp_pc + ln_conflict_duration +
    cf_class_cohm + cf_class_definitive |
    country_code_fe + cf_year
  
  f_pkocont <- ln_ntl ~ post + post_x_iq + iq_x_period +
    period + period_post + period_post_sq +
    ln_battle_deaths + ln_gdp_pc + ln_conflict_duration +
    cf_class_cohm + cf_class_definitive + ln_pko_troops |
    country_code_fe + cf_year
  
  f_libdem <- ln_ntl ~ post + post_x_libdem + libdem_x_period +
    period + period_post + period_post_sq +
    ln_battle_deaths + ln_gdp_pc + ln_conflict_duration +
    cf_class_cohm + cf_class_definitive + pko_present |
    country_code_fe + cf_year
  
  models <- list(
    "Main (±180, PKO)"    = list(feols(f_main,   dt,                        cluster = ~cf_period_id), "post_x_iq"),
    "R1: ±90 days"        = list(feols(f_main,   dt[abs(period) <= 9],      cluster = ~cf_period_id), "post_x_iq"),
    "R2: ±60 days"        = list(feols(f_main,   dt[abs(period) <= 6],      cluster = ~cf_period_id), "post_x_iq"),
    "R3: v2x_libdem"      = list(feols(f_libdem, dt,                        cluster = ~cf_period_id), "post_x_libdem"),
    "R4: exclude Syria"   = list(feols(f_main,   dt[country_code_fe != 652],cluster = ~cf_period_id), "post_x_iq"),
    "R5: dropping ±1"      = list(feols(f_main,   dt[abs(period) > 1],       cluster = ~cf_period_id), "post_x_iq"),
    "R6: PKO-out"             = list(feols(f_nopko,  dt,                        cluster = ~cf_period_id), "post_x_iq"),
    "R7: PKO as troop count"  = list(feols(f_pkocont,dt,                        cluster = ~cf_period_id), "post_x_iq")
  )
  
  out <- lapply(names(models), function(lbl) {
    m    <- models[[lbl]][[1]]
    term <- models[[lbl]][[2]]
    ct   <- summary(m)$coeftable
    data.frame(
      Geography = geography_label,
      Check     = lbl,
      beta2     = round(ct[term, "Estimate"], 4),
      p_value   = round(ct[term, "Pr(>|t|)"], 4),
      N         = m$nobs
    )
  })
  do.call(rbind, out)
}

battery_interest <- run_battery(panel_other,   "Interest")
battery_salient  <- run_battery(panel_salient, "Conflict Salient")
battery_all      <- run_battery(panel_all,     "All")

battery_full <- rbind(battery_interest, battery_salient, battery_all)
print(battery_full)



#extra regressionen for Syria only
feols(f_main, panel_salient[country_code_fe == 652], cluster = ~cf_period_id)


#Parallel pre-trend for syria exclusion  robsustness
f_main_pre_trend_test <- ln_ntl ~ iq_x_period + period + 
  ln_gdp_pc + ln_battle_deaths + ln_conflict_duration +
  cf_class_cohm + cf_class_definitive + pko_present |
  country_code_fe + cf_year


feols(f_main_pre_trend_test, panel_salient_pre[country_code_fe != 652],
      cluster = ~cf_period_id)
#now null -> the salient pre-trend is Syria too.



# --- 8.4 Robustness Tables ---------------------------------------------


# Packages
library(dplyr)
library(flextable)
library(officer)

# function to install and load packages

install_and_load_packages <- function(packages) {
  # check for packages that are not installed
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  
  # install missing packages
  if (length(new_packages)) {
    install.packages(new_packages)
    message("The following packages were installed: ", paste(new_packages, collapse = ", "))
  } else {
    message("All packages are already installed.")
  }
  
  # load all the packages
  sapply(packages, require, character.only = TRUE)
}



packages_to_install <- c("officer","flextable")
install_and_load_packages(packages_to_install)


#adding significance stars
battery_full_clean <- battery_full |>
  mutate(
    stars = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.10 ~ "*",
      TRUE ~ ""
    ),
    beta2 = paste0(formatC(beta2, format = "f", digits = 4), stars),
    p_value = formatC(p_value, format = "f", digits = 4),
    N = formatC(N, format = "d", big.mark = ",")
  ) |>
  select(
    Geography,
    Check,
    `Post × IQ / institutional moderator (β₂)` = beta2,
    `p-value` = p_value,
    Observations = N
  )

make_ft <- function(df) {
  flextable(df) |>
    autofit() |>
    theme_booktabs() |>
    fontsize(size = 10, part = "all") |>
    align(align = "center", part = "all") |>
    align(j = "Check", align = "left", part = "body")
}

ft_full <- make_ft(battery_full_clean)

ft_interest <- battery_full_clean |>
  filter(Geography == "Interest") |>
  select(-Geography) |>
  make_ft()

ft_salient <- battery_full_clean |>
  filter(Geography == "Conflict Salient") |>
  select(-Geography) |>
  make_ft()

ft_all <- battery_full_clean |>
  filter(Geography == "All") |>
  select(-Geography) |>
  make_ft()


doc <- read_docx()

doc <- doc |>
  body_add_par("Robustness checks: institutional moderation of post-ceasefire NTL response", style = "heading 1") |>
  body_add_par(
    "The tables report the coefficient on the institutional moderation term from each robustness specification. Standard errors are clustered at the period-ceasefire level. Significance levels: * p < 0.10, ** p < 0.05, *** p < 0.01.",
    style = "Normal"
  ) |>
  body_add_par("Table A1. Full robustness battery", style = "heading 2") |>
  body_add_flextable(ft_full) |>
  body_add_par("", style = "Normal") |>
  body_add_par("Table A2. Robustness checks: Interest cells", style = "heading 2") |>
  body_add_flextable(ft_interest) |>
  body_add_par("", style = "Normal") |>
  body_add_par("Table A3. Robustness checks: Conflict-salient cells", style = "heading 2") |>
  body_add_flextable(ft_salient) |>
  body_add_par("", style = "Normal") |>
  body_add_par("Table A4. Robustness checks: All cells", style = "heading 2") |>
  body_add_flextable(ft_all)

# Saving document
print(
  doc,
  target = file.path(path_output, "robustness_checks_tables.docx")
)












#_____________________________________________________________________________

#extras




#_______________________________
#descriptive statistics table

binned <- panel_other[, .(mean_ntl = mean(ln_ntl, na.rm = TRUE)), by = period][order(period)]
print(binned)
library(data.table)
library(modelsummary)

setDT(panel_all)

Pct_Missing <- function(x) sprintf("%.1f%%", 100 * mean(is.na(x)))

datasummary(
  (`ln(NTL)` = ln_ntl) +
    (`v2x_liberal (raw)` = v2x_liberal) +
    (`Institutional quality (standardised)` = iq_std) +
    (`ln GDP per capita` = ln_gdp_pc) +
    (`ln Battle deaths` = ln_battle_deaths) +
    (`ln Conflict duration` = ln_conflict_duration) +
    (`UN peacekeeping (binary)` = pko_present) ~
    N + Mean + SD + Min + Max + Pct_Missing,
  data   = panel_all,
  title  = "Table X: Summary statistics (full sample)",
  notes  = c(
    "Full analytical sample (all cells with prior signatory activity).",
    "Pct. missing reports the share of panel rows with no valid value; missingness in ln(NTL) reflects quality masking and the minimum-valid-days rule (§3.2.1).",
    "Institutional quality is V-Dem v2x_liberal, standardised to mean 0, SD 1."
  ),
  output = file.path(path_output, "summary_stats_methodology.docx")
)




#_______________________________
#data table ceasefiire
library(data.table)
library(countrycode)
install.packages("countrycode")
library(countrycode)
library(modelsummary)

setDT(panel_all)
cf_level <- unique(
  panel_all[, .(uniq_id, country_code_fe, cf_year,
                cf_class_cohm, cf_class_definitive)]
)

nrow(cf_level)  

cf_level[, region := countrycode(country_code_fe,
                                    origin = "cown",
                                    destination = "region",
                                    custom_match = c("678" = "Asia"))]

cf_level[, cf_class := fifelse(cf_class_cohm == 1, "CoHm",
                               fifelse(cf_class_definitive == 1, "Definitive", "CoH"))]


cf_level[cf_class_cohm == 1 & cf_class_definitive == 1, .N]
cf_level[, .N, by = cf_class]   # three rows, summing to 243



# Ceasefire class shares
class_tab <- cf_level[, .(N = .N, Pct = round(100 * .N / nrow(cf_level), 1)), by = cf_class]
print(class_tab)

# Continent shares
cont_tab <- cf_level[, .(N = .N, Pct = round(100 * .N / nrow(cf_level), 1)), by = region][order(-N)]
print(cont_tab)

# Ceasefires per year
year_tab <- cf_level[, .(N = .N, Pct = round(100 * .N / nrow(cf_level), 1)), by = cf_year][order(cf_year)]
print(year_tab)

datasummary(
  (`Ceasefire class` = cf_class) + (`Region` = region) ~ 
    N + Percent(),
  data = cf_level,
  output = file.path(path_output, "ceasefire_descriptives_2.docx")
)


cf_level[, cf_year_f := factor(cf_year)]
datasummary(
  (`Ceasefire year` = cf_year_f) ~ N + Percent(),
  data = cf_level,
  output = file.path(path_output, "ceasefire_year_dist.docx")
  
)


cf_level[region == "Asia", unique(country_code_fe)]   # identify them
cf_level[country_code_fe == 678, region := "Middle East & North Africa"]  # Yemen

cf_level[, region_grouped := fcase(
  region == "Middle East & North Africa", "Middle East & North Africa",
  region == "Sub-Saharan Africa",          "Sub-Saharan Africa",
  region == "Europe & Central Asia",       "Europe & Central Asia",
  region %in% c("East Asia & Pacific", "South Asia", "Asia"), "Asia",
  region == "Latin America & Caribbean",   "Americas",
  default = region
)]

# Recomputing shares
cf_level[, .(N = .N, Pct = round(100 * .N / nrow(cf_level), 1)),
         by = region_grouped][order(-N)]



datasummary(
  (`Ceasefire class` = cf_class) + (`Region` = region) ~ 
    N + Percent(),
  data = cf_level,
  output = file.path(path_output, "ceasefire_descriptives_2.docx")
)

cf_level[, cf_year_f := factor(cf_year)]
datasummary(
  (`Ceasefire year` = cf_year_f) ~ N + Percent(),
  data = cf_level,
  output = file.path(path_output, "ceasefire_year_dist.docx")
  
)


