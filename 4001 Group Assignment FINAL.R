# ================================================================================
# ACTL 4001 MAIN GROUP ASSIGNMENT — FULL ACTUARIAL PRICING WORKFLOW
# ================================================================================
# AUTHOR:   ChatGPT draft, adapted and validated by student team (Aadi Arora & Nicholas Choi)
# PURPOSE:  End-to-end pricing of four lines of business (BI, Cargo, Equipment,
#           Workers' Compensation) for Cosmic Quarry's new interstellar operations.
# NOTES:
#   - Validate every assumption, parameter, and output before submission.
#   - AI usage must be disclosed per SOA report rules.
#   - The historical data uses Epsilon/Zeta/Helionis Cluster solar systems;
#     new business uses Helionis Cluster/Bayesian System/Oryn Delta.
#     A relativity-factor bridge is used to price across this naming gap.
# ================================================================================

# ================================================================================
# 0. PACKAGE SETUP
# ================================================================================

rm(list = ls())
options(stringsAsFactors = FALSE)

required_packages <- c(
  "readxl",       # reading Excel source files
  "dplyr",        # data manipulation
  "tidyr",        # reshaping (pivot_longer, crossing, etc.)
  "stringr",      # string handling
  "purrr",        # functional iteration
  "ggplot2",      # plotting
  "forcats",      # factor reordering in ggplot
  "lubridate",    # date arithmetic
  "janitor",      # clean_names() — standardises column names to snake_case
  "fitdistrplus", # MLE distribution fitting (lnorm, gamma, weibull)
  "MASS",         # glm.nb() — negative binomial GLM
  "actuar",       # heavy-tailed actuarial distributions
  "mgcv",         # GAMs (not used directly but supports smoothing diagnostics)
  "broom",        # tidy model outputs
  "broom.mixed",  # tidy outputs for mixed-effects models
  "scales",       # axis formatting (dollar_format, etc.)
  "patchwork",    # composing multi-panel ggplots
  "writexl",      # writing Excel outputs
  "glue",         # string interpolation
  "tibble",       # modern data frames
  "DescTools"     # supplementary statistics (e.g., MeanCI, Gini)
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# ================================================================================
# 1. FILE PATHS AND OUTPUT DIRECTORIES
# ================================================================================

path_bi <- "srcsc-2026-claims-business-interruption.xlsx"
path_cargo <- "srcsc-2026-claims-cargo.xlsx"
path_equip <- "srcsc-2026-claims-equipment-failure.xlsx"
path_wc <- "srcsc-2026-claims-workers-comp.xlsx"
path_inventory <- "srcsc-2026-cosmic-quarry-inventory.xlsx"
path_personnel <- "srcsc-2026-cosmic-quarry-personnel.xlsx"
path_macro <- "srcsc-2026-interest-and-inflation.xlsx"

output_dir <- "outputs"
dir.create(output_dir, showWarnings = FALSE)
dir.create(file.path(output_dir, "plots"), showWarnings = FALSE)
dir.create(file.path(output_dir, "tables"), showWarnings = FALSE)

# ================================================================================
# 2. HELPER FUNCTIONS
# ================================================================================

# --- Winsorise a numeric vector at the p-th percentile (default 99.5%).
# Used to cap extreme values before distribution fitting, not before modelling.
winsorise_vec <- function(x, p = 0.995) {
  cap <- quantile(x, p, na.rm = TRUE)
  pmin(x, cap)
}

# --- Safe log: avoids -Inf by flooring near-zero values before taking log.
safe_log <- function(x) log(pmax(x, 1e-8))


# --- RMSE and MAE for model validation.
rmse_fn <- function(actual, pred) {
  sqrt(mean((actual - pred)^2, na.rm = TRUE))
}

mae_fn <- function(actual, pred) {
  mean(abs(actual - pred), na.rm = TRUE)
}

# --- Exposure-weighted mean (used when aggregating across heterogeneous cells).
weighted_mean <- function(x, w) {
  sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
}

# --- Statistical mode (most frequent value).
mode_value <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}


# --- Compact numeric summary with key percentiles.
summ_num <- function(x) {
  tibble(
    n = sum(!is.na(x)),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    p50 = quantile(x, 0.50, na.rm = TRUE),
    p75 = quantile(x, 0.75, na.rm = TRUE),
    p90 = quantile(x, 0.90, na.rm = TRUE),
    p95 = quantile(x, 0.95, na.rm = TRUE),
    p99 = quantile(x, 0.99, na.rm = TRUE),
    max = max(x, na.rm = TRUE)
  )
}

# --- Risk summary including VaR and TVaR at 95%, 99%, and 99.5%.
# TVaR (Tail Value-at-Risk) = expected loss given loss exceeds VaR — the
# primary capital and pricing risk measure used throughout this analysis.
# Variance is included explicitly to satisfy SOA Req 2b.
risk_summary <- function(x) {
  q95 <- unname(quantile(x, 0.95, na.rm = TRUE))
  q99 <- unname(quantile(x, 0.99, na.rm = TRUE))
  q995 <- unname(quantile(x, 0.995, na.rm = TRUE))
  tibble(
    mean = mean(x),
    sd = sd(x),
    var_95 = q95,
    var_99 = q99,
    var_995 = q995,
    tvar_95 = mean(x[x >= q95]),
    tvar_99 = mean(x[x >= q99]),
    tvar_995 = mean(x[x >= q995])
  )
}

# ================================================================================
# 3. DATA LOAD
# ================================================================================
# Each LOB file has two sheets: "freq" (policy-level frequency) and "sev" (claim-level severity).

bi_freq_raw <- read_excel(path_bi, sheet = "freq") %>% clean_names()
bi_sev_raw  <- read_excel(path_bi, sheet = "sev")  %>% clean_names()

cargo_freq_raw <- read_excel(path_cargo, sheet = "freq") %>% clean_names()
cargo_sev_raw  <- read_excel(path_cargo, sheet = "sev")  %>% clean_names()

equip_freq_raw <- read_excel(path_equip, sheet = "freq") %>% clean_names()
equip_sev_raw  <- read_excel(path_equip, sheet = "sev")  %>% clean_names()

wc_freq_raw <- read_excel(path_wc, sheet = "freq") %>% clean_names()
wc_sev_raw  <- read_excel(path_wc, sheet = "sev")  %>% clean_names()

# Support files (unstructured Excel — no headers, parsed manually below).
inventory_raw <- read_excel(path_inventory, sheet = "Equipment", col_names = FALSE)
personnel_raw <- read_excel(path_personnel, sheet = "Personnel", col_names = FALSE)
macro_raw     <- read_excel(path_macro, sheet = 1, col_names = FALSE)

# ================================================================================
# 4. DATA CLEANING AND STANDARDISATION
# ================================================================================
# Important observed issue from the case files:
# - Historical solar systems use Epsilon / Zeta / Helionis Cluster.
# - New business files use Helionis Cluster / Bayesian System / Oryn Delta.
# Therefore, direct one-for-one pricing by solar system is impossible without a
# mapping layer. We therefore:
#   1) fit base technical models using historical risk drivers,
#   2) build a relative solar-system risk index for the new systems,
#   3) price the new systems by applying scenario factors justified from exposure mix.


# 4.1 Helper functions

clean_names_safe <- function(df) {
  janitor::clean_names(df)
}

trim_chr <- function(df) {
  df %>%
    mutate(across(where(is.character), ~ stringr::str_squish(.x)))
}

blank_to_na <- function(df) {
  df %>%
    mutate(across(where(is.character), ~ na_if(.x, "")))
}

normalise_container_colname <- function(df) {
  nm <- names(df)
  nm <- ifelse(nm == "cointainer_type", "container_type", nm)
  names(df) <- nm
  df
}

clean_claim_df <- function(df) {
  df %>%
    clean_names_safe() %>%
    normalise_container_colname() %>%
    trim_chr() %>%
    blank_to_na()
}

existing_cols <- function(df, cols) {
  intersect(cols, names(df))
}

coerce_numeric_if_present <- function(df, cols) {
  cols <- existing_cols(df, cols)
  if (length(cols) == 0) return(df)
  
  df %>%
    mutate(across(all_of(cols), ~ suppressWarnings(as.numeric(.x))))
}

coerce_int <- function(x) {
  x_num <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x_num), NA_real_, round(x_num))
}

cap_numeric <- function(x, lower = -Inf, upper = Inf) {
  x <- suppressWarnings(as.numeric(x))
  x <- ifelse(is.na(x), NA_real_, x)
  x <- pmax(x, lower, na.rm = FALSE)
  x <- pmin(x, upper, na.rm = FALSE)
  x
}

cap_positive_only <- function(x, upper = Inf, lower = 0) {
  x <- suppressWarnings(as.numeric(x))
  x <- ifelse(is.na(x) | x <= 0, NA_real_, x)
  x <- pmax(x, lower, na.rm = FALSE)
  x <- pmin(x, upper, na.rm = FALSE)
  x
}

restrict_to_levels <- function(x, allowed_levels, ignore_case = TRUE) {
  x_chr <- as.character(x)
  
  if (ignore_case) {
    allowed_map <- setNames(allowed_levels, tolower(allowed_levels))
    out <- allowed_map[tolower(x_chr)]
    out[is.na(out)] <- NA_character_
    return(unname(out))
  } else {
    return(ifelse(x_chr %in% allowed_levels, x_chr, NA_character_))
  }
}

safe_factor <- function(x, levels = NULL, ordered = FALSE) {
  factor(x, levels = levels, ordered = ordered)
}

standardise_title_case <- function(x) {
  stringr::str_to_title(as.character(x))
}

mutate_if_present <- function(df, col, fun) {
  if (!col %in% names(df)) return(df)
  
  fun <- rlang::as_function(fun)
  df[[col]] <- fun(df[[col]])
  df
}

# 4.2 Canonical levels from data dictionary

allowed_solar_system_hist <- c("Helionis Cluster", "Epsilon", "Zeta")
allowed_score_1_5 <- c("1", "2", "3", "4", "5")
allowed_binary_flag <- c("0", "1")
allowed_route_risk <- c("1", "2", "3", "4", "5")
allowed_employment_type <- c("Full time", "Contract")
allowed_hours_per_week <- c(20, 25, 30, 35, 40)

allowed_equipment_type <- c(
  "Quantum Bore",
  "Graviton Extractor",
  "FluxStream Carrier",
  "Mag-Lift Aggregator",
  "Fusion Transport",
  "Ion Pulverizer"
)


# 4.3 Common standardisation helpers

standardise_common_ids <- function(df) {
  df %>%
    mutate(across(any_of(c(
      "policy_id", "station_id", "shipment_id", "equipment_id", "worker_id",
      "origin", "destination", "cargo_type", "container_type",
      "injury_type", "injury_cause"
    )), as.character))
}

standardise_common_cats <- function(df) {
  df %>%
    mutate(
      across(any_of("solar_system"), ~ restrict_to_levels(.x, allowed_solar_system_hist)),
      across(any_of("occupation"), standardise_title_case),
      across(any_of("employment_type"), ~ case_when(
        str_to_lower(.x) %in% c("full time", "full-time", "fulltime") ~ "Full time",
        str_to_lower(.x) == "contract" ~ "Contract",
        TRUE ~ .x
      )),
      across(any_of("employment_type"), ~ restrict_to_levels(.x, allowed_employment_type)),
      across(any_of("equipment_type"), ~ case_when(
        .x %in% c("Fluxstream Carrier", "FluxStream Carrier") ~ "FluxStream Carrier",
        .x %in% c("Mag Lift Aggregator", "Mag-Lift Aggregator") ~ "Mag-Lift Aggregator",
        TRUE ~ .x
      )),
      across(any_of("equipment_type"), ~ restrict_to_levels(.x, allowed_equipment_type))
    )
}

factorise_ordered_scores <- function(df) {
  df %>%
    mutate(
      across(any_of(c("energy_backup_score", "safety_compliance",
                      "psych_stress_index", "safety_training_index",
                      "protective_gear_quality")),
             ~ safe_factor(as.character(coerce_int(.x)),
                           levels = allowed_score_1_5, ordered = TRUE)),
      across(any_of("accident_history_flag"),
             ~ safe_factor(as.character(coerce_int(.x)),
                           levels = allowed_binary_flag)),
      across(any_of("route_risk"),
             ~ safe_factor(as.character(coerce_int(.x)),
                           levels = allowed_route_risk, ordered = TRUE)),
      across(any_of("equipment_type"),
             ~ safe_factor(.x, levels = allowed_equipment_type))
    )
}


# 4.4 Business interruption cleaner
# Dictionary:
# production_load 0-1
# energy_backup_score {1,2,3,4,5}
# supply_chain_index 0-1
# avg_crew_exp 1-30
# maintenance_freq 0-6
# safety_compliance {1,2,3,4,5}
# exposure 0-1
# claim_count 0-4
# claim_amount ~28K-1,426K


clean_bi <- function(df) {
  df %>%
    clean_claim_df() %>%
    coerce_numeric_if_present(c(
      "production_load", "energy_backup_score", "supply_chain_index",
      "avg_crew_exp", "maintenance_freq", "safety_compliance",
      "exposure", "claim_count", "claim_amount"
    )) %>%
    standardise_common_ids() %>%
    standardise_common_cats() %>%
    mutate_if_present("production_load", ~ cap_numeric(.x, 0, 1)) %>%
    mutate_if_present("supply_chain_index", ~ cap_numeric(.x, 0, 1)) %>%
    mutate_if_present("avg_crew_exp", ~ cap_numeric(.x, 1, 30)) %>%
    mutate_if_present("maintenance_freq", ~ cap_numeric(.x, 0, 6)) %>%
    mutate_if_present("exposure", ~ cap_numeric(.x, 0, 1)) %>%
    mutate_if_present("energy_backup_score", ~ cap_numeric(coerce_int(.x), 1, 5)) %>%
    mutate_if_present("safety_compliance", ~ cap_numeric(coerce_int(.x), 1, 5)) %>%
    mutate_if_present("claim_count", function(x) {
      x <- suppressWarnings(as.numeric(x))
      x <- ifelse(is.na(x), 0, x)   # <- KEY FIX
      x <- round(x)
      x <- pmax(x, 0)
      x <- pmin(x, 4)   # adjust per dataset
      x
    }) %>%
    mutate_if_present("claim_amount", ~ cap_positive_only(.x, upper = 1426000, lower = 28000)) %>%
    factorise_ordered_scores()
}


# 4.5 Cargo cleaner
# Dictionary:
# cargo_value ~50K-680,000K
# weight 1.5K-250K
# route_risk {1,...,5}
# distance 1-100
# transit_duration 1-60
# pilot_experience 1-30
# vessel_age 1-50
# solar_radiation 0-1
# debris_density 0-1
# exposure 0-1
# claim_count 0-5
# claim_amount ~31K-678,000K


clean_cargo <- function(df) {
  df %>%
    clean_claim_df() %>%
    coerce_numeric_if_present(c(
      "cargo_value", "weight", "route_risk", "distance", "transit_duration",
      "pilot_experience", "vessel_age", "solar_radiation",
      "debris_density", "exposure", "claim_count", "claim_amount"
    )) %>%
    standardise_common_ids() %>%
    standardise_common_cats() %>%
    mutate_if_present("cargo_value", ~ cap_positive_only(.x, upper = 680000000, lower = 50000)) %>%
    mutate_if_present("weight", ~ cap_numeric(.x, 1500, 250000)) %>%
    mutate_if_present("route_risk", ~ cap_numeric(coerce_int(.x), 1, 5)) %>%
    mutate_if_present("distance", ~ cap_numeric(.x, 1, 100)) %>%
    mutate_if_present("transit_duration", ~ cap_numeric(.x, 1, 60)) %>%
    mutate_if_present("pilot_experience", ~ cap_numeric(.x, 1, 30)) %>%
    mutate_if_present("vessel_age", ~ cap_numeric(.x, 1, 50)) %>%
    mutate_if_present("solar_radiation", ~ cap_numeric(.x, 0, 1)) %>%
    mutate_if_present("debris_density", ~ cap_numeric(.x, 0, 1)) %>%
    mutate_if_present("exposure", ~ cap_numeric(.x, 0, 1)) %>%
    mutate_if_present("claim_count", function(x) {
      x <- suppressWarnings(as.numeric(x))
      x <- ifelse(is.na(x), 0, x)   # <- KEY FIX
      x <- round(x)
      x <- pmax(x, 0)
      x <- pmin(x, 5)   # adjust per dataset
      x
    }) %>%
    mutate_if_present("claim_amount", ~ cap_positive_only(.x, upper = 678000000, lower = 31000)) %>%
    {
      df_tmp <- .
      if (all(c("claim_amount", "cargo_value") %in% names(df_tmp))) {
        df_tmp %>%
          mutate(loss_ratio = ifelse(!is.na(claim_amount) & !is.na(cargo_value) & cargo_value > 0,
                                     claim_amount / cargo_value,
                                     NA_real_))
      } else {
        df_tmp
      }
    } %>%
    factorise_ordered_scores()
}


# 4.6 Equipment cleaner
# Dictionary:
# equipment_type in six listed levels
# equipment_age 0-10
# maintenance_int 100-5000
# usage_intensity 0-24
# exposure 0-1
# claim_count 0-3
# claim_amount ~11K-790K


clean_equip <- function(df) {
  df %>%
    clean_claim_df() %>%
    coerce_numeric_if_present(c(
      "equipment_age", "maintenance_int", "usage_intensity",
      "exposure", "claim_count", "claim_amount"
    )) %>%
    standardise_common_ids() %>%
    standardise_common_cats() %>%
    mutate_if_present("equipment_age", ~ cap_numeric(.x, 0, 10)) %>%
    mutate_if_present("maintenance_int", ~ cap_numeric(.x, 100, 5000)) %>%
    mutate_if_present("usage_intensity", ~ cap_numeric(.x, 0, 24)) %>%
    mutate_if_present("exposure", ~ cap_numeric(.x, 0, 1)) %>%
    mutate_if_present("claim_count", function(x) {
      x <- suppressWarnings(as.numeric(x))
      x <- ifelse(is.na(x), 0, x)   # <- KEY FIX
      x <- round(x)
      x <- pmax(x, 0)
      x <- pmin(x, 3)   # adjust per dataset
      x
    }) %>%
    mutate_if_present("claim_amount", ~ cap_positive_only(.x, upper = 790000, lower = 11000)) %>%
    factorise_ordered_scores()
}


# 4.7 Workers' compensation cleaner
# Dictionary:
# employment_type {Full time, Contract}
# experience_yrs ~0.2-40
# accident_history_flag {0,1}
# psych_stress_index {1,...,5}
# hours_per_week {20,25,30,35,40}
# supervision_level 0-1
# gravity_level 0.75-1.50
# safety_training_index {1,...,5}
# protective_gear_quality {1,...,5}
# base_salary ~20K-130K
# exposure 0-1
# claim_count 0-2
# claim_length 3-1000
# claim_amount 5-170


clean_wc <- function(df) {
  df %>%
    clean_claim_df() %>%
    coerce_numeric_if_present(c(
      "experience_yrs", "accident_history_flag", "psych_stress_index",
      "hours_per_week", "supervision_level", "gravity_level",
      "safety_training_index", "protective_gear_quality",
      "base_salary", "exposure", "claim_count", "claim_length", "claim_amount"
    )) %>%
    standardise_common_ids() %>%
    standardise_common_cats() %>%
    mutate_if_present("experience_yrs", ~ cap_numeric(.x, 0.2, 40)) %>%
    mutate_if_present("accident_history_flag", ~ cap_numeric(coerce_int(.x), 0, 1)) %>%
    mutate_if_present("psych_stress_index", ~ cap_numeric(coerce_int(.x), 1, 5)) %>%
    mutate_if_present("hours_per_week", ~ {
      x <- coerce_int(.x)
      x <- ifelse(x < min(allowed_hours_per_week), min(allowed_hours_per_week), x)
      x <- ifelse(x > max(allowed_hours_per_week), max(allowed_hours_per_week), x)
      sapply(x, function(v) {
        if (is.na(v)) return(NA_real_)
        allowed_hours_per_week[which.min(abs(allowed_hours_per_week - v))]
      })
    }) %>%
    mutate_if_present("supervision_level", ~ cap_numeric(.x, 0, 1)) %>%
    mutate_if_present("gravity_level", ~ cap_numeric(.x, 0.75, 1.50)) %>%
    mutate_if_present("safety_training_index", ~ cap_numeric(coerce_int(.x), 1, 5)) %>%
    mutate_if_present("protective_gear_quality", ~ cap_numeric(coerce_int(.x), 1, 5)) %>%
    mutate_if_present("base_salary", ~ cap_numeric(.x, 20000, 130000)) %>%
    mutate_if_present("exposure", ~ cap_numeric(.x, 0, 1)) %>%
    mutate_if_present("claim_count", function(x) {
      x <- suppressWarnings(as.numeric(x))
      x <- ifelse(is.na(x), 0, x)   # <- KEY FIX
      x <- round(x)
      x <- pmax(x, 0)
      x <- pmin(x, 2)   # adjust per dataset
      x
    }) %>%
    mutate_if_present("claim_length", ~ cap_numeric(.x, 3, 1000)) %>%
    mutate_if_present("claim_amount", ~ cap_numeric(.x, 5, 170)) %>%
    factorise_ordered_scores()
}


# 4.8 Apply cleaning immediately after import

bi_freq    <- clean_bi(bi_freq_raw)
bi_sev     <- clean_bi(bi_sev_raw)

cargo_freq <- clean_cargo(cargo_freq_raw)
cargo_sev  <- clean_cargo(cargo_sev_raw)

equip_freq <- clean_equip(equip_freq_raw)
equip_sev  <- clean_equip(equip_sev_raw)

wc_freq    <- clean_wc(wc_freq_raw)
wc_sev     <- clean_wc(wc_sev_raw)


sheet_structure_summary <- tibble(
  dataset = c("bi_freq","bi_sev","cargo_freq","cargo_sev","equip_freq","equip_sev","wc_freq","wc_sev"),
  n_rows = c(nrow(bi_freq), nrow(bi_sev), nrow(cargo_freq), nrow(cargo_sev),
             nrow(equip_freq), nrow(equip_sev), nrow(wc_freq), nrow(wc_sev)),
  n_cols = c(ncol(bi_freq), ncol(bi_sev), ncol(cargo_freq), ncol(cargo_sev),
             ncol(equip_freq), ncol(equip_sev), ncol(wc_freq), ncol(wc_sev))
)

print(names(bi_freq))
print(names(bi_sev))
print(sheet_structure_summary)

# ================================================================================
# 5. EXPLORATORY DATA ANALYSIS
# ================================================================================

eda_frequency <- function(df, lob_name) {
  tibble(
    lob = lob_name,
    records = nrow(df),
    exposure_years = sum(df$exposure, na.rm = TRUE),
    claims = sum(df$claim_count, na.rm = TRUE),
    claim_freq_per_exp = sum(df$claim_count, na.rm = TRUE) / sum(df$exposure, na.rm = TRUE),
    zero_claim_pct = mean(df$claim_count == 0, na.rm = TRUE)
  )
}

eda_severity <- function(df, lob_name) {
  tibble(
    lob = lob_name,
    claims = nrow(df),
    mean_sev = mean(df$claim_amount, na.rm = TRUE),
    median_sev = median(df$claim_amount, na.rm = TRUE),
    p95 = quantile(df$claim_amount, 0.95, na.rm = TRUE),
    p99 = quantile(df$claim_amount, 0.99, na.rm = TRUE),
    max = max(df$claim_amount, na.rm = TRUE)
  )
}

freq_overview <- bind_rows(
  eda_frequency(bi_freq, "Business interruption"),
  eda_frequency(cargo_freq, "Cargo"),
  eda_frequency(equip_freq, "Equipment"),
  eda_frequency(wc_freq, "Workers compensation")
)

sev_overview <- bind_rows(
  eda_severity(bi_sev, "Business interruption"),
  eda_severity(cargo_sev, "Cargo"),
  eda_severity(equip_sev, "Equipment"),
  eda_severity(wc_sev, "Workers compensation")
)

write_xlsx(list(freq_overview = freq_overview, sev_overview = sev_overview),
           file.path(output_dir, "tables", "eda_overview.xlsx"))

# ================================================================================
# 6. NEW BUSINESS EXPOSURE CONSTRUCTION
# ================================================================================

# 6A. Inventory parse
inv_top <- inventory_raw[4:9, 1:4]
colnames(inv_top) <- c("equipment_type", "helionis_cluster", "bayesian_system", "oryn_delta")
inv_counts <- inv_top %>%
  mutate(across(-equipment_type, as.numeric)) %>%
  pivot_longer(-equipment_type, names_to = "solar_system", values_to = "equipment_count")

# Service-year blocks
inv_service <- inventory_raw[14:18, 1:7]
colnames(inv_service) <- c("service_band", "quantum_bores", "graviton_extractors",
                           "fexstram_carriers", "regl_aggregators", "flux_riders", "ion_pulverizers")

# Equipment naming alignment
map_equipment <- c(
  "Quantum Bores" = "quantum_bores",
  "Graviton Extractors" = "graviton_extractors",
  "Fexstram Carriers" = "fexstram_carriers",
  "ReglAggregators" = "regl_aggregators",
  "Regl-Aggregators" = "regl_aggregators",
  "Flux Riders" = "flux_riders",
  "Ion Pulverizers" = "ion_pulverizers",
  "Flux Rider" = "flux_riders"
)

# Approximate mean age by service band midpoint
service_mid <- tibble(
  service_band = c("<5", "5-9", "10-14", "15-19", "20+"),
  mean_age = c(2.5, 7, 12, 17, 22)
)

service_dist_long <- inv_service %>%
  pivot_longer(-service_band,
               names_to = "equipment_type_std",
               values_to = "count") %>%
  mutate(
    count = as.numeric(count)   # <-- FIX
  ) %>%
  left_join(service_mid, by = "service_band") %>%
  group_by(equipment_type_std) %>%
  summarise(
    mean_equipment_age = weighted_mean(mean_age, count),
    .groups = "drop"
  )

inv_counts <- inv_counts %>%
  mutate(equipment_type_std = unname(map_equipment[equipment_type])) %>%
  left_join(service_dist_long, by = "equipment_type_std")

# 6B. Personnel parse
personnel_tbl <- personnel_raw[3:nrow(personnel_raw), 1:6]
colnames(personnel_tbl) <- c("occupation", "num_employees", "full_time", "contract", "avg_salary", "avg_age")
personnel_tbl <- personnel_tbl %>%
  filter(!is.na(occupation)) %>%
  mutate(
    occupation = trimws(as.character(occupation)),
    num_employees = suppressWarnings(as.numeric(num_employees)),
    full_time = suppressWarnings(as.numeric(full_time)),
    contract = suppressWarnings(as.numeric(contract)),
    avg_salary = suppressWarnings(as.numeric(avg_salary)),
    avg_age = suppressWarnings(as.numeric(avg_age))
  ) %>%
  filter(!is.na(num_employees))

# Simple mapping from organisational roles to historical WC occupations
map_occ <- tibble(
  occupation = c("Executive", "Vice President", "Director", "HR", "IT", "Legal",
                 "Finance & Accounting", "Environmental Scientists", "Safety Officer",
                 "Medical Personel", "Engineers", "Pilots", "Cargo Handlers",
                 "Drill Operators", "Technicians", "Geologists", "Data Scientists"),
  wc_occupation = c("Executive", "Manager", "Manager", "Administrator", "Scientist",
                    "Administrator", "Administrator", "Scientist", "Technician",
                    "Scientist", "Engineer", "Pilot", "Technician", "Operator",
                    "Technician", "Scientist", "Scientist")
)

personnel_model <- personnel_tbl %>%
  left_join(map_occ, by = "occupation") %>%
  mutate(
    wc_occupation = ifelse(is.na(wc_occupation), "Technician", wc_occupation),
    contract_share = contract / num_employees,
    full_time_share = full_time / num_employees,
    avg_experience_proxy = pmax(avg_age - 22, 1),
    hours_per_week = ifelse(full_time_share >= 0.75, 40, 32),
    safety_training_index = 4,
    protective_gear_quality = ifelse(str_detect(tolower(occupation), "safety|drill|tech|cargo|pilot"), 4, 3),
    supervision_level = ifelse(wc_occupation %in% c("Executive", "Manager"), 0.2,
                               ifelse(wc_occupation %in% c("Scientist", "Engineer"), 0.5, 0.7)),
    accident_history_flag = 0,
    psych_stress_index = ifelse(wc_occupation %in% c("Pilot", "Engineer", "Operator"), 3, 2)
  )

# Split new personnel across solar systems proportional to equipment counts
solar_weights <- inv_counts %>%
  group_by(solar_system) %>%
  summarise(total_eq = sum(equipment_count), .groups = "drop") %>%
  mutate(weight = total_eq / sum(total_eq)) %>%
  dplyr::select(solar_system, weight)

new_personnel_by_system <- tidyr::crossing(personnel_model, solar_weights) %>%
  mutate(
    num_employees_system = round(num_employees * weight),
    full_time_system = round(full_time * weight),
    contract_system = round(contract * weight)
  )

# ================================================================================
# 7. MACROECONOMIC PARAMETERS
# ================================================================================
# Used throughout for inflation trending and discounting future cash flows.
macro <- macro_raw[3:nrow(macro_raw), 1:5]
colnames(macro) <- c("year", "inflation", "overnight_rate", "rf_1y", "rf_10y")
macro <- macro %>% mutate(across(everything(), as.numeric))

latest_macro <- macro %>% arrange(desc(year)) %>% slice(1)
mean_infl <- mean(macro$inflation, na.rm = TRUE)
long_rf <- mean(tail(macro$rf_10y, 5), na.rm = TRUE)
short_rf <- mean(tail(macro$rf_1y, 5), na.rm = TRUE)


# ================================================================================
# 8. MODELLING FUNCTIONS
# ================================================================================

# ---- 8A. Count (frequency) model fitting ----
# Fits both Poisson and Negative Binomial GLMs; use AIC to select best.
# NB is preferred when overdispersion is present (var > mean in claim counts).
fit_count_models <- function(df, formula_main) {
  poisson_fit <- glm(formula_main, family = poisson(), data = df)
  nb_fit <- MASS::glm.nb(formula_main, data = df)
  list(poisson = poisson_fit, nb = nb_fit)
}

choose_count_model <- function(model_list) {
  aic_tbl <- tibble(
    model = c("poisson", "nb"),
    aic = c(AIC(model_list$poisson), AIC(model_list$nb))
  )
  best_name <- aic_tbl$model[which.min(aic_tbl$aic)]
  list(best_name = best_name, best_model = model_list[[best_name]], aic = aic_tbl)
}

# ---- 8B. Severity distribution fitting ----
# Fits lognormal, gamma, and Weibull to the observed claim amounts using MLE.
# Values above the 99.5th percentile are excluded from fitting to prevent
# extreme observations from distorting parameter estimates; they are still
# included in simulation via the fitted tail.

fit_sev_candidates <- function(x) {
  x <- x[is.finite(x) & !is.na(x) & x > 0]
  
  # optional: remove absurd optimiser-killing extremes for fitting only
  x_fit <- x[x <= quantile(x, 0.995, na.rm = TRUE)]
  
  fit_lnorm <- tryCatch(
    fitdist(x_fit, "lnorm"),
    error = function(e) NULL
  )
  
  fit_gamma <- tryCatch(
    fitdist(
      x_fit,
      "gamma",
      start = list(
        shape = (mean(x_fit)^2) / var(x_fit),
        rate  = mean(x_fit) / var(x_fit)
      )
    ),
    error = function(e) NULL
  )
  
  fit_weibull <- tryCatch(
    fitdist(x_fit, "weibull"),
    error = function(e) NULL
  )
  
  fits <- list(
    lnorm = fit_lnorm,
    gamma = fit_gamma,
    weibull = fit_weibull
  )
  
  # drop failed fits
  fits <- fits[!sapply(fits, is.null)]
  
  return(fits)
}

choose_sev_model <- function(fits) {
  aic_tbl <- tibble(
    model = names(fits),
    aic = sapply(fits, AIC)
  )
  best_name <- aic_tbl$model[which.min(aic_tbl$aic)]
  list(best_name = best_name, best_fit = fits[[best_name]], aic = aic_tbl)
}

# ---- 8C. Pre-computed severity sampler and moment functions ----
# Used inside the hybrid simulation engine for high-volume cargo cells.
# Closures capture parameters at construction time to avoid repeated lookups.

simulate_severity <- function(n, fit_obj, inflation = 0) {
  if (fit_obj$distname == "lnorm") {
    x <- rlnorm(n, meanlog = fit_obj$estimate[["meanlog"]], sdlog = fit_obj$estimate[["sdlog"]])
  } else if (fit_obj$distname == "gamma") {
    x <- rgamma(n, shape = fit_obj$estimate[["shape"]], rate = fit_obj$estimate[["rate"]])
  } else if (fit_obj$distname == "weibull") {
    x <- rweibull(n, shape = fit_obj$estimate[["shape"]], scale = fit_obj$estimate[["scale"]])
  } else {
    stop("Unsupported severity distribution")
  }
  x * (1 + inflation)
}


prepare_model_data <- function(df, response, exposure = NULL) {
  
  df <- df %>%
    mutate(
      # FIX counts
      !!response := ifelse(is.na(.data[[response]]), 0, .data[[response]])
    )
  
  if (!is.null(exposure) && exposure %in% names(df)) {
    df <- df %>%
      mutate(
        !!exposure := ifelse(
          is.na(.data[[exposure]]) | .data[[exposure]] <= 0,
          0.01,
          .data[[exposure]]
        )
      )
  }
  
  # DROP ONLY rows that break GLM
  df <- df %>%
    filter(
      is.finite(.data[[response]]),
      if (!is.null(exposure)) is.finite(.data[[exposure]]) else TRUE
    )
  
  # Drop unused factor levels
  df <- df %>%
    mutate(across(where(is.factor), droplevels))
  
  # Remove completely constant columns (EXCEPT response/exposure)
  df <- df[, sapply(df, function(x) length(unique(x)) > 1)]
  
  df
}

# ================================================================================
# 9. BUSINESS INTERRUPTION MODELS
# ================================================================================


bi_freq_mod <- prepare_model_data(
  bi_freq,
  response = "claim_count",
  exposure = "exposure"
)

bi_sev_mod <- bi_sev %>%
  filter(
    !is.na(claim_amount),
    is.finite(claim_amount),
    claim_amount > 0
  )


bi_count_formula <- claim_count ~ offset(log(exposure)) + production_load + factor(energy_backup_score) +
  supply_chain_index + avg_crew_exp + maintenance_freq + factor(safety_compliance) + factor(solar_system)

bi_count_models <- fit_count_models(bi_freq_mod, bi_count_formula)
bi_count_best <- choose_count_model(bi_count_models)

bi_sev_fits <- fit_sev_candidates(bi_sev_mod$claim_amount)
bi_sev_best <- choose_sev_model(bi_sev_fits)

# Severity relativity by cause-like drivers not available in BI severity dataset,
# so we use frequency differentiation + overall severity distribution.

# ================================================================================
# 10. CARGO MODELS
# ================================================================================
cargo_freq_mod <- prepare_model_data(
  cargo_freq,
  response = "claim_count",
  exposure = "exposure"
)

cargo_sev_mod <- cargo_sev %>%
  filter(
    !is.na(claim_amount),
    is.finite(claim_amount),
    claim_amount > 0
  )

cargo_freq_mod <- cargo_freq_mod %>%
  mutate(log_cargo_value = safe_log(cargo_value),
         log_weight = safe_log(weight),
         log_distance = safe_log(distance))

cargo_count_formula <- claim_count ~ offset(log(exposure)) + cargo_type + log_cargo_value + log_weight +
  factor(route_risk) + log_distance + transit_duration + pilot_experience + vessel_age +
  container_type + solar_radiation + debris_density

cargo_count_models <- fit_count_models(cargo_freq_mod, cargo_count_formula)
cargo_count_best <- choose_count_model(cargo_count_models)

cargo_sev_mod <- cargo_sev_mod %>% mutate(loss_ratio = claim_amount / cargo_value)
cargo_sev_fits <- fit_sev_candidates(cargo_sev_mod$claim_amount)
cargo_sev_best <- choose_sev_model(cargo_sev_fits)

# Loss-ratio model to ensure prices scale sensibly with declared cargo value
cargo_lr_fit <- glm(
  pmin(loss_ratio, 1) ~ cargo_type + factor(route_risk) + safe_log(distance) + transit_duration +
  pilot_experience + vessel_age + container_type + solar_radiation + debris_density,
family = quasibinomial(link = "logit"),
data = cargo_sev_mod
)

# ================================================================================
# 11. EQUIPMENT MODELS
# ================================================================================
equip_freq_mod <- prepare_model_data(
  equip_freq,
  response = "claim_count",
  exposure = "exposure"
)

equip_sev_mod <- equip_sev %>%
  filter(
    !is.na(claim_amount),
    is.finite(claim_amount),
    claim_amount > 0
  )


equip_freq_mod <- equip_freq_mod %>%
  mutate(log_maint = safe_log(maintenance_int),
         utilisation = usage_int / 24)

equip_count_formula <- claim_count ~ offset(log(exposure)) + equipment_type + equipment_age +
  log_maint + utilisation + factor(solar_system)

equip_count_models <- fit_count_models(equip_freq_mod, equip_count_formula)
equip_count_best <- choose_count_model(equip_count_models)

equip_sev_fits <- fit_sev_candidates(equip_sev_mod$claim_amount)
equip_sev_best <- choose_sev_model(equip_sev_fits)


equip_sev_glm <- lm(
  log(claim_amount) ~ equipment_type + equipment_age + safe_log(maintenance_int) + usage_int + factor(solar_system),
  data = equip_sev_mod
)

# ================================================================================
# 12. WORKERS' COMP MODELS
# ================================================================================
wc_freq_mod <- prepare_model_data(
  wc_freq,
  response = "claim_count",
  exposure = "exposure"
)

wc_sev_mod <- wc_sev %>%
  filter(
    !is.na(claim_amount),
    is.finite(claim_amount),
    claim_amount > 0
  )

wc_freq_mod <- wc_freq_mod %>% mutate(log_salary = safe_log(base_salary))

wc_count_formula <- claim_count ~ offset(log(exposure)) + occupation + employment_type + experience_yrs +
  factor(accident_history_flag) + factor(psych_stress_index) + hours_per_week + supervision_level +
  gravity_level + factor(safety_training_index) + factor(protective_gear_quality) + log_salary + factor(solar_system)

wc_count_models <- fit_count_models(wc_freq_mod, wc_count_formula)
wc_count_best <- choose_count_model(wc_count_models)

wc_sev_fits <- fit_sev_candidates(wc_sev_mod$claim_amount)
wc_sev_best <- choose_sev_model(wc_sev_fits)

wc_sev_glm <- lm(
  log(claim_amount) ~ occupation + employment_type + experience_yrs + factor(accident_history_flag) +
    factor(psych_stress_index) + hours_per_week + supervision_level + gravity_level +
    factor(safety_training_index) + factor(protective_gear_quality) + safe_log(base_salary) +
    injury_type + injury_cause + claim_length + factor(solar_system),
  data = wc_sev_mod
)

# ================================================================================
# 13. NEW BUSINESS CONSTRUCTION
# ================================================================================


normalize_key <- function(x) {
  x |>
    as.character() |>
    tolower() |>
    stringr::str_trim() |>
    stringr::str_replace_all("[^a-z0-9]+", "") |>
    stringr::str_replace_all("aggregators", "aggregator") |>
    stringr::str_replace_all("carriers", "carrier") |>
    stringr::str_replace_all("extractors", "extractor") |>
    stringr::str_replace_all("bores", "bore") |>
    stringr::str_replace_all("pulverizers", "pulverizer") |>
    stringr::str_replace_all("riders", "rider")
}

clean_system_name <- function(x) {
  x |>
    as.character() |>
    tolower() |>
    stringr::str_trim() |>
    stringr::str_replace_all("[[:space:]]+", "_")
}

stop_if_unmatched <- function(df, col) {
  if (any(is.na(df[[col]]))) {
    bad_rows <- df |> dplyr::filter(is.na(.data[[col]]))
    print(bad_rows)
    stop(paste0("Unmatched values found in column: ", col))
  }
}

# ================================================================================
# 14. SOLAR SYSTEM RELATIVITIES
# ================================================================================


solar_relativities <- tibble::tibble(
  solar_system = c("helionis_cluster", "bayesian_system", "oryn_delta"),
  equipment_freq_factor = c(0.95, 1.00, 1.15),
  equipment_sev_factor  = c(1.00, 1.05, 1.12),
  bi_freq_factor        = c(1.05, 1.00, 1.20),
  bi_sev_factor         = c(1.05, 1.00, 1.10),
  wc_freq_factor        = c(1.00, 0.95, 1.15),
  wc_sev_factor         = c(1.00, 0.98, 1.12),
  cargo_freq_factor     = c(1.00, 1.05, 1.20),
  cargo_sev_factor      = c(1.00, 1.08, 1.15)
)

# ================================================================================
# 15. EXACT EQUIPMENT LABEL MAPPING TO HISTORICAL LEVELS
# ================================================================================

historical_equipment_levels <- sort(unique(as.character(equip_freq$equipment_type)))
historical_equipment_lookup <- tibble::tibble(
  historical_equipment_type = historical_equipment_levels,
  equipment_key = normalize_key(historical_equipment_levels)
)

observed_equipment_types <- sort(unique(as.character(equip_freq$equipment_type)))
observed_equipment_types <- observed_equipment_types[!is.na(observed_equipment_types)]

print("Observed equipment types in cleaned historical equipment data:")
print(observed_equipment_types)

fallback_equipment_type <- names(sort(table(equip_freq$equipment_type), decreasing = TRUE))[1]

pick_observed_equipment_type <- function(target_std, observed_levels, fallback_level) {
  observed_levels_chr <- as.character(observed_levels)
  obs_lower <- tolower(observed_levels_chr)
  
  find_first <- function(patterns) {
    hits <- which(Reduce(`|`, lapply(patterns, function(p) stringr::str_detect(obs_lower, p))))
    if (length(hits) > 0) {
      observed_levels_chr[hits[1]]
    } else {
      NA_character_
    }
  }
  
  out <- dplyr::case_when(
    target_std == "quantum_bores" ~ find_first(c("quantum", "bore")),
    target_std == "graviton_extractors" ~ find_first(c("graviton", "extractor")),
    target_std == "fexstram_carriers" ~ find_first(c("carrier", "transport", "flux")),
    target_std == "regl_aggregators" ~ find_first(c("aggregator", "mag", "transport")),
    target_std == "flux_riders" ~ find_first(c("transport", "carrier", "flux")),
    target_std == "ion_pulverizers" ~ find_first(c("ion", "pulverizer")),
    TRUE ~ NA_character_
  )
  
  if (is.na(out)) fallback_level else out
}

equipment_mapping <- tibble::tibble(
  equipment_type_std = c(
    "quantum_bores",
    "graviton_extractors",
    "fexstram_carriers",
    "regl_aggregators",
    "flux_riders",
    "ion_pulverizers"
  )
) %>%
  dplyr::mutate(
    historical_equipment_type = purrr::map_chr(
      equipment_type_std,
      ~ pick_observed_equipment_type(.x, observed_equipment_types, fallback_equipment_type)
    )
  )

print("Final equipment mapping used for modelling:")
print(equipment_mapping)

bad_equipment_map <- setdiff(
  equipment_mapping$historical_equipment_type,
  observed_equipment_types
)

if (length(bad_equipment_map) > 0) {
  print("These mapped equipment types still do not exist in observed historical data:")
  print(bad_equipment_map)
  stop("Fix equipment_mapping - mapped types must exist in observed equip_freq$equipment_type")
}

# ================================================================================
# 16. NEW EQUIPMENT PORTFOLIO
# ================================================================================

valid_equipment_std <- c(
  "quantum_bores",
  "graviton_extractors",
  "fexstram_carriers",
  "regl_aggregators",
  "flux_riders",
  "ion_pulverizers"
)

new_equip <- inv_counts %>%
  dplyr::filter(equipment_type_std %in% equipment_mapping$equipment_type_std) %>%
  dplyr::left_join(equipment_mapping, by = "equipment_type_std") %>%
  dplyr::transmute(
    equipment_type = historical_equipment_type,
    solar_system = clean_system_name(solar_system),
    equipment_count = as.numeric(equipment_count),
    equipment_age = as.numeric(mean_equipment_age),
    maintenance_int = median(equip_freq$maintenance_int, na.rm = TRUE),
    usage_int = median(equip_freq$usage_int, na.rm = TRUE),
    exposure = 1
  ) %>%
  dplyr::mutate(
    log_maint = safe_log(maintenance_int),
    utilisation = usage_int / 24
  ) %>%
  dplyr::left_join(solar_relativities, by = "solar_system")

new_equip <- new_equip %>%
  dplyr::mutate(
    equipment_type = factor(
      equipment_type,
      levels = unique(as.character(equip_freq$equipment_type))
    )
  )

stop_if_unmatched(new_equip, "equipment_type")
stop_if_unmatched(new_equip, "equipment_freq_factor")
stop_if_unmatched(new_equip, "equipment_sev_factor")

new_equip <- new_equip |>
  dplyr::mutate(
    equipment_type = factor(equipment_type, levels = levels(factor(equip_freq$equipment_type))),
    solar_system_model = factor(
      mode_value(as.character(equip_freq$solar_system)),
      levels = levels(factor(equip_freq$solar_system))
    )
  )

# Use solar_system_model for prediction to avoid unseen future solar system factor levels.
new_equip_pred <- new_equip |>
  dplyr::transmute(
    equipment_type = equipment_type,
    equipment_age = equipment_age,
    log_maint = log_maint,
    utilisation = utilisation,
    exposure = exposure,
    solar_system = solar_system_model
  )

new_equip$base_lambda <- as.numeric(
  predict(equip_count_best$best_model, newdata = new_equip_pred, type = "response")
)

new_equip$adj_lambda <- new_equip$base_lambda * new_equip$equipment_freq_factor

# Equipment severity relativity
# Predict using historical solar system reference level, then apply future system severity factor externally.
new_equip_sev_pred <- new_equip |>
  dplyr::transmute(
    equipment_type = equipment_type,
    equipment_age = equipment_age,
    maintenance_int = maintenance_int,
    usage_int = usage_int,
    solar_system = solar_system_model
  )

equip_sev_ref_pred <- exp(predict(equip_sev_glm, newdata = new_equip_sev_pred))
equip_sev_hist_mean <- exp(mean(predict(equip_sev_glm, newdata = equip_sev), na.rm = TRUE))

new_equip$sev_rel <- equip_sev_ref_pred / equip_sev_hist_mean
new_equip$sev_rel[!is.finite(new_equip$sev_rel)] <- 1
new_equip$adj_sev_factor <- new_equip$sev_rel * new_equip$equipment_sev_factor

# ================================================================================
# 17. SOLAR WEIGHTS FOR PERSONNEL ALLOCATION
# ================================================================================

solar_weights <- inv_counts |>
  dplyr::group_by(solar_system) |>
  dplyr::summarise(total_eq = sum(as.numeric(equipment_count), na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(
    solar_system = clean_system_name(solar_system),
    weight = total_eq / sum(total_eq)
  ) |>
  dplyr::select(solar_system, weight)

# ================================================================================
# 18. PERSONNEL / WC PORTFOLIO
# ================================================================================

normalize_key <- function(x) {
  x |>
    as.character() |>
    tolower() |>
    stringr::str_trim() |>
    stringr::str_replace_all("&", "and") |>
    stringr::str_replace_all("[^a-z0-9]+", "")
}

historical_occ <- sort(unique(as.character(wc_freq$occupation)))

historical_occ_tbl <- tibble::tibble(
  historical_occ = historical_occ,
  occ_key = normalize_key(historical_occ)
)

# target role classes 
target_classes <- tibble::tibble(
  target_class = c(
    "executive",
    "manager",
    "administrator",
    "scientist",
    "technician",
    "engineer",
    "pilot",
    "operator"
  )
)

target_to_hist <- target_classes |>
  dplyr::mutate(
    historical_occ = purrr::map_chr(target_class, function(tc) {
      exact_match <- historical_occ_tbl |>
        dplyr::filter(occ_key == normalize_key(tc)) |>
        dplyr::pull(historical_occ)
      
      if (length(exact_match) > 0) return(exact_match[1])
      
      partial_match <- historical_occ_tbl |>
        dplyr::filter(stringr::str_detect(occ_key, normalize_key(tc))) |>
        dplyr::pull(historical_occ)
      
      if (length(partial_match) > 0) return(partial_match[1])
      
      if (tc == "manager") {
        pm <- historical_occ_tbl |>
          dplyr::filter(stringr::str_detect(occ_key, "manager|director|supervisor|lead")) |>
          dplyr::pull(historical_occ)
        if (length(pm) > 0) return(pm[1])
      }
      
      if (tc == "administrator") {
        pm <- historical_occ_tbl |>
          dplyr::filter(stringr::str_detect(occ_key, "admin|finance|legal|account|hr")) |>
          dplyr::pull(historical_occ)
        if (length(pm) > 0) return(pm[1])
      }
      
      if (tc == "scientist") {
        pm <- historical_occ_tbl |>
          dplyr::filter(stringr::str_detect(occ_key, "scient|geolog|medical|data")) |>
          dplyr::pull(historical_occ)
        if (length(pm) > 0) return(pm[1])
      }
      
      if (tc == "technician") {
        pm <- historical_occ_tbl |>
          dplyr::filter(stringr::str_detect(occ_key, "techn")) |>
          dplyr::pull(historical_occ)
        if (length(pm) > 0) return(pm[1])
      }
      
      if (tc == "engineer") {
        pm <- historical_occ_tbl |>
          dplyr::filter(stringr::str_detect(occ_key, "engine")) |>
          dplyr::pull(historical_occ)
        if (length(pm) > 0) return(pm[1])
      }
      
      if (tc == "pilot") {
        pm <- historical_occ_tbl |>
          dplyr::filter(stringr::str_detect(occ_key, "pilot")) |>
          dplyr::pull(historical_occ)
        if (length(pm) > 0) return(pm[1])
      }
      
      if (tc == "operator") {
        pm <- historical_occ_tbl |>
          dplyr::filter(stringr::str_detect(occ_key, "operator|handler|drill|cargo")) |>
          dplyr::pull(historical_occ)
        if (length(pm) > 0) return(pm[1])
      }
      
      names(sort(table(wc_freq$occupation), decreasing = TRUE))[1]
    })
  )

personnel_occ_lookup <- tibble::tibble(
  occupation = c(
    "Executive",
    "Vice President",
    "Director",
    "HR",
    "IT",
    "Legal",
    "Finance & Accounting",
    "Environmental Scientists",
    "Safety Officer",
    "Medical Personel",
    "Medical Personnel",
    "Engineers",
    "Pilots",
    "Cargo Handlers",
    "Drill Operators",
    "Technicians",
    "Geologists",
    "Data Scientists"
  ),
  target_class = c(
    "executive",
    "manager",
    "manager",
    "administrator",
    "scientist",
    "administrator",
    "administrator",
    "scientist",
    "technician",
    "scientist",
    "scientist",
    "engineer",
    "pilot",
    "operator",
    "operator",
    "technician",
    "scientist",
    "scientist"
  )
) |>
  dplyr::left_join(target_to_hist, by = "target_class") |>
  dplyr::transmute(
    occupation,
    wc_occupation = historical_occ
  )

bad_occ <- setdiff(
  unique(as.character(personnel_occ_lookup$wc_occupation)),
  unique(as.character(wc_freq$occupation))
)

if (length(bad_occ) > 0) {
  print(bad_occ)
  stop("Some mapped occupations still do not exist in wc_freq$occupation")
}

print(personnel_occ_lookup)

personnel_model <- personnel_tbl %>%
  dplyr::left_join(personnel_occ_lookup, by = "occupation") %>%
  dplyr::mutate(
    wc_occupation = ifelse(
      is.na(wc_occupation),
      names(sort(table(wc_freq$occupation), decreasing = TRUE))[1],
      wc_occupation
    ),
    contract_share = dplyr::if_else(num_employees > 0, contract / num_employees, 0),
    full_time_share = dplyr::if_else(num_employees > 0, full_time / num_employees, 1),
    avg_experience_proxy = pmax(avg_age - 22, 1),
    hours_per_week = dplyr::if_else(full_time_share >= 0.75, 40, 32),
    safety_training_index = 4,
    protective_gear_quality = dplyr::if_else(
      stringr::str_detect(tolower(occupation), "safety|drill|tech|cargo|pilot"),
      4, 3
    ),
    supervision_level = dplyr::case_when(
      wc_occupation %in% c("Executive", "Manager") ~ 0.2,
      wc_occupation %in% c("Scientist", "Engineer") ~ 0.5,
      TRUE ~ 0.7
    ),
    accident_history_flag = 0,
    psych_stress_index = dplyr::if_else(
      wc_occupation %in% c("Pilot", "Engineer", "Operator"),
      3, 2
    )
  )

stop_if_unmatched(personnel_model, "wc_occupation")

new_personnel_by_system <- tidyr::crossing(personnel_model, solar_weights) %>%
  dplyr::mutate(
    num_employees_system = round(num_employees * weight),
    full_time_system = round(full_time * weight),
    contract_system = round(contract * weight)
  )

new_personnel_by_system <- tidyr::crossing(personnel_model, solar_weights) |>
  mutate(
    num_employees_system = round(num_employees * weight),
    full_time_system = round(full_time * weight),
    contract_system = round(contract * weight)
  )

new_wc <- new_personnel_by_system %>%
  dplyr::mutate(
    contract_share_system = dplyr::if_else(
      num_employees_system > 0,
      contract_system / num_employees_system,
      0
    ),
    full_time_share_system = dplyr::if_else(
      num_employees_system > 0,
      full_time_system / num_employees_system,
      1
    ),
    solar_system_clean = clean_system_name(solar_system),
    employment_type = ifelse(contract_share_system > 0.25, "Contract", "Full-time"),
    experience_yrs = avg_experience_proxy,
    gravity_level = dplyr::case_when(
      solar_system_clean == "helionis_cluster" ~ 1.05,
      solar_system_clean == "bayesian_system" ~ 0.95,
      solar_system_clean == "oryn_delta" ~ 0.82,
      TRUE ~ 1.00
    ),
    employee_count = pmax(num_employees_system, 0)
  ) %>%
  dplyr::filter(employee_count > 0) %>%
  dplyr::transmute(
    solar_system = solar_system_clean,
    occupation = wc_occupation,
    employment_type = employment_type,
    experience_yrs = experience_yrs,
    accident_history_flag = accident_history_flag,
    psych_stress_index = psych_stress_index,
    hours_per_week = hours_per_week,
    supervision_level = supervision_level,
    gravity_level = gravity_level,
    safety_training_index = safety_training_index,
    protective_gear_quality = protective_gear_quality,
    base_salary = avg_salary,
    exposure = 1,
    employee_count = employee_count
  ) %>%
  dplyr::mutate(
    log_salary = safe_log(base_salary)
  ) %>%
  dplyr::left_join(solar_relativities, by = "solar_system")


# Align factor levels to historical model levels
new_wc <- new_wc |>
  dplyr::mutate(
    occupation = factor(occupation, levels = levels(factor(wc_freq$occupation))),
    employment_type = factor(employment_type, levels = levels(factor(wc_freq$employment_type))),
    solar_system_model = factor(
      mode_value(as.character(wc_freq$solar_system)),
      levels = levels(factor(wc_freq$solar_system))
    )
  )

# Frequency prediction
new_wc_pred <- new_wc |>
  dplyr::transmute(
    occupation = occupation,
    employment_type = employment_type,
    experience_yrs = experience_yrs,
    accident_history_flag = accident_history_flag,
    psych_stress_index = psych_stress_index,
    hours_per_week = hours_per_week,
    supervision_level = supervision_level,
    gravity_level = gravity_level,
    safety_training_index = safety_training_index,
    protective_gear_quality = protective_gear_quality,
    log_salary = log_salary,
    exposure = exposure,
    solar_system = solar_system_model
  )

new_wc$base_lambda <- as.numeric(
  predict(wc_count_best$best_model, newdata = new_wc_pred, type = "response")
)

new_wc$adj_lambda <- new_wc$base_lambda * new_wc$wc_freq_factor

# Severity relativity for WC
wc_occ_sev_rel <- wc_sev |>
  dplyr::group_by(occupation, employment_type) |>
  dplyr::summarise(
    mean_claim = mean(claim_amount, na.rm = TRUE),
    .groups = "drop"
  )

wc_overall_mean <- mean(wc_sev$claim_amount, na.rm = TRUE)

new_wc <- new_wc |>
  dplyr::left_join(wc_occ_sev_rel, by = c("occupation", "employment_type")) |>
  dplyr::mutate(
    mean_claim = ifelse(is.na(mean_claim), wc_overall_mean, mean_claim),
    salary_rel = sqrt(base_salary / mean(wc_freq$base_salary, na.rm = TRUE)),
    sev_rel = (mean_claim / wc_overall_mean) * salary_rel,
    adj_sev_factor = sev_rel * wc_sev_factor
  )

# ================================================================================
# 19. BUSINESS INTERRUPTION PORTFOLIO
# ================================================================================

bi_system_exposure <- inv_counts |>
  dplyr::group_by(solar_system) |>
  dplyr::summarise(total_equipment = sum(as.numeric(equipment_count), na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(
    solar_system = clean_system_name(solar_system),
    station_count_proxy = c(12, 8, 6),
    production_load = c(0.88, 0.82, 0.91),
    energy_backup_score = c(4, 4, 3),
    supply_chain_index = c(0.72, 0.60, 0.48),
    avg_crew_exp = c(12, 11, 9),
    maintenance_freq = c(4, 4, 3),
    safety_compliance = c(4, 4, 3),
    exposure = 1
  ) |>
  dplyr::left_join(solar_relativities, by = "solar_system")

stop_if_unmatched(bi_system_exposure, "bi_freq_factor")
stop_if_unmatched(bi_system_exposure, "bi_sev_factor")

bi_system_exposure <- bi_system_exposure |>
  dplyr::mutate(
    solar_system_model = factor(
      mode_value(as.character(bi_freq$solar_system)),
      levels = levels(factor(bi_freq$solar_system))
    )
  )

bi_pred_data <- bi_system_exposure |>
  dplyr::transmute(
    production_load = production_load,
    energy_backup_score = energy_backup_score,
    supply_chain_index = supply_chain_index,
    avg_crew_exp = avg_crew_exp,
    maintenance_freq = maintenance_freq,
    safety_compliance = safety_compliance,
    exposure = exposure,
    solar_system = solar_system_model
  )

bi_system_exposure$base_lambda <- as.numeric(
  predict(bi_count_best$best_model, newdata = bi_pred_data, type = "response")
)

# Scale BI by system size
bi_system_exposure$adj_lambda <- bi_system_exposure$base_lambda *
  bi_system_exposure$bi_freq_factor *
  (bi_system_exposure$total_equipment / 100)

bi_system_exposure$adj_sev_factor <- bi_system_exposure$bi_sev_factor * c(1.05, 1.00, 1.15)

# ================================================================================
# 20. CARGO PORTFOLIO
# ================================================================================

cargo_mix <- cargo_freq %>%
  dplyr::count(cargo_type, sort = TRUE) %>%
  dplyr::mutate(weight = n / sum(n))

cargo_mix <- cargo_mix %>%
  dplyr::slice_head(n = min(6, nrow(cargo_mix)))

container_mode <- mode_value(cargo_freq$container_type)

route_risk_by_system <- tibble::tibble(
  solar_system = c("helionis_cluster", "bayesian_system", "oryn_delta"),
  route_risk = c(3, 4, 5),
  distance = c(18, 24, 36),
  transit_duration = c(8, 11, 16),
  pilot_experience = c(14, 12, 10),
  vessel_age = c(10, 12, 15),
  solar_radiation = c(0.32, 0.36, 0.44),
  debris_density = c(0.20, 0.28, 0.41),
  cargo_value = c(18e6, 20e6, 24e6),
  weight_val = c(90e3, 100e3, 110e3),
  shipment_count = c(2500, 1800, 1400)
)

new_cargo <- tidyr::crossing(route_risk_by_system, cargo_mix) |>
  dplyr::transmute(
    solar_system = clean_system_name(solar_system),
    cargo_type = cargo_type,
    cargo_value = cargo_value,
    weight = weight_val,
    route_risk = route_risk,
    distance = distance,
    transit_duration = transit_duration,
    pilot_experience = pilot_experience,
    vessel_age = vessel_age,
    container_type = container_mode,
    solar_radiation = solar_radiation,
    debris_density = debris_density,
    exposure = 1,
    shipment_count = pmax(round(shipment_count * weight), 1),
    log_cargo_value = safe_log(cargo_value),
    log_weight = safe_log(weight),
    log_distance = safe_log(distance)
  ) |>
  dplyr::left_join(solar_relativities, by = "solar_system")

stop_if_unmatched(new_cargo, "cargo_freq_factor")
stop_if_unmatched(new_cargo, "cargo_sev_factor")

# align factors
new_cargo <- new_cargo |>
  dplyr::mutate(
    cargo_type = factor(cargo_type, levels = levels(factor(cargo_freq$cargo_type))),
    route_risk = route_risk,
    container_type = factor(container_type, levels = levels(factor(cargo_freq$container_type)))
  )

new_cargo$base_lambda <- as.numeric(
  predict(cargo_count_best$best_model, newdata = new_cargo, type = "response")
)

new_cargo$adj_lambda <- new_cargo$base_lambda * new_cargo$cargo_freq_factor

new_cargo$pred_lr <- as.numeric(
  predict(cargo_lr_fit, newdata = new_cargo, type = "response")
)

new_cargo$adj_sev_factor <- (new_cargo$pred_lr / mean(cargo_sev$loss_ratio, na.rm = TRUE)) *
  new_cargo$cargo_sev_factor

new_cargo$adj_sev_factor <- pmax(new_cargo$adj_sev_factor, 0.25)

# ================================================================================
# 21. PORTFOLIO SIMULATION ENGINE
# ================================================================================

simulate_lob_losses <- function(exposure_df, count_col, unit_col, sev_fit, sev_factor_col,
                                sims = 10000, inflation = mean_infl, dependency_mult = 1) {
  
  n_rows <- nrow(exposure_df)
  
  lambda_vec <- exposure_df[[count_col]] * exposure_df[[unit_col]] * dependency_mult
  sev_factor_vec <- exposure_df[[sev_factor_col]]
  
  out <- numeric(sims)
  
  for (i in seq_len(sims)) {
    claim_counts <- rpois(n_rows, lambda = lambda_vec)
    
    total_loss <- 0
    
    for (j in which(claim_counts > 0)) {
      n_claims <- claim_counts[j]
      sev <- simulate_severity(n_claims, sev_fit, inflation = inflation)
      total_loss <- total_loss + sum(sev) * sev_factor_vec[j]
    }
    
    out[i] <- total_loss
  }
  
  out
}

set.seed(123)

equip_losses_base <- simulate_lob_losses(
  new_equip, "adj_lambda", "equipment_count",
  equip_sev_best$best_fit, "adj_sev_factor", sims = 10000
)

wc_losses_base <- simulate_lob_losses(
  new_wc, "adj_lambda", "employee_count",
  wc_sev_best$best_fit, "adj_sev_factor", sims = 10000
)

bi_losses_base <- simulate_lob_losses(
  bi_system_exposure, "adj_lambda", "station_count_proxy",
  bi_sev_best$best_fit, "adj_sev_factor", sims = 10000
)


make_sev_sampler <- function(fit_obj, inflation = 0) {
  
  if (fit_obj$distname == "lnorm") {
    meanlog <- fit_obj$estimate[["meanlog"]]
    sdlog <- fit_obj$estimate[["sdlog"]]
    
    function(n) {
      rlnorm(n, meanlog = meanlog, sdlog = sdlog) * (1 + inflation)
    }
    
  } else if (fit_obj$distname == "gamma") {
    shape <- fit_obj$estimate[["shape"]]
    rate  <- fit_obj$estimate[["rate"]]
    
    function(n) {
      rgamma(n, shape = shape, rate = rate) * (1 + inflation)
    }
    
  } else if (fit_obj$distname == "weibull") {
    shape <- fit_obj$estimate[["shape"]]
    scale <- fit_obj$estimate[["scale"]]
    
    function(n) {
      rweibull(n, shape = shape, scale = scale) * (1 + inflation)
    }
    
  } else {
    stop("Unsupported severity distribution")
  }
}

make_sev_moments <- function(fit_obj, inflation = 0, n_sim = 200000) {
  sev_sampler <- make_sev_sampler(fit_obj, inflation)
  x <- sev_sampler(n_sim)
  list(
    mean = mean(x),
    var = var(x)
  )
}

simulate_lob_losses_hybrid <- function(exposure_df, count_col, unit_col, sev_fit, sev_factor_col,
                                       sims = 1000, inflation = mean_infl, dependency_mult = 1,
                                       count_threshold = 200) {
  
  lambda_vec <- exposure_df[[count_col]] * exposure_df[[unit_col]] * dependency_mult
  sev_factor_vec <- exposure_df[[sev_factor_col]]
  sev_sampler <- make_sev_sampler(sev_fit, inflation = inflation)
  sev_mom <- make_sev_moments(sev_fit, inflation = inflation)
  
  out <- numeric(sims)
  
  for (i in seq_len(sims)) {
    counts <- rpois(length(lambda_vec), lambda_vec)
    total_loss <- 0
    
    idx <- which(counts > 0)
    if (length(idx) == 0) {
      out[i] <- 0
      next
    }
    
    for (j in idx) {
      n <- counts[j]
      sf <- sev_factor_vec[j]
      
      if (n <= count_threshold) {
        total_loss <- total_loss + sum(sev_sampler(n)) * sf
      } else {
        agg_mean <- n * sev_mom$mean
        agg_sd <- sqrt(n * sev_mom$var)
        approx_loss <- rnorm(1, mean = agg_mean, sd = agg_sd)
        total_loss <- total_loss + max(approx_loss, 0) * sf
      }
    }
    
    out[i] <- total_loss
  }
  
  out
}

cargo_losses_base <- simulate_lob_losses_hybrid(
  new_cargo,
  "adj_lambda",
  "shipment_count",
  cargo_sev_best$best_fit,
  "adj_sev_factor",
  sims = 10000,
  inflation = mean_infl,
  dependency_mult = 1,
  count_threshold = 200
)


portfolio_base <- equip_losses_base + wc_losses_base + bi_losses_base + cargo_losses_base

# ================================================================================
# 22. SCENARIO FRAMEWORK
# ================================================================================

simulate_all_scenarios <- function() {
  best <- list(
    equip = simulate_lob_losses(
      new_equip, "adj_lambda", "equipment_count",
      equip_sev_best$best_fit, "adj_sev_factor",
      sims = 10000, inflation = mean_infl * 0.75, dependency_mult = 0.80
    ),
    wc = simulate_lob_losses(
      new_wc, "adj_lambda", "employee_count",
      wc_sev_best$best_fit, "adj_sev_factor",
      sims = 10000, inflation = mean_infl * 0.75, dependency_mult = 0.85
    ),
    bi = simulate_lob_losses(
      bi_system_exposure, "adj_lambda", "station_count_proxy",
      bi_sev_best$best_fit, "adj_sev_factor",
      sims = 10000, inflation = mean_infl * 0.75, dependency_mult = 0.85
    ),
    cargo = simulate_lob_losses_hybrid(
      new_cargo, "adj_lambda", "shipment_count",
      cargo_sev_best$best_fit, "adj_sev_factor",
      sims = 10000, inflation = mean_infl * 0.75, dependency_mult = 0.85
    )
  )
  
  moderate <- list(
    equip = equip_losses_base,
    wc = wc_losses_base,
    bi = bi_losses_base,
    cargo = cargo_losses_base
  )
  
  worst <- list(
    equip = simulate_lob_losses(
      new_equip, "adj_lambda", "equipment_count",
      equip_sev_best$best_fit, "adj_sev_factor",
      sims = 10000, inflation = mean_infl * 1.40, dependency_mult = 1.60
    ),
    wc = simulate_lob_losses(
      new_wc, "adj_lambda", "employee_count",
      wc_sev_best$best_fit, "adj_sev_factor",
      sims = 10000, inflation = mean_infl * 1.30, dependency_mult = 1.45
    ),
    bi = simulate_lob_losses(
      bi_system_exposure, "adj_lambda", "station_count_proxy",
      bi_sev_best$best_fit, "adj_sev_factor",
      sims = 10000, inflation = mean_infl * 1.50, dependency_mult = 1.85
    ),
    cargo = simulate_lob_losses_hybrid(
      new_cargo, "adj_lambda", "shipment_count",
      cargo_sev_best$best_fit, "adj_sev_factor",
      sims = 10000, inflation = mean_infl * 1.35, dependency_mult = 1.70
    )
  )
  
  list(best = best, moderate = moderate, worst = worst)
}

scenario_sims <- simulate_all_scenarios()

combine_scenario <- function(lst) {
  lst$equip + lst$wc + lst$bi + lst$cargo
}

portfolio_best  <- combine_scenario(scenario_sims$best)
portfolio_mod   <- combine_scenario(scenario_sims$moderate)
portfolio_worst <- combine_scenario(scenario_sims$worst)

portfolio_1_in_100 <- unname(quantile(portfolio_worst, 0.99))

# ================================================================================
# 23. PRICING
# ================================================================================

price_from_losses <- function(losses, expense_ratio = 0.12, profit_ratio = 0.08, capital_ratio = 0.10) {
  rs <- risk_summary(losses)
  expected_loss <- rs$mean
  risk_margin <- 0.20 * (rs$tvar_99 - rs$mean)
  technical_premium <- expected_loss + risk_margin
  gross_premium <- technical_premium / (1 - expense_ratio - profit_ratio - capital_ratio)
  
  tibble::tibble(
    expected_loss = expected_loss,
    risk_margin = risk_margin,
    technical_premium = technical_premium,
    gross_premium = gross_premium,
    expense_ratio = expense_ratio,
    profit_ratio = profit_ratio,
    capital_ratio = capital_ratio
  )
}

pricing_table <- dplyr::bind_rows(
  price_from_losses(equip_losses_base) |> dplyr::mutate(lob = "Equipment failure"),
  price_from_losses(cargo_losses_base) |> dplyr::mutate(lob = "Cargo loss"),
  price_from_losses(wc_losses_base)    |> dplyr::mutate(lob = "Workers compensation"),
  price_from_losses(bi_losses_base)    |> dplyr::mutate(lob = "Business interruption"),
  price_from_losses(portfolio_base)    |> dplyr::mutate(lob = "Total portfolio")
) |>
  dplyr::select(lob, dplyr::everything())

# ================================================================================
# 24. SHORT-TERM AND LONG-TERM ECONOMICS
# ================================================================================

discount_factor <- function(t, r) {
  1 / (1 + r)^t
}

project_multiyear <- function(base_premium, base_loss_mean, years = 10,
                              premium_growth = 0.03, loss_trend = mean_infl,
                              disc_rate = long_rf) {
  tibble::tibble(year = 1:years) |>
    dplyr::mutate(
      premium = base_premium * (1 + premium_growth)^(year - 1),
      cost = base_loss_mean * (1 + loss_trend)^(year - 1),
      net_revenue = premium - cost,
      pv_premium = premium * discount_factor(year, disc_rate),
      pv_cost = cost * discount_factor(year, disc_rate),
      pv_net_revenue = net_revenue * discount_factor(year, disc_rate)
    )
}

long_term_tables <- purrr::map_dfr(seq_len(nrow(pricing_table)), function(i) {
  lob <- pricing_table$lob[i]
  base_prem <- pricing_table$gross_premium[i]
  base_loss <- pricing_table$expected_loss[i]
  
  project_multiyear(base_prem, base_loss, years = 10) |>
    dplyr::mutate(lob = lob)
})

long_term_summary <- long_term_tables |>
  dplyr::group_by(lob) |>
  dplyr::summarise(
    pv_premium = sum(pv_premium),
    pv_cost = sum(pv_cost),
    pv_net_revenue = sum(pv_net_revenue),
    .groups = "drop"
  )

# ================================================================================
# 25. THREAT TABLE / PRODUCT DESIGN / ASSUMPTIONS
# ================================================================================

threat_table <- tibble::tibble(
  threat = c(
    "Correlated solar storm disrupting transport and operations",
    "Oryn Delta operational fragility due to lower redundancy",
    "Cargo route debris surge",
    "Equipment ageing concentration in core fleet",
    "Labour injury accumulation from technical roles",
    "Supply-chain interruption causing BI clustering",
    "Inflation shock on replacement cost and medical cost",
    "Model risk from imperfect mapping between historical and new solar systems"
  ),
  affected_lob = c(
    "Cargo, BI, equipment, WC",
    "BI, equipment, WC",
    "Cargo",
    "Equipment",
    "WC",
    "BI",
    "All",
    "All"
  ),
  likelihood = c("Medium", "Medium", "High", "High", "Medium", "Medium", "Medium", "High"),
  severity = c("Extreme", "High", "High", "High", "Medium", "High", "High", "High"),
  rank = c(1, 3, 4, 2, 6, 5, 7, 8),
  mitigation = c(
    "Cross-system event sublimits, reinsurance, coordinated emergency protocols",
    "System-specific deductibles, stronger backup standards, staged rollout",
    "Route-based pricing, convoy requirements, high-risk lane exclusions",
    "Age-based maintenance warranties and inspection conditions",
    "Role-based safety incentives and fatigue management",
    "Waiting periods, restoration clauses, minimum redundancy requirements",
    "Inflation indexing and annual repricing clauses",
    "Conservative margin, scenario overlays, governance sign-off"
  )
) |>
  dplyr::arrange(rank)

solar_system_narrative_table <- tibble::tibble(
  solar_system = c("Helionis Cluster", "Bayesian System", "Oryn Delta"),
  profile = c(
    "Largest and most established footprint; better redundancy but higher concentration risk.",
    "Middle-ground operating profile; balanced risk and likely pricing benchmark.",
    "Smaller footprint but harsher operating fragility; higher tail risk and lower resilience."
  ),
  risk_view = c(
    "Best operational control; still exposed to concentration and BI scale.",
    "Base-case system for portfolio pricing and planning.",
    "Requires the strongest deductibles, tighter wordings, and capital loading."
  )
)

product_design <- tibble::tibble(
  lob = c("Equipment failure", "Cargo loss", "Workers compensation", "Business interruption"),
  include_product = c("Yes", "Yes", "Yes", "Yes, but tightly structured"),
  benefit_structure = c(
    "Repair/replacement cost with age-sensitive deductibles and maintenance compliance credits.",
    "Declared value cover with route-based deductibles and limits tied to container / route class.",
    "Medical and wage replacement benefits with role-based pricing and safety credits.",
    "Revenue and extra-expense cover with waiting periods, sublimits and restoration requirements."
  ),
  trigger = c(
    "Mechanical or operational failure causing covered damage or downtime.",
    "Physical loss or damage during interstellar transit.",
    "Work-related injury or illness during covered employment.",
    "Operational interruption due to covered system outage, logistics breakdown or power event."
  ),
  exclusions = c(
    "Wear and tear without maintenance compliance, intentional misuse, cyber sabotage unless endorsed.",
    "Unmanifested shrinkage, ordinary leakage, contraband, prohibited routes unless endorsed.",
    "Pre-existing non-work conditions beyond policy scope, intentional self-harm, non-covered contractors.",
    "Non-damage market loss, regulatory shutdowns unless endorsed, chronic under-capacity, known unresolved defects."
  ),
  solar_system_tailoring = c(
    "Higher deductibles and stronger inspection terms for Oryn Delta; credits for Helionis backup depth.",
    "Route and debris-based pricing; stricter convoy conditions for Oryn Delta lanes.",
    "Gravity, fatigue and contractor mix adjustments by system.",
    "Different waiting periods and redundancy requirements by system resilience."
  )
)

assumptions_table <- tibble::tibble(
  category = c(
    "Historical-to-new solar system mapping",
    "Inflation trend",
    "Long-term discount rate",
    "Exposure allocation of personnel across solar systems",
    "Cargo flow proxy",
    "Service-year age midpoint",
    "Scenario dependency multipliers",
    "Expense / profit / capital loadings"
  ),
  assumption = c(
    "New solar systems priced using base technical models plus explicit relativity factors.",
    paste0("Loss severity inflated at mean historical inflation of ", round(mean_infl * 100, 2), "%"),
    paste0("Long-term PV discounted at average recent 10Y risk-free rate of ", round(long_rf * 100, 2), "%"),
    "Employees allocated in proportion to equipment footprint where direct station-level counts were unavailable.",
    "Shipment volume proxied using system production scale and stylised route profile.",
    "Equipment age approximated via service-band midpoints.",
    "Best / moderate / worst scenarios represented through multiplicative dependence overlays.",
    "Gross premium derived from expected loss plus risk margin and target commercial loadings."
  ),
  rationale = c(
    "Direct historical experience for Bayesian System and Oryn Delta is unavailable in the claims files.",
    "Case requires short- and long-term cost ranges and inflation-sensitive pricing.",
    "Needed to convert long-term projected values to present values.",
    "Personnel file is company-wide rather than system-specific.",
    "No shipment forecast file was provided for the RFP, so cargo exposures must be engineered.",
    "Inventory provides grouped service years rather than exact ages.",
    "Case explicitly asks for correlated risk scenarios and 1-in-100 style events.",
    "Needed to translate actuarial loss costs into marketable product pricing."
  )
)

data_limitations <- tibble::tibble(
  limitation = c(
    "Historical solar-system labels do not align with new business solar systems.",
    "No direct prospective BI exposure file exists at station level.",
    "No shipment forecast table exists for prospective cargo volume by route.",
    "Personnel file is not split by solar system or station.",
    "Potential naming inconsistencies in equipment taxonomy.",
    "Some variables appear outside dictionary ranges or require pragmatic cleaning.",
    "No explicit dependence structure is supplied across lines or systems."
  ),
  impact = c(
    "Requires relativity-based rather than pure credibility pricing by solar system.",
    "BI pricing relies on operational proxies and therefore deserves conservative margins.",
    "Cargo pricing is scenario-based and should be refreshed once shipment plans are known.",
    "WC pricing by system is partly allocation-based rather than directly observed.",
    "Requires harmonisation before prediction and may introduce classification noise.",
    "May affect model stability and tail fit if left untreated.",
    "Tail capital estimates are sensitive to scenario design and should be stress-tested."
  )
)

# ================================================================================
# 26. RISK TABLES
# ================================================================================

lob_risk_table <- dplyr::bind_rows(
  risk_summary(equip_losses_base) |> dplyr::mutate(lob = "Equipment failure"),
  risk_summary(cargo_losses_base) |> dplyr::mutate(lob = "Cargo loss"),
  risk_summary(wc_losses_base)    |> dplyr::mutate(lob = "Workers compensation"),
  risk_summary(bi_losses_base)    |> dplyr::mutate(lob = "Business interruption"),
  risk_summary(portfolio_base)    |> dplyr::mutate(lob = "Total portfolio")
) |>
  dplyr::select(lob, dplyr::everything())

scenario_table <- dplyr::bind_rows(
  risk_summary(portfolio_best)  |> dplyr::mutate(scenario = "Best case"),
  risk_summary(portfolio_mod)   |> dplyr::mutate(scenario = "Moderate / base case"),
  risk_summary(portfolio_worst) |> dplyr::mutate(scenario = "Worst case")
) |>
  dplyr::select(scenario, dplyr::everything())

short_long_table <- pricing_table |>
  dplyr::select(
    lob,
    short_term_expected_cost = expected_loss,
    short_term_expected_return = gross_premium
  ) |>
  dplyr::mutate(
    short_term_net_revenue = short_term_expected_return - short_term_expected_cost
  ) |>
  dplyr::left_join(long_term_summary, by = "lob")

# ================================================================================
# 27. MODEL COMPARISON TABLES
# ================================================================================

count_model_aic <- dplyr::bind_rows(
  bi_count_best$aic    |> dplyr::mutate(lob = "Business interruption"),
  cargo_count_best$aic |> dplyr::mutate(lob = "Cargo"),
  equip_count_best$aic |> dplyr::mutate(lob = "Equipment"),
  wc_count_best$aic    |> dplyr::mutate(lob = "Workers compensation")
)

sev_model_aic <- dplyr::bind_rows(
  bi_sev_best$aic    |> dplyr::mutate(lob = "Business interruption"),
  cargo_sev_best$aic |> dplyr::mutate(lob = "Cargo"),
  equip_sev_best$aic |> dplyr::mutate(lob = "Equipment"),
  wc_sev_best$aic    |> dplyr::mutate(lob = "Workers compensation")
)

# ================================================================================
# 28. PLOTS
# ================================================================================

plot_density <- function(x, title) {
  tibble::tibble(loss = x) |>
    ggplot2::ggplot(ggplot2::aes(loss)) +
    ggplot2::geom_histogram(bins = 80, fill = "grey70", colour = "white") +
    ggplot2::scale_x_continuous(labels = scales::dollar_format()) +
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    ggplot2::theme_minimal()
}

p1 <- plot_density(equip_losses_base, "Equipment failure - annual aggregate loss")
p2 <- plot_density(cargo_losses_base, "Cargo loss - annual aggregate loss")
p3 <- plot_density(wc_losses_base, "Workers compensation - annual aggregate loss")
p4 <- plot_density(bi_losses_base, "Business interruption - annual aggregate loss")
p5 <- plot_density(portfolio_base, "Total portfolio - annual aggregate loss")
p6 <- plot_density(portfolio_worst, "Total portfolio - worst-case scenario")

ggplot2::ggsave(
  filename = file.path(output_dir, "plots", "lob_and_portfolio_distributions.png"),
  plot = (p1 / p2 / p3 / p4 / p5 / p6),
  width = 12,
  height = 20
)

# ================================================================================
# 29. EXPORT OUTPUTS
# ================================================================================

writexl::write_xlsx(
  list(
    pricing_table = pricing_table,
    short_long_table = short_long_table,
    lob_risk_table = lob_risk_table,
    scenario_table = scenario_table,
    threat_table = threat_table,
    solar_system_narrative_table = solar_system_narrative_table,
    product_design = product_design,
    assumptions_table = assumptions_table,
    data_limitations = data_limitations,
    count_model_aic = count_model_aic,
    severity_model_aic = sev_model_aic
  ),
  file.path(output_dir, "tables", "case_study_outputs.xlsx")
)

report_insights <- list(
  headline_1 = "All four products can be offered, but business interruption should be written with the tightest wording and strongest sublimits because its tail is the most sensitive to correlated operational shocks.",
  headline_2 = "Oryn Delta appears to warrant the highest technical margin despite its smaller scale because lower redundancy increases fragility and pushes up tail risk.",
  headline_3 = "The most material pricing uncertainty is not ordinary attritional frequency - it is model risk from translating historical experience into a different future solar-system footprint.",
  headline_4 = "A portfolio approach is commercially attractive because diversification softens ordinary-year volatility, but catastrophe-style dependency can still dominate the 1-in-100 outcome.",
  headline_5 = "Indexation, annual repricing, and explicit engineering / safety conditions are essential design features rather than optional polish."
)

saveRDS(report_insights, file.path(output_dir, "tables", "report_insights.rds"))

# ================================================================================
# 30. CONSOLE SUMMARY
# ================================================================================

cat("\n==============================\n")
cat("CASE STUDY SUMMARY\n")
cat("==============================\n")
cat("\nPricing table:\n")
print(pricing_table)

cat("\nScenario table:\n")
print(scenario_table)

cat("\n1-in-100 proxy worst-case portfolio loss:\n")
print(portfolio_1_in_100)

cat("\nOutputs written to:\n")
print(normalizePath(output_dir, winslash = "/"))