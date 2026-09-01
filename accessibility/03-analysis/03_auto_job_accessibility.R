# Calculate City of Austin H8 automobile access to 2023 jobs on the pinned
# 2026 OpenStreetMap network. This is modeled road-network access; it does not
# include observed or historical traffic congestion.

source("accessibility/config.R")

Sys.setenv(R_USER_CACHE_DIR = cache_dir)
options(java.parameters = "-Xmx12G")
Sys.setenv(TZ = "America/Chicago")

source("00_setup_packages.R")
setup_project_packages(c(
  "dplyr", "h3jsr", "readr", "sf", "tidyr", "r5r"
))

if (!file.exists(lodes_jobs_path)) {
  stop(
    paste0(
      "Missing LODES destinations. Run accessibility/02-data-processing/",
      "01_pull_lodes_wac_jobs.R first."
    )
  )
}

if (!file.exists(origins_path)) {
  stop(
    "Missing cached H8 origins. Run the transit accessibility script once ",
    "or regenerate origins before running automobile accessibility."
  )
}

jobs <- read_csv(lodes_jobs_path, show_col_types = FALSE)
job_points <- cell_to_point(jobs$h3_id)
job_coordinates <- st_coordinates(job_points)

destinations <- jobs %>%
  transmute(
    id = h3_id,
    lon = job_coordinates[, "X"],
    lat = job_coordinates[, "Y"],
    total_jobs,
    low_wage_jobs,
    middle_wage_jobs,
    high_wage_jobs
  )

origins <- read_csv(origins_path, show_col_types = FALSE)

if (
  any(origins$h3_resolution != h3_resolution) ||
  any(origins$city_boundary_year != city_boundary_year)
) {
  stop("Cached H8 origins do not match the configured resolution/boundary year.")
}

origins <- origins %>%
  select(id, lon, lat) %>%
  arrange(id)

# Set AUTO_SMOKE_TEST=true to route only the first configured number of H8
# origins and write a separate test file. The default is the complete run.
smoke_test <- identical(tolower(Sys.getenv("AUTO_SMOKE_TEST")), "true")

if (smoke_test) {
  origins <- slice_head(origins, n = auto_smoke_test_origins)
  run_output_path <- sub("[.]csv$", "_smoke_test.csv", auto_accessibility_output_path)
  message("AUTO_SMOKE_TEST=true: routing ", nrow(origins), " origins only.")
} else {
  run_output_path <- auto_accessibility_output_path
}

source("accessibility/01-setup/r5r_setup.R")
on.exit(r5r::stop_r5(r5r_network), add = TRUE)

departure_datetime <- as.POSIXct(
  departure_datetime_text,
  format = "%Y-%m-%d %H:%M:%S",
  tz = "America/Chicago"
)

message(
  "Calculating automobile accessibility for ", nrow(origins),
  " City of Austin H8 cells to ", nrow(destinations), " job cells using ",
  auto_n_threads, " threads..."
)

batch_root <- file.path(processed_access_dir, "auto_batches")
batch_signature <- paste0(
  "car_h", h3_resolution,
  "_osm", gsub("-", "", osm_snapshot_date),
  "_jobs", lodes_year,
  "_c", paste(auto_access_cutoffs_minutes, collapse = "-")
)
batch_dir <- file.path(batch_root, batch_signature)
dir.create(batch_dir, recursive = TRUE, showWarnings = FALSE)

origin_batches <- split(
  seq_len(nrow(origins)),
  ceiling(seq_len(nrow(origins)) / auto_batch_size)
)
auto_access_batches <- vector("list", length(origin_batches))

for (batch_number in seq_along(origin_batches)) {
  batch_origins <- origins[origin_batches[[batch_number]], ]
  batch_path <- file.path(batch_dir, sprintf("batch_%03d.csv", batch_number))

  if (!smoke_test && file.exists(batch_path)) {
    message(
      "Reusing automobile batch ", batch_number, " of ", length(origin_batches),
      ": ", batch_path
    )
    auto_access_batches[[batch_number]] <- read_csv(batch_path, show_col_types = FALSE)
    next
  }

  message(
    "Routing automobile batch ", batch_number, " of ", length(origin_batches),
    " (", nrow(batch_origins), " origins)..."
  )

  batch_result <- r5r::accessibility(
    r5r_core = r5r_network,
    origins = batch_origins,
    destinations = destinations,
    opportunities_colnames = c(
      "total_jobs", "low_wage_jobs", "middle_wage_jobs", "high_wage_jobs"
    ),
    mode = "CAR",
    departure_datetime = departure_datetime,
    time_window = 1L,
    percentiles = travel_time_percentile,
    decay_function = "step",
    cutoffs = auto_access_cutoffs_minutes,
    max_car_time = auto_max_trip_minutes,
    max_trip_duration = auto_max_trip_minutes,
    draws_per_minute = auto_draws_per_minute,
    n_threads = auto_n_threads,
    progress = TRUE
  ) %>%
    as_tibble()

  if (!smoke_test) {
    write_csv(batch_result, batch_path)
  }
  auto_access_batches[[batch_number]] <- batch_result
}

auto_access_long <- bind_rows(auto_access_batches)

auto_access_results <- auto_access_long %>%
  as_tibble() %>%
  select(id, opportunity, percentile, cutoff, accessibility) %>%
  pivot_wider(
    names_from = opportunity,
    values_from = accessibility,
    names_prefix = "access_"
  ) %>%
  left_join(origins, by = "id") %>%
  rename(h3_id = id) %>%
  mutate(
    routing_mode = "CAR",
    congestion_model = "none_osm_network_speeds",
    network_snapshot = osm_snapshot_date,
    jobs_year = lodes_year,
    h3_resolution = h3_resolution
  ) %>%
  arrange(h3_id, cutoff)

expected_rows <- nrow(origins) * length(auto_access_cutoffs_minutes)
if (nrow(auto_access_results) != expected_rows) {
  stop("Expected ", expected_rows, " origin-cutoff rows; found ", nrow(auto_access_results), ".")
}

if (anyDuplicated(select(auto_access_results, h3_id, cutoff))) {
  stop("Automobile accessibility output contains duplicate origin-cutoff rows.")
}

access_columns <- c(
  "access_total_jobs", "access_low_wage_jobs",
  "access_middle_wage_jobs", "access_high_wage_jobs"
)

if (anyNA(select(auto_access_results, all_of(access_columns)))) {
  stop("Automobile accessibility output contains missing accessibility values.")
}

job_totals <- colSums(
  select(
    destinations,
    total_jobs, low_wage_jobs, middle_wage_jobs, high_wage_jobs
  ),
  na.rm = TRUE
)
names(job_totals) <- paste0("access_", names(job_totals))

for (column in access_columns) {
  if (any(auto_access_results[[column]] > job_totals[[column]])) {
    stop(column, " exceeds the corresponding five-county job total.")
  }
}

monotonic_check <- auto_access_results %>%
  arrange(h3_id, cutoff) %>%
  group_by(h3_id) %>%
  summarise(
    across(all_of(access_columns), ~ all(diff(.x) >= 0)),
    .groups = "drop"
  )

if (!all(as.matrix(select(monotonic_check, all_of(access_columns))))) {
  stop("Automobile accessibility decreases for at least one larger cutoff.")
}

write_csv(auto_access_results, run_output_path)
message("Saved ", nrow(auto_access_results), " rows to ", run_output_path)
