# Add resident-worker weights, validate, summarize, and map H8 automobile job
# accessibility. Run after 03_auto_job_accessibility.R.

source("accessibility/config.R")

source("00_setup_packages.R")
setup_project_packages(c(
  "dplyr", "ggplot2", "h3jsr", "patchwork", "purrr", "readr",
  "scales", "sf", "tidyr"
))

if (!file.exists(auto_accessibility_output_path) || !file.exists(lodes_workers_path)) {
  stop("Run the LODES preparation and full automobile accessibility scripts first.")
}

access <- read_csv(auto_accessibility_output_path, show_col_types = FALSE)
workers <- read_csv(lodes_workers_path, show_col_types = FALSE)

worker_columns <- c("workers_all", "workers_low", "workers_middle", "workers_high")

access_workers <- access %>%
  select(-any_of(worker_columns)) %>%
  left_join(workers, by = "h3_id") %>%
  mutate(across(starts_with("workers_"), ~ replace_na(.x, 0)))

write_csv(access_workers, auto_accessibility_output_path)

measure_config <- tibble(
  access_column = c(
    "access_total_jobs", "access_low_wage_jobs",
    "access_middle_wage_jobs", "access_high_wage_jobs"
  ),
  worker_column = c("workers_all", "workers_low", "workers_middle", "workers_high"),
  job_type = c("All jobs", "Low-wage jobs", "Middle-wage jobs", "High-wage jobs")
)

summary_table <- crossing(
  cutoff_value = sort(unique(access_workers$cutoff)),
  measure_config
) %>%
  pmap_dfr(function(cutoff_value, access_column, worker_column, job_type) {
    cutoff_data <- filter(access_workers, .data$cutoff == cutoff_value)
    values <- cutoff_data[[access_column]]
    weights <- cutoff_data[[worker_column]]
    valid <- !is.na(values) & weights > 0

    tibble(
      cutoff = cutoff_value,
      job_type = job_type,
      weighted_mean = if (any(valid)) weighted.mean(values[valid], weights[valid]) else NA_real_,
      unweighted_mean = mean(values, na.rm = TRUE),
      median = median(values, na.rm = TRUE),
      minimum = min(values, na.rm = TRUE),
      maximum = max(values, na.rm = TRUE),
      worker_count = sum(weights, na.rm = TRUE)
    )
  })

summary_path <- file.path(
  output_dir,
  paste0("h", h3_resolution, "_auto_job_accessibility_summary.csv")
)
write_csv(summary_table, summary_path)

map_cutoffs <- sort(unique(c(15, 30, auto_primary_cutoff_minutes)))

map_paths <- vapply(map_cutoffs, function(map_cutoff) {
  cutoff_access <- access_workers %>%
    filter(cutoff == map_cutoff)

  if (nrow(cutoff_access) == 0) {
    stop("No automobile records found for the ", map_cutoff, "-minute cutoff.")
  }

  map_data <- st_sf(
    cutoff_access,
    geometry = cell_to_polygon(cutoff_access$h3_id),
    crs = 4326
  ) %>%
    pivot_longer(
      cols = starts_with("access_"),
      names_to = "measure",
      values_to = "jobs_accessible"
    ) %>%
    mutate(
      measure = recode(
        measure,
        access_total_jobs = "All jobs",
        access_low_wage_jobs = "Low-wage jobs",
        access_middle_wage_jobs = "Middle-wage jobs",
        access_high_wage_jobs = "High-wage jobs"
      )
    )

  map_plots <- lapply(measure_config$job_type, function(measure_name) {
    data <- filter(map_data, measure == measure_name)
    legend_max <- max(data$jobs_accessible, na.rm = TRUE)

    ggplot(data) +
      geom_sf(aes(fill = jobs_accessible), color = NA) +
      scale_fill_viridis_c(
        labels = scales::label_number(scale_cut = scales::cut_short_scale()),
        breaks = c(0, legend_max / 2, legend_max),
        option = "mako",
        guide = guide_colorbar(
          title.position = "top",
          barwidth = grid::unit(3, "cm"),
          barheight = grid::unit(0.3, "cm")
        )
      ) +
      labs(title = unique(data$measure), fill = "Jobs") +
      theme_void() +
      theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
  })

  access_map <- wrap_plots(map_plots, ncol = 2) +
    plot_annotation(
      title = paste0("City of Austin H", h3_resolution, " Automobile Access to Jobs"),
      subtitle = paste0(
        "Jobs reachable within ", map_cutoff,
        " minutes using modeled OSM road-network speeds"
      ),
      caption = paste0(
        lodes_year, " LODES jobs; OSM snapshot ", osm_snapshot_date,
        "; no observed congestion model"
      ),
      theme = theme(plot.title = element_text(face = "bold"))
    )

  map_path <- file.path(
    output_dir,
    if (map_cutoff == auto_primary_cutoff_minutes) {
      paste0("h", h3_resolution, "_auto_job_accessibility_map.png")
    } else {
      paste0("h", h3_resolution, "_auto_job_accessibility_map_", map_cutoff, "min.png")
    }
  )
  ggsave(map_path, access_map, width = 13, height = 10, dpi = 300, bg = "white")
  map_path
}, character(1))

sensitivity_plot <- summary_table %>%
  filter(job_type == "All jobs") %>%
  ggplot(aes(x = cutoff, y = weighted_mean)) +
  geom_line(linewidth = 1, color = "#2c7fb8") +
  geom_point(size = 2.5, color = "#2c7fb8") +
  scale_x_continuous(breaks = auto_access_cutoffs_minutes) +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  labs(
    title = "Automobile Job Accessibility Sensitivity",
    subtitle = "Resident-worker-weighted mean across City of Austin H8 origins",
    x = "Automobile travel-time cutoff (minutes)",
    y = "Mean jobs accessible",
    caption = "Modeled OSM road-network speeds; no observed congestion model"
  ) +
  theme_minimal()

sensitivity_path <- file.path(
  output_dir,
  paste0("h", h3_resolution, "_auto_job_accessibility_sensitivity.png")
)
ggsave(sensitivity_path, sensitivity_plot, width = 9, height = 6, dpi = 300, bg = "white")

print(summary_table)
message("Updated automobile output with resident-worker weights at ", auto_accessibility_output_path)
message("Saved summary to ", summary_path)
message("Saved maps to ", paste(map_paths, collapse = ", "))
message("Saved cutoff sensitivity plot to ", sensitivity_path)
