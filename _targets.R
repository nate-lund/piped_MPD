#================================ Packages ================================

# List libraries needed
libs <- c("httr", "jsonlite", "ggplot2", "terra", "leaflet", "ncdf4", "tidyr", "dplyr", "readr", "targets", "usethis", "sf", "targets", "visNetwork", "tarchetypes", "tidyterra", "performance", "see", "RColorBrewer", "lme4", "nlme", "readxl", "writexl", "emmeans", "splines", "lspline", "ggeffects", "lubridate", "cowplot", "gridGraphics", "broom", "DT", "flextable", "wesanderson", "ggspatial", "extrafont", "officer", "svglite", "ggspatial", "stringr", "ggh4x", "ggrepel", "magick", "broom.mixed", "lmerTest")

# Install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# Load libraries
lapply(libs, library, character.only = T)


#================================ Setup ================================

# Set target options:
tar_option_set(
  packages = c(libs), # Packages that your targets need for their tasks.
  error = "continue" # tar_make() will continue even if it hits one error
)

# Run the R scripts in the R/ folder with functions
tar_source(
  # Contains functions that rely exclusively on erosion pin measurement data.
  "R/MPD-functions.R"
)


#' [WHEN RUNNING FOR THE FIRST TIME]
# Run commented out code below to clear storage:
# tar_destroy(destroy = c("objects")); file.remove(list.files("_plot_outputs", full.names = TRUE))


#================================ Targets ================================

# Replace the target list below with your own:
list(
  
  ### Read bulk data ====
  tar_target(
    raw_data,
    read_excel(path = list.files("_inputs/_measurements", full.names = TRUE),
               sheet = "Sheet1"),
    cue = tar_cue(mode = "always")
  ),
  
  ### Split bulk data for sharding? ====
  tar_target(
    split_list,
    split_data(raw_data)
  ),
  
  ## Compute Ksat values ====
  tar_target(
    Ksat_list,
    compute_Ksat(split_list),
    pattern = map(split_list),
    iteration = "list"
  ),
  
  ## Plot each individual measurement ====
  tar_target(
    infiltration_plots,
    plotting(Ksat_list),
    pattern = map(Ksat_list),
    iteration = "list",
    format = "file"
  ),
  
  ## Bind ksats ====
  tar_target(
    all_Ksats,
    bind_rows(Ksat_list[[2]])
  )
  
  
)
