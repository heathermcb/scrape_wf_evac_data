# Purpose: webscrape wildfire evacuation boundaries from public ARC GIS
# dataset for California wildfires. Designed to be triggered on a schedule
# (e.g. every 20 minutes via GitHub Actions cron) rather than looping
# internally. Each run checks for changes since the last saved snapshot
# and, if the data changed, saves a new timestamped file.
#
# Author: Heather
# Last updated: January 16th, 2025


# Libraries ---------------------------------------------------------------

library(httr)
library(jsonlite)
library(here)
library(digest)


# Function ----------------------------------------------------------------

download_dataset <- function() {
  # URL of the feature service
  feature_service_url <-
    paste0("https://services.arcgis.com",
    "/BLN4oKB0N1YSgvY8/arcgis/rest/services/CA_EVACUATIONS_CalOESHosted_view",
    "/FeatureServer/0/query")

  # Parameters for the query
  params <- list(
    where = "1=1",  # Query all features
    outFields = "*",
    f = "geojson"  # Request the data in GeoJSON format
  )

  # Create the directory if it doesn't exist
  save_dir <- here("evac_boundaries")
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }

  # Send a GET request to the feature service, with a couple of retries
  # since a single transient failure shouldn't fail the whole scheduled run
  get_with_retry <- function(...) {
    for (attempt in 1:3) {
      resp <- tryCatch(
        GET(..., timeout(60), user_agent("ca-wildfire-scraper (github actions)")),
        error = function(e) e
      )
      if (!inherits(resp, "error") && status_code(resp) == 200) return(resp)
      if (attempt < 3) {
        cat("Request attempt", attempt, "failed, retrying...\n")
        Sys.sleep(5 * attempt)
      }
    }
    resp
  }

  response <- get_with_retry(feature_service_url, query = params)

  # Check if the request was successful
  if (!inherits(response, "error") && status_code(response) == 200) {

    # Generate a timestamp
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M", tz = "UTC")

    # Save the GeoJSON file locally in the specified folder with a timestamp
    temp_filename <- file.path(save_dir, "temp.geojson")
    writeBin(content(response, "raw"), temp_filename)

    # Check if there is a previous file to compare with. IMPORTANT: we sort
    # by the timestamp embedded in the filename, not by file mtime. On a
    # fresh `git checkout` (as happens on every GitHub Actions run), all
    # files get the same mtime (checkout time), so mtime-based ordering
    # silently breaks after the first run.
    previous_files <-
      list.files(save_dir,
                 pattern = "california_active_evacuation_zones_.*\\.geojson",
                 full.names = TRUE)

    if (length(previous_files) > 0) {
      extract_ts <- function(f) {
        ts_str <- regmatches(f, regexpr("\\d{8}_\\d{4}", f))
        as.POSIXct(ts_str, format = "%Y%m%d_%H%M", tz = "UTC")
      }
      last_file <- previous_files[order(sapply(previous_files, extract_ts),
                                        decreasing = TRUE)][1]
      if (digest(file = temp_filename) == digest(file = last_file)) {
        cat("No changes detected in the dataset.\n")
        file.remove(temp_filename)
        return(invisible(FALSE))
      }
    }

    # If there are changes, save the new file with a timestamp
    filename <-
      paste0("california_active_evacuation_zones_", timestamp, ".geojson")
    file.rename(temp_filename, file.path(save_dir, filename))
    cat("Dataset downloaded successfully as", filename, "\n")
    return(invisible(TRUE))

  } else {
    status <- if (inherits(response, "error")) "request error" else status_code(response)
    cat("Failed to query feature service after retries. Status:", status, "\n")
    quit(status = 1)  # non-zero exit so the GitHub Actions run shows as failed
  }
}


# DO ------------------------------------------------------------------------
# One run per invocation. The 20-minute cadence is handled by the GitHub
# Actions workflow's cron schedule (see .github/workflows/scrape-wildfires.yml),
# not by an internal loop.

download_dataset()
