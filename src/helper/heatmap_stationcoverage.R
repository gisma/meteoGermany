# ============================================================
# Main Climate Interpolation Coverage + Kriging Validity Index Script
# ============================================================
# This script:
# 1. Loads DWD station data for selected variables
# 2. Calculates station coverage statistics and a Kriging Validity Index
# 3. Generates:
#    - Spatial coverage "pseudo heatmap" plots (vectorized station points)
#    - Summary statistics tables
#    - Time series of coverage and Kriging index
#
# ============================================================

library(sf)
library(dplyr)
library(ggplot2)
library(kableExtra)

# ---------------------------
# Parameters
# ---------------------------

# Interpolation resolution in meters (matches your Kriging output grid)
res <- 500

# ------------------------------------------------------------
# WMO correlation length ranges and 75th percentile selection
# ------------------------------------------------------------
# Source: WMO-No. 1203 (2017) "Guide to Climatological Practices",
# Chapter 4: Spatial density of observations
#
# Variable   | Range (km)  | 75% percentile used as default target spacing
# -------------------------------------------------------------------------
# RSK        | 5–20        | 15
# SDK, NM    | 20–80       | 65
# UPM, TXK, TNK, TMK, VPM | 30–100 | 85
# TGK        | 20–80       | 65
# PM         | 100–300     | 225
#
# Reasoning:
# - The 75% percentile ensures a conservative but realistic threshold
#   for network density based on observed correlation decay in each variable.
# - This replaces the arbitrary spacing factor and links the minStations
#   calculation directly to physical spatial autocorrelation properties.
#
# The minStations calculation remains:
#   minStations = ceiling(area_km2 / (pi * target_spacing_km^2))
#
# This is analogous to the Nyquist sampling theorem in signal processing:
#   - Nyquist says: sample at least twice as often as the highest frequency
#     you want to reconstruct.
#   - Here: have stations spaced closely enough to resolve features at the
#     target interpolation resolution.
# ------------------------------------------------------------

# Area of Germany in km²
area_km2 <- 357000

# Lookup table for WMO-based target spacings (75% percentile values)
target_spacing_km_lookup <- c(
  "RSK" = 15,
  "SDK" = 65,
  "NM"  = 65,
  "UPM" = 85,
  "TXK" = 85,
  "TNK" = 85,
  "TMK" = 85,
  "TGK" = 65,
  "VPM" = 85,
  "PM"  = 225
)

# Variables to process (DWD abbreviations)
var_list <- names(target_spacing_km_lookup)

# Variable labels
var_labels <- c(
  "RSK" = "Precipitation height [mm]",
  "SDK" = "Sunshine duration [h]",
  "NM"  = "Cloud cover [1/8]",
  "UPM" = "Relative humidity [%]",
  "TXK" = "Daily max air temp [°C]",
  "TNK" = "Daily min air temp [°C]",
  "TMK" = "Daily mean air temp [°C]",
  "TGK" = "Daily mean soil temp 5 cm [°C]",
  "VPM" = "Water vapour pressure [hPa]",
  "PM"  = "Air pressure at station height [hPa]"
)

# Paths (adapt to your envrmt if needed)
data_path <- "/datadisk/meteoGermany/data/data_lev0/CDC_KL"

# Storage lists
heatmap_list <- list()
timeseries_list <- list()

# ---------------------------
# Loop through variables
# ---------------------------
for (v in var_list) {
  # Construct file path dynamically based on startDate, endDate, and variable v
  file_path <- file.path(data_path, paste0(
    format(as.Date(startDate), "%Y%m%d"), "_",
    format(as.Date(endDate), "%Y%m%d"), "_",
    v, ".gpkg"
  ))
  
  if (!file.exists(file_path)) {
    message("File not found, generating: ", file_path)
    cVar.sf <- ex_clim_new(
      startDate = startDate,
      endDate   = endDate,
      reso      = reso,
      var       = var,
      type      = type,
      param     = v
    )
    # Optionally save the output to the gpkg for reuse
    st_write(cVar.sf, file_path, delete_dsn = TRUE)
  } else {
    message("Using existing file: ", file_path)
    cVar.sf <- st_read(file_path, quiet = TRUE)
  }
  
  message("Processing ", v, " ...")
  

    
    sf_data <- st_read(file_path, quiet = TRUE)
    
    # Ensure geometry column is properly named and set
    if (!"geometry" %in% names(sf_data)) {
      geom_col <- attr(sf_data, "sf_column")
      names(sf_data)[names(sf_data) == geom_col] <- "geometry"
      st_geometry(sf_data) <- "geometry"   # <-- re‑register as sf geometry
    } else {
      st_geometry(sf_data) <- "geometry"   # <-- ensure it's active
    }
    
    
    # Extract coords for plotting
    sf_data_coords <- sf_data %>%
      mutate(
        lon = st_coordinates(.)[, 1],
        lat = st_coordinates(.)[, 2]
      )
    
    temp_df <- sf_data_coords %>%
      st_drop_geometry()
    
    temp_df <- temp_df[, c("MESS_DATUM", v), drop = FALSE]
    
    daily_counts <- temp_df %>%
      group_by(MESS_DATUM) %>%
      summarise(n_valid = sum(!is.na(.data[[v]])), .groups = "drop")
    # ------------------------------------------------------------------------------
    # Kriging Validity Index ("Traffic Light") – Concept and Weighting
    #
    # The index combines three components, weighted as follows:
    #   1. Coverage ratio (50 % weight)
    #      - Ratio of stations with valid data on a given day to the total number 
    #        of stations in the dataset.
    #   2. Stability (30 % weight)
    #      - Measures how close the day's coverage is to the long-term average 
    #        coverage. Large deviations reduce stability.
    #   3. Minimum station count criterion (20 % weight)
    #      - Ensures that days with very few stations (< minStations) are penalised.
    #
    # The index is calculated per day and used to classify the quality of data coverage for
    # Kriging interpolation as a “traffic light” signal:
    #   - Green: High data quality and coverage, suitable for reliable interpolation.
    #   - Yellow: Moderate coverage or stability, proceed with caution.
    #   - Red: Insufficient data coverage or station count, interpolation likely unreliable.
    #
    # Weighting rationale:
    # - Coverage ratio is most important as it directly impacts spatial representation.
    # - Stability ensures that sudden drops or spikes in coverage do not skew interpolation.
    # - Minimum station count guarantees a physical baseline density for the interpolation method.
    #
    # This balanced weighting helps to quantify and communicate daily interpolation suitability
    # for the given spatial and temporal climate data.
    #
    # Reference:
    # Hengl, Heuvelink, & Stein (2003). Comparison of kriging with external drift and regression-kriging.
    # ITC technical note.
    # ------------------------------------------------------------------------------
    
    # Calculate daily mean coverage across all days (long-term average)
    mean_valid_stations <- mean(daily_counts$n_valid, na.rm = TRUE)
    
    # Calculate stability component as inverse absolute deviation ratio
    stability <- 1 - (abs(daily_counts$n_valid - mean_valid_stations) / mean_valid_stations)
    stability[stability < 0] <- 0  # Clamp negative values to zero
    
    # Calculate coverage ratio component
    coverage_ratio <- daily_counts$n_valid / max(daily_counts$n_valid, na.rm = TRUE)
    
    # Calculate minimum station count component (binary: 1 if >= minStations, else 0)
    min_station_criterion <- as.numeric(daily_counts$n_valid >= minStations)
    
    # Weighted sum for Kriging Validity Index (0 to 1 scale)
    daily_counts$kriging_validity_index <- (
      0.3 * coverage_ratio +
        0.3 * stability +
        0.4 * min_station_criterion
    )
    
    # Classify traffic light status based on thresholds
    daily_counts$kriging_validity <- cut(
      daily_counts$kriging_validity_index,
      breaks = c(-Inf, 0.5, 0.75, Inf),
      labels = c("Red", "Yellow", "Green")
    )
    
    max_valid <- max(daily_counts$n_valid, na.rm = TRUE)
    
    ts_counts <- daily_counts %>%
      mutate(
        variable = v,
        valid_pct = (n_valid / max_valid) * 100,
        kriging_validity = case_when(
          n_valid >= minStations ~ "Green",
          n_valid >= minStations * 0.75 ~ "Yellow",
          TRUE ~ "Red"
        )
      )
    
    
    timeseries_list[[v]] <- ts_counts
    
    # --- Heatmap dataset (vectorized points) ---
    temp_df <- sf_data_coords %>%
      st_drop_geometry()
    
    agg_df <- aggregate(temp_df[[v]], by = list(STATIONS_ID = temp_df$STATIONS_ID, lon = temp_df$lon, lat = temp_df$lat), FUN = function(x) sum(!is.na(x)))
    
    names(agg_df)[4] <- "days_with_data"
    agg_df$pct_days <- (agg_df$days_with_data / max(agg_df$days_with_data)) * 100
    agg_df$variable <- v
    
    station_mean_days <- agg_df
    
    # Wichtig: Dataframe zur Liste hinzufügen!
    heatmap_list[[v]] <- station_mean_days
    
  }
  
  # ---------------------------
  # Combine and summarise
  # ---------------------------
  
  # Heatmap DF
  heatmap_df <- bind_rows(heatmap_list)
  
  heatmap_stats <- heatmap_df %>%
    group_by(variable) %>%
    summarise(
      min_pct     = round(min(pct_days, na.rm = TRUE), 1),
      max_pct     = round(max(pct_days, na.rm = TRUE), 1),
      mean_pct    = round(mean(pct_days, na.rm = TRUE), 1),
    stations_lt50  = round(mean(pct_days < 50, na.rm = TRUE) * 100, 1),  # in %
    stations_na    = round(mean(is.na(pct_days)) * 100, 1),               # in %
    .groups = "drop"
  )

# Timeseries DF
timeseries_df <- bind_rows(timeseries_list)

# ---------------------------
# Plots
# ---------------------------

# 1. Heatmap (vectorized station points)
ggplot(heatmap_df, aes(x = lon, y = lat, color = pct_days)) +
  geom_point(size = 1) +
  scale_color_gradientn(
    colors = c("darkblue", "lightblue", "yellow", "orange", "red"),
    na.value = "grey80",
    limits = c(0, 100)
  ) +
  facet_wrap(~ variable, labeller = labeller(variable = var_labels)) +
  theme_minimal(base_size = 10) +
  labs(
    title = "Station coverage percentage by variable (2003–2024)",
    x = "Longitude", y = "Latitude", color = "% Days with Data"
  )

# 2. Summary table
heatmap_stats %>%
  rename(
    "Variable" = variable,
    "Min. Coverage [%]" = min_pct,
    "Max. Coverage [%]" = max_pct,
    "Mean Coverage [%]" = mean_pct,
    "% Stations <50% coverage" = stations_lt50,
    "% Missing" = stations_na
  ) %>%
  kable(format = "html", table.attr = "style='width:100%;'") %>%
  kable_styling(full_width = TRUE)

# 3. Time series of coverage
ggplot(timeseries_df, aes(x = MESS_DATUM, y = valid_pct, color = variable)) +
  geom_line() +
  facet_wrap(~ variable, labeller = labeller(variable = var_labels)) +
  theme_minimal(base_size = 10) +
  labs(
    title = "Daily coverage percentage over time",
    x = "Date", y = "Coverage [% of max available stations]"
  )

# 4. Time series of Kriging validity index
library(ggplot2)
library(dplyr)

# Create an ID for grouping continuous runs of same kriging_validity
timeseries_df <- timeseries_df %>%
  arrange(variable, MESS_DATUM) %>%
  group_by(variable) %>%
  mutate(group_id = cumsum(kriging_validity != lag(kriging_validity, default = first(kriging_validity))))

ggplot(timeseries_df, aes(x = MESS_DATUM, y = n_valid, color = kriging_validity, group = group_id)) +
  geom_line() +
  facet_wrap(~ variable, labeller = labeller(variable = var_labels)) +
  scale_color_manual(values = c("Red" = "red", "Yellow" = "gold", "Green" = "green3")) +
  theme_minimal(base_size = 10) +
  labs(
    title = "Kriging validity index over time",
    x = "Date", y = "Number of valid stations", color = "Index"
  )

