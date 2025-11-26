# ================================================================================
# INTEGRATED ALFAM2 AMMONIA FORECAST & N APPLICATION TOOL
# ================================================================================
# 
# PURPOSE: 
# - Predict ammonia emissions from slurry spreading over 72-hour forecast window
# - Calculate optimal spreading timing and required application rates
# - Account for predicted N losses to achieve target effective N delivery
# - Provide ensemble-based uncertainty estimates
# - Calculate required tractor speed, tank loads, and spreading logistics
#
# REQUIREMENTS:
# - R packages: httr, jsonlite, readxl, writexl, dplyr, lubridate, zoo, 
#               ALFAM2, purrr, ggplot2, ggrepel, cowplot (or patchwork)
#
# HOW TO USE:
# 1. Edit the USER PARAMETER BLOCK below with your farm-specific values
# 2. Source this script: source("integrated_alfam2_tool.R")
# 3. Review console output and generated plots/files
# ================================================================================

# ---- REQUIRED LIBRARIES ----
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(zoo)
library(ALFAM2)
library(purrr)
library(ggplot2)
library(ggrepel)
library(cowplot)  # For plot insets; alternatively use: library(patchwork)

options(scipen = 999)

# ================================================================================
# USER PARAMETER BLOCK - EDIT THESE VALUES
# ================================================================================

USER_PARAMS <- list(
  # --- LOCATION ---
  # Option 1: Use place name (will be geocoded)
  place = "Johnstown Castle, Wexford, Ireland",
  
  # Option 2: Use explicit coordinates (overrides place if both provided)
  lat = NULL,  # e.g., 52.2928
  lon = NULL,  # e.g., -6.5028
  
  # --- TIMING ---
  # Start datetime for forecast (NULL = use current time)
  start_datetime = NULL,  # e.g., "2025-11-17 08:00:00"
  timezone = "UTC",       # Timezone for start_datetime
  
  # --- SLURRY PROPERTIES ---
  manure_type = "cattle_slurry",  # Options: "cattle_slurry", "pig_slurry", "poultry_slurry"
  dry_matter_pct = 6,             # Dry matter percentage (numeric, e.g., 4, 6, 8, 10)
  slurry_density_kg_m3 = 1000,    # Slurry density (default: 1000 kg/m³)
  
  # Lab-measured TAN (optional override)
  use_lab_tan = FALSE,            # Set TRUE to use lab value instead of Teagasc lookup
  lab_tan_kg_per_t = NULL,        # Lab-measured TAN (kg N/t) if use_lab_tan = TRUE
  
  # --- APPLICATION PARAMETERS ---
  target_effective_N_kg_ha = 80,  # Target effective N delivery after losses (kg N/ha)
  
  # Tank and flow parameters
  tank_size_L = 10000,            # Tank capacity (litres)
  time_to_empty_min = 10,         # Time to empty tank (minutes)
  # OR specify pump_flow_L_min directly:
  pump_flow_L_min = NULL,         # Pump flow rate (L/min) - overrides time_to_empty_min if provided
  
  working_width_m = 6,            # Spreading width (metres)
  
  # --- APPLICATOR TYPE (for mitigation scenarios) ---
  applicator_type = "trailing_shoe",  # Options: "splashplate", "trailing_hose", "trailing_shoe", "injection"
  
  # --- SPREADING TIME SELECTION ---
  # NULL = automatic (finds best time), or specify hour (0-71) within forecast window
  chosen_time_hour = NULL,        # NULL for automatic best time, or integer 0-71
  
  # --- FORECAST OPTIONS ---
  forecast_days = 10,              # Forecast length in days
  
  # --- OUTPUT OPTIONS ---
  output_dir = "C:/Users/shaun/Desktop",  # Directory for output files
  output_prefix = "ALFAM2_Integrated",     # Prefix for output filenames
  
  # Mitigation scenario comparison
  run_mitigation_scenarios = FALSE,  # Compare different applicator types
  
  # Plot options
  save_plots = TRUE,              # Save plots to files
  plot_dpi = 300,                 # Plot resolution
  show_inset_bar = TRUE           # Include N savings inset in main plot
)

# ================================================================================
# TEAGASC AVAILABLE N LOOKUP TABLES
# ================================================================================
# Source: Teagasc manure N tables (interpolated from DM%)
# Units: kg available N per tonne of slurry

TEAGASC_AVAIL_N <- list(
  cattle_slurry = data.frame(
    DM_pct = c(2, 4, 6, 8, 10, 12),
    avail_N_kg_t = c(1.8, 2.3, 2.9, 3.6, 4.3, 5.0)
  ),
  pig_slurry = data.frame(
    DM_pct = c(2, 4, 6, 8),
    avail_N_kg_t = c(2.5, 3.2, 4.0, 4.8)
  ),
  poultry_slurry = data.frame(
    DM_pct = c(20, 25, 30, 35),
    avail_N_kg_t = c(8.0, 10.0, 12.0, 14.0)
  )
)

# ================================================================================
# HELPER FUNCTIONS - LOGISTICS CALCULATIONS
# ================================================================================

#' Calculate pump flow rate from tank size and empty time
#' @param tank_L Tank capacity (litres)
#' @param empty_min Time to empty tank (minutes)
#' @return Flow rate (L/min)
flow_from_tank <- function(tank_L, empty_min) {
  if (is.null(tank_L) || is.null(empty_min) || empty_min <= 0) {
    stop("ERROR: Valid tank_size_L and time_to_empty_min required")
  }
  tank_L / empty_min
}

#' Convert application rate from t/ha to L/ha
#' @param t_per_ha Application rate (tonnes/ha)
#' @param density_kg_m3 Slurry density (kg/m³, default 1000)
#' @return Application rate (L/ha)
t_per_ha_to_L_per_ha <- function(t_per_ha, density_kg_m3 = 1000) {
  # 1 tonne = 1000 kg; volume (m³) = mass (kg) / density (kg/m³); 1 m³ = 1000 L
  (t_per_ha * 1000 / density_kg_m3) * 1000
}

#' Calculate application rate from flow, speed, and width
#' Teagasc calibration formula: AppRate (L/ha) = (Flow (L/min) × 600) / (Speed (km/h) × Width (m))
#' @param flow_Lmin Pump flow rate (L/min)
#' @param speed_kmh Forward speed (km/h)
#' @param width_m Working width (m)
#' @return Application rate (L/ha)
app_rate_from_flow_speed <- function(flow_Lmin, speed_kmh, width_m) {
  if (speed_kmh <= 0 || width_m <= 0) {
    stop("ERROR: Speed and width must be positive")
  }
  (flow_Lmin * 600) / (speed_kmh * width_m)
}

#' Calculate required speed from flow, target app rate, and width
#' Rearranged: Speed (km/h) = (Flow (L/min) × 600) / (AppRate (L/ha) × Width (m))
#' @param flow_Lmin Pump flow rate (L/min)
#' @param app_rate_L_ha Target application rate (L/ha)
#' @param width_m Working width (m)
#' @return Required forward speed (km/h)
speed_from_flow_app_rate <- function(flow_Lmin, app_rate_L_ha, width_m) {
  if (app_rate_L_ha <= 0 || width_m <= 0) {
    stop("ERROR: Application rate and width must be positive")
  }
  (flow_Lmin * 600) / (app_rate_L_ha * width_m)
}

#' Calculate required t/ha to achieve target effective N given predicted loss
#' Formula: required_applied_N = target_effective_N / (1 - predicted_loss_fraction)
#'          required_t_ha = required_applied_N / available_N_per_t
#' @param target_N_kg_ha Target effective N (kg N/ha)
#' @param predicted_loss_frac Predicted ammonia loss fraction (0-1)
#' @param available_kg_per_t Available N in slurry (kg N/t)
#' @return Required application rate (t/ha)
required_t_per_ha_for_target <- function(target_N_kg_ha, predicted_loss_frac, available_kg_per_t) {
  if (predicted_loss_frac >= 1) {
    stop("ERROR: Predicted loss fraction cannot be >= 1 (100%)")
  }
  if (available_kg_per_t <= 0) {
    stop("ERROR: Available N per tonne must be positive")
  }
  required_applied_N <- target_N_kg_ha / (1 - predicted_loss_frac)
  required_applied_N / available_kg_per_t
}

#' Calculate tank loads per hectare
#' @param required_L_per_ha Required application (L/ha)
#' @param tank_L Tank capacity (L)
#' @return Number of tank loads per hectare
tank_loads_per_ha <- function(required_L_per_ha, tank_L) {
  required_L_per_ha / tank_L
}

#' Calculate time per hectare
#' @param required_L_per_ha Required application (L/ha)
#' @param flow_Lmin Pump flow rate (L/min)
#' @return Time per hectare (hours)
time_per_ha_hours <- function(required_L_per_ha, flow_Lmin) {
  (required_L_per_ha / flow_Lmin) / 60
}

# ================================================================================
# TEAGASC AVAILABLE N ESTIMATION
# ================================================================================

#' Estimate available N from Teagasc tables with linear interpolation
#' @param manure_type Manure type (cattle_slurry, pig_slurry, poultry_slurry)
#' @param DM_pct Dry matter percentage
#' @return Available N (kg/t) or NA if not found
estimate_teagasc_available_N <- function(manure_type, DM_pct) {
  type_key <- tolower(manure_type)
  
  if (!type_key %in% names(TEAGASC_AVAIL_N)) {
    warning("Manure type '", manure_type, "' not found in Teagasc tables")
    return(NA_real_)
  }
  
  lookup <- TEAGASC_AVAIL_N[[type_key]]
  
  # Linear interpolation (extrapolation allowed with warning)
  if (DM_pct < min(lookup$DM_pct) || DM_pct > max(lookup$DM_pct)) {
    warning("DM% ", DM_pct, " outside Teagasc table range [", 
            min(lookup$DM_pct), "-", max(lookup$DM_pct), 
            "] - extrapolating")
  }
  
  approx(x = lookup$DM_pct, y = lookup$avail_N_kg_t, xout = DM_pct, rule = 2)$y
}

# ================================================================================
# GEOCODING
# ================================================================================

#' Geocode place name using Nominatim
#' @param place Place name string
#' @return List with lat and lon
geocode_place <- function(place) {
  url <- paste0("https://nominatim.openstreetmap.org/search?format=json&q=", 
                URLencode(place))
  res <- GET(url, user_agent("ALFAM2 Forecast Tool"))
  stop_for_status(res)
  dat <- fromJSON(content(res, "text", encoding = "UTF-8"))
  if (length(dat) == 0) stop("No geocode result for: ", place)
  list(lat = as.numeric(dat$lat[1]), lon = as.numeric(dat$lon[1]))
}

# ================================================================================
# WEATHER FORECAST RETRIEVAL
# ================================================================================

#' Fetch weather forecast using ECMWF (most accurate for Ireland)
#' @param lat Latitude
#' @param lon Longitude
#' @param forecast_days Number of days (max 16)
#' @return Dataframe with datetime, air_temp, rain_rate, wind.2m
get_weather_forecast <- function(lat, lon, forecast_days = 10) {
  
  cat("  Fetching from Open-Meteo API...\n")
  
  # Try ECMWF model first (best for Ireland)
  res <- tryCatch({
    GET("https://api.open-meteo.com/v1/forecast", 
        query = list(
          latitude = lat,
          longitude = lon,
          hourly = "temperature_2m,precipitation,wind_speed_10m",
          forecast_days = min(forecast_days, 16),
          timezone = "UTC"
        ),
        timeout(30))
  }, error = function(e) {
    stop("ERROR: Failed to connect to weather API: ", e$message)
  })
  
  if (status_code(res) != 200) {
    stop("ERROR: Weather API returned error code ", status_code(res))
  }
  
  content_text <- content(res, "text", encoding = "UTF-8")
  
  if (is.null(content_text) || nchar(content_text) == 0) {
    stop("ERROR: Weather API returned empty response")
  }
  
  j <- tryCatch({
    fromJSON(content_text)
  }, error = function(e) {
    stop("ERROR: Failed to parse weather data JSON: ", e$message)
  })
  
  # Validate response structure
  if (is.null(j$hourly)) {
    stop("ERROR: Weather API response missing 'hourly' data")
  }
  
  if (is.null(j$hourly$time)) {
    stop("ERROR: Weather API response missing 'time' field")
  }
  
  # Parse datetime
  dt <- tryCatch({
    ymd_hm(j$hourly$time, tz = "UTC")
  }, error = function(e) {
    stop("ERROR: Failed to parse weather timestamps: ", e$message)
  })
  
  # Build dataframe
  weather_df <- tibble(
    datetime = dt,
    air_temp = as.numeric(j$hourly$temperature_2m),
    rain_rate = as.numeric(j$hourly$precipitation),
    wind.2m = as.numeric(j$hourly$wind_speed_10m) / 3.6  # km/h -> m/s
  )
  
  # Check for missing data
  na_temp <- sum(is.na(weather_df$air_temp))
  na_wind <- sum(is.na(weather_df$wind.2m))
  na_rain <- sum(is.na(weather_df$rain_rate))
  
  if (na_temp > 0) {
    cat("  WARNING:", na_temp, "hours with missing temperature\n")
  }
  if (na_wind > 0) {
    cat("  WARNING:", na_wind, "hours with missing wind speed\n")
  }
  if (na_rain > 0) {
    cat("  Note:", na_rain, "hours with missing precipitation (will use 0)\n")
  }
  
  # Fill missing values
  weather_df <- weather_df %>%
    mutate(
      # Rain: assume 0 if missing
      rain_rate = ifelse(is.na(rain_rate), 0, rain_rate),
      # Temperature: interpolate with max gap of 3 hours
      air_temp = zoo::na.approx(air_temp, na.rm = FALSE, maxgap = 3),
      # Wind: interpolate with max gap of 3 hours
      wind.2m = zoo::na.approx(wind.2m, na.rm = FALSE, maxgap = 3)
    )
  
  # Final check - if still NAs, try linear interpolation without gap limit
  if (any(is.na(weather_df$air_temp))) {
    cat("  Applying aggressive interpolation for temperature gaps...\n")
    weather_df$air_temp <- zoo::na.approx(weather_df$air_temp, na.rm = FALSE)
  }
  
  if (any(is.na(weather_df$wind.2m))) {
    cat("  Applying aggressive interpolation for wind gaps...\n")
    weather_df$wind.2m <- zoo::na.approx(weather_df$wind.2m, na.rm = FALSE)
  }
  
  # If STILL NA at start/end, use nearest value
  if (any(is.na(weather_df$air_temp))) {
    weather_df$air_temp <- zoo::na.locf(weather_df$air_temp, na.rm = FALSE, fromLast = TRUE)
    weather_df$air_temp <- zoo::na.locf(weather_df$air_temp, na.rm = FALSE)
  }
  
  if (any(is.na(weather_df$wind.2m))) {
    weather_df$wind.2m <- zoo::na.locf(weather_df$wind.2m, na.rm = FALSE, fromLast = TRUE)
    weather_df$wind.2m <- zoo::na.locf(weather_df$wind.2m, na.rm = FALSE)
  }
  
  # Final validation
  remaining_na_temp <- sum(is.na(weather_df$air_temp))
  remaining_na_wind <- sum(is.na(weather_df$wind.2m))
  
  if (remaining_na_temp > 0 || remaining_na_wind > 0) {
    cat("\n  ERROR: Unable to fill all missing weather data:\n")
    cat("    Missing temperature:", remaining_na_temp, "hours\n")
    cat("    Missing wind:", remaining_na_wind, "hours\n")
    cat("\n  First few rows:\n")
    print(head(weather_df, 10))
    stop("Weather data quality insufficient for ALFAM2 modeling")
  }
  
  cat("  ✓ Weather data validated:", nrow(weather_df), "hours complete\n")
  
  weather_df
}

# ================================================================================
# ALFAM2 EMISSION MODELING
# ================================================================================

#' Run ALFAM2 for a 168-hour window
#' @param weather_window Weather data for 168 hours
#' @param available_N_kg_t Available N (kg/t)
#' @param app_rate_t_ha Application rate (t/ha)
#' @param app_mthd Application method
#' @param man_dm Manure dry matter (fraction, e.g., 0.06 for 6%)
#' @param man_ph Manure pH
#' @param time_incorp Time to incorporation (hours, NA if not incorporated)
#' @param crop_z Crop height (cm, 0 if bare soil)
#' @return Tibble with cumulative emission (kg N/ha) and loss fraction
run_alfam2_window <- function(weather_window, available_N_kg_t, app_rate_t_ha,
                              app_mthd = "bc", man_dm = 0.06, man_ph = 7.5,
                              time_incorp = NA, crop_z = 0) {
  
  if (nrow(weather_window) != 168) {
    warning("Weather window must be exactly 168 hours, got ", nrow(weather_window))
    return(tibble(cum_emission_kg_ha = NA_real_, loss_fraction = NA_real_))
  }
  
  # Check for missing weather data
  if (any(is.na(weather_window$air_temp)) || any(is.na(weather_window$wind.2m))) {
    warning("Missing weather data in window")
    return(tibble(cum_emission_kg_ha = NA_real_, loss_fraction = NA_real_))
  }
  
  TAN_app <- available_N_kg_t * app_rate_t_ha
  
  if (is.na(TAN_app) || TAN_app <= 0) {
    warning("Invalid TAN application: ", TAN_app)
    return(tibble(cum_emission_kg_ha = NA_real_, loss_fraction = NA_real_))
  }
  
  df_in <- tibble(
    ctime = 0:167,
    TAN.app = TAN_app,
    man.dm = man_dm,
    air.temp = weather_window$air_temp,
    app.mthd = app_mthd,
    man.ph = man_ph,
    rain.rate = weather_window$rain_rate,
    app.rate = app_rate_t_ha,
    wind.2m = weather_window$wind.2m,
    time.incorp = time_incorp,
    crop.z = crop_z
  )
  
  out <- tryCatch({
    alfam2(dat = df_in, app.name = "TAN.app", time.name = "ctime", 
           group = NULL, check = FALSE)
  }, error = function(e) {
    warning("ALFAM2 error: ", e$message)
    return(NULL)
  })
  
  if (is.null(out) || nrow(out) == 0) {
    return(tibble(cum_emission_kg_ha = NA_real_, loss_fraction = NA_real_))
  }
  
  last <- out[nrow(out), ]
  
  # Validate output
  if (is.null(last$e) || is.na(last$e)) {
    warning("ALFAM2 returned NA emission")
    return(tibble(cum_emission_kg_ha = NA_real_, loss_fraction = NA_real_))
  }
  
  tibble(
    cum_emission_kg_ha = last$e,
    loss_fraction = last$e / TAN_app
  )
}

#' Run ALFAM2 across entire forecast with sliding 168h windows
#' @param weather_df Full weather forecast
#' @param available_N_kg_t Available N (kg/t)
#' @param app_rate_t_ha Application rate (t/ha)
#' @param app_mthd Application method
#' @param params Additional ALFAM2 parameters
#' @return Tibble with start_datetime, cum_emission, loss_fraction
run_alfam2_forecast <- function(weather_df, available_N_kg_t, app_rate_t_ha,
                                app_mthd = "bc", params = list()) {
  
  # Sliding 168-hour windows
  n_windows <- nrow(weather_df) - 167
  if (n_windows <= 0) {
    warning("Insufficient data for 168-hour windows: ", nrow(weather_df), " hours available")
    return(tibble(
      start_datetime = as.POSIXct(character()),
      cum_emission_kg_ha = numeric(),
      loss_fraction = numeric()
    ))
  }
  
  cat("    Processing", n_windows, "windows...\n")
  
  # Use progress indication for long runs
  results_list <- vector("list", n_windows)
  
  for (i in 1:n_windows) {
    if (i %% 24 == 0) {  # Progress every 24 hours
      cat("      Window", i, "of", n_windows, "\n")
    }
    
    window <- weather_df[i:(i + 167), ]
    
    result <- run_alfam2_window(
      weather_window = window,
      available_N_kg_t = available_N_kg_t,
      app_rate_t_ha = app_rate_t_ha,
      app_mthd = app_mthd,
      man_dm = params$man_dm %||% 0.06,
      man_ph = params$man_ph %||% 7.5,
      time_incorp = params$time_incorp %||% NA,
      crop_z = params$crop_z %||% 0
    )
    
    results_list[[i]] <- result %>%
      mutate(start_datetime = window$datetime[1])
  }
  
  # Combine all results
  bind_rows(results_list)
}

# ================================================================================
# APPLICATOR TYPE MITIGATION FACTORS
# ================================================================================

#' Get ALFAM2 application method code for applicator type
#' @param applicator_type User-friendly applicator name
#' @return ALFAM2 app.mthd code
get_alfam2_method <- function(applicator_type) {
  methods <- list(
    splashplate = "bc",           # Broadcast (splashplate)
    trailing_hose = "ts",         # Trailing hose
    trailing_shoe = "os",         # Open slot / trailing shoe
    injection = "cs"              # Closed slot injection
  )
  
  type_lower <- tolower(applicator_type)
  code <- methods[[type_lower]]
  
  if (is.null(code)) {
    warning("Unknown applicator type: ", applicator_type, ". Using 'bc' (splashplate)")
    return("bc")
  }
  
  # Validate against ALFAM2 accepted codes
  valid_codes <- c("bc", "bsth", "ts", "os", "cs")
  if (!code %in% valid_codes) {
    warning("Method code '", code, "' may not be valid for ALFAM2. Using 'bc'")
    return("bc")
  }
  
  code
}

# ================================================================================
# MAIN INTEGRATED FORECAST FUNCTION
# ================================================================================

run_integrated_forecast <- function(params = USER_PARAMS) {
  
  cat("\n")
  cat("================================================================================\n")
  cat("ALFAM2 INTEGRATED FORECAST & N APPLICATION TOOL\n")
  cat("================================================================================\n\n")
  
  # --- 1. RESOLVE LOCATION ---
  cat("► STEP 1: Resolving location...\n")
  
  if (!is.null(params$lat) && !is.null(params$lon)) {
    coords <- list(lat = as.numeric(params$lat), lon = as.numeric(params$lon))
    location_name <- sprintf("Lat: %.4f, Lon: %.4f", coords$lat, coords$lon)
  } else if (!is.null(params$place)) {
    coords <- geocode_place(params$place)
    location_name <- params$place
  } else {
    stop("ERROR: Must provide either 'place' or 'lat' & 'lon'")
  }
  
  cat("  Location:", location_name, "\n")
  cat("  Coordinates:", sprintf("%.4f, %.4f", coords$lat, coords$lon), "\n\n")
  
  # --- 2. ESTIMATE AVAILABLE N ---
  cat("► STEP 2: Estimating available N...\n")
  
  if (params$use_lab_tan && !is.null(params$lab_tan_kg_per_t)) {
    available_N_kg_t <- params$lab_tan_kg_per_t
    cat("  Using lab-measured TAN:", available_N_kg_t, "kg N/t\n")
  } else {
    available_N_kg_t <- estimate_teagasc_available_N(
      params$manure_type, 
      params$dry_matter_pct
    )
    if (is.na(available_N_kg_t)) {
      stop("ERROR: Could not estimate available N for ", params$manure_type, 
           " at ", params$dry_matter_pct, "% DM")
    }
    cat("  Manure type:", params$manure_type, "\n")
    cat("  Dry matter:", params$dry_matter_pct, "%\n")
    cat("  Teagasc available N:", round(available_N_kg_t, 2), "kg N/t\n")
  }
  cat("\n")
  
  # --- 3. FETCH WEATHER FORECAST ---
  cat("► STEP 3: Fetching weather forecast...\n")
  cat("  Model: ECMWF/GFS ensemble (Open-Meteo API)\n")
  cat("  Duration:", params$forecast_days, "days\n")
  
  weather_df <- tryCatch({
    get_weather_forecast(
      coords$lat, coords$lon,
      forecast_days = params$forecast_days
    )
  }, error = function(e) {
    cat("\nERROR fetching weather data:\n")
    cat(e$message, "\n\n")
    cat("Troubleshooting tips:\n")
    cat("1. Check internet connection\n")
    cat("2. Verify coordinates are valid (lat:", coords$lat, ", lon:", coords$lon, ")\n")
    cat("3. Try reducing forecast_days parameter\n")
    cat("4. Check Open-Meteo API status: https://open-meteo.com/\n\n")
    stop(e)
  })
  
  if (nrow(weather_df) == 0) {
    stop("ERROR: No weather data returned from API")
  }
  
  cat("\n")
  
  # --- 4. CALCULATE FLOW RATE ---
  cat("► STEP 4: Calculating pump flow rate...\n")
  
  if (!is.null(params$pump_flow_L_min)) {
    flow_Lmin <- params$pump_flow_L_min
    cat("  Using specified flow rate:", flow_Lmin, "L/min\n")
  } else {
    flow_Lmin <- flow_from_tank(params$tank_size_L, params$time_to_empty_min)
    cat("  Tank size:", params$tank_size_L, "L\n")
    cat("  Empty time:", params$time_to_empty_min, "min\n")
    cat("  Calculated flow:", round(flow_Lmin, 1), "L/min\n")
  }
  cat("\n")
  
  # --- 5. RUN ALFAM2 FOR BASE SCENARIO ---
  cat("► STEP 5: Running ALFAM2 simulations...\n")
  cat("  Applicator type:", params$applicator_type, "\n")
  
  app_mthd <- get_alfam2_method(params$applicator_type)
  cat("  ALFAM2 method code:", app_mthd, "\n")
  
  # Initial application rate estimate (will be adjusted based on losses)
  initial_app_rate_t_ha <- 30  # Starting point
  
  alfam2_params <- list(
    man_dm = params$dry_matter_pct / 100,
    man_ph = 7.5,
    time_incorp = NA,
    crop_z = 0
  )
  
  cat("  Weather hours available:", nrow(weather_df), "\n")
  cat("  Possible 168h windows:", nrow(weather_df) - 167, "\n")
  
  # Check if we have enough data
  if (nrow(weather_df) < 168) {
    stop("ERROR: Insufficient weather data. Need at least 168 hours, have ", nrow(weather_df))
  }
  
  cat("  Running ALFAM2 for", nrow(weather_df) - 167, "sliding windows...\n")
  
  results <- run_alfam2_forecast(
    weather_df = weather_df,
    available_N_kg_t = available_N_kg_t,
    app_rate_t_ha = initial_app_rate_t_ha,
    app_mthd = app_mthd,
    params = alfam2_params
  )
  
  cat("  Raw results generated:", nrow(results), "rows\n")
  
  # Check for NA values
  na_count <- sum(is.na(results$cum_emission_kg_ha))
  valid_count <- sum(!is.na(results$cum_emission_kg_ha))
  
  cat("  Valid emissions:", valid_count, "\n")
  cat("  NA emissions:", na_count, "\n")
  
  if (valid_count == 0) {
    cat("\n")
    cat("DEBUG INFO:\n")
    cat("  Available N (kg/t):", available_N_kg_t, "\n")
    cat("  App rate (t/ha):", initial_app_rate_t_ha, "\n")
    cat("  TAN applied (kg/ha):", available_N_kg_t * initial_app_rate_t_ha, "\n")
    cat("  Method code:", app_mthd, "\n")
    cat("  DM fraction:", alfam2_params$man_dm, "\n")
    cat("\n")
    cat("  Weather data sample:\n")
    print(head(weather_df, 3))
    stop("ERROR: ALFAM2 produced no valid emissions. Check parameters above.")
  }
  
  cat("  Completed: ", valid_count, " valid scenarios\n\n")
  
  # --- 6. FIND BEST SPREADING TIME ---
  cat("► STEP 6: Identifying optimal spreading time...\n")
  
  # Debug: Check results structure
  if (nrow(results) == 0) {
    stop("ERROR: No ALFAM2 results generated. Check weather data and ALFAM2 parameters.")
  }
  
  cat("  Total windows:", nrow(results), "\n")
  
  # Filter out NA results
  results_valid <- results %>%
    filter(!is.na(cum_emission_kg_ha), !is.na(loss_fraction))
  
  cat("  Valid windows:", nrow(results_valid), "\n")
  
  if (nrow(results_valid) == 0) {
    stop("ERROR: All ALFAM2 results are NA. Check ALFAM2 parameters and weather data quality.")
  }
  
  # Use valid results from here on
  results <- results_valid
  
  # Filter for working hours (6am to 11pm = hours 6-22 inclusive)
  # Add traffic light classification and rain warnings
  results <- results %>%
    mutate(
      hour_of_day = hour(start_datetime),
      in_working_hours = hour_of_day >= 6 & hour_of_day <= 22
    )
  
  # Join weather data safely
  weather_join <- weather_df %>% 
    select(datetime, rain_rate) %>%
    distinct(datetime, .keep_all = TRUE)
  
  results <- results %>%
    left_join(
      weather_join,
      by = c("start_datetime" = "datetime")
    ) %>%
    mutate(
      # Replace NA rain_rate with 0
      rain_rate = ifelse(is.na(rain_rate), 0, rain_rate)
    )
  
  # Calculate 48h rain separately to avoid warnings
  cat("  Calculating 48h rainfall forecasts...\n")
  results$rain_48h <- map_dbl(results$start_datetime, function(dt) {
    idx <- which(weather_df$datetime == dt)
    if (length(idx) == 0) return(0)
    end_idx <- min(idx + 47, nrow(weather_df))
    sum(weather_df$rain_rate[idx:end_idx], na.rm = TRUE)
  })
  
  # Add remaining classifications
  results <- results %>%
    mutate(
      # Rain ban conditions
      rain_at_application = rain_rate > 0,
      heavy_rain_forecast = rain_48h > 10,
      rain_ban = rain_at_application | heavy_rain_forecast,
      # Traffic light classification
      traffic_light = case_when(
        is.na(loss_fraction) ~ "UNKNOWN",
        rain_ban ~ "RED",
        !in_working_hours ~ "AMBER",
        loss_fraction < 0.15 ~ "GREEN",
        loss_fraction < 0.25 ~ "AMBER",
        TRUE ~ "RED"
      ),
      traffic_light_numeric = case_when(
        traffic_light == "GREEN" ~ 1,
        traffic_light == "AMBER" ~ 2,
        traffic_light == "RED" ~ 3,
        TRUE ~ 4
      )
    )
  
  # Debug: Show traffic light distribution
  traffic_summary <- results %>%
    filter(!is.na(traffic_light)) %>%
    count(traffic_light)
  
  if (nrow(traffic_summary) > 0) {
    # Add numeric values for sorting
    traffic_summary <- traffic_summary %>%
      mutate(
        sort_order = case_when(
          traffic_light == "GREEN" ~ 1,
          traffic_light == "AMBER" ~ 2,
          traffic_light == "RED" ~ 3,
          TRUE ~ 4
        )
      ) %>%
      arrange(sort_order)
    
    cat("  Traffic light distribution:\n")
    for (i in 1:nrow(traffic_summary)) {
      tl <- traffic_summary$traffic_light[i]
      n <- traffic_summary$n[i]
      cat("    ", tl, ": ", n, " windows\n")
    }
  }
  
  if (!is.null(params$chosen_time_hour)) {
    best_idx <- params$chosen_time_hour + 1
    cat("  Using user-specified time (hour", params$chosen_time_hour, ")\n")
    best_result <- results[best_idx, ]
  } else {
    # Find best time: GREEN lights only, or AMBER if no GREEN available
    green_results <- results %>% 
      filter(!is.na(traffic_light), traffic_light == "GREEN", !is.na(loss_fraction))
    
    if (nrow(green_results) > 0) {
      best_result <- green_results %>%
        filter(loss_fraction == min(loss_fraction, na.rm = TRUE)) %>%
        slice(1)
      cat("  ✅ GREEN (excellent) spreading window found\n")
    } else {
      amber_results <- results %>% 
        filter(!is.na(traffic_light), traffic_light == "AMBER", !is.na(loss_fraction))
      
      if (nrow(amber_results) > 0) {
        best_result <- amber_results %>%
          filter(loss_fraction == min(loss_fraction, na.rm = TRUE)) %>%
          slice(1)
        cat("  ⚠️  Only AMBER (acceptable) windows available - no ideal conditions\n")
      } else {
        # No good options - take least bad RED
        red_results <- results %>%
          filter(!is.na(loss_fraction))
        
        if (nrow(red_results) > 0) {
          best_result <- red_results %>%
            filter(loss_fraction == min(loss_fraction, na.rm = TRUE)) %>%
            slice(1)
          cat("  🚫 WARNING: Only RED (poor) conditions available - spreading NOT recommended\n")
        } else {
          stop("ERROR: No valid results found in ALFAM2 simulations")
        }
      }
    }
  }
  
  # Validate best_result has required columns
  if (nrow(best_result) == 0) {
    stop("ERROR: Could not identify best spreading window from results")
  }
  
  worst_result <- results %>%
    filter(!is.na(loss_fraction)) %>%
    filter(loss_fraction == max(loss_fraction, na.rm = TRUE)) %>%
    slice(1)
  
  if (nrow(worst_result) == 0) {
    stop("ERROR: Could not identify worst spreading window from results")
  }
  
  # Safe access to result columns with defaults
  best_traffic <- if("traffic_light" %in% names(best_result)) best_result$traffic_light else "UNKNOWN"
  best_rain_ban <- if("rain_ban" %in% names(best_result) && !is.na(best_result$rain_ban)) best_result$rain_ban else FALSE
  best_working_hours <- if("in_working_hours" %in% names(best_result) && !is.na(best_result$in_working_hours)) best_result$in_working_hours else TRUE
  
  worst_traffic <- if("traffic_light" %in% names(worst_result)) worst_result$traffic_light else "UNKNOWN"
  
  cat("  Best window:", format(best_result$start_datetime, "%Y-%m-%d %H:%M UTC"), 
      "[", best_traffic, "]\n")
  cat("  N loss:", sprintf("%.1f%%", best_result$loss_fraction * 100), "\n")
  
  # Add risk warnings with safe checks
  if (best_rain_ban) {
    cat("  🌧️  RAIN WARNING: Spreading banned due to rain conditions\n")
  }
  if (!best_working_hours) {
    cat("  ⏰ Outside normal working hours (6am-11pm)\n")
  }
  
  cat("  Worst window:", format(worst_result$start_datetime, "%Y-%m-%d %H:%M UTC"), 
      "[", worst_traffic, "]\n")
  cat("  N loss:", sprintf("%.1f%%", worst_result$loss_fraction * 100), "\n\n")
  
  # --- 7. CALCULATE REQUIRED APPLICATION RATE ---
  cat("► STEP 7: Calculating required application rate...\n")
  cat("  Target effective N:", params$target_effective_N_kg_ha, "kg N/ha\n")
  
  # Extract and validate predicted loss fraction
  predicted_loss_frac <- best_result$loss_fraction
  
  # Validation checks
  if (is.null(predicted_loss_frac) || length(predicted_loss_frac) == 0) {
    stop("ERROR: Predicted loss fraction is missing or empty from ALFAM2 results")
  }
  if (length(predicted_loss_frac) > 1) {
    warning("Multiple loss fraction values found, using first value")
    predicted_loss_frac <- predicted_loss_frac[1]
  }
  if (is.na(predicted_loss_frac)) {
    stop("ERROR: Predicted loss fraction is NA - check ALFAM2 simulation results")
  }
  if (predicted_loss_frac >= 1) {
    stop("ERROR: Predicted loss fraction cannot be >= 1 (100%). Check ALFAM2 parameters.")
  }
  if (predicted_loss_frac < 0) {
    stop("ERROR: Predicted loss fraction cannot be negative")
  }
  
  cat("  Predicted N loss:", sprintf("%.1f%%", predicted_loss_frac * 100), "\n")
  
  required_t_ha <- required_t_per_ha_for_target(
    target_N_kg_ha = params$target_effective_N_kg_ha,
    predicted_loss_frac = predicted_loss_frac,
    available_kg_per_t = available_N_kg_t
  )
  
  required_L_ha <- t_per_ha_to_L_per_ha(required_t_ha, params$slurry_density_kg_m3)
  
  cat("  Required application rate:", round(required_t_ha, 1), "t/ha\n")
  cat("  Equivalent:", round(required_L_ha, 0), "L/ha\n")
  cat("  Applied N:", round(required_t_ha * available_N_kg_t, 1), "kg N/ha\n")
  cat("  Effective N (after loss):", 
      round(required_t_ha * available_N_kg_t * (1 - best_result$loss_fraction), 1), 
      "kg N/ha\n\n")
  
  # --- 8. CALCULATE SPREADING LOGISTICS ---
  cat("► STEP 8: Calculating spreading logistics...\n")
  
  required_speed_kmh <- speed_from_flow_app_rate(
    flow_Lmin = flow_Lmin,
    app_rate_L_ha = required_L_ha,
    width_m = params$working_width_m
  )
  
  tank_loads <- tank_loads_per_ha(required_L_ha, params$tank_size_L)
  time_per_ha <- time_per_ha_hours(required_L_ha, flow_Lmin)
  
  cat("  Working width:", params$working_width_m, "m\n")
  cat("  Required forward speed:", round(required_speed_kmh, 2), "km/h\n")
  cat("  Tank loads per hectare:", round(tank_loads, 2), "\n")
  cat("  Spreading time per hectare:", round(time_per_ha, 2), "hours\n")
  
  # Speed warnings
  if (required_speed_kmh < 2) {
    cat("  ⚠️  WARNING: Speed very low (<2 km/h) - consider reducing flow rate\n")
  } else if (required_speed_kmh > 12) {
    cat("  ⚠️  WARNING: Speed very high (>12 km/h) - may be impractical\n")
    cat("      Consider: increasing flow rate or using wider applicator\n")
  } else {
    cat("  ✅ Speed is practical for field operations\n")
  }
  cat("\n")
  
  # --- 9. MITIGATION SCENARIOS (OPTIONAL) ---
  mitigation_results <- NULL
  
  if (params$run_mitigation_scenarios) {
    cat("► STEP 9: Running mitigation scenario comparisons...\n")
    
    applicator_types <- c("splashplate", "trailing_hose", "trailing_shoe", "injection")
    
    mitigation_results <- map_dfr(applicator_types, function(app_type) {
      app_method <- get_alfam2_method(app_type)
      
      # Run only for best time window to save computation
      best_weather <- weather_df %>%
        filter(datetime >= best_result$start_datetime,
               datetime < best_result$start_datetime + hours(168))
      
      # Quick re-run with different method
      result_mitigation <- run_alfam2_window(
        weather_window = best_weather,
        available_N_kg_t = available_N_kg_t,
        app_rate_t_ha = initial_app_rate_t_ha,
        app_mthd = app_method,
        man_dm = alfam2_params$man_dm,
        man_ph = alfam2_params$man_ph,
        time_incorp = alfam2_params$time_incorp,
        crop_z = alfam2_params$crop_z
      )
      
      loss_value <- result_mitigation$loss_fraction
      
      # Recalculate required rate for this method
      req_t_ha <- required_t_per_ha_for_target(
        params$target_effective_N_kg_ha,
        loss_value,
        available_N_kg_t
      )
      
      req_L_ha <- t_per_ha_to_L_per_ha(req_t_ha, params$slurry_density_kg_m3)
      
      req_speed <- speed_from_flow_app_rate(
        flow_Lmin, req_L_ha, params$working_width_m
      )
      
      tibble(
        applicator = app_type,
        loss_pct = loss_value * 100,
        required_t_ha = req_t_ha,
        required_speed_kmh = req_speed,
        n_saved_vs_splashplate = NA_real_
      )
    })
    
    # Calculate N savings vs splashplate
    splashplate_loss <- mitigation_results %>% 
      filter(applicator == "splashplate") %>% 
      pull(loss_pct)
    
    mitigation_results <- mitigation_results %>%
      mutate(
        n_saved_vs_splashplate = (splashplate_loss - loss_pct) * 
          available_N_kg_t * initial_app_rate_t_ha / 100
      )
    
    cat("\n")
    cat("  Mitigation Scenario Results:\n")
    cat("  ", strrep("-", 70), "\n")
    print(mitigation_results, n = Inf)
    cat("\n")
  }
  
  # --- 10. CREATE SUMMARY OUTPUT ---
  cat("► STEP 10: Generating output summary...\n")
  
  summary_output <- tibble(
    Parameter = c(
      "Location",
      "Coordinates (lat, lon)",
      "Manure type",
      "Dry matter (%)",
      "Available N (kg/t)",
      "",
      "Target effective N (kg/ha)",
      "Best spreading time (UTC)",
      "Predicted NH3 loss (%)",
      "",
      "Required application rate (t/ha)",
      "Required application rate (L/ha)",
      "Applied N (kg/ha)",
      "Effective N after loss (kg/ha)",
      "",
      "Pump flow rate (L/min)",
      "Working width (m)",
      "Required forward speed (km/h)",
      "Tank loads per hectare",
      "Spreading time per hectare (hours)",
      "",
      "N saved vs worst timing (kg/ha)",
      "N saved vs worst timing (%)"
    ),
    Value = as.character(c(
      location_name,
      sprintf("%.4f, %.4f", coords$lat, coords$lon),
      params$manure_type,
      params$dry_matter_pct,
      round(available_N_kg_t, 2),
      "",
      params$target_effective_N_kg_ha,
      format(best_result$start_datetime, "%Y-%m-%d %H:%M"),
      sprintf("%.1f%%", best_result$loss_fraction * 100),
      "",
      round(required_t_ha, 1),
      round(required_L_ha, 0),
      round(required_t_ha * available_N_kg_t, 1),
      round(required_t_ha * available_N_kg_t * (1 - best_result$loss_fraction), 1),
      "",
      round(flow_Lmin, 1),
      params$working_width_m,
      round(required_speed_kmh, 2),
      round(tank_loads, 2),
      round(time_per_ha, 2),
      "",
      round((worst_result$loss_fraction - best_result$loss_fraction) * 
              available_N_kg_t * required_t_ha, 1),
      sprintf("%.1f%%", (worst_result$loss_fraction - best_result$loss_fraction) * 100)
    ))
  )
  
  # --- 11. CREATE TRACTOR CARD ---
  tractor_card <- paste0(
    "================================================================================\n",
    "                          TRACTOR SPREADING CARD\n",
    "================================================================================\n\n",
    "Location: ", location_name, "\n",
    "Optimal spreading time: ", format(best_result$start_datetime, "%d %B %Y, %H:%M UTC"), "\n\n",
    "CRITICAL SETTINGS:\n",
    "  • Forward speed: ", round(required_speed_kmh, 2), " km/h\n",
    "  • Working width: ", params$working_width_m, " m\n",
    "  • Application rate: ", round(required_t_ha, 1), " t/ha (", round(required_L_ha, 0), " L/ha)\n\n",
    "LOGISTICS:\n",
    "  • Tank loads per hectare: ", round(tank_loads, 2), "\n",
    "  • Spreading time per hectare: ", round(time_per_ha * 60, 0), " minutes\n",
    "  • Flow rate: ", round(flow_Lmin, 1), " L/min\n\n",
    "PREDICTED PERFORMANCE:\n",
    "  • Ammonia loss: ", sprintf("%.1f%%", best_result$loss_fraction * 100), "\n",
    "  • Effective N delivery: ", round(required_t_ha * available_N_kg_t * (1 - best_result$loss_fraction), 1), 
    " kg N/ha\n",
    "  • N saved vs worst timing: ", 
    round((worst_result$loss_fraction - best_result$loss_fraction) * available_N_kg_t * required_t_ha, 1), 
    " kg N/ha\n\n",
    "Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M %Z"), "\n",
    "================================================================================\n"
  )
  
  # --- 12. SAVE OUTPUTS ---
  if (params$save_plots) {
    output_dir <- params$output_dir
    prefix <- params$output_prefix
    
    # Save summary table
    summary_file <- file.path(output_dir, paste0(prefix, "_summary.csv"))
    write.csv(summary_output, summary_file, row.names = FALSE)
    cat("  ✅ Summary saved:", summary_file, "\n")
    
    # Save tractor card
    card_file <- file.path(output_dir, paste0(prefix, "_tractor_card.txt"))
    writeLines(tractor_card, card_file)
    cat("  ✅ Tractor card saved:", card_file, "\n")
    
    # Save mitigation results if available
    if (!is.null(mitigation_results)) {
      mitigation_file <- file.path(output_dir, paste0(prefix, "_mitigation.csv"))
      write.csv(mitigation_results, mitigation_file, row.names = FALSE)
      cat("  ✅ Mitigation scenarios saved:", mitigation_file, "\n")
    }
  }
  
  # --- 13. CREATE PLOTS ---
  cat("\n► STEP 11: Creating visualizations...\n")
  
  # Plotting furniture (from original code)
  min_dt <- min(results$start_datetime, na.rm = TRUE)
  max_dt <- max(results$start_datetime, na.rm = TRUE)
  today_utc_date <- as.Date(with_tz(Sys.time(), "UTC"))
  day_seq <- seq.Date(from = as.Date(floor_date(min_dt, "day")), 
                      to = as.Date(floor_date(max_dt, "day")), by = "day")
  if (!(today_utc_date %in% day_seq)) day_seq <- sort(unique(c(day_seq, today_utc_date)))
  
  # Working hours shading (06:00-23:00 UTC)
  shade_df <- tibble(
    xmin = as_datetime(day_seq, tz = "UTC") + hours(6), 
    xmax = as_datetime(day_seq, tz = "UTC") + hours(23), 
    ymin = -Inf, 
    ymax = Inf
  ) %>%
    filter(xmax >= min_dt & xmin <= max_dt) %>% 
    mutate(xmin = pmax(xmin, min_dt), xmax = pmin(xmax, max_dt))
  
  # Date labels at bottom
  days_present <- seq.Date(from = as.Date(floor_date(min_dt, "day")), 
                           to = as.Date(floor_date(max_dt, "day")), by = "day")
  date_labels_df <- map_dfr(days_present, function(d) {
    day_times <- results %>% filter(as.Date(start_datetime) == d)
    center <- if (nrow(day_times) == 0) {
      as_datetime(d, tz = "UTC") + hours(12)
    } else {
      median(day_times$start_datetime)
    }
    tibble(date = d, x = center)
  })
  
  # Y-axis calculations
  y_min <- min(results$cum_emission_kg_ha, na.rm = TRUE)
  y_max <- max(results$cum_emission_kg_ha, na.rm = TRUE)
  y_rng <- ifelse(is.finite(y_max - y_min) && (y_max - y_min) > 0, y_max - y_min, 1)
  date_label_y <- y_min - 0.12 * y_rng
  top_annot_y  <- y_max + 0.06 * y_rng
  
  # X position for best window annotation (clipped to plot limits)
  x_center_best_clipped <- pmax(pmin(best_result$start_datetime, 
                                     max_dt + minutes(30)), 
                                min_dt - minutes(30))
  
  # X-axis time breaks
  x_breaks <- seq(from = floor_date(min_dt, "hour"), 
                  to = ceiling_date(max_dt, "hour"), by = "3 hours")
  
  # Min and max points
  min_row <- results %>% filter(!is.na(cum_emission_kg_ha)) %>% 
    slice_min(cum_emission_kg_ha, n = 1, with_ties = FALSE)
  max_row <- results %>% filter(!is.na(cum_emission_kg_ha)) %>% 
    slice_max(cum_emission_kg_ha, n = 1, with_ties = FALSE)
  
  # Rain warnings
  rain_df <- results %>% 
    filter(rain_ban) %>% 
    select(start_datetime, cum_emission_kg_ha) %>%
    rename(y = cum_emission_kg_ha)
  
  # Build main emissions plot (original style)
  subtitle_text <- paste0(
    "ALFAM2 forecast. Light green = 06:00–23:00 UTC (working hours). Dark green = best 4-hour window. ",
    "☔ = rain ban (rain at application or >10 mm in 48 h). ",
    "Applicator: ", params$applicator_type
  )
  
  
  # Best 4-hour window for highlighting
  best_window_df <- data.frame(
    xmin = best_result$start_datetime,
    xmax = best_result$start_datetime + hours(4),
    ymin = -Inf,
    ymax = Inf
  )
  p_main <- ggplot() +
    # Working hours shading (green tint)
    geom_rect(data = shade_df, 
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
              inherit.aes = FALSE, fill = "palegreen3", alpha = 0.12) +
    # Best 4-hour spreading window (darker green highlight)
    geom_rect(data = best_window_df,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "darkgreen", alpha = 0.15) +
    # Main emission line (colored by magnitude)
    geom_line(data = results, 
              aes(x = start_datetime, y = cum_emission_kg_ha, 
                  color = cum_emission_kg_ha), 
              linewidth = 1) +
    scale_color_gradientn(colors = c("green", "orange", "red"), 
                          name = "Cumulative NH₃\n(kg ha⁻¹)", 
                          na.value = "grey50") +
    # Rain ban markers (umbrella emoji)
    {if (nrow(rain_df) > 0) {
      geom_text(data = rain_df, 
                aes(x = start_datetime, y = y), 
                label = "\u2614", 
                color = "dodgerblue", 
                size = 5, 
                vjust = -0.6)
    }} +
    # Best point marker
    geom_point(data = best_result, 
               aes(x = start_datetime, y = cum_emission_kg_ha), 
               shape = 21, size = 3, fill = "green", color = "black") +
    geom_text_repel(data = best_result, 
                    aes(x = start_datetime, y = cum_emission_kg_ha, 
                        label = paste0("Best: ", round(cum_emission_kg_ha, 2))), 
                    nudge_y = -0.06*y_rng, direction = "y", 
                    min.segment.length = 0, size = 3, color = "darkgreen") +
    # Worst point marker
    geom_point(data = worst_result, 
               aes(x = start_datetime, y = cum_emission_kg_ha), 
               shape = 21, size = 3, fill = "red", color = "black") +
    geom_text_repel(data = worst_result, 
                    aes(x = start_datetime, y = cum_emission_kg_ha, 
                        label = paste0("Worst: ", round(cum_emission_kg_ha, 2))), 
                    nudge_y = 0.06*y_rng, direction = "y", 
                    min.segment.length = 0, size = 3, color = "red4") +
    # Date labels at bottom
    geom_text(data = date_labels_df, 
              aes(x = x, y = date_label_y, label = format(date, "%d-%b")), 
              vjust = 1, size = 3.2, fontface = "bold") +
    # X-axis formatting
    scale_x_datetime(breaks = x_breaks, 
                     labels = function(x) format(as_datetime(x, tz = "UTC"), "%H:%M"), 
                     timezone = "UTC", 
                     limits = c(min_dt - hours(1), max_dt + hours(1))) +
    # Y-axis formatting
    scale_y_continuous(expand = expansion(mult = c(0.16, 0.24))) +
    labs(x = NULL, 
         y = "Cumulative NH₃ emissions (kg ha⁻¹) — lower is better",
         title = paste("Ammonia emissions forecast —", location_name),
         subtitle = subtitle_text,
         caption = "Line color (green→orange→red) shows emission magnitude. Generated by ALFAM2 model.") +
    theme_bw(base_size = 12) +
    theme(panel.grid.major = element_line(color = "grey92"), 
          panel.grid.minor = element_line(color = "grey97"), 
          axis.text.x = element_text(angle = 0, hjust = 0.5), 
          legend.position = "right", 
          plot.title = element_text(size = 14, face = "bold"), 
          plot.subtitle = element_text(size = 10), 
          plot.caption = element_text(size = 8), 
          plot.margin = margin(t = 8, r = 10, b = 36, l = 10))
  
  # Add best window annotation with traffic light
  best_traffic_safe <- if("traffic_light" %in% names(best_result) && !is.na(best_result$traffic_light)) {
    best_result$traffic_light
  } else {
    "UNKNOWN"
  }
  
  p_main <- p_main + 
    annotate("label", x = x_center_best_clipped, y = top_annot_y, 
             label = paste0("✅ Best window: ", 
                            format(best_result$start_datetime, "%d-%b %H:%M"), 
                            " (UTC)\n",
                            "Status: ", best_traffic_safe, 
                            " • Loss: ", sprintf("%.1f%%", best_result$loss_fraction * 100)), 
             size = 3.2, color = "darkgreen", fill = "white", 
             label.size = 0.25)
  
  # No inset graph as requested
  p_combined <- p_main
  
  # Save plots
  if (params$save_plots) {
    plot_file <- file.path(output_dir, paste0(prefix, "_forecast.png"))
    ggsave(plot_file, p_combined, width = 14, height = 7, dpi = params$plot_dpi)
    cat("  ✅ Forecast plot saved:", plot_file, "\n")
    
    # Create separate mitigation comparison plot if requested
    if (!is.null(mitigation_results)) {
      p_mitigation <- ggplot(mitigation_results, 
                             aes(x = reorder(applicator, -loss_pct), 
                                 y = loss_pct, fill = applicator)) +
        geom_col(width = 0.6, color = "black") +
        geom_text(aes(label = sprintf("%.1f%%", loss_pct)), 
                  vjust = -0.5, fontface = "bold") +
        geom_text(aes(label = sprintf("Speed: %.1f km/h", required_speed_kmh)),
                  vjust = 1.5, size = 3) +
        scale_fill_brewer(palette = "Set2") +
        labs(
          x = "Applicator type",
          y = "Predicted ammonia loss (% of applied N)",
          title = "Mitigation scenario comparison",
          subtitle = paste("All scenarios at optimal time:", 
                           format(best_result$start_datetime, "%d-%b %H:%M UTC"))
        ) +
        theme_minimal(base_size = 12) +
        theme(
          legend.position = "none",
          plot.title = element_text(face = "bold"),
          axis.text.x = element_text(angle = 15, hjust = 1)
        )
      
      mitigation_plot_file <- file.path(output_dir, paste0(prefix, "_mitigation.png"))
      ggsave(mitigation_plot_file, p_mitigation, width = 10, height = 6, dpi = params$plot_dpi)
      cat("  ✅ Mitigation plot saved:", mitigation_plot_file, "\n")
    }
  } else {
    print(p_combined)
  }
  
  cat("\n")
  cat("================================================================================\n")
  cat("FORECAST COMPLETE\n")
  cat("================================================================================\n\n")
  
  cat("QUICK SUMMARY:\n")
  cat("  • Best spreading time:", format(best_result$start_datetime, "%d-%b %H:%M UTC"), "\n")
  cat("  • Predicted loss:", sprintf("%.1f%%", best_result$loss_fraction * 100), "\n")
  cat("  • Required speed:", round(required_speed_kmh, 2), "km/h\n")
  cat("  • Application rate:", round(required_t_ha, 1), "t/ha\n")
  cat("  • N saved vs worst timing:", 
      round((worst_result$loss_fraction - best_result$loss_fraction) * 
              available_N_kg_t * required_t_ha, 1), "kg/ha\n\n")
  
  # Print tractor card
  cat(tractor_card)
  
  # Return results invisibly
  invisible(list(
    summary = summary_output,
    results = results,
    best_result = best_result,
    worst_result = worst_result,
    mitigation_results = mitigation_results,
    logistics = list(
      required_t_ha = required_t_ha,
      required_L_ha = required_L_ha,
      required_speed_kmh = required_speed_kmh,
      tank_loads = tank_loads,
      time_per_ha = time_per_ha
    ),
    plots = list(
      main = p_combined
    ),
    tractor_card = tractor_card
  ))
}

# ================================================================================
# EXAMPLE RUN - EXECUTE THIS TO RUN THE TOOL
# ================================================================================

# Run with default parameters from USER_PARAMS block at top
results <- run_integrated_forecast(USER_PARAMS)

# ================================================================================
# EXAMPLE: CUSTOM RUN WITH MODIFIED PARAMETERS
# ================================================================================

# Uncomment below to run with custom parameters:
# custom_params <- USER_PARAMS
# custom_params$place <- "Cork, Ireland"
# custom_params$dry_matter_pct <- 8
# custom_params$target_effective_N_kg_ha <- 100
# custom_params$applicator_type <- "injection"
# results_custom <- run_integrated_forecast(custom_params)

# ================================================================================
# END OF SCRIPT
# ================================================================================