# ALFAM2-DST
An intelligent forecasting tool for optimizing slurry spreading timing to minimize ammonia emissions and maximize nitrogen retention. Uses the ALFAM2 model with real-time weather forecasts to identify optimal spreading windows for Irish farmers.
✨ Features
Core Capabilities

🌦️ 10-day weather forecast integration (ECMWF model via Open-Meteo API)
📊 ALFAM2 modeling - Industry-standard ammonia volatilization model
🚦 Traffic light system - Instant visual guidance (GREEN/AMBER/RED conditions)
⏰ 72-hour rolling forecast - Identifies best spreading windows
🌧️ Rain ban detection - Automatic flagging of unsuitable conditions (rain at application or >10mm in 48h)
🚜 Tractor speed calculator - Precise calibration for target N delivery
📈 N savings quantification - Shows potential savings vs worst timing

Advanced Features

⚖️ Loss-adjusted application rates - Automatically compensates for predicted ammonia losses
🔧 Mitigation scenario comparison - Compare splashplate, trailing hose, trailing shoe, and injection
📍 Location-based - Works anywhere (geocoding via Nominatim)
🇮🇪 Irish regulations - Built-in closed season warnings (Oct 1 - Feb 28)
📥 Tractor card generation - Printable field reference with critical settings
📊 Professional visualizations - Publication-quality forecast graphs


🔧 Requirements
System Requirements

R: ≥4.0.0
RAM: 2GB minimum (4GB recommended)
Storage: 500MB for packages and data
Internet: Required for weather API calls

R Packages
r# Core dependencies
install.packages(c(
  "httr",           # API calls
  "jsonlite",       # JSON parsing
  "dplyr",          # Data manipulation
  "lubridate",      # Date/time handling
  "zoo",            # Time series interpolation
  "ALFAM2",         # Ammonia model
  "purrr",          # Functional programming
  "ggplot2",        # Plotting
  "ggrepel",        # Label positioning
  "cowplot"         # Plot composition
))
External APIs (Free)

Open-Meteo - Weather forecasting (no API key required)
Nominatim - Geocoding (no API key required)
📊 Understanding the Results
Traffic Light System
🟢 GREEN (Good Conditions)

Ammonia loss <15%
No rain ban
Within working hours (06:00-23:00 UTC)
Action: Ideal time to spread

🟡 AMBER (Acceptable Conditions)

Ammonia loss 15-25%
OR outside working hours
Action: Acceptable but not optimal

🔴 RED (Poor Conditions)

Ammonia loss >25%
OR rain ban active
Action: Avoid spreading - high loss risk

Rain Ban Triggers
Spreading is flagged as BANNED when:

Rain at application - Precipitation >0mm at spreading time
Heavy rain forecast - >10mm cumulative rain in next 48 hours

Why it matters: Rain shortly after spreading causes nutrient runoff to waterways (environmental pollution + nutrient loss).
Closed Season (Ireland)
October 1 - February 28: Slurry spreading is prohibited by law.
The tool automatically detects if current date falls in closed season and displays warning:
