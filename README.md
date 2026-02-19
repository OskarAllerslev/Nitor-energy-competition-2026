# Nitor-energy-competition-2026
This is our submission for the nitor energy competition 2026



# Feature engineering

- Markets: vi skal nok lave markets om til dummy variable 



# Features


📚 Feature Documentation
Moisture / temperature
temperature_2m: Air temperature measured ~2 meters above ground.
apparent_temperature: “Feels like” temperature adjusted for wind and humidity (and sometimes radiation).
dew_point_2m: Temperature at which air would become saturated (higher = more moisture in the air).
wet_bulb_temperature_2m: Temperature air would cool to by evaporation at constant pressure (proxy for combined heat + humidity stress).
relative_humidity_2m: Percent saturation of air at ~2 m (how close the air is to holding maximum water vapor).
Pressure / boundary layer
surface_pressure: Atmospheric pressure at the surface (influences winds; often drops near storms).
boundary_layer_height: Estimated depth of the turbulent “mixed” layer near the surface (higher = stronger mixing, often daytime/heating driven).
freezing_level_height: Altitude where temperature crosses 0°C (useful for snow vs rain, icing risk).
Clouds / visibility
cloud_cover: Total sky cover by clouds (0–100%).
cloud_cover_low: Cloud cover in the low-level layer (e.g., stratus/fog-type regimes).
cloud_cover_mid: Cloud cover in the mid-level layer.
cloud_cover_high: Cloud cover in the high-level layer (e.g., cirrus).
visibility: Horizontal distance you can see (lower often means fog, heavy precipitation, haze).
Wind
wind_speed_10m: Sustained wind speed ~10 m above ground.
wind_gusts_10m: Short peak wind speed (gust) ~10 m above ground.
wind_speed_80m: Wind speed at ~80 m (often relevant for wind energy / low-level jets).
wind_direction_80m: Direction wind is coming from at ~80 m (degrees).
Radiation / solar
shortwave_radiation: Total incoming solar radiation at the surface (direct + diffuse).
direct_normal_irradiance: Solar radiation coming directly from the sun on a surface perpendicular to the sun’s rays (high on clear days).
diffuse_radiation: Solar radiation scattered by atmosphere/clouds arriving from all directions (higher when cloudy/hazy relative to direct).
Precipitation
precipitation: Amount of precipitation over the period (rain/snow water equivalent depending on product).
precipitation_probability: Probability (0–100%) that precipitation occurs in the period.
Convection / thunderstorm potential
cape: Convective Available Potential Energy — “fuel” for updrafts; higher often means more potential for strong convection/storms.
convective_inhibition: Energy barrier preventing convection (“cap”); stronger inhibition suppresses storm initiation until it breaks.
lifted_index: Stability index comparing a lifted air parcel to surrounding air aloft; lower (more negative) typically indicates more instability.
Energy forecasts (MW)
solar_forecast: Forecasted solar power generation in megawatts (MW) based on expected irradiance, cloud cover, and panel efficiency. Influenced by time of day, season, weather conditions (clear vs cloudy), and atmospheric factors affecting solar radiation reaching ground level. Higher MW values indicate stronger solar generation expected.

wind_forecast: Forecasted wind power generation in megawatts (MW) based on expected wind speeds at turbine hub height (typically 80-100m). Dependent on weather patterns, pressure systems, and local topography. Wind power output typically has a cubic relationship with wind speed between cut-in and rated speeds. Forecasts account for both sustained winds and variability.

demand_forecast: Forecasted electricity demand in megawatts (MW) for the region over the forecast period. Driven by temperature (heating/cooling loads), time of day (daily patterns), day of week (weekday vs weekend), season, and economic activity. Demand typically peaks during temperature extremes (very hot or very cold) and during business hours on weekdays. Holiday patterns and special events can also impact demand.