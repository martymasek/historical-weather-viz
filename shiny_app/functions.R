
get_location <- function(location_input) {
  
  # get coordinates
  location_coords <- geo(location_input)
  
  # get full place info for coords, in case of user typos or multiple matching places
  full_location <- reverse_geo(
    lat = location_coords$lat, 
    long = location_coords$long,
    full_results = TRUE
  )
  
  return(full_location)
  
}


get_weather_data <- function(location_df, start_dt, end_dt, temp_unit) {
  
  ## configure API -- James's code
  ## URL configured at https://open-meteo.com/en/docs/historical-weather-api
  api_url <- paste0(
    "https://archive-api.open-meteo.com/v1/archive?",
    "latitude=",    location_df$lat,
    "&longitude=",  location_df$long,
    "&start_date=", start_dt,
    "&end_date=",   end_dt,
    "&daily=temperature_2m_max,temperature_2m_min,",
    "&temperature_unit=",tolower(temp_unit),
    "&timezone=auto"
  )
  
  ## fetch data
  resp <- request(api_url) |>
    req_perform()
  
  ## convert to list
  weather_list <- resp_body_string(resp) |>
    fromJSON()
  
  # create clean string for location that will print to subtitle of plot
  location_string <- paste0(
    coalesce(location_df$city, location_df$town),
    ", ",
    location_df$state,
    if (!is.null(location_df$state)) {", "},
    location_df$country
  )
  
  # Clean data
  plot_df <- data.frame(weather_list$daily) |>
    mutate(
      date = ymd(time),
      year_nm = year(date),
      month_nm = factor(month(date),
                        levels = 1:12,
                        labels = month.name)
    ) |>
    pivot_longer(
      cols = contains("temperature"),
      names_to  = "temp_type",
      names_prefix = "temperature_2m_",
      values_to = "temp_value"
    ) |>
    mutate(
      temp_type = case_match(
        temp_type,
        "min"  ~ "Daily Low Temp.",
        "max"  ~ "Daily High Temp."
      ),
      temp_type = factor(
        temp_type,
        levels = c("Daily High Temp.",
                   "Daily Low Temp.")
      ),
      location_nm = location_string
    ) |>
    filter(!is.na(temp_type))
  
  return(plot_df)
  
}

filter_to_month <- function(df, my_month) {
  
  df |> filter(month_nm == my_month)
  
}

plot_boxplot_for_month <- function(df) {

  boxplot_labels_df <- df |>
    group_by(year_nm, temp_type) |>
    summarize(median = median(temp_value),
              min  = min(temp_value),
              max  = max(temp_value),
              .groups = "drop") |>
    pivot_longer(
      cols      = c(median, min, max),
      names_to  = "summary_type",
      values_to = "temp_value"
    )

  temp_min <- min(df$temp_value, na.rm = TRUE)
  temp_max <- max(df$temp_value, na.rm = TRUE)

  y_nudge_amt_median   <- (temp_max - temp_min) * 0.020
  y_nudege_amt_min_max <- (temp_max - temp_min) * 0.025

  ### make the box plots
  my_plot <- ggplot() +
    geom_boxplot(
      data = df,
      mapping = aes(y = temp_value,
                    x = as.factor(year_nm),
                    group = year_nm,
                    fill = as.factor(year_nm))) +
    facet_wrap(
      vars(temp_type),
      ncol = 1,
      scales = "free",
      strip.position = "left"
    ) +
    labs(
      x = "\nYear",
      y = "",
      title = paste0(
        unique(df$month_nm), 
        "'s Daily Temparature Distributions by Year"
      ),
      subtitle = unique(df$location_nm)
    ) +
    geom_text(
      data = boxplot_labels_df |> filter(summary_type %in% c("median")),
      mapping = aes(y = temp_value,
                    x = as.factor(year_nm),
                    label = round(temp_value)),
      nudge_y = y_nudge_amt_median,
      size = 5
    ) +
    geom_text(
      data = boxplot_labels_df |> filter(summary_type %in% c("max")),
      mapping = aes(y = temp_value,
                    x = as.factor(year_nm),
                    label = round(temp_value)),
      nudge_y = y_nudege_amt_min_max,
      size = 4.5
    ) +
    geom_text(
      data = boxplot_labels_df |> filter(summary_type %in% c("min")),
      mapping = aes(y = temp_value,
                    x = as.factor(year_nm),
                    label = round(temp_value)),
      nudge_y = -y_nudege_amt_min_max,
      size = 4.5
    ) +
    theme_bw() +
    theme(
      plot.title.position = "plot",
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 20),
      strip.text = element_text(face = "bold", size = 15),
      strip.background = element_blank(),
      strip.placement = "outside",
      panel.spacing = unit(2, "lines"),
      axis.text = element_text(size = 13),
      axis.title = element_text(size = 15)
    )

  return(my_plot)

}



