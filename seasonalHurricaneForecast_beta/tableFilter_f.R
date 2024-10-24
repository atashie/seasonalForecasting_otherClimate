get_filtered_values <- function(basin_data, dataType, basin, seasonalFilter) {
  filtered_data <- basin_data %>%
    filter(
      dataType == !!dataType,
      basin == !!basin,
      seasonalFilter == !!seasonalFilter
    )
  
  metrics <- c("energy", "all_storms", "hurricanes", "major_hurricanes")
  
  result <- filtered_data %>%
    filter(metric %in% metrics) %>%
    arrange(match(metric, metrics)) %>%
    pull(value)
  
  return(result)
}