#' Create Summary Statistics Plot for Hydrological Data
#' 
#' This function processes and visualizes hydrological discharge data by creating summary 
#' statistics plots for different seasons. It handles data aggregation, seasonal splitting, 
#' and creates a comprehensive visualization with optional smoothing and normalization lines.
#' 
#' @param data A data frame containing at least two columns:
#'   - date: Date column
#'   - value: Numeric column containing discharge measurements in m3/s
#' @param n_years2aggregate Integer specifying the number of years to aggregate in the analysis (default = 5)
#' @param smoothing Logical indicating whether to add a LOESS smoothing line to the plot (default = TRUE)
#' @param plotnorm Logical indicating whether to add a horizontal line representing the long-term norm (default = FALSE)
#' @param print_title Logical indicating whether to display the plot title (default = FALSE)
#' 
#' @return A list containing two elements:
#'   - data: A data frame with computed summary statistics
#'   - plot: A ggplot object with the visualization
#'
#' @details The function processes hydrological years (starting in September) and splits
#'          the data into cold season (Oct-Mar) and warm season (Apr-Sep). It computes
#'          summary statistics (mean, SD, min, max) for each period and creates a
#'          visualization showing the temporal development of these statistics.
plot_summary_stats <- function(data, n_years2aggregate = 5, smoothing = TRUE, 
                               plotnorm = FALSE, print_title = FALSE) {
  # Shift dates to align with hydrological year (starting in September)
  data_shifted <- data |> 
    mutate(
      date = date + days(1) - months(9) - days(1),
      month = month(date),
      quarter = quarter(date),
      grouping = semester(date),
      year = year(date) + 1  # Add one year to account for hydrological year
    )
  
  # Filter incomplete year 2024
  data_shifted <- data_shifted |> 
    filter(year < 2024)
  
  # Compute summary statistics for cold and warm seasons
  data_shifted_stats <- data_shifted |> 
    group_by(grouping, year = cut(year, breaks = seq(1915, 2023, n_years2aggregate))) |> 
    summarize(
      mean = mean(value),
      sd = sd(value),
      min = min(value),
      max = max(value),
      n = n()
    )
  
  # Add summary statistics for the entire year
  data_shifted_stats <- data_shifted_stats |> 
    bind_rows(
      data_shifted |> 
        group_by(year = cut(year, breaks = seq(1915, 2023, n_years2aggregate))) |> 
        summarize(
          mean = mean(value),
          sd = sd(value),
          min = min(value),
          max = max(value),
          n = n()
        ) |> 
        mutate(grouping = 3)
    )
  
  # Rename groupings with descriptive labels
  data_shifted_stats <- data_shifted_stats |> 
    mutate(
      grouping = case_when(
        grouping == 1 ~ "Cold Season",
        grouping == 2 ~ "Warm Season",
        grouping == 3 ~ "Entire Year"
      )
    ) |> 
    arrange(year, grouping)
  
  # Create base plot
  pl <- data_shifted_stats |> 
    na.omit() |> 
    ggplot(aes(x = year, y = mean, color = factor(grouping), group = grouping)) +
    geom_point() +
    geom_line() +
    geom_ribbon(
      aes(ymin = mean - sd, ymax = mean + sd, fill = factor(grouping)),
      alpha = 0.05
    ) +
    xlab("Date Range") +
    ylab("Discharge [m3/s]") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(color = "Legend", fill = "Legend")
  
  # Add optional smoothing line
  if (smoothing) {
    pl <- pl + geom_smooth(method = "loess", se = FALSE, aes(group = grouping))
  }
  
  # Add optional long-term norm line
  if (plotnorm) {
    lt_norm <- data_shifted_stats |> 
      summarize(mean = mean(mean))
    pl <- pl + geom_hline(yintercept = lt_norm$mean, linetype = "dotted")
  }
  
  # Add optional title
  if (print_title) {
    pl <- pl + ggtitle("Development of Cold and Warm Season Discharge")
  }
  
  return(list(data = data_shifted_stats, plot = pl))
}