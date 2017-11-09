## QUESTION ----

#' Function to Compare Crime Count in Two Suburbs
#'
#' \code{<compare_suburbs>} <This function draw a plot to compare the crime aomunt of two suburbs.>
#' @param crime_data A data.table object with the following columns:
#'     "date" (POSIXct), "suburb" (chr), "postcode" (chr), "offence_level_1" (chr),
#'     "offence_level_2" (chr), "offence_level_3" (chr), "offence_count" (num).
#' @param offence_description A character string of either "OFFENCES AGAINST PROPERTY" or "OFFENCES AGAINST THE PERSON".
#' @param suburbs A two-element character vector. Each element is the name (UPPERCASE)
#'     of an SA suburb.
#' @export
#' @return  A ggplot object showing the correlation in offence count between the two input suburbs.
#' @examples For example, you can input "OFFENCES AGAINST PROPERTY" as offence_description, and
#'    "LONSDALE" and "REYNELLA" as suburbs. You will see a ggplot objects showing the trend of crime
#'    in the two subrubs.
#'
compare_suburbs <- function(crime_data, offence_description, suburbs) {
  require(data.table)
  require(ggplot2)

  # Error catching
  if (length(suburbs) != 2) {
    stop("Please enter two suburbs")
  }

  expected_colnames <- c("date", "suburb", "postcode", "offence_level_1", "offence_level_2",
                         "offence_level_3", "offence_count")

  if (!all.equal(names(crime_data), expected_colnames)) {
    stop(paste("Input table columns need to match: ",
               paste(expected_colnames, collapse = ", ")))
  }

  # Check that the input suburbs and offence description exist in crime_data
  if (any(!suburbs %in% crime_data$suburb) |
      !offence_description %in% crime_data$offence_level_3) {
    stop("Fail to find suburb or description!")
  }

  # Make a data table for plotting using data.table transformations
  # You will need to filter, summarise and group by
  # Expect cols: "date", "suburb", "total_offence_count"
  plot_data <- crime_data[suburb %in% suburbs & offence_level_3 == offence_description, list(total_offence_count = sum(offence_count)), by = list(suburb, date)]
  plot_data[,date := strftime(plot_data$date, "%Y/%m") ]

  # These lines will transform the plot_data structure to allow us to plot
  # correlations. Try them out
  plot_data[, suburb := plyr::mapvalues(suburb, suburbs, c("x", "y"))]

  plot_data <- dcast(plot_data, date ~ suburb, fun = sum,
                     fill = 0, value.var = "total_offence_count")

  # Generate the plot
  suburb <- suburbs[1]
  suburb_to_compare <- suburbs[2]
  min_data <- min(crime$date)
  max_data <- max(crime$date)

  return_plot <- ggplot(plot_data, aes(x = factor(date), group = 1)) +
    geom_line(aes(y = y, colour = suburb)) +
    geom_line(aes(y = x, colour = suburb_to_compare)) +
    scale_x_discrete(name = "Time Line") +
    labs(y = offence_description, title = "Crim Count", subtitle = paste("(", min_data, " ~ ", max_data, ")"))

  return(return_plot)
}











