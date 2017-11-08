## QUESTION ----

#' <Title>
#'
#' \code{<compareSuburbs>} <... does what? A short description.>
#' @param crime_data A data.table object with the following columns:
#'     "date" (POSIXct), "suburb" (chr), "postcode" (chr), "offence_level_1" (chr),
#'     "offence_level_2" (chr), "offence_level_3" (chr), "offence_count" (num).
#' @param offence_description A character string of <What are your expected inputs?>.
#' @param suburbs A two-element character vector. Each element is the name (UPPERCASE)
#'     of an SA suburb.
#' @import data.table
#' @export
#' @return  A ggplot object showing the correlation in offence count between the two input suburbs.
#' @examples
#' <one or two examples showing how to use the function>
compareSuburbs <- function(crime_data, offence_description, suburbs) {
  print("1")
  require(data.table)
  require(ggplot2)

  print("2")

  # Error catching

  if (length(suburbs)!=2) {
    stop("Please enter two suburbs")
  }


  expected_colnames <- c("date", "suburb", "postcode", "offence_level_1", "offence_level_2",
                         "offence_level_3", "offence_count")

  print("3")

  if (!all.equal(names(crime_data), expected_colnames)) {
    stop(paste("Input table columns need to match: ",
               paste(expected_colnames, collapse = ", ")))
  }

  # Check that the input suburbs and offence description exist in crime_data
  if (any(!suburbs %in% crime_data$suburb) |
      !offence_description %in% crime_data$offence_level_3) {
    stop("Fail to find suburb or description!")
  }

  print("4")
  # Make a data table for plotting using data.table transformations
  # You will need to filter, summarise and group by
  # Expect cols: "date", "suburb", "total_offence_count"
  plot_data <- crime_data[suburb %in% suburbs & offence_level_3==offence_description,list(total_offence_count = sum(offence_count)),by=list(suburb,date = month(date))]

  print("5")
  # These lines will transform the plot_data structure to allow us to plot
  # correlations. Try them out
  plot_data[, suburb := plyr::mapvalues(suburb, suburbs, c("x", "y"))]

  plot_data <- dcast(plot_data, date ~ suburb, fun = sum,
                     fill = 0, value.var = "total_offence_count")

  # Generate the plot
  ggplot(plot_data) +
    geom_line(aes(date, y),color='steelblue') +
    geom_line(aes(date, x),color='red') +
    labs(x = "Time Line (Month)",
         y = offence_description)
}









