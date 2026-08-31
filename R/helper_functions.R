#' Convert time components to standardized wall time format for a scheduler
#'
#' This function takes numeric inputs for days, hours, minutes, and seconds, and
#' converts them to a standardized wall time format that can be used in HPC job
#' schedulers. The output format will be "MM[:SS]", "HH:MM:SS" or
#' "D-HH[:MM][:SS]", depending on the values of the input.
#'
#' @param days Numeric. Number of days to convert to wall time format.
#' @param hours Numeric. Number of hours to convert to wall time format.
#' @param minutes Numeric. Number of minutes to convert to wall time format.
#' @param seconds Numeric. Number of seconds to convert to wall time format.
#' @return Character string in format "MM[:SS]", "HH:MM:SS" or "D-HH[:MM][:SS]"
#' representing the total time specified by the input arguments.
#' @name helper_functions
#' @export
format_wall_time <- function(
  days = 0, hours = 0, minutes = 0, seconds = 0
) {
  if (
    any(
      !inherits(c(days, hours, minutes, seconds), "numeric") |
        is.na(c(days, hours, minutes, seconds)) |
        c(days, hours, minutes, seconds) < 0
    )
  ) {
    cli::cli_abort(
      "All inputs must be positive numeric values."
    )
  }
  if (all(c(days, hours, minutes, seconds) == 0)) {
    cli::cli_abort(
      paste("At least one of 'days', 'hours', 'minutes', or 'seconds'",
            "must be greater than 0.")
    )
  }
  hours_string <- sprintf("%02d:", as.integer(hours))
  minutes_string <- sprintf("%02d:", as.integer(minutes))
  seconds_string <- sprintf("%02d", as.integer(seconds))
  if (days > 0) {
    days_string <- sprintf("%d-", as.integer(days))
    wall_time <- paste0(
      days_string, hours_string, minutes_string, seconds_string
    )
  } else {
    wall_time <- paste0(hours_string, minutes_string, seconds_string)
  }
  wall_time
}