#' Standardize scheduler name
#' @param scheduler Scheduler name to standardize
#' @param strict Logical. If \code{TRUE}, throws an error if the scheduler name 
#' is not recognized. Otherwise, returns the input scheduler name.
#' @noRd
.standardize_scheduler_name <- function(scheduler, strict = FALSE) {
  scheduler <- tolower(scheduler)
  scheduler <- switch(scheduler,
    "sbatch" = "slurm",
    "qsub" = "torque",
    "sh" = "local",
    if (strict) {
      cli::cli_abort("Invalid scheduler: {scheduler}")
    } else {
      scheduler
    }
  )
  return(scheduler)
}

#' Get valid scheduler names
#' @noRd
.get_valid_schedulers <- function() {
  return(c("slurm", "torque", "local"))
}