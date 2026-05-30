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

#' Handle `variable=value` and `variable` combinations
#' @noRd
.paste_args <- function(str_vec) {
  nms <- names(str_vec)
  sapply(seq_along(str_vec), function(x) {
    if (is.na(str_vec[x])) {
      # in case of NA just return name
      out <- nms[x]
    } else {
      # else return name and quoted argument separated by equal sign
      val <- ifelse(
        grepl("^[\"'].*[\"']$", str_vec[x], perl = TRUE),
        str_vec[x], paste0("\"", str_vec[x], "\"")
      )
      out <- paste0(nms[x], "=", val)
    }
    return(out)
  })
}

#' Assert local scheduler support on the current platform
#' @noRd
.assert_local_supported <- function() {
  if (.Platform$OS.type == "windows") {
    cli::cli_abort(
      paste("Local scheduler requires UNIX-like shell tools (backgrounding +",
            "ps); unsupported on Windows.")
    )
  }
}
