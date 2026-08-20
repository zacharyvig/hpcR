#' Internal function to hydrate a job with default values.
#' @noRd
.hydrate_defaults <- function(job) {
  if (S7::prop(job, ".defaulted"))  {
    return(job)
  }
  job <- update_job(
    e1 = job, e2 = .job_defaults(),
    skip_validation = TRUE,
    overwrite = FALSE
  )
  job@.defaulted <- TRUE
  job
}

#' Internal function to create a job update object with default values.
#' @noRd
.job_defaults <- function() {
  defaults <- list(
    packages = list(
      install = "never"
    ),
    .run_settings = list(
      print_session_info = FALSE,
      print_environment = FALSE
    )
  )
  class_job_update(updates = defaults)
}