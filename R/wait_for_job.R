#' Pause execution of an R script while a scheduled job is not yet complete.
#'
#' `wait_for_job` gives you control over job dependencies within R when the
#' formal scheduling approach is insufficient, especially in the case of a
#' script that spawns sub jobs that need to be scheduled or complete before the
#' parent script should continue.
#'
#' Note that for the \code{scheduler} argument, 'torque' and 'qsub' are aliases;
#' 'slurm' and 'sbatch' are aliases, and 'sh' and 'local' are aliases.
#'
#' @param job_ids One or more job ids of existing TORQUE or Slurm jobs, or
#' process ids of a local process for \code{scheduler = 'sh'}.
#' @param repolling_interval How often to recheck the job status, in seconds.
#' Default: 60
#' @param max_wait How long to wait on the job before giving up, in seconds.
#' Default: 24 hours (86,400 seconds)
#' @param scheduler_name What scheduler is used for job execution. Options are
#' 'torque' and 'qsub' (aliases), 'slurm' and 'sbatch' (aliases), and 'local' or
#' 'sh' (aliases). Default: 'local'
#' @param quiet Logical. If \code{TRUE}, \code{wait_for_job} will not print out
#' any status updates on jobs. If \code{FALSE}, the function prints out status
#' updates for each tracked job so that the user knows what's holding up
#' progress. Default: \code{TRUE}
#' @param stop_on_timeout Logical. If `TRUE`, the function throws an error if
#' the `max_wait` is exceeded. If `FALSE`, it returns `FALSE` instead of
#' stopping. Default: `TRUE`.
#'
#' @return Returns (invisibly) `TRUE` if all jobs completed successfully,
#' \code{FALSE} if any job failed or timeout occurred and
#' \code{stop_on_timeout = FALSE}. Otherwise, stops execution with an error if
#' the timeout is exceeded.
#'
#'
#' @examplesIf interactive() && nzchar(Sys.which("sacct"))
#' wait_for_job(job_ids = "28465826", repolling_interval = 60L, quiet = TRUE)
#'
#' @name wait_for_job
#' @export
wait_for_job <- function(
  job_ids,
  repolling_interval = 60L,
  max_wait = 60 * 60 * 24,
  scheduler_name = get_supported_schedulers(),
  quiet = TRUE,
  stop_on_timeout = TRUE
) {

  job_ids <- .job_obj_guard(job_ids, "wait_for_job")

  scheduler_name <- match.arg(scheduler_name)

  # validate input
  checkmate::assert_character(job_ids, min.len = 1, min.chars = 1)
  checkmate::assert_number(repolling_interval, lower = 0.1, upper = 2e5)
  checkmate::assert_number(max_wait, lower = 1, upper = 60 * 60 * 24 * 21)
  checkmate::assert_logical(quiet)
  checkmate::assert_logical(stop_on_timeout)

  return_code <- .wait_for_job(
    job_ids = job_ids,
    repolling_interval = repolling_interval,
    max_wait = max_wait,
    scheduler_name = scheduler_name,
    quiet = quiet,
    stop_on_timeout = stop_on_timeout,
    .call = rlang::caller_env()
  )

  return(invisible(return_code))
}

#' Main internal loop function for `wait_for_job()`
#' @noRd
.wait_for_job <- function(
  job_ids,
  repolling_interval,
  max_wait,
  scheduler_name,
  quiet,
  stop_on_timeout,
  .call = rlang::caller_env()
) {

  # initialize loop variables
  job_complete <- FALSE
  wait_start <- Sys.time()
  return_code <- NULL
  terminal_statuses <- c("failed", "cancelled", "complete")
  failure_statuses <- c("failed", "cancelled")

  # start main loop
  while (isFALSE(job_complete)) {
    # get job status information
    job_statuses <- .check_job_status(
      job_ids, scheduler_name = scheduler_name, standardize_output = TRUE,
      .call = .call
    )

    # update wait time
    wait_total <- as.numeric(
      difftime(Sys.time(), wait_start, units = "secs")
    )

    if (isFALSE(quiet)) {
      .give_status_update(
        job_statuses, c(
          "running", "queued", "suspended", "missing", "cancelled"
        )
      )
    }
    if (wait_total > max_wait) {
      if (isTRUE(stop_on_timeout)) {
        cli::cli_abort(
          paste(
            "Maximum wait time: {max_wait} exceeded.",
            "Stopping execution of parent script because something is wrong."
          ),
          call = .call
        )
      } else {
        # if not stopping on timeout, return FALSE
        return(FALSE)
      }
    } else if (all(job_statuses %in% terminal_statuses)) {
      # drop out of the loop
      job_complete <- TRUE
      if (isFALSE(quiet)) {
        cli::cli_inform(
          "All jobs have finished."
        )
      }
      # if any jobs failed or were cancelled, return FALSE; otherwise return TRUE
      unsuccessful <- .give_status_update(job_statuses, failure_statuses)
      return_code <- !any(unsuccessful)
    } else {
      # wait and repoll jobs
      Sys.sleep(repolling_interval)
    }
  }
  return(unname(return_code))
}
