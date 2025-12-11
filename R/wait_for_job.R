#' Pause execution of an R script while a scheduled job is not yet complete.
#'
#' `wait_for_job` gives you control over job dependencies within R when the formal
#' PBS depend approach is insufficient, especially in the case of a script that
#' spawns child jobs that need to be scheduled or complete before the parent script
#' should continue.
#'
#' Note that for the \code{scheduler} argument, 'torque' and 'qsub' are aliases;
#' 'slurm' and 'sbatch' are aliases, and 'sh' and 'local' are aliases.
#'
#' @param job_ids One or more job ids of existing TORQUE or Slurm jobs, or process
#' ids of a local process for \code{scheduler = 'sh'}.
#' @param repolling_interval How often to recheck the job status, in seconds.
#' Default: 60
#' @param max_wait How long to wait on the job before giving up, in seconds.
#' Default: 24 hours (86,400 seconds)
#' @param scheduler What scheduler is used for job execution. Options are 'qsub'
#' and 'torque' (aliases), 'sbatch' and 'slurm' (aliases), and 'sh' or 'local'
#' (aliases). Default: 'local'
#' @param quiet Logical. If \code{TRUE}, \code{wait_for_job} will not print out
#' any status updates on jobs. If \code{FALSE}, the function prints out status
#' updates for each tracked job so that the user knows what's holding up progress.
#' Default: \code{TRUE}
#' @param stop_on_timeout Logical. If `TRUE`, the function throws an error if
#' the `max_wait` is exceeded. If `FALSE`, it returns `FALSE` instead of stopping.
#' Default: `TRUE`.
#'
#' @return Returns (invisibly) `TRUE` if all jobs completed successfully, `FALSE`
#' if any job failed or timeout occurred and `stop_on_timeout = FALSE`. Otherwise,
#' stops execution with an error if the timeout is exceeded.
#'
#' @importFrom cli cli_inform cli_abort qty
#' @importFrom checkmate assert_number assert_subset assert_logical
#'
#' @examples
#' \dontrun{
#' wait_for_job(job_ids = "28465826", repolllig_interval = 60L,
#'              scheduler = "local", quiet = TRUE)}
#'
#' @author Michael Hallquist, Zach Vig
#' @export
wait_for_job <- function(
    job_ids,
    repolling_interval = 60L,
    max_wait = 60 * 60 * 24,
    scheduler = "local",
    quiet = TRUE,
    stop_on_timeout = TRUE
    ) {
  assert_number(repolling_interval, lower = 0.1, upper = 2e5)
  assert_number(max_wait, lower = 1, upper = 60 * 60 * 24 * 21) # 21 days
  scheduler <- tolower(scheduler) # ignore case
  assert_subset(scheduler, c("torque", "qsub", "slurm", "sbatch", "sh", "local"))
  assert_logical(quiet)
  assert_logical(stop_on_timeout)

  job_complete <- FALSE
  wait_start <- Sys.time()
  ret_code <- NULL # should be set to TRUE if all jobs complete and FALSE if any job fails

  while (isFALSE(job_complete)) {
    status_table <- check_job_status(job_ids, scheduler)
    status <- get_standard_status(status_table, scheduler)
    # update wait time
    wait_total <- as.numeric(difftime(Sys.time(), wait_start, units = "secs"))
    if (any(status == "running") && isFALSE(quiet)) {
      running_jobs <- job_ids[status == "running"]
      cli_inform(
        "{qty(length(running_jobs))} Job{?s} still running: {running_jobs}"
      )
    }
    if (any(status == "queued") && isFALSE(quiet)) {
      queued_jobs <- job_ids[status == "queued"]
      cli_inform(
        "{qty(length(queued_jobs))} Job{?s} still queued: {queued_jobs}"
      )
    }
    if (any(status == "suspended") && isFALSE(quiet)) {
      suspended_jobs <- job_ids[status == "suspended"]
      cli_inform(
        "{qty(length(suspended_jobs))} Job{?s} suspended: {suspended_jobs}"
      )
    }
    if (any(status == "missing") && isFALSE(quiet)) {
      missing_jobs <- job_ids[status == "missing"]
      cli_inform(
        "{qty(length(missing_jobs))} Job{?s} missing from scheduler response: {missing_jobs}"
      )
    }
    if (wait_total > max_wait) {
      if (isTRUE(stop_on_timeout)) {
        cli_abort(
          "Maximum wait time: {max_wait} exceeded. Stopping execution of parent script because something is wrong."
        )
      } else {
        return(FALSE)
      }
    } else if (all(status %in% c("failed", "complete"))) {
      job_complete <- TRUE # drop out of this loop
      if (isFALSE(quiet)) {
        cli_inform(
          "All jobs have finished."
        )
      }
      if (any(status == "failed")) {
        failed_jobs <- job_ids[status == "failed"]
        cli_inform(
          "The following {qty(length(failed_jobs))} job{?s} failed: {failed_jobs}"
        )
        ret_code <- FALSE
      } else {
        ret_code <- TRUE
      }
    } else {
      Sys.sleep(repolling_interval) # wait and repoll jobs
    }
  }
  return(invisible(ret_code))
}
