#' Query job status by ID or user
#'
#' `check_job_status` returns a data frame with requested job information including status.
#'  Also available are scheduler-specific submit functions of the form `check_job_status_<scheduler>`
#'  where `<scheduler>` can be `slurm` or `sbatch` (aliases), `torque` or `qsub`
#'  (aliases), or `local` or `sh` (aliases).
#'
#' @details {
#' `check_job_status` is useful for checking the status of local jobs launched via
#' background processes or system calls, particularly in scripting or pipeline
#' execution.
#'
#' `check_job_status(scheduler = "slurm")` submits via `sacct`.
#' `check_job_status(scheduler = "torque")` submits via `qselect`.
#'
#' \code{check_job_status(scheduler = "local")} is platform-dependent and intended
#' for UNIX-like systems, submiting via `ps`. If a PID does not appear in the `ps`
#' output, it is assumed to have completed and its `STAT` value will be set to
#' `"C"`. For running or sleeping processes, `STAT` reflects the current process
#' state reported by the OS.
#'
#' Columns of the output names are harmonized across systems (e.g., renaming `S`
#' to `STAT` and `COMMAND` to `COMM` if present). It uses `fread()` to parse output.
#'
#' The \code{control} argument can include additional, scheduler-specific arguments
#' as a named list:
#' \describe{
#'    \item{\code{columns}}{
#'      A character vector of scheduler-specific columns to include in
#'      the output. The default for Slurm is \code{c("jobid", "submit", "timelimit",
#'      "start", "end", "state")} and the default for local is \code{c("user",
#'      "pid", "state", "time", "etime", "\%cpu", "\%mem", "comm", "xstat")}
#'    }
#' }
#' }
#'
#' @param scheduler Which scheduler to use for status query. Options are 'qsub'
#' and 'torque' (aliases), sbatch' and 'slurm' (aliases), and 'sh' or 'local'
#' (aliases). Default: 'slurm'
#' @param job_ids A numeric or character vector of job IDs (Slurm/TORQUE) or process
#' IDS (local) to query.
#' @param user Optional character vector of username(s) used to filter processes
#' by owner.
#' @param standardize Logical. If \code{TRUE}, will return standardized values in
#' the status column (e.g., "complete", "failed", etc.). Otherwise, returns raw
#' statuses depending on the scheduler. Default: TRUE
#' @param control A list of additional, scheduler-specific arguments. See Details.
#'
#' @return A data frame with the status information of the queried job(s). Columns
#' may vary depending on \code{columns} argument specification in \code{control}.
#'
#' @importFrom glue glue
#' @importFrom checkmate test_true assert_string assert_subset assert_true
#' assert_integerish assert_flag
#' @importFrom cli cli_warn cli_abort
#' @importFrom data.table fread setnames
#' @importFrom glue glue
#' @importFrom utils type.convert
#'
#' @examples
#' \dontrun{
#'   check_job_status(scheduler = "slurm", job_ids = "16346781")}
#' @author Michael Hallquist, Zach Vig
#' @export
check_job_status <- function(
    job_ids = NULL,
    user = NULL,
    scheduler = "slurm",
    standardize = TRUE,
    control = list()
    ) {
  if(test_true(is.null(job_ids) & is.null(user))) {
    cli_abort(
      "Must specify {.code job_ids}, {.code user}, or both"
    )
  }
  assert_string(scheduler)
  scheduler <- tolower(scheduler) # ignore case
  assert_subset(scheduler, c("qsub", "torque", "sbatch", "slurm", "sh", "local"))
  scheduler <- switch(scheduler,
                      "sbatch" = "slurm",
                      "qsub" = "torque",
                      "sh" = "local",
                      scheduler
  )
  fn <- glue(".check_job_status_{scheduler}_int")
  assert_flag(standardize)
  unknown_args <- setdiff(names(control), formalArgs(fn))
  if (length(unknown_args) > 0) {
    cli_warn(
      "Ignoring control arguments: {unknown_args}"
    )
  }
  out <- do.call(
    fn, args = c(
      list(job_ids = job_ids, user = user),
      control
      )
    )
  return(out)
}

#' Internal function for getting slurm job status
#' @noRd
.check_job_status_slurm_int <- function(
    job_ids = NULL,
    user = NULL,
    standardize = TRUE,
    columns = c("jobid", "submit", "timelimit", "start", "end", "state"),
    ...
    ) {

  jstring <- if (!is.null(job_ids)) paste("-j", paste(job_ids, collapse = ",")) else ""
  ustring <- if (!is.null(user)) paste("-u", paste(user, collapse = ",")) else ""

  # check columns input
  columns <- tolower(columns)
  assert_subset(columns, c("jobid", "submit", "timelimit", "start", "end", "state"))
  assert_true(all(c("jobid", "state") %in% columns))
  sacct_format <- paste(columns, collapse = ",")

  # calls sacct with a job list
  # -P specifies a parsable output separated by pipes
  # -X avoids printing subsidiary jobs within each job id
  cmd <- paste(jstring, ustring, "-X -P -o", sacct_format)
  res <- system2("sacct", args = cmd, stdout = TRUE)

  # handle non-zero exit status -- return empty data
  if (!is.null(attr(res, "status"))) {
    cli_warn(
      "sacct call generated non-zero exit status"
    )
    print(cmd)
    return(data.frame(JobID = job_ids, State = "MISSING"))
  }

  # parse sacct output into data frame
  dt <- fread(text = res, data.table=FALSE)
  dt$JobID <- as.character(dt$JobID)

  if (is.null(job_ids)) {
    merged_df <- dt
  } else {
    base_df <- data.frame(JobID = job_ids, stringsAsFactors = FALSE)
    merged_df <- merge(base_df, dt, by = "JobID", all.x = TRUE) # base R left join
  }
  # fill in missing State values with "MISSING"
  if ("State" %in% names(merged_df)) {
    merged_df$State[is.na(merged_df$State)] <- "MISSING"
  } else {
    merged_df$State <- "MISSING"
  }

  out <- if (standardize) {
    get_standard_status(merged_df, "slurm")
  } else {
    merged_df
  }

  return(out)
}

#' Internal function for getting TORQUE job stats
#' @noRd
.check_job_status_torque_int <- function(
    job_ids = NULL,
    user = NULL,
    standardize = TRUE,
    ...
    ) {

  # TORQUE does not keep information about completed jobs available in qstat or qselect
  # thus, need to log when a job is listed as queued, so that it 'going missing' is
  # evidence of it being completed

  # Retrieve job lists from Torque scheduler via qselect
  if (is.null(user)) {
    user = "$USER"
  }
  q_jobs <- system2("qselect", args = glue("-u {user} -s QW"), stdout = TRUE) # queued jobs
  r_jobs <- system2("qselect", args = glue("-u {user} -s EHRT"), stdout = TRUE) # running jobs
  c_jobs <- system2("qselect", args = glue("-u {user} -s C"), stdout = TRUE) # complete jobs
  m_jobs <- setdiff(job_ids, c(q_jobs, r_jobs, c_jobs)) # missing jobs

  state_labels <- c("queued", "running", "complete", "complete")

  # TORQUE clusters only keep jobs with status C (complete) for a limited period
  # of time. After that, the job comes back as missing.

  # Because of this, if one job finishes at time X and another finishes at time Y,
  # job X will be 'missing' if job Y takes a very long time.

  # Thus, we return any missing jobs as complete, which could be problematic if
  # they are truly missing immediately after submission (as happened with slurm).

  # Ideally, we would track a job within wait_for_job such that it can be missing
  # initially, then move into running, then move into complete.

  job_lists <- list(q_jobs, r_jobs, c_jobs, m_jobs)

  # Create a data frame for each state
  state_dfs <- vector("list", length(job_lists))
  for (i in seq_along(job_lists)) {
    if (length(job_lists[[i]]) > 0L) {
      state_dfs[[i]] <- data.frame(JobID = job_lists[[i]], State = rep(state_labels[i], length(job_lists[[i]])), stringsAsFactors = FALSE)
    } else {
      state_dfs[[i]] <- NULL
    }
  }

  # Combine all job states into one data frame
  state_df <- do.call(rbind, state_dfs)

  if (!is.null(attr(q_jobs, "status"))) {
    cli_warn(
      "qselect call generated non-zero exit status"
    )
    return(data.frame(JobID = job_ids, State = "missing"))
  }

  out <- if (standardize) {
    get_standard_status(state_df, "torque")
  } else {
    state_df
  }

  #job_state <- sub(".*job_state = ([A-z]).*", "\\1", res, perl = TRUE)

  return(out)
}

#' Internal function for getting local job status
#' @noRd
.check_job_status_local_int <- function(
    job_ids = NULL,
    user = NULL,
    standardize = TRUE,
    columns = c("user", "pid", "state", "time", "etime", "%cpu", "%mem", "comm", "xstat"),
    ...
    ) {

  job_ids <- type.convert(job_ids, as.is = TRUE) # convert to integers
  assert_integerish(job_ids)

  jstring <- if (!is.null(job_ids)) paste("-p", paste(job_ids, collapse = ",")) else ""
  ustring <- if (!is.null(user)) paste("-u", paste(user, collapse = ",")) else ""

  columns <- tolower(columns)
  assert_subset(columns, c("user", "pid", "state", "time", "etime", "%cpu", "%mem", "comm", "xstat"))
  assert_true(all(c("pid", "state") %in% columns))
  ps_format <- paste(columns, collapse = ",")

  res <- suppressWarnings(system2("ps", args = paste(jstring, ustring, "-o", ps_format), stdout = TRUE))

  # need to trap res of length 1 (just header row) to avoid data.table bug.
  if (!is.null(attr(res, "status")) && attr(res, "status") != 0) {
    hrow <- strsplit(res, "\\s+")[[1]]
    dt <- data.frame(matrix(NA, nrow = length(job_ids), ncol = length(hrow)))
    names(dt) <- hrow
    dt$PID <- as.integer(job_ids)
  } else {
    stopifnot(length(res) > 1)
    # fread and any other parsing can break down with consecutive spaces in body of output.
    # This happens with lstart and start, avoid these for now.
    # header <- gregexpr("\\b", res[1], perl = T)
    # l2 <- gregexpr("\\b", res[2], perl=T)
    dt <- fread(text = res)
  }

  # fix difference in column naming between FreeBSD and *nux (make all like FreeBSD)
  setnames(dt, c("S", "COMMAND"), c("STAT", "COMM"), skip_absent = TRUE)

  if (is.null(job_ids)) {
    merged_df <- dt
  } else {
    # Build full job ID frame, filling in missing jobs (completed/killed)
    base_df <- data.frame(PID = as.integer(job_ids), stringsAsFactors = FALSE)
    # Full join in base R
    merged_df <- merge(base_df, dt, by = "PID", all = TRUE, sort = FALSE)
  }

  # Replace missing STAT values with "C"
  if ("STAT" %in% names(merged_df)) {
    merged_df$STAT <- substr(merged_df$STAT, 1, 1)
    merged_df$STAT[is.na(merged_df$STAT)] <- "C"
  } else {
    merged_df$STAT <- rep("C", nrow(merged_df))
  }

  out <- if (standardize) {
    get_standard_status(merged_df, "local")
  } else {
    merged_df
  }

  return(out)
}

#' Get standardized status vector from status table
#'
#' `get_standard_status` takes a status table from `check_job_status` and
#' returns a standardized status as a vector for `wait_for_job`
#' @noRd
get_standard_status <- function(status_table, scheduler) {
  if (scheduler %in% c("slurm", "sbatch")) {
    state <- vapply(status_table$State, function(x) {
      switch(x,
             "BOOT_FAIL" = "failed",
             "CANCELLED" = "cancelled",
             "COMPLETED" = "complete",
             "DEADLINE" = "failed",
             "FAILED" = "failed",
             "NODE_FAIL" = "failed",
             "OUT_OF_MEMORY" = "failed",
             "PENDING" = "queued",
             "PREEMPTED" = "failed",
             "RUNNING" = "running",
             "REQUEUED" = "queued",
             "REVOKED" = "failed",
             "SUSPENDED" = "suspended",
             "TIMEOUT" = "failed",
             "MISSING" = "missing", # scheduler has not registered the job
             "unknown"
      )
    }, character(1))
  } else if (scheduler %in% c("sh", "local")) {
    state <- vapply(status_table$STAT, function(x) {
      switch(x,
             "C" = "complete",
             "I" = "running", # idle/sleeping
             "R" = "running",
             "S" = "running", # sleeping
             "T" = "suspended",
             "U" = "running",
             "Z" = "failed", # zombie
             "unknown"
      )
    }, character(1))
  } else if (scheduler %in% c("torque", "qsub")) {
    # QSUB
    state <- status_table$State
  }
  return(state)
}


#' Aliases for slurm
#' @rdname check_job_status
#' @export
check_job_status_sbatch <- check_job_status
#' @rdname check_job_status
#' @export
check_job_status_slurm <- check_job_status

formals(check_job_status_sbatch)$scheduler <- formals(check_job_status_slurm)$scheduler <- "slurm"


#' Aliases for torque
#' @rdname check_job_status
#' @export
check_job_status_qsub <- check_job_status
#' @rdname check_job_status
#' @export
check_job_status_torque <- check_job_status

formals(check_job_status_qsub)$scheduler <- formals(check_job_status_torque)$scheduler <- "torque"

#' Aliases for local
#' @rdname check_job_status
#' @export
check_job_status_sh <- check_job_status
#' @rdname check_job_status
#' @export
check_job_status_local <- check_job_status

formals(check_job_status_sh)$scheduler <- formals(check_job_status_local)$scheduler <- "local"

