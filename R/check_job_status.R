#' Check the status of a job running on an HPC or locally
#'
#' @description \code{check_job_status} returns a data frame with requested job
#' information including status.
#'
#' @details {
#' \code{check_job_status} is useful for checking the status of local jobs
#' launched via background processes or system calls, particularly in scripting 
#' or pipeline execution.
#'
#' \code{check_job_status(scheduler = "slurm")} submits via \code{sacct}.
#' \code{check_job_status(scheduler = "torque")} submits via \code{qselect}.
#'
#' \code{check_job_status(scheduler = "local")} is platform-dependent and
#' intended for UNIX-like systems, submiting via \code{ps}. If a PID does not
#' appear in the \code{ps} output, it is assumed to have completed and its
#' \code{STAT} value will be set to\code{"C"}. For running or sleeping
#' processes, \code{STAT} reflects the current process reported by the OS.
#'
#' Columns of the output names are harmonized across systems (e.g., renaming
#' \code{S} to \code{STAT} and \code{COMMAND} to \code{COMM} if present). It
#' uses [data.table::fread()] to parse output.
#'
#' The \code{control} argument can include additional, scheduler-specific
#' arguments as a named list:
#' \describe{
#'    \item{\code{columns}}{
#'      A character vector of scheduler-specific columns to include in
#'      the output. The default for Slurm is \code{c("jobid", "submit",
#'      "timelimit", "start", "end", "state")} and the default for local is
#'      \code{c("user", "pid", "state", "time", "etime", "\%cpu", "\%mem",
#'      "comm", "xstat")}
#'    }
#'  }
#' }
#'
#' @param job_ids A numeric or character vector of job IDs (Slurm/TORQUE) or
#' process IDS (local) to query.
#' @param user Optional character vector of username(s) used to filter processes
#' by owner.
#' @param scheduler_name Which scheduler to use for status query. Options are
#' 'torque' and 'qsub' (aliases), 'slurm' and 'sbatch' (aliases), and 'local'
#' or 'sh' (aliases).
#' @param standardize_output Logical. If \code{TRUE}, will return standardized
#' values in the status column (e.g., "complete", "failed", etc.). Otherwise,
#' returns raw statuses depending on the scheduler. Default: \code{TRUE}.
#' @param control A list of additional, scheduler-specific arguments. See
#' Details.
#' 
#' @return A data frame with the status information of the queried job(s).
#' Columns may vary depending on \code{columns} argument specification in
#' \code{control}.
#'
#' @examples
#' \dontrun{
#'   check_job_status(scheduler = "slurm", job_ids = "16346781")
#' }
#'
#' @name check_job_status
#' @export
check_job_status <- function(
  job_ids = NULL,
  user = NULL,
  scheduler_name = get_supported_schedulers(),
  standardize_output = TRUE,
  control = NULL
) {

  scheduler_name <- match.arg(scheduler_name)
  scheduler_name <- standardize_scheduler_name(scheduler_name)

  # validate input
  checkmate::assert_character(job_ids, min.len = 1)
  checkmate::assert_string(user, null.ok = TRUE)
  checkmate::assert_flag(standardize_output)
  checkmate::assert_list(control, null.ok = TRUE)
  if(checkmate::test_true(is.null(job_ids) && is.null(user))) {
    cli::cli_abort(
      "Must specify {.code job_ids}, {.code user}, or both"
    )
  }

  out <- .check_job_status(
    job_ids = job_ids,
    user = user,
    scheduler_name = scheduler_name,
    standardize_output = standardize_output,
    control = control,
    .call = rlang::caller_env()
  )

  return(out)
}


#' Internal check_job_status function
#' @noRd
.check_job_status <- function(
  job_ids = NULL,
  user = NULL,
  scheduler_name = NULL,
  standardize_output = TRUE,
  columns = NULL,
  control = NULL,
  .call = rlang::caller_env()
) {

  check_status_function <- switch(
    scheduler_name,
    "slurm" = .check_job_status_slurm,
    "torque" = .check_job_status_torque,
    "local" = .check_job_status_local,
    cli::cli_abort(
      "Unsupported scheduler: {scheduler_name}", internal = TRUE
    )
  )

  # check control arguments
  if (length(control) > 0) {
    unknown_args <- setdiff(
      names(control), methods::formalArgs(check_status_function)
    )
    if (length(unknown_args) > 0) {
      cli::cli_warn(
        "Ignoring control arguments: {unknown_args}",
        call = .call
      )
    }
  }

  out <- rlang::exec(
    check_status_function,
    job_ids = job_ids,
    user = user,
    standardize_output = standardize_output,
    .call = .call,
    !!!control
  )

  return(out)

}

get_default_status_columns <- function(scheduler_name) {
  switch(
    scheduler_name,
    "slurm" = c(
      "jobid", "submit", "timelimit", "start", "end", "state"
    ),
    "local" = c(
      "user", "pid", "state", "time", "etime", "%cpu", "%mem", "comm", "xstat"
    ),
    NULL
  )
}

#' Internal function for getting slurm job status
#' @noRd
.check_job_status_slurm <- function(
  job_ids = NULL,
  user = NULL,
  standardize = TRUE,
  columns = get_default_status_columns("slurm"),
  .call = rlang::caller_env(),
  ...
) {

  build_arg <- function(obj, directive) {
    if (!is.null(obj)) {
      paste(directive, paste(obj, collapse = ","))
    } else {
      ""
    }
  }

  job_id_string <- build_arg(job_ids, "-j")
  user_string <- build_arg(user, "-u")

  # check columns input
  columns <- tolower(columns)
  checkmate::assert_subset(columns, get_default_status_columns("slurm"))
  checkmate::assert_true(all(c("jobid", "state") %in% columns))
  sacct_format <- paste(columns, collapse = ",")

  # calls sacct with a job list
  # -P specifies a parsable output separated by pipes
  # -X avoids printing subsidiary jobs within each job id
  command <- paste(job_id_string, user_string, "-X -P -o", sacct_format)
  result <- system2("sacct", args = command, stdout = TRUE)

  # handle non-zero exit status -- return empty data
  if (!is.null(attr(result, "status"))) {
    cli::cli_warn(
      c("sacct call generated non-zero exit status",
        "Command: sacct {command}"),
      call = .call
    )
    return(data.frame(JobID = job_ids, State = "MISSING"))
  }

  # parse sacct output into data frame
  df <- data.table::fread(text = result, data.table=FALSE)
  df$JobID <- as.character(df$JobID)

  if (is.null(job_ids)) {
    merged_df <- df
  } else {
    base_df <- data.frame(JobID = job_ids, stringsAsFactors = FALSE)
    # base R left join
    merged_df <- merge(base_df, df, by = "JobID", all.x = TRUE)
  }
  # fill in missing State values with "MISSING"
  if ("State" %in% names(merged_df)) {
    merged_df$State[is.na(merged_df$State)] <- "MISSING"
  } else {
    merged_df$State <- "MISSING"
  }

  out <- if (standardize) {
    .standardize_statuses(merged_df, "slurm")
  } else {
    merged_df
  }

  return(out)
}

#' Internal function for getting TORQUE job stats
#' @noRd
.check_job_status_torque <- function(
  job_ids = NULL,
  user = NULL,
  standardize = TRUE,
  .call = rlang::caller_env(),
  ...
) {

  # TORQUE does not keep information about completed jobs available in qstat or
  # qselect thus, need to log when a job is listed as queued, so that it 'going
  # missing' is evidence of it being completed

  # Retrieve job lists from Torque scheduler via qselect
  if (is.null(user)) {
    user <- "$USER"
  }

  get_job_ids <- function(status, user) {
    code <- switch(
      status,
      "queued" = "QW",
      "running" = "EHRT",
      "complete" = "C"
    )
    args <- sprintf("-u %s -s %s", user, code)
    system2("qselect", args = args, stdout = TRUE)
  }

  queued_jobs <- get_job_ids("queued", user)
  running_jobs <- get_job_ids("running", user)
  complete_jobs <- get_job_ids("complete", user)
  missing_jobs <- setdiff(
    job_ids, c(queued_jobs, running_jobs, complete_jobs)
  ) # missing jobs

  state_labels <- c("queued", "running", "complete", "complete")

  # TORQUE clusters only keep jobs with status C (complete) for a limited period
  # of time. After that, the job comes back as missing.

  # Because of this, if one job finishes at time X and another finishes at time
  # Y, job X will be 'missing' if job Y takes a very long time.

  # Thus, we return any missing jobs as complete, which could be problematic if
  # they are truly missing immediately after submission (as happened with
  # slurm).

  # Ideally, we would track a job within wait_for_job such that it can be
  # missing initially, then move into running, then move into complete.

  job_lists <- list(queued_jobs, running_jobs, complete_jobs, missing_jobs)

  create_state_df <- function(job_list, state_label) {
    if (length(job_list) > 0L) {
      state <- rep(state_label, length(job_list))
      data.frame(JobID = job_list, State = state, stringsAsFactors = FALSE)
    } else {
      NULL
    }
  }

  # Create a data frame for each state
  state_dfs <- vector("list", length(job_lists))
  for (i in seq_along(job_lists)) {
    state_dfs[[i]] <- create_state_df(job_lists[[i]], state_labels[i])
  }

  # Combine all job states into one data frame
  state_df <- do.call(rbind, state_dfs)

  if (!is.null(attr(queued_jobs, "status"))) {
    cli::cli_warn(
      "qselect call generated non-zero exit status",
      call = .call
    )
    return(data.frame(JobID = job_ids, State = "missing"))
  }

  out <- if (standardize) {
    .standardize_statuses(state_df, "torque")
  } else {
    state_df
  }

  #job_state <- sub(".*job_state = ([A-z]).*", "\\1", res, perl = TRUE)

  return(out)
}


#' Internal function to check job status for local jobs
#' @noRd
.check_job_status_local <- function(
  job_ids = NULL,
  user = NULL,
  standardize_output = TRUE,
  columns = get_default_status_columns("local"),
  .call = rlang::caller_env(),
  ...
) {

  # make sure local OS is supported
  .assert_local_supported()

  job_ids <- utils::type.convert(job_ids, as.is = TRUE) # convert to integers
  checkmate::assert_integerish(job_ids)

  build_arg <- function(obj, directive) {
    if (!is.null(obj)) {
      paste(directive, paste(obj, collapse = ","))
    } else {
      ""
    }
  }

  job_id_string <- build_arg(job_ids, "-p")
  user_string <- build_arg(user, "-u")

  columns <- tolower(columns)
  checkmate::assert_subset(columns, get_default_status_columns("local"))
  checkmate::assert_true(all(c("pid", "state") %in% columns))
  ps_format <- paste(columns, collapse = ",")

  result <- suppressWarnings(
    system2(
      "ps", args = paste(job_id_string, user_string, "-o", ps_format),
      stdout = TRUE
    )
  )

  # need to trap result of length 1 (just header row) to avoid data.table bug.
  if (!is.null(attr(result, "status")) && attr(result, "status") != 0) {
    header_row <- strsplit(result, "\\s+")[[1]]
    df <- data.frame(
      matrix(NA, nrow = length(job_ids), ncol = length(header_row))
    )
    names(df) <- header_row
    df$PID <- as.integer(job_ids)
  } else {
    checkmate::assert_true(length(result) > 1)
    # fread and any other parsing can break down with consecutive spaces in
    # body of output. This happens with lstart and start, avoid these for now.
    # header <- gregexpr("\\b", result[1], perl = T)
    # l2 <- gregexpr("\\b", result[2], perl=T)
    df <- data.table::fread(text = result)
  }

  # fix difference in column naming between FreeBSD and *nux
  # (make all like FreeBSD)
  data.table::setnames(
    df, c("S", "COMMAND"), c("STAT", "COMM"), skip_absent = TRUE
  )

  if (is.null(job_ids)) {
    merged_df <- df
  } else {
    # Build full job ID frame, filling in missing jobs (completed/killed)
    base_df <- data.frame(PID = as.integer(job_ids), stringsAsFactors = FALSE)
    # Full join in base R
    merged_df <- merge(base_df, df, by = "PID", all = TRUE, sort = FALSE)
  }

  # Replace missing STAT values with "C", i.e., assume missing jobs have
  # completed
  if ("STAT" %in% names(merged_df)) {
    merged_df$STAT <- substr(merged_df$STAT, 1, 1)
    merged_df$STAT[is.na(merged_df$STAT)] <- "C"
  } else {
    merged_df$STAT <- rep("C", nrow(merged_df))
  }

  out <- if (standardize_output) {
    .standardize_statuses(merged_df, "local")
  } else {
    merged_df
  }

  return(out)
}

#' Internal function to throw applicable message based on status
#' @param job_statuses A named vector of job statuses with job IDs as names.
#' @param status_to_check A named vector of statuses to check for in
#' `job_statuses`. Note that the function is vectorized.
#' @return A named logical vector indicating which statuses are present in
#' \code{job_statuses} and used to trigger messages.
#' @noRd
.give_status_update <- function(job_statuses, status_to_check) {
  out <- rep(FALSE, length(status_to_check))
  names(out) <- names(status_to_check)
  for (status in status_to_check) {
    if (any(job_statuses == status)) {
      jobs <- names(job_statuses)[job_statuses == status]
      status_message <- switch(
        status,
        "running" = "still running",
        "queued" = "still queued",
        "suspended" = "suspended",
        "missing" = "missing from scheduler response",
        "failed" = "failed",
      )
      cli::cli_inform(
        "{cli::qty(length(jobs))} job{?s} {status_message}: {jobs}"
      )
      out[status] <- TRUE
    }
  }
  return(invisible(out))
}



#' Get standardized status vector from status table
#' @param status_table A data frame status table from `check_job_status()`
#' @param scheduler_name The scheduler used to submit jobs
#' @return A character vector of standardized job statuses to be used in
#' `wait_for_job()`.
#' @noRd
.standardize_statuses <- function(status_table, scheduler_name) {
  if (scheduler_name %in% c("slurm", "sbatch")) {
    if(!checkmate::test_subset("State", names(status_table))) {
      cli::cli_abort(
        paste("Expected column 'State' not found in status table returned by",
              "check_job_status() for scheduler {scheduler_name}."),
        internal = TRUE
      )
    }
    state <- vapply(
      status_table$State,
      function(x) {
        switch(
          x,
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
      },
      character(1)
    )
  } else if (scheduler_name %in% c("sh", "local")) {
    if (!checkmate::test_subset("STAT", names(status_table))) {
      cli::cli_abort(
        paste("Expected column 'STAT' not found in status table returned by",
              "check_job_status() for scheduler {scheduler_name}."),
        internal = TRUE
      )
    }
    state <- vapply(
      status_table$STAT,
      function(x) {
        switch(
          x,
          "C" = "complete",
          "I" = "running", # idle/sleeping
          "R" = "running",
          "S" = "running", # sleeping
          "T" = "suspended",
          "U" = "running",
          "Z" = "failed", # zombie
          "unknown"
        )
      },
      character(1)
    )
  } else if (scheduler_name %in% c("torque", "qsub")) {
    if (!checkmate::test_subset("State", names(status_table))) {
      cli::cli_abort(
        paste("Expected column 'State' not found in status table returned by",
              "check_job_status() for scheduler {scheduler_name}."),
        internal = TRUE
      )
    }
    state <- status_table$State
  } else {
    cli::cli_abort(
      "Unsupported scheduler: {scheduler_name}", internal = TRUE
    )
  }
  return(state)
}