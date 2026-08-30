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
#' @param .call The calling environment, used for error messages.
#'
#' @return A data frame with the status information of the queried job(s).
#' Columns may vary depending on \code{columns} argument specification in
#' \code{control}.
#'
#' @examplesIf interactive() && nzchar(Sys.which("sacct"))
#' check_job_status(scheduler = "slurm", job_ids = "16346781")
#'
#' @name check_job_status
#' @export
check_job_status <- function(
  job_ids = NULL,
  user = NULL,
  scheduler_name = get_supported_schedulers(),
  standardize_output = TRUE,
  control = NULL,
  .call = rlang::caller_env()
) {

  job_ids <- .job_obj_guard(job_ids, "check_job_status")

  scheduler_name <- match.arg(scheduler_name)
  scheduler_name <- standardize_scheduler_name(scheduler_name)

  # validate input
  checkmate::assert_character(job_ids, min.len = 1)
  checkmate::assert_string(user, null.ok = TRUE)
  checkmate::assert_flag(standardize_output)
  checkmate::assert_list(control, null.ok = TRUE)
  if (checkmate::test_true(is.null(job_ids) && is.null(user))) {
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

  out
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
      "Unsupported scheduler: {scheduler_name}", .internal = TRUE
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

  out

}


#' @rdname hpcR_utils
#' @export
get_default_status_columns <- function(
  scheduler_name = c("slurm", "torque", "local")
) {
  scheduler_name <- match.arg(scheduler_name)
  if (identical(scheduler_name, "torque")) {
    cli::cli_abort(
      "TORQUE does not support column selection in job status queries",
      .internal = TRUE
    )
  }
  switch(
    scheduler_name,
    "slurm" = c(
      "jobid", "submit", "timelimit", "start", "end", "state"
    ),
    "local" = c(
      "user", "pid", "state", "time", "etime", "%cpu", "%mem", "comm", "xstat"
    )
  )
}

#' Internal function for getting slurm job status
#' @noRd
.check_job_status_slurm <- function(
  job_ids = NULL,
  user = NULL,
  standardize_output = TRUE,
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
  df <- data.table::fread(text = result, data.table = FALSE)
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

  out <- if (standardize_output) {
    .standardize_statuses(merged_df, "slurm")
  } else {
    merged_df
  }

  out
}

#' Internal function for getting TORQUE job stats
#' @noRd
.check_job_status_torque <- function(
  job_ids = NULL,
  user = NULL,
  standardize_output = TRUE,
  .call = rlang::caller_env(),
  ...
) {

  if (is.null(job_ids)) {
    cli::cli_abort(
      "TORQUE status checks require {.code job_ids}",
      call = .call
    )
  }

  state_df <- do.call(
    rbind,
    lapply(job_ids, .resolve_torque_job_status)
  )

  out <- if (standardize_output) {
    .standardize_statuses(state_df, "torque")
  } else {
    state_df
  }

  #job_state <- sub(".*job_state = ([A-z]).*", "\\1", res, perl = TRUE)

  out
}

#' Internal command wrapper for TORQUE qstat.
#' @noRd
.torque_qstat_full <- function(job_id) {
  suppressWarnings(
    system2("qstat", args = c("-f", job_id), stdout = TRUE, stderr = TRUE)
  )
}

#' Internal command wrapper for TORQUE tracejob.
#' @noRd
.torque_tracejob <- function(job_id) {
  suppressWarnings(
    system2("tracejob", args = job_id, stdout = TRUE, stderr = TRUE)
  )
}

#' Resolve one TORQUE job to hpcR's standardized scheduler states.
#' @noRd
.resolve_torque_job_status <- function(job_id) {
  qstat_output <- .torque_qstat_full(job_id)
  if (.status_command_succeeded(qstat_output)) {
    qstat_state <- .parse_torque_qstat_output(qstat_output)
    if (!identical(qstat_state, "unknown")) {
      return(
        data.frame(JobID = job_id, State = qstat_state, stringsAsFactors = FALSE)
      )
    }
  }

  trace_output <- .torque_tracejob(job_id)
  trace_state <- if (.status_command_succeeded(trace_output)) {
    .parse_torque_tracejob_output(trace_output)
  } else {
    "missing"
  }
  data.frame(JobID = job_id, State = trace_state, stringsAsFactors = FALSE)
}

#' Check whether a scheduler status command returned usable output.
#' @noRd
.status_command_succeeded <- function(result) {
  is.null(attr(result, "status")) && length(result) > 0L
}

#' Parse qstat -f output into hpcR's standardized states.
#' @noRd
.parse_torque_qstat_output <- function(output) {
  output <- paste(output, collapse = "\n")
  if (.has_torque_cancel_evidence(output)) return("cancelled")

  job_state <- .parse_torque_field(output, "job_state")
  exit_status <- .parse_torque_field(output, "exit_status")
  if (identical(job_state, character(0))) return("unknown")

  state_map <- c(
    Q = "queued",
    W = "queued",
    H = "queued",
    R = "running",
    E = "running",
    S = "suspended",
    T = "suspended"
  )
  if (job_state %in% names(state_map)) {
    return(unname(state_map[[job_state]]))
  }
  if (identical(job_state, "C")) {
    return(.map_torque_exit_status(exit_status))
  }
  "unknown"
}

#' Parse tracejob output into hpcR's standardized states.
#' @noRd
.parse_torque_tracejob_output <- function(output) {
  output <- paste(output, collapse = "\n")
  if (.has_torque_cancel_evidence(output)) return("cancelled")

  exit_status <- .parse_torque_field(output, "exit_status")
  if (!identical(exit_status, character(0))) {
    return(.map_torque_exit_status(exit_status))
  }

  "missing"
}

#' Extract a simple field assignment from TORQUE status output.
#' @noRd
.parse_torque_field <- function(output, field) {
  lines <- unlist(strsplit(output, "\n", fixed = TRUE))
  pattern <- paste0("^\\s*", field, "\\s*=\\s*([^\\s]+).*$")
  matched <- grep(pattern, lines, perl = TRUE, ignore.case = TRUE, value = TRUE)
  if (!length(matched)) {
    return(character(0))
  }
  sub(pattern, "\\1", matched[[1]], perl = TRUE, ignore.case = TRUE)
}

#' Detect cancellation evidence in TORQUE status/accounting output.
#' @noRd
.has_torque_cancel_evidence <- function(output) {
  grepl("cancel|deleted|removed|qdel", output, ignore.case = TRUE)
}

#' Map TORQUE terminal exit status to hpcR's standardized states.
#' @noRd
.map_torque_exit_status <- function(exit_status) {
  if (identical(exit_status, character(0))) return("unknown")
  exit_status <- suppressWarnings(as.integer(exit_status))
  if (is.na(exit_status)) return("unknown")
  if (identical(exit_status, 0L)) "complete" else "failed"
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
      stdout = TRUE, stderr = FALSE
    )
  )

  # Need to trap missing processes and header-only output to avoid parser
  # failures. For local jobs, absence from ps means the process has finished.
  ps_failed <- !is.null(attr(result, "status")) && attr(result, "status") != 0
  if (ps_failed || length(result) <= 1L) {
    df <- .empty_local_status_table(job_ids, columns)
  } else {
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

  out
}

#' Build an empty local status table for PIDs absent from ps output.
#' @noRd
.empty_local_status_table <- function(job_ids, columns) {
  column_names <- vapply(
    columns,
    function(column) {
      switch(
        column,
        "user" = "USER",
        "pid" = "PID",
        "state" = "STAT",
        "time" = "TIME",
        "etime" = "ELAPSED",
        "%cpu" = "%CPU",
        "%mem" = "%MEM",
        "comm" = "COMM",
        "xstat" = "XSTAT",
        toupper(column)
      )
    },
    character(1)
  )

  df <- data.frame(
    matrix(NA, nrow = length(job_ids), ncol = length(column_names))
  )
  names(df) <- column_names
  if ("PID" %in% names(df)) {
    df$PID <- as.integer(job_ids)
  }
  df
}

#' Internal function to throw applicable message based on status
#' @param job_statuses A named vector of job statuses with job IDs as names.
#' @param status_to_check A vector of statuses to check for in `job_statuses`.
#' Note that the function is vectorized.
#' @return A named logical vector indicating which statuses are present in
#' \code{job_statuses} and used to trigger messages.
#' @noRd
.give_status_update <- function(job_statuses, status_to_check) {
  out <- rep(FALSE, length(status_to_check))
  names(out) <- status_to_check
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
        "cancelled" = "cancelled",
        cli::cli_abort(
          "Unsupported status: {status}", .internal = TRUE
        )
      )
      cli::cli_inform(
        "{cli::qty(length(jobs))} Job{?s} {status_message}: {jobs}"
      )
      out[status] <- TRUE
    }
  }
  invisible(out)
}



#' Get standardized status vector from status table
#' @param status_table A data frame status table from `check_job_status()`
#' @param scheduler_name The scheduler used to submit jobs
#' @return A character vector of standardized job statuses to be used in
#' `wait_for_job()`.
#' @noRd
.standardize_statuses <- function(status_table, scheduler_name) {
  if (scheduler_name %in% c("slurm", "sbatch")) {
    if (!checkmate::test_subset("State", names(status_table))) {
      cli::cli_abort(
        paste("Expected column 'State' not found in status table returned by",
              "check_job_status() for scheduler {scheduler_name}."),
        .internal = TRUE
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
        .internal = TRUE
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
        .internal = TRUE
      )
    }
    state <- status_table$State
  } else {
    cli::cli_abort(
      "Unsupported scheduler: {scheduler_name}", .internal = TRUE
    )
  }
  state
}
