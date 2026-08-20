#' Invoke R's `system` for local commands
#'
#' @description This function is a wrapper for R's native `system`/`system2`
#' functions that throws error codes in a standardized format and returns the
#' job ID/PID. It supports both HPC and local submission via the `where`
#' argument.
#'
#' @details The function writes temporary stdout/stderr files to determine
#' job IDs and error messages.
#'
#' @param input A character string. Either the path to a script to execute or
#' a single line command.
#' @param input_type A character string. Either "script" or "command" to
#' indicate whether the input is a script path or a one-liner command.
#' @param where A character string. Either "local" or "hpc" to indicate whether
#' to submit the job to a HPC scheduler or run locally.
#' @param push_command A character string. The command to call to push the
#' script/command
#' @param scheduler_arguments A character vector of scheduler arguments.
#' @param fail_on_error If \code{TRUE}, fails when an error occurs. Otherwise,
#' just a warning is thrown.
#' @param echo Logical. If \code{TRUE}, prints final command to console.
#'
#' @return A character string of the job ID (for HPC submission) or process ID
#' (for local submission). If submission fails, returns \code{NULL}.
#'
#' @examplesIf interactive() && nzchar(Sys.which("sbatch"))
#' invoke_system(input = "my_script.R", input_type = "script", where = "hpc")
#'
#' @name invoke_system
#' @export
invoke_system <- function(
  input,
  input_type = c("script", "command"),
  where = c("local", "hpc"),
  push_command = "sh",
  scheduler_arguments = NULL,
  fail_on_error = FALSE,
  echo = FALSE
) {

  input_type <- match.arg(input_type)
  where <- match.arg(where)

  # validate arguments
  checkmate::assert_character(input, len = 1)
  checkmate::assert_character(push_command, len = 1)
  checkmate::assert_character(scheduler_arguments, null.ok = TRUE)
  checkmate::assert_flag(fail_on_error)
  checkmate::assert_flag(echo)

  invoke_function <- switch(
    where,
    "local" = .invoke_system_local,
    "hpc" = .invoke_system_hpc
  )

  job_id <- invoke_function(
    input = input,
    input_type = input_type,
    push_command = push_command,
    scheduler_arguments = scheduler_arguments,
    fail_on_error = fail_on_error,
    echo = echo
  )

  return(job_id)

}

#' Internal function to invoke R's `system` for local commands
#' @noRd
.invoke_system_local <- function(
  input,
  input_type = c("script", "oneliner"),
  push_command = "sh",
  scheduler_arguments = NULL,
  fail_on_error = FALSE,
  echo = FALSE
) {

  # use unique temp files to avoid parallel collisions in job tracking
  file_path <- tools::file_path_sans_ext(basename(input))
  script_label <- switch(
    input_type,
    "script" = file_path,
    input_type
  )
  sub_stdout <- paste0(tempfile(), "_", script_label, "_stdout")
  sub_stderr <- paste0(tempfile(), "_", script_label, "_stderr")
  sub_pid <- paste0(tempfile(), "_", script_label, "_pid")

  run_part <- switch(
    input_type,
    "script" = paste(push_command, input),
    "oneliner" = input
  )

  # for direct execution, need to pass environment variables by prepending
  full_command <- paste(scheduler_arguments, run_part)
  if (isTRUE(echo)) cat(full_command, "\n") # echo command to terminal
  # submit the job script and return the job_id by forking to background and
  # returning PID
  job_resolution <- system(
    paste(
      full_command, ">", sub_stdout, "2>", sub_stderr, "& echo $! >", sub_pid
    ),
    wait = FALSE
  )
  # sometimes the pid file is not in place when file.exists executes --
  # add a bit of time to ensure that it reads
  wait_for <- function() checkmate::test_file_exists(sub_pid)
  .wait_until(wait_for, timeout = 5)
  job_id <- if (wait_for()) {
    Sys.sleep(.1)
    scan(file = sub_pid, what = "char", sep = "\n", quiet = TRUE)
  } else {
    ""
  }

  job_error <- if (checkmate::test_file_exists(sub_stderr)) {
    paste(
      scan(file = sub_stderr, what = "char", sep = "\n", quiet = TRUE), 
      collapse = ". "
    )
  } else {
    ""
  }

  if (job_resolution != 0) {
    job_id <- NULL
    fail_msg <- c("Submission failed: {input}",
                  "stderr: {if (nzchar(job_error)) job_error else '(empty)'}",
                  "errcode: {job_resolution}")
    if (isTRUE(fail_on_error)) {
      cli::cli_abort(fail_msg)
    } else {
      cli::cli_warn(fail_msg)
    }
  } else {
    # replace irrelevant details if needed
    job_id <- sub("Submitted batch job ", "", job_id, fixed = TRUE)
  }

  return(job_id)

}

#' Internal function to invoke R's `system`/`system2` for HPC job submission
#' @noRd
.invoke_system_hpc <- function(
  input,
  input_type = c("script", "oneliner"),
  push_command = "sbatch",
  scheduler_arguments = NULL,
  fail_on_error = FALSE,
  echo = FALSE
) {

  input_type <- match.arg(input_type)

  # use unique temp files to avoid parallel collisions in job tracking
  file_path <- tools::file_path_sans_ext(basename(input))
  script_label <- switch(
    input_type,
    "script" = file_path,
    "oneliner" = "oneliner"
  )
  # create output files
  sub_stdout <- paste0(tempfile(), "_", script_label, "_stdout")
  sub_stderr <- paste0(tempfile(), "_", script_label, "_stderr")

  if (input_type == "oneliner" && push_command %in% c("sbatch", "qsub")) {
    command <- switch(
      push_command,
      "sbatch" = "sbatch",
      "qsub" = paste(
        "echo", shQuote(input), "|", "qsub"
      )
    )

    system_arguments <- switch(
      push_command,
      "sbatch" = paste0(scheduler_arguments, " --wrap=", shQuote(input)),
      "qsub" = paste(scheduler_arguments, ">", sub_stdout, "2>", sub_stderr)
    )

    if (isTRUE(echo)) cat(paste(command, system_arguments), "\n")

    job_resolution <- switch(
      push_command,
      "sbatch" = system2(
        command, args = system_arguments, stdout = sub_stdout, 
        stderr = sub_stderr
      ),
      "qsub" = system(paste(command, system_arguments))
    )

  } else {
    system_arguments <- paste(scheduler_arguments, input)

    if (isTRUE(echo)) cat(paste(push_command, system_arguments), "\n")

    job_resolution <- system2(
      push_command, args = system_arguments, stdout = sub_stdout,
      stderr = sub_stderr
    )
  }

  job_id <- if (checkmate::test_file_exists(sub_stdout)) {
    scan(file = sub_stdout, what = "char", sep = "\n", quiet = TRUE)
  } else {
    ""
  }

  job_error <- if (checkmate::test_file_exists(sub_stderr)) {
    paste(
      scan(file = sub_stderr, what = "char", sep = "\n", quiet = TRUE),
      collapse = ". "
    )
  } else {
    ""
  }

  if (job_resolution != 0) {
    job_id <- NULL
    fail_msg <- c("Submission failed: {input}",
                  "stderr: {if (nzchar(job_error)) job_error else '(empty)'}",
                  "errcode: {job_resolution}")
    if (isTRUE(fail_on_error)) {
      cli::cli_abort(fail_msg)
    } else {
      cli::cli_warn(fail_msg)
    }
  } else {
    # replace irrelevant details if needed
    job_id <- sub("Submitted batch job ", "", job_id, fixed = TRUE)
  }

  return(job_id)

}
