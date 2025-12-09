#' Invoke R's `system` for local commands
#'
#' @param input A character string. Either the path to a script to execute or
#' a single line command.
#' @param is_script. Logical. Set to \code{TRUE} if the input is a script.
#' @param push_cmd A character string. The command to call to push the script/command
#' @param sched_args A character vector of scheduler arguments.
#' @param echo Logical. If \code{TRUE}, prints final command to console.
#' @param fail_on_error If \code{TRUE}, fails when an error occurs. Otherwise,
#' just a warning is thrown.
#'
#' @importFrom tools file_path_sans_ext
#' @importFrom checkmate test_file_exists
#' @importFrom cli cli_abort cli_warn
#' @keywords internal
invoke_system <- function(
    input,
    is_script = TRUE,
    push_cmd = "sh",
    sched_args = NULL,
    echo = TRUE,
    fail_on_error = FALSE
) {

  # use unique temp files to avoid parallel collisions in job tracking
  file_path <- file_path_sans_ext(basename(input))
  script_label <- if (isTRUE(is_script)) file_path else "oneliner"
  sub_stdout <- paste0(tempfile(), "_", file_path, "_stdout")
  sub_stderr <- paste0(tempfile(), "_", file_path, "_stderr")
  sub_pid <- paste0(tempfile(), "_", file_path, "_pid")

  if (isTRUE(is_script)) {
    run_part <- paste(push_cmd, input)
  } else {
    run_part <- input
  }

  # for direct execution, need to pass environment variables by prepending
  cmd <- paste(sched_args, run_part)
  if (isTRUE(echo)) cat(cmd, "\n") # echo command to terminal
  # submit the job script and return the job_id by forking to background and returning PID
  job_res <- system(paste(cmd, ">", sub_stdout, "2>", sub_stderr, "& echo $! >", sub_pid), wait = FALSE)
  Sys.sleep(.05) # sometimes the pid file is not in place when file.exists executes -- add a bit of time to ensure that it reads
  job_id <- if (file.exists(sub_pid)) {
    scan(file = sub_pid, what = "char", sep = "\n", quiet = TRUE)
  } else {
    ""
  }

  job_err <- if (test_file_exists(sub_stderr)) {
    paste(scan(file = sub_stderr, what = "char", sep = "\n", quiet = TRUE), collapse = ". ")
  } else {
    ""
  }

  if (job_res != 0) {
    job_id <- NULL
    fail_msg <- c("Submission failed: {input}",
                  "{job_err}",
                  "errcode: {job_res}")
    if (isTRUE(fail_on_error)) {
      cli_abort(fail_msg)
    } else {
      cli_warn(fail_msg)
    }
  } else {
    # replace irrelevant details if needed
    job_id <- sub("Submitted batch job ", "", job_id, fixed = TRUE)
  }

  return(job_id)

}

#' Invoke R's `system2` for scheduler commands
#'
#' @param input A character string. Either the path to a script to execute or
#' a single line command.
#' @param is_script. Logical. Set to \code{TRUE} if the input is a script.
#' @param push_cmd A character string. The command to call to push the script/command
#' @param sched_args A character vector of scheduler arguments.
#' @param echo Logical. If \code{TRUE}, prints final command to console.
#' @param fail_on_error If \code{TRUE}, fails when an error occurs. Otherwise,
#' just a warning is thrown.
#'
#' @importFrom checkmate test_file_exists
#' @keywords internal
invoke_system2 <- function(
    input,
    is_script = TRUE,
    push_cmd = "sbatch",
    sched_args = NULL,
    echo = TRUE,
    fail_on_error = FALSE
) {

  # use unique temp files to avoid parallel collisions in job tracking
  file_path <- file_path_sans_ext(basename(input))
  script_label <- if (isTRUE(is_script)) { file_path } else { "oneliner" }
  sub_stdout <- paste0(tempfile(), "_", file_path, "_stdout")
  sub_stderr <- paste0(tempfile(), "_", file_path, "_stderr")
  sub_pid <- paste0(tempfile(), "_", file_path, "_pid")

  if (isTRUE(is_script)) {
    cmd <- paste(push_cmd, sched_args, input)
    if (isTRUE(echo)) cat(cmd, "\n")
    job_res <- system2(push_cmd, args = paste(sched_args, input), stdout = sub_stdout, stderr = sub_stderr)
  } else if (push_cmd == "sbatch") {
    cmd <- paste(push_cmd, sched_args, paste0("--wrap=", shQuote(input)))
    if (isTRUE(echo)) cat(cmd, "\n")
    job_res <- system2(push_cmd, args = paste0(sched_args, " --wrap=", shQuote(input)), stdout = sub_stdout, stderr = sub_stderr)
  } else if (push_cmd == "qsub") {
    cmd <- paste("echo", shQuote(input), "|", push_cmd, sched_args)
    if (isTRUE(echo)) cat(cmd, "\n")
    job_res <- system(paste(cmd, ">", sub_stdout, "2>", sub_stderr))
  } else {
    cmd <- paste(push_cmd, sched_args, input)
    if (isTRUE(echo)) cat(cmd, "\n")
    job_res <- system2(push_cmd, args = paste(sched_args, input), stdout = sub_stdout, stderr = sub_stderr)
  }

  job_id <- if (file.exists(sub_stdout)) {
    scan(file = sub_stdout, what = "char", sep = "\n", quiet = TRUE)
  } else {
    ""
  }


  job_err <- if (test_file_exists(sub_stderr)) {
    paste(scan(file = sub_stderr, what = "char", sep = "\n", quiet = TRUE), collapse = ". ")
  } else {
    ""
  }

  if (job_res != 0) {
    job_id <- NULL
    fail_msg <- c("Submission failed: {input}",
                  "{job_err}",
                  "errcode: {job_res}")
    if (isTRUE(fail_on_error)) {
      cli_abort(fail_msg)
    } else {
      cli_warn(fail_msg)
    }
  } else {
    # replace irrelevant details if needed
    job_id <- sub("Submitted batch job ", "", job_id, fixed = TRUE)
  }

  return(job_id)

}
