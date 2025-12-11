#' Submit a script or command to a high-performance cluster using a scheduler
#'
#' `submit_job` submits a single script or a single command to a high-performance
#' cluster (HPC) using a specified scheduler (TORQUE, Slurm) or locally. The function
#' `submit_cmd` is available to the user for submitting single commands
#' (`submit_job` assumes a script by default; see `is_script` argument). Also
#' available are scheduler-specific submit functions of the form `submit_job_<scheduler>`
#' or `submit_cmd_<scheduler>` where `<scheduler>` can be `slurm` or `sbatch` (aliases),
#' `torque` or `qsub` (aliases), or `local` or `sh` (aliases).
#'
#' The \code{control} argument can include additional, scheduler-specific arguments
#' as a named list:
#' \describe{
#'    \item{\code{export_all}}{
#'    (For TORQUE or Slurm schedulers) Whether to export all environment variables
#'    to the compute node at runtime. Default: FALSE
#'    }
#'    \item{\code{sched_args}}{
#'    An optional character vector of arguments to be included in
#'    the scheduling command. On TORQUE, these will typically begin with '-l'
#'    such as '-l wall_time=10:00:00'.
#'    }
#'    \item{\code{wait_signal}}{
#'    (For TORQUE or Slurm schedulers) The signal that should
#'    indicate that parent jobs have finished. Default: 'afterok'
#'    }
#'    \item{\code{repolling_interval}}{
#'    (For local jobs) The number of seconds to wait
#'    before rechecking job status. Default: 60
#'    }
#'    \item{\code{max_wait}}{
#'    How long to wait on the job before giving up, in seconds.
#'    Default: 24 hours (86,400 seconds)}
#' }
#'
#' @param input A path to a script that should be executed by the scheduler or a
#' single command to be run. In the case of conflicts, the directives passed with
#' \code{sched_args} will take precedence. Required.
#' @param scheduler Which scheduler to use for job submission. Options are 'qsub',
#' 'torque', 'sbatch', 'slurm', 'sh', or 'local'. The terms 'qsub' and 'torque'
#' are aliases (where 'torque' submits via the qsub command). Likewise for 'sbatch'
#' and 'slurm'. The scheduler 'sh' (alias, 'local') does not submit to any scheduler
#' at all, but instead executes the command immediately via sh. Default: 'slurm'
#' @param is_script Logical. If \code{TRUE}, assumes \code{input} is a script. If
#' false, assumes \code{input} is a single command. If NULL, the function will
#' internally determine what the input is. Default: \code{TRUE}
#' @param echo Whether to echo the job submission command to the terminal at the
#' time it is scheduled. Default: \code{TRUE}
#' @param fail_on_error Whether to stop execution of the script (\code{TRUE}), or issue
#' a warning (\code{FALSE}) if the job submission fails. Defaults: FALSE (i.e., issue a
#' warning).
#' @param wait_jobs An optional character string of jobs or process ids that should
#' complete before this job is executed
#' @param env_variables An optional named character vector containing environment variables
#' and their values to be passed to the \code{script} at execution time. This is
#' handled by the '-v' directive on TORQUE clusters and by '--export' on Slurm
#' clusters. The names of this vector are the environment variable names and the
#' values of the vector are the environment variable values to be passed in. If you
#' want to propagate the current value of an environment variable to the compute
#' node at runtime, use NA as the value of the element in \code{env_variables}.
#' See examples.
#' @param control A list of additional, scheduler-specific arguments. See Details.
#'
#' @return A character string containing the job ID of the scheduled job.
#'
#' @importFrom cli cli_warn cli_abort cli_inform
#' @importFrom checkmate assert_string assert_subset assert_logical
#' assert_file_exists assert_character assert_list assert_integerish
#' @importFrom glue glue
#' @importFrom methods formalArgs
#'
#' @examples
#' \dontrun{
#' cmd <- 'squeue'
#' job_id <- submit_job(scheduler = "slurm", input = cmd, is_script = FALSE)}
#' @author Michael Hallquist, Zach Vig
#' @export
submit_job <- function(
    input,
    scheduler = "slurm",
    is_script = TRUE,
    echo = TRUE,
    fail_on_error = FALSE,
    wait_jobs = NULL,
    env_variables = NULL,
    control = list()
  ) {
  assert_string(scheduler)
  scheduler <- tolower(scheduler) # ignore case
  assert_subset(scheduler, c("qsub", "torque", "sbatch", "slurm", "sh", "local"))
  scheduler <- switch(scheduler,
    "sbatch" = "slurm",
    "qsub" = "torque",
    "sh" = "local",
    scheduler
  )
  fn <- glue(".submit_job_{scheduler}_int")
  if (missing(input)) {
    cli_abort(
      "Argument {.code input} is required"
    )
  } else {
    assert_string(input)
  }
  if (is.null(is_script)) {
    is_script <- file.exists(input)
  } else {
    assert_logical(is_script, max.len = 1L)
  }
  if (is_script) {
    assert_file_exists(input)
  }
  assert_logical(echo, max.len = 1L)
  assert_logical(fail_on_error, max.len = 1L)
  if (!is.null(wait_jobs)) {
    wait_jobs <- as.character(wait_jobs)
    assert_character(wait_jobs)
  }
  assert_list(control)
  if (length(control) > 0) {
    control[sapply(control, is.null)] <- NULL
  }
  unknown_args <- setdiff(names(control), formalArgs(fn))
  if (length(unknown_args) > 0) {
    cli_warn(
      "Ignoring control arguments: {unknown_args}"
    )
  }
  out <- do.call(
    fn,
    args = c(
      # universal arguments
      list(
        input = input,
        is_script = is_script,
        echo = echo,
        fail_on_error = fail_on_error,
        wait_jobs = wait_jobs,
        env_variables = env_variables
      ),
      # additional arguments
      control
      )
    )
  return(out)
}

#' Internal function for submitting jobs to slurm
#' @noRd
.submit_job_slurm_int <- function(
    input = NULL,
    is_script = TRUE,
    echo = TRUE,
    fail_on_error = FALSE,
    wait_jobs = NULL,
    env_variables = NULL,
    sched_args = NULL,
    export_all = FALSE,
    wait_signal = "afterok",
    # tracking_db = NULL,
    # tracking_args = NULL
    ...
    ) {
  if (!is.null(sched_args)) {
    # scheduler arguments are pasted together with spaces
    # arguments like '--mem=5g' and '-n 12' are not handled differently
    sched_args <- paste(sched_args, collapse = " ")
  }
  if (!is.null(wait_signal)) assert_string(wait_signal)
  if (!is.null(wait_jobs)) {
    jcomb <- paste(wait_jobs, collapse = ":") # multiple jobs are separated by colons
    sched_args <- paste0(sched_args, " --dependency=", wait_signal, ":", jcomb)
  }
  if (!is.null(env_variables)) {
    env_variables <- paste0("--export=", paste(paste_args(env_variables), collapse = ","))
    sched_args <- paste(sched_args, env_variables)
  }
  assert_logical(export_all, max.len = 1L)
  if (isTRUE(export_all)) {
    # directive to export all environment variables to script
    env_variables <- c(ALL = NA, env_variables)
  }
  job_id <- invoke_system2(
    input = input,
    is_script = is_script,
    push_cmd = "sbatch",
    sched_args = sched_args,
    echo = echo,
    fail_on_error = fail_on_error
  )
  return(job_id)
}

#' Internal function for submitting jobs to torque
#' @noRd
.submit_job_torque_int <- function(
    input = NULL,
    is_script = TRUE,
    echo = TRUE,
    fail_on_error = FALSE,
    wait_jobs = NULL,
    env_variables = NULL,
    sched_args = NULL,
    export_all = FALSE,
    wait_signal = "afterok",
    #tracking_db = NULL,
    #tracking_args = NULL
    ...
  ) {
  if (!is.null(sched_args)) {
    # scheduler arguments are pasted together with spaces.
    # arguments like '--mem=5g' and '-n 12' are not handled differently
    sched_args <- paste(sched_args, collapse = " ")
  }
  if (!is.null(wait_signal)) assert_string(wait_signal)
  if (!is.null(wait_jobs)) {
    jcomb <- paste(wait_jobs, collapse = ":") # multiple jobs are separated by colons
    sched_args <- paste0(sched_args, " -W depend=", wait_signal, ":", jcomb)
  }
  if (!is.null(env_variables)) {
    env_variables <- paste("-v", paste(paste_args(env_variables), collapse = ","))
    sched_args <- paste(sched_args, env_variables)
  }
  assert_logical(export_all, max.len = 1L)
  if (isTRUE(export_all)) {
    # directive to export all environment variables to script
    sched_args <- c("-V", sched_args)
  }
  job_id <- invoke_system2(
    input = input,
    is_script = is_script,
    push_cmd = "qsub",
    sched_args = sched_args,
    echo = echo,
    fail_on_error = fail_on_error
  )
  return(job_id)
}

#' Internal function for submitting jobs locally
#' @noRd
.submit_job_local_int <- function(
    input = NULL,
    is_script = TRUE,
    echo = TRUE,
    fail_on_error = FALSE,
    wait_jobs = NULL,
    env_variables = NULL,
    repolling_interval = 60,
    max_wait = 60 * 60 * 24,
    # tracking_db = NULL,
    # tracking_args = NULL,
    ...
  ) {
  if (!is.null(repolling_interval)) {
    assert_number(repolling_interval, lower = 0.1, upper = 2e5)
  } else {
    cli_abort(
      "{.code repolling_interval} is required for local submission"
    )
  }
  if (exists("sched_args")) {
    cli_warn(
      "Ignoring scheduler arguments for sh/local execution"
      )
  }
  sched_args <- NULL
  if (!is.null(env_variables)) {
    env_variables <- paste(sapply(paste_args(env_variables), function(x) {
      ifelse(grepl("=", x, fixed = TRUE), x, paste0(x, "=\"$", x, "\""))
    }), collapse = " ")
    sched_args <- env_variables
  }
  # for local scheduler, hold jobs manually by waiting for relevant parents to complete
  if (!is.null(wait_jobs)) {
    cli_inform(
      c("Waiting for the following jobs to finish:",
        "{wait_jobs}")
    )
    wait_for_job(wait_jobs, repolling_interval = repolling_interval,
                 max_wait = max_wait, scheduler = "sh")
  }
  # if an R script file is provided, execute with Rscript --vanilla
  if (grepl(".+\\.R$", input, ignore.case = TRUE)) {
    push_cmd <- "Rscript --vanilla"
  } else {
    push_cmd <- "sh"
  }
  job_id <- invoke_system(
    input = input,
    is_script = is_script,
    push_cmd = push_cmd,
    sched_args = sched_args,
    echo = echo,
    fail_on_error = fail_on_error
  )
  return(job_id)
}

#' Alias for commands
#' @rdname submit_job
#' @export
submit_cmd <- submit_job
formals(submit_cmd)$is_script <- FALSE

#' Aliases for slurm
#' @rdname submit_job
#' @export
submit_job_sbatch <- submit_job
#' @rdname submit_job
#' @export
submit_job_slurm <- submit_job

formals(submit_job_sbatch)$scheduler <- formals(submit_job_slurm)$scheduler <- "slurm"

#' @rdname submit_job
#' @export
submit_cmd_sbatch <- submit_job
#' @rdname submit_job
#' @export
submit_cmd_slurm <- submit_job

formals(submit_cmd_sbatch)$scheduler <- formals(submit_cmd_slurm)$scheduler <- "slurm"
formals(submit_cmd_sbatch)$is_script <- formals(submit_cmd_slurm)$is_script <- FALSE

#' Aliases for torque
#' @rdname submit_job
#' @export
submit_job_qsub <- submit_job
#' @rdname submit_job
#' @export
submit_job_torque <- submit_job

formals(submit_job_qsub)$scheduler <- formals(submit_job_torque)$scheduler <- "torque"

#' @rdname submit_job
#' @export
submit_cmd_qsub <- submit_job
#' @rdname submit_job
#' @export
submit_cmd_torque <- submit_job

formals(submit_cmd_qsub)$scheduler <- formals(submit_cmd_torque)$scheduler <- "torque"
formals(submit_cmd_qsub)$is_script <- formals(submit_cmd_torque)$is_script <- FALSE

#' Aliases for local
#' @rdname submit_job
#' @export
submit_job_sh <- submit_job
submit_job_local <- submit_job

formals(submit_job_sh)$scheduler <- formals(submit_job_local)$scheduler <- "local"

#' @rdname submit_job
#' @export
submit_cmd_sh <- submit_job
#' @rdname submit_job
#' @export
submit_cmd_local <- submit_job

formals(submit_cmd_sh)$scheduler <- formals(submit_cmd_local)$scheduler <- "local"
formals(submit_cmd_sh)$is_script <- formals(submit_cmd_local)$is_script <- FALSE
