#' Submit a script or command to a high-performance cluster using a scheduler
#'
#' @description  \code{submit_job} submits a single script or a single command
#' to a high-performance cluster (HPC) using a specified scheduler (TORQUE,
#' Slurm) or locally.
#'
#' @details The \code{control} argument can include additional, scheduler-
#' specific arguments as a named list:
#' \describe{
#'    \item{\code{export_all}}{
#'    (For TORQUE or Slurm schedulers) Whether to export all environment
#'    variables to the compute node at runtime. Default: FALSE
#'    }
#'    \item{\code{scheduler_arguments}}{
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
#' single command to be run. In the case of conflicts, the directives passed
#' with \code{scheduler_arguments} will take precedence. Required.
#' @param input_type Character. Either 'script' or 'oneliner' to indicate
#' whether the input is a script or a single line command. Default: 'script'
#' @param scheduler_name Which scheduler to use for job submission. Options are
#' 'torque', 'slurm', 'local', 'sbatch', 'qsub', and 'sh'. The terms 'qsub' and
#' 'torque' are aliases (where 'torque' submits via the qsub command). Likewise
#' for 'sbatch' and 'slurm'. The scheduler 'sh' (alias, 'local') does not submit
#' to any scheduler at all, but instead executes the command immediately via sh.
#' Default: 'slurm'
#' @param fail_on_error Whether to stop execution of the script (\code{TRUE}),
#' or issue a warning (\code{FALSE}) if the job submission fails. Default:
#' FALSE (i.e., issue a warning).
#' @param upstream An optional character string of jobs or process ids that
#' should complete before this job is executed.
#' @param env_variables An optional named character vector containing
#' environment variables and their values to be passed to the \code{script} at
#' execution time. This is handled by the '-v' directive on TORQUE clusters and
#' by '--export' on Slurm clusters. The names of this vector are the environment
#' variable names and the values of the vector are the environment variable
#' values to be passed in. If you want to propagate the current value of an
#' environment variable to the compute node at runtime, use NA as the value of
#' the element in \code{env_variables}. See examples.
#' @param echo Whether to echo the job submission command to the terminal at the
#' time it is scheduled. Default: \code{TRUE}
#' @param control A list of additional, scheduler-specific arguments. See
#' details.
#'
#' @return A character string containing the job ID of the scheduled job.
#'
#' @examplesIf interactive() && nzchar(Sys.which("sbatch"))
#' job_id <- submit_job(input = "my_script.R", scheduler_name = "slurm")
#' @name submit_job
#' @export
submit_job  <- function(
  input,
  input_type = c("script", "oneliner"),
  scheduler_name = get_supported_schedulers(),
  fail_on_error = FALSE,
  upstream = NULL,
  env_variables = NULL,
  echo = FALSE,
  control = list()
) {

  input <- .job_obj_guard(input, "submit_job", alt_fn = "submit")

  input_type <- match.arg(input_type)
  scheduler_name <- match.arg(scheduler_name)
  scheduler_name <- standardize_scheduler_name(scheduler_name)

  # validate arguments
  checkmate::assert_string(input)
  checkmate::assert_string(scheduler_name)
  checkmate::assert_flag(fail_on_error)
  checkmate::assert_character(upstream, null.ok = TRUE)
  checkmate::assert_character(env_variables, null.ok = TRUE)
  checkmate::assert_list(control, null.ok = TRUE)

  out <- rlang::exec(
    .submit_job,
    input = input,
    input_type = input_type,
    fail_on_error = fail_on_error,
    upstream = upstream,
    env_variables = env_variables,
    echo = echo,
    scheduler_name = scheduler_name,
    control = control,
    .call = rlang::caller_call()
  )

  out
}

#' Internal function to dispatch to correct submission function
#' @noRd
.submit_job <- function(
  input,
  input_type = c("script", "oneliner"),
  scheduler_name,
  fail_on_error = FALSE,
  upstream = NULL,
  env_variables = NULL,
  echo = FALSE,
  control = list(),
  .call = rlang::caller_call()
) {

  # get submit function
  submit_function <- switch(
    scheduler_name,
    "local" = .submit_to_local,
    .submit_to_hpc
  )

  # check control arguments
  if (length(control) > 0) {
    unknown_args <- setdiff(
      names(control), methods::formalArgs(submit_function)
    )
    if (length(unknown_args) > 0) {
      cli::cli_warn("Ignoring control arguments: {unknown_args}", call = .call)
    }
  }

  out <- rlang::exec(
    submit_function,
    input = input,
    input_type = input_type,
    fail_on_error = fail_on_error,
    upstream = upstream,
    env_variables = env_variables,
    echo = echo,
    scheduler_name = scheduler_name,
    .call = .call,
    !!!control
  )

  out
}


#' Internal function to submit a job to HPC scheduler
#' @noRd
.submit_to_hpc <- function(
  input,
  input_type,
  scheduler_name,
  fail_on_error,
  upstream,
  env_variables,
  echo = FALSE,
  scheduler_arguments = NULL,
  export_all = FALSE,
  wait_signal = c(
    "afterok", "after", "afterany", "afterburstbuffer",
    "aftercorr", "afternotok"
  ),
  .call = rlang::caller_call(),
  ...
) {

  wait_signal <- match.arg(wait_signal)

  # validate arguments unique to HPC submission
  checkmate::assert_string(scheduler_arguments, null.ok = TRUE)
  checkmate::assert_flag(export_all)

  if (isTRUE(export_all)) {
    # directive to export all environment variables to script
    if (scheduler_name == "slurm") {
      env_variables <- c(ALL = NA, env_variables)
    } else if (scheduler_name == "torque") {
      scheduler_arguments <- c("-V", scheduler_arguments)
    } else {
      cli::cli_warn(
        paste("{.code export_all} is currently not supported for scheduler",
              "{.code {scheduler_name}}"),
        "Manually exporting via {.code scheduler_arguments} is recommended.",
        call = .call
      )
    }
  }

  if (!is.null(scheduler_arguments) && length(scheduler_arguments) > 0) {
    # scheduler arguments are pasted together with spaces
    # arguments like '--mem=5g' and '-n 12' are not handled differently
    scheduler_arguments <- paste(scheduler_arguments, collapse = " ")
  }

  if (!is.null(env_variables) && length(env_variables) > 0) {
    export_directive <- switch(
      scheduler_name,
      "slurm" = "--export=",
      "torque" = "-v ",
      cli::cli_warn(
        paste("{.code env_variables} is currently not supported for scheduler",
              "{.code {scheduler_name}}"),
        "Manually exporting via {.code scheduler_arguments} is recommended.",
        call = .call
      )
    )
    checkmate::assert_true(
      length(names(env_variables)) == length(env_variables)
    )
    env_variables <- paste0(
      export_directive,
      paste(.paste_args(env_variables), collapse = ",")
    )
    scheduler_arguments <- paste(scheduler_arguments, env_variables)
  }

  if (!is.null(upstream) && length(upstream) > 0) {
    # multiple jobs are separated by colons
    jcomb <- paste(upstream, collapse = ":")
    dependency_directive <- switch(
      scheduler_name,
      "slurm" = "--dependency=",
      "torque" = "-W depend=",
      cli::cli_warn(
        paste("{.code upstream} is currently not supported for scheduler",
              "{.code {scheduler_name}}"),
        paste("Manually handling dependencies via {.code scheduler_arguments}",
              "is recommended."),
        call = .call
      )
    )
    scheduler_arguments <- paste0(
      scheduler_arguments, " ", dependency_directive,
      wait_signal, ":", jcomb
    )
  }

  if (!exists("push_command")) {
    push_command <- switch(
      scheduler_name,
      "slurm" = "sbatch",
      "torque" = "qsub",
      cli::cli_warn(
        "No push command defined for scheduler {.code {scheduler_name}}",
        "You can manually supply via {.code push_command} in {.code ...}.",
        call = .call
      )
    )
  }

  job_id <- .invoke_system_hpc(
    input = input,
    input_type = input_type,
    push_command = push_command,
    scheduler_arguments = scheduler_arguments,
    fail_on_error = fail_on_error,
    echo = echo
  )

  job_id

}

#' Internal function to submit a job to local scheduler
#' @noRd
.submit_to_local <- function(
  input,
  input_type,
  fail_on_error,
  upstream,
  env_variables,
  echo,
  repolling_interval = 60L,
  max_wait = 60 * 60 * 24,
  .call = rlang::caller_call(),
  ...
) {

  # ensure local submission is supported on the current platform
  .assert_local_supported()

  # in case scheduler arguments are passed via ...
  if (exists("scheduler_arguments")) {
    cli::cli_warn(
      "Ignoring scheduler arguments for sh/local execution", .call = .call
    )
  }

  if (!is.null(env_variables) && length(env_variables) > 0) {
    scheduler_arguments <- paste(
      sapply(.paste_args(env_variables), function(x) {
        # variables without values treated differently by local
        ifelse(
          grepl("=", x, fixed = TRUE),
          x, paste0(x, "=\"$", x, "\"")
        )
      }),
      collapse = " "
    )
  } else {
    scheduler_arguments <- NULL
  }

  # manually wait for dependencies for local
  if (!is.null(upstream) && length(upstream) > 0) {
    cli::cli_inform(
      c(
        "i" = "Local scheduler requires manually waiting for dependencies.",
        "Waiting for the following jobs to finish: {upstream}"
      ),
      .call = .call
    )
    # wait parameters are validated inside `wait_for_job()`
    wait_for_job(
      upstream, repolling_interval = repolling_interval,
      max_wait = max_wait, scheduler_name = "sh"
    )
  }

  # if an R script file is provided, execute with Rscript --vanilla
  # TODO: more robustly handle other cases?
  if (grepl(".+\\.R$", input, ignore.case = TRUE)) {
    push_command <- "Rscript --vanilla"
  } else {
    push_command <- "sh"
  }

  # submit job
  job_id <- .invoke_system_local(
    input = input,
    input_type = input_type,
    push_command = push_command,
    scheduler_arguments = scheduler_arguments,
    fail_on_error = fail_on_error,
    echo = echo
  )

  job_id
}