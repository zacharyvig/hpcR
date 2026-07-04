#' Prepare a job object for submission
#'
#' @param job A job object to be compiled for submission
#' @param ... Not currently used
#'
#' @name compile_job
#' @include job_classes.R
#' @docType methods
NULL

#' @rdname compile_job
#' @export
compile_job <- S7::new_generic("compile_job", "job")


S7::method(compile_job, class_job) <- function(
  job, .call = rlang::caller_env(), ...
) {
  .compile_job(job)
}


#' Internal function to compile a job object for submission
#' @noRd
.compile_job <- function(job) {
  scheduler_name <- job@scheduler@scheduler_name

  # unlock if necessary
  has_lock <- ".locked" %in% S7::prop_names(job)
  if (has_lock) job@.locked <- FALSE

  # gather scheduler arguments from job properties
  if (scheduler_name == "local") {
    scheduler_arguments <- character(0)
  } else {
    scheduler_arguments <- .get_scheduler_arguments(job)
  }

  # gather env variables for submission
  env_variables <- .get_env_variables(job)

  # gather submission control arguments
  submit_control <- .get_submit_control(job)

  # retrieve system file for running job
  submit_system_file <- .get_system_file(
    file_type = "submit",
    scheduler_name = job@scheduler@scheduler_name,
    job_language = job@input@language
  )

  # store compiled information in job object
  job@.compiled <- class_pb_compiled(
    env_variables = env_variables,
    submit_control = submit_control,
    submit_system_file = submit_system_file
  )
  # re-lock job object
  if (has_lock) job@.locked <- TRUE

  return(job)
}

#' Internal function to gather environmental variables necessary for job
#' submission
#' \code{.null0()} converts empty properties to NULL; only used for
#' optional properties
#' @noRd
.get_env_variables <- function(job) {
  c(
    job_dir = job@job_directory,
    R_HOME = R.home(),
    run_system_file = .get_system_file(
      file_type = "run",
      scheduler_name = job@scheduler@scheduler_name,
      job_language = job@input@language
    ),
    input = job@input@input_value,
    scheduler_name = job@scheduler@scheduler_name,
    print_session_info = job@.settings@run_settings@print_session_info,
    print_environment = job@.settings@run_settings@print_environment,
    packages = paste(job@packages@package_names, collapse = ",")
  )
}

#' Internal function to retreive system files for submission
#' @noRd
.get_system_file <- function(
  file_type = c("submit", "run"),
  scheduler_name = get_supported_schedulers(include_alias = FALSE),
  job_language = get_supported_languages()
) {
  file_type <- match.arg(file_type)
  scheduler_name <- match.arg(scheduler_name)
  job_language <- match.arg(job_language)

  if (file_type == "submit") {
    file_name <- switch(
      scheduler_name,
      slurm = "submit_to_slurm.sbatch",
      torque = "submit_to_torque.pbs",
      local = "submit_to_local.sh",
      cli::cli_abort(
        "Unsupported scheduler: {scheduler_name}", internal = TRUE
      )
    )
  } else if (file_type == "run") {
    file_name <- switch(
      job_language,
      R = "run_r_job.R",
      cli::cli_abort(
        "Unsupported job language: {job_language}", internal = TRUE
      )
    )
  }
  tryCatch(
    system.file(
      file_name,
      package = "hpcR",
      mustWork = TRUE
    ),
    error = function(e) {
      cli::cli_abort(
        "Failed to find system file: {file_name}",
        internal = TRUE
      )
    }
  )
}

#' Internal function to iterate over job properties and convert to scheduler
#' directives
#' @param job A job object
#' @param exclude A regex pattern to exclude properties from conversion
#' @noRd
.get_scheduler_arguments <- function(job, exclude = "^[.]") {
  properties <- S7::prop_names(job)
  properties <- properties[!grepl(exclude, properties)]
  out <- sapply(properties, function(property) {
    value <- S7::prop(job, property)
    .get_scheduler_directive(
      job@scheduler@scheduler_name, property, value
    )
  })
  return(unlist(out))
}

#' Internal function to convert job property to the corresponding scheduler
#' directive
#' @noRd
.get_scheduler_directive <- function(scheduler_name, property, value) {
  if (!inherits(value, c("character", "list", "hpcR::class_property_block"))) {
    return(invisible(NULL))
  }
  if (inherits(value, "hpcR::class_property_block")) {
    # this converts properties in block to a named list
    value <- S7::props(value)
  }
  if (scheduler_name == "slurm") {
    switch(
      property,
      job_name = .sprintf_null("--job-name=%s", value),
      resources = c(
        .sprintf_null("-N %s", value$n_nodes),
        .sprintf_null("-n %s", value$n_cores),
        .sprintf_null("--time=%s", value$wall_time),
        .sprintf_null("--mem-per-cpu=%s", value$memory_per_core),
        .sprintf_null("--mem=%s", value$total_memory)
      ),
      NULL
    )
  } else if (scheduler_name == "torque") {
    switch(
      property,
      job_name = .sprintf_null("-N %s", value),
      resources = c(
        .sprintf_null("-l nodes=%s:ppn=%s", value$n_nodes, value$n_cores),
        .sprintf_null("-l walltime=%s", value$wall_time),
        .sprintf_null("-l pmem=%s", value$memory_per_core),
        .sprintf_null("-l mem=%s", value$total_memory)
      ),
      NULL
    )
  } else {
    return(NULL)
  }
}


#' Interal function to gather control arguments for submission
#' @noRd
.get_submit_control <- function(job) {
  control <- list()
  if (job@scheduler@scheduler_name != "local") {
    control$scheduler_arguments <- .get_scheduler_arguments(job)
  }
  return(control)
}
