#' Prepare a job object for submission
#'
#' @param x The object to be compiled.
#'
#' @name compile_job
#' @include job_classes.R
#' @docType methods
NULL

#' @rdname compile_job
#' @export
compile <- S7::new_generic("compile", "x", function(x) {
  S7::S7_dispatch()
})


S7::method(compile, class_job) <- function(x) {
  .compile_job(job = x)
}

S7::method(compile, class_job_sequence) <- function(x) {
  cli::cli_abort(
    "Job sequence compilation is not yet implemented.",
    call = rlang::caller_call()
  )
}

S7::method(compile, S7::class_any) <- function(x) {
  cli::cli_abort(
    "Invalid object type for compilation; must be a job or job sequence.",
    call = rlang::caller_call()
  )
}

#' Internal function to compile a job object for submission
#' @noRd
.compile_job <- function(job) {
  # unlock if necessary
  has_lock <- ".locked" %in% S7::prop_names(job)
  if (has_lock) {
    old_lock <- job@.locked
    job@.locked <- FALSE
    on.exit({
      job@.locked <- old_lock
    }, add = TRUE)
  }

  # if code is supplied, write temp files and add the temp script as the input_value
  if (identical(job@input@input_type, "code")) {
    generated_script_path <- .prepare_input_code(job)
    job@input@input_value <- generated_script_path
  }

  # gather env variables for submission
  env_variables <- .get_env_variables(job)

  # gather submission control arguments
  submit_control <- .get_submit_control(job)

  # retrieve system file for running job
  input_type <- job@input@input_type
  if (identical(input_type, "script") || identical(input_type, "code")) {
    submission_input <- .get_system_file(
      file_type = "submit",
      scheduler_name = job@scheduler@scheduler_name,
      job_language = job@input@language
    )
    submission_input_type <- "script"
  } else if (identical(job@input@input_type, "oneliner")) {
    submission_input <- job@input@input_value
    submission_input_type <- "oneliner"
  } else {
    cli::cli_abort(
      "Unknown input type: {.code {job@input@input_type}}",
      .internal = TRUE
    )
  }

  # store compiled information in job object
  job@.compiled <- class_pb_compiled(
    submission_input = submission_input,
    submission_input_type = submission_input_type,
    env_variables = env_variables,
    submit_control = submit_control
  )

  if (has_lock) {
    job@.locked <- old_lock
  }

  job
}

#' Internal function to gather environmental variables necessary for job
#' submission
#' @noRd
.get_env_variables <- function(job) {
  vars <- c(
    job_dir = job@job_directory@path,
    R_HOME = R.home(),
    input = job@input@input_value,
    scheduler_name = job@scheduler@scheduler_name,
    print_session_info = job@.run_settings@print_session_info,
    print_environment = job@.run_settings@print_environment
  )
  # script only environmental variables
  if (identical(job@input@input_type, "script")) {
    vars <- c(
      vars,
      run_system_file = .get_system_file(
        file_type = "run",
        scheduler_name = job@scheduler@scheduler_name,
        job_language = job@input@language
      ),
      packages = paste(job@packages@package_names, collapse = ","),
      .get_library_env_vars(job)
    )
  }
  return(vars)
}

#' Internal function to retrieve system files for submission
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
        "Unsupported scheduler: {scheduler_name}", .internal = TRUE
      )
    )
  } else if (file_type == "run") {
    file_name <- switch(
      job_language,
      R = "run_r_job.R",
      cli::cli_abort(
        "Unsupported job language: {job_language}", .internal = TRUE
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
        .internal = TRUE
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

#' Internal function to prepare code input for submission (if supplied)
#' @noRd
.prepare_input_code <- function(job) {
  code_quo <- job@input@code_quo
  if (!rlang::is_quosure(code_quo)) {
    cli::cli_abort(
      "No code provided for job with {.code input_type = 'code'}",
      .internal = TRUE
    )
  }
  # convert quosure to character string
  code_str <- tryCatch(
    .deparse_code_quo(code_quo),
    error = function(e) {
      cli::cli_abort(
        "Failed to convert code quosure to a character string.",
        parent = e
      )
    }
  )
  generated_script_path <- .generate_staged_script_path(job)
  tryCatch(
    writeLines(code_str, generated_script_path, useBytes = TRUE),
    error = function(e) {
      cli::cli_abort(
        "Failed to write code to generated script file: {.file {generated_script_path}}.",
        parent = e
      )
    }
  )
  as.character(generated_script_path)
}

#' Convert a code quosure to script text
#' @noRd
.deparse_code_quo <- function(code_quo) {
  expr <- rlang::quo_get_expr(code_quo)

  # If code was captured as `{ ... }`, remove outer braces for a cleaner script.
  if (rlang::is_call(expr, "{")) {
    exprs <- as.list(expr)[-1]
    lines <- unlist(
      lapply(exprs, rlang::expr_deparse, width = Inf),
      use.names = FALSE
    )
  } else {
    lines <- rlang::expr_deparse(expr, width = Inf)
  }

  paste(lines, collapse = "\n")
}

.generate_staged_script_path <- function(job) {
  job_dir <- job@job_directory@path
  job_name <- if (length(job@job_name)) job@job_name else .get_default_job_name()

  if (!length(job_dir)) {
    job_dir <- .generate_staging_dir()
  }

  job_dir <- .normalize_staging_dir(job_dir)

  if (file.access(job_dir, mode = 2) != 0) {
    cli::cli_abort(
      c(
        "Directory for generated script is not writable: {.file {job_dir}}.",
        "A writable directory is required for jobs with {.code input_type = 'code'}.",
        "i" = "Please set a writable job directory with {.code job_directory()} or set a writable working directory."
      )
    )
  }

  file.path(
    job_dir,
    paste0(
      ".hpcR_generated_",
      .sanitize_file_component(job_name),
      "_",
      format(Sys.time(), "%Y%m%d%H%M%S"),
      "_",
      paste(sample(c(letters, 0:9), 6, replace = TRUE), collapse = ""),
      ".R"
    )
  )
}

#' Sanitize string for use in generated file names
#' @noRd
.sanitize_file_component <- function(x) {
  x <- as.character(x)
  x <- gsub("[^A-Za-z0-9_.-]+", "_", x)
  x <- gsub("^_+|_+$", "", x)

  if (!nzchar(x)) {
    x <- "job"
  }

  x
}

#' Normalize a directory path
#' @noRd
.normalize_staging_dir <- function(path, arg = "path", .call = rlang::caller_env()) {
  tryCatch(
    normalizePath(path, mustWork = TRUE),
    error = function(e) {
      cli::cli_abort(
        "Failed to normalize directory for generated files: {.file {path}}.",
        parent = e,
        call = .call
      )
    }
  )
}

.generate_staging_dir <- function() {
  wd <- getwd()

  if (file.access(wd, mode = 2) != 0) {
    cli::cli_abort(
      c(
        "Supplying code directly requires a writable directory for generated files.",
        "Current working directory is not writable: {.file {wd}}",
        "i" = "Please set a writable working directory or specify a job directory with {.code job_directory()}")
    )
  }
  
  dir <- file.path(
    wd,
    paste0(".hpcR_generated_", format(Sys.time(), "%Y%m%d%H%M%S"))
  )

  ok <- dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  if (!isTRUE(ok)) {
    cli::cli_abort(
      c(
        "Failed to create directory for generated script: {.file {dir}}.",
        "A writable directory is required for jobs with {.code input_type = 'code'}.",
        "i" = "Please set a writable job directory with {.code job_directory()} or set a writable working directory."
      )
    )
  }

  dir

}