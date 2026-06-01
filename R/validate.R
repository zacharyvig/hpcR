# These are validation methods and internal validators
# Internals are named `.validate_*` where `*` is the name of the property

# #' Validate a job object
# #' @keywords internal
# validate_job <- S7::new_generic("validate_job", c("job_obj"))

# S7::method(validate_job, Job) <- function(
#   job_obj, .call = rlang::caller_env(), strict = TRUE, optional = FALSE
# ) {
#   .validate_job(job_obj, .call = .call, strict = strict, optional = optional)
# }

#' Validate a property
#' @keywords internal
validate_property <- S7::new_generic("validate_property", "name")

S7::method(
  validate_property, S7::class_character
) <- function(
  name, value, .call = rlang::caller_env(), use_default_settings = TRUE,
  fail_on_invalid = TRUE, allow_na = FALSE, allow_missing = FALSE
) {
  validator <- get(
    paste0(".validate_", name), mode = "function", inherits = TRUE
  )
  if (use_default_settings) {
    rlang::exec(validator, value = value, .call = .call)
  } else {
    settings <- list(
      fail_on_invalid = fail_on_invalid, allow_na = allow_na,
      allow_missing = allow_missing
    )
    rlang::exec(
      validator, value = value, .call = .call, settings = settings
    )
  }
}

# # TODO: don't add a line for every validator (too cumbersome)
# #' Internal job validator
# #' @noRd
# .validate_job <- function(
#   job_obj, .call = rlang::caller_env(), strict = TRUE, optional = FALSE
# ) {
#   args <- list(
#     job_obj = job_obj,
#     .call = .call,
#     strict = strict,
#     optional = optional # this one might not always apply so come back later
#   )
#   do.call(validate_script, args)
#   do.call(validate_job_name, args)
#   do.call(validate_job_directory, args)
#   do.call(validate_resources, args)
# }

#' Central function for defining validation defaults
#' @param property The name of the property to get defaults for
#' @description
#' \itemize{
#'  \item{\code{fail_on_invalid}}{
#'    Throw an error (TRUE) or a warning (FALSE) if the value is invalid
#'  }
#'  \item{\code{allow_na}}{
#'    Allow NA values, which are used to delete properties
#'  }
#'  \item{\code{allow_missing}}{
#'    Allow missing values, which are used to skip validation
#'  }
#' }
#' @noRd
.get_validator_defaults <- function(property, stage = c("update", "submit")) {
  stage <- match.arg(stage)
  if (stage == "update") {
    switch(
      property,
      input = list(
        fail_on_invalid = FALSE,
        allow_na = FALSE,
        allow_missing = FALSE
      ),
      job_name = list(
        fail_on_invalid = TRUE,
        allow_na = TRUE,
        allow_missing = TRUE
      ),
      job_directory = list(
        fail_on_invalid = TRUE,
        allow_na = TRUE,
        allow_missing = TRUE
      ),
      resources = list(
        fail_on_invalid = TRUE,
        allow_na = TRUE,
        allow_missing = TRUE
      ),
      scheduler = list(
        fail_on_invalid = TRUE,
        allow_na = TRUE,
        allow_missing = TRUE
      ),
      packages = list(
        fail_on_invalid = TRUE,
        allow_na = TRUE,
        allow_missing = TRUE
      ),
      cli::cli_abort("Unknown property: {property}", internal = TRUE)
    )
  } else if (stage == "submit") {
    switch(
      property,
      input = list(
        fail_on_invalid = TRUE,
        allow_na = FALSE,
        allow_missing = FALSE
      ),
      job_name = list(
        fail_on_invalid = TRUE,
        allow_na = TRUE,
        allow_missing = TRUE
      ),
      job_directory = list(
        fail_on_invalid = TRUE,
        allow_na = TRUE,
        allow_missing = TRUE
      ),
      resources = list(
        fail_on_invalid = TRUE,
        allow_na = TRUE,
        allow_missing = TRUE
      ),
      scheduler = list(
        fail_on_invalid = TRUE,
        allow_na = FALSE,
        allow_missing = FALSE
      ),
      # still just warn at submission in case of wrong package install location
      packages = list(
        fail_on_invalid = FALSE,
        allow_na = TRUE,
        allow_missing = TRUE
      ),
      cli::cli_abort("Unknown property: {property}", internal = TRUE)
    )
  }
}

# how "missing" is defined for validation
.is_missing <- function(x) {
  missing(x) || is.null(x) || !length(x)
}

#' Internal input validator
#' @noRd
.validate_input <- function(
  value, .call = rlang::caller_env(),
  settings = .get_validator_defaults("input")
) {
  items <- class_pb_input@properties |> names()
  invalid_fields <- setdiff(names(value), items)
  if (length(invalid_fields)) {
    # rare case when working directly with the validate functions directly
    cli::cli_abort(
      c("Input has invalid fields:", "{.list {invalid_fields}}"),
      call = .call
    )
    return(invisible())
  } else {
    if (value$input_type == "script") {
      .validate_script(
        value = value, .call = .call,
        settings = settings
      )
    } else if (value$input_type == "oneliner") {
      cli::cli_abort(
        "Work in progress: oneliner validation not yet implemented",
        call = .call, internal = TRUE
      )
    } else {
      cli::cli_abort("Unknown input type: {.code {value$input_type}}",
                     call = .call, internal = TRUE)
    }
  }
}

#' Internal script validator
#' @noRd
.validate_script <- function(
  value, .call = rlang::caller_env(),
  settings = .get_validator_defaults("script")
) {
  notify <- if (settings$fail_on_invalid) cli::cli_abort else cli::cli_warn
  script_path <- value$input_value
  if (.is_missing(script_path)) {
    if (settings$allow_missing) return(invisible())
    notify("Script is missing", call = .call)
  } else if (settings$allow_na && isTRUE(is.na(script_path))) {
    return(invisible())
  } else if (!checkmate::test_string(script_path, min.chars = 1)) {
    notify("Script must be a single, non-empty character string", call = .call)
  } else if (!checkmate::test_file_exists(script_path)) {
    msg <- "Script file does not exist: {.code {script_path}}"
    if (!settings$fail_on_invalid) {
      msg <- c(msg, "i" = "Did you supply the full path?")
    }
    notify(msg, call = .call)
  }
  if (length(value$extension) && length(value$language)) {
    ext <- tools::file_ext(script_path)
    if (ext != value$extension) {
      notify(
        paste("Script must end in {.code {value$extension}} for",
              "{.code {value$language}} jobs"),
        call = .call
      )
    }
  }
}

#' Internal oneliner validator
#' @noRd
.validate_oneliner <- function(
  value, .call = rlang::caller_env(),
  settings = .get_validator_defaults("oneliner")
) {
  stop("Work in progress: oneliner validation not yet implemented",
       call. = FALSE)
}

#' Internal job name validator
#' @noRd
.validate_job_name <- function(
  value, .call = rlang::caller_env(),
  settings = .get_validator_defaults("job_name")
) {
  notify <- if (settings$fail_on_invalid) cli::cli_abort else cli::cli_warn
  # if optional, allow NA, otherwise default to warn/error on missing
  # warn/error on missing
  if (.is_missing(value)) {
    if (settings$allow_missing) return(invisible())
    notify("Job name is missing", call = .call)
    return(invisible())
  } else if (settings$allow_na && isTRUE(is.na(value))) {
    return(invisible())
  } else if (!checkmate::test_string(value, min.chars = 1)) {
    notify("Job name must be a single, non-empty character string",
           call = .call)
  } else if (nchar(value) > 15) {
    cli::cli_warn(
      "Job name may be too long for some schedulers: {.code {value}}",
      call = .call
    )
  }
}

#' Internal job directory validator
#' @noRd
.validate_job_directory <- function(
  value, .call = rlang::caller_env(),
  settings = .get_validator_defaults("job_directory")
) {
  notify <- if (settings$fail_on_invalid) cli::cli_abort else cli::cli_warn
  if (.is_missing(value)) {
    if (settings$allow_missing) return(invisible())
    notify("Job directory is missing", call = .call)
    return(invisible())
  } else if (settings$allow_na && isTRUE(is.na(value))) {
    return(invisible())
  } else if (!checkmate::test_string(value, min.chars = 1)) {
    notify("Job directory must be a single, non-empty character string",
           call = .call)
  } else if (!checkmate::test_directory_exists(value)) {
    msg <- "Job directory does not exist: {.code {value}}"
    if (settings$fail_on_invalid) {
      msg <- c(msg, "Make sure it exists before submitting the job")
    }
    notify(msg, call = .call)
  }
}

#' Internal resources validator
#' @noRd
.validate_resources <- function(
  value, .call = rlang::caller_env(),
  settings = .get_validator_defaults("resources")
) {
  notify <- if (settings$fail_on_invalid) cli::cli_abort else cli::cli_warn
  items <- class_pb_resources@properties |> names()
  invalid_fields <- setdiff(names(value), items)
  if (length(invalid_fields)) {
    # rare case when working directly with the validate functions directly
    cli::cli_abort(
      c("Resources has invalid fields:", "{.list {invalid_fields}}"),
      call = .call
    )
  } else if (length(value$total_memory) &&
               length(value$memory_per_core)) {
    notify("Total memory and memory per core are mutually exclusive",
           call = .call)
  } else {
    for (item in names(value)) {
      fn <- get(paste0(".validate_", item), mode = "function", inherits = TRUE)
      fn(value[[item]], .call = .call, settings = settings)
    }
  }
}

#' Internal resources validators
#' @noRd
.validate_n_nodes <- function(
  value, .call = rlang::caller_env(),
  settings = .get_validator_defaults("resources")
) {
  notify <- if (settings$fail_on_invalid) cli::cli_abort else cli::cli_warn
  if (.is_missing(value)) {
    if (settings$allow_missing) return(invisible())
    notify("Number of nodes is missing", call = .call)
  } else if (settings$allow_na && isTRUE(is.na(value))) {
    return(invisible())
  } else if (length(value) > 1) {
    notify("Number of nodes must be a single value", call = .call)
  } else if (is.character(value) && !nzchar(value)) {
    notify("Number of nodes is empty", call = .call)
  } else {
    n_nodes_num <- suppressWarnings(as.numeric(value))
    if (is.na(n_nodes_num) || n_nodes_num <= 0) {
      notify("Number of nodes must be a positive number", call = .call)
    }
  }
}

#' Internal resources validators
#' @noRd
.validate_n_cores <- function(
  value, .call = rlang::caller_env(),
  settings = .get_validator_defaults("resources")
) {
  notify <- if (settings$fail_on_invalid) cli::cli_abort else cli::cli_warn
  if (.is_missing(value)) {
    if (settings$allow_missing) return(invisible())
    notify("Number of cores is missing", call = .call)
  } else if (settings$allow_na && isTRUE(is.na(value))) {
    return(invisible())
  } else if (length(value) > 1) {
    notify("Number of cores must be a single value", call = .call)
  } else if (is.character(value) && !nzchar(value)) {
    notify("Number of cores is empty", call = .call)
  } else {
    n_cores_num <- suppressWarnings(as.numeric(value))
    if (is.na(n_cores_num) || n_cores_num <= 0) {
      notify("Number of cores must be a positive number", call = .call)
    }
  }
}

#' Internal resources validators
#' @noRd
.validate_wall_time <- function(
  value, .call = rlang::caller_env(),
  settings = .get_validator_defaults("resources")
) {
  notify <- if (settings$fail_on_invalid) cli::cli_abort else cli::cli_warn
  if (.is_missing(value)) {
    if (settings$allow_missing) return(invisible())
    notify("Wall time is missing", call = .call)
  } else if (settings$allow_na && isTRUE(is.na(value))) {
    return(invisible())
  } else if (length(value) > 1) {
    notify("Wall time must be a single value", call = .call)
  } else if (is.character(value) && !nzchar(value)) {
    notify("Wall time is empty", call = .call)
  } else {
    regex <- paste0("^(\\d+-\\d{2}(:\\d{2}(:\\d{2})?)?|",
                    "\\d{1,2}:\\d{2}(:\\d{2})?|\\d+)$")
    wall_time_chr <- suppressWarnings(as.character(value))
    if (!grepl(regex, wall_time_chr)) {
      notify(
        paste("Wall time must be a character string in the format", 
              "MM[:SS], HH:MM:SS or dd-HH[:MM][:SS]"),
        call = .call
      )
    }
  }
}

#' Internal resources validators
#' @noRd
.validate_total_memory <- function(
  value, .call = rlang::caller_env(),
  settings = .get_validator_defaults("resources")
) {
  notify <- if (settings$fail_on_invalid) cli::cli_abort else cli::cli_warn
  if (.is_missing(value)) {
    if (settings$allow_missing) return(invisible())
    notify("Total memory is missing", call = .call)
  } else if (settings$allow_na && isTRUE(is.na(value))) {
    return(invisible())
  } else if (length(value) > 1) {
    notify("Total memory must be a single value", call = .call)
  } else if (is.numeric(value)) {
    notify("Total memory must have a unit (e.g., 4G)", call = .call)
  } else {
    total_memory_chr <- suppressWarnings(as.character(value))
    if (!nzchar(total_memory_chr)) {
      notify("Total memory is empty", call = .call)
    }
    if (!grepl("(K|M|G|T)$", total_memory_chr)) {
      notify("Total memory must end with K, M, G, or T", call = .call)
    }
  }
}

#' Internal resources validators
#' @noRd
.validate_memory_per_core <- function(
  value, .call = rlang::caller_env(),
  settings = .get_validator_defaults("resources")
) {
  notify <- if (settings$fail_on_invalid) cli::cli_abort else cli::cli_warn
  if (.is_missing(value)) {
    if (settings$allow_missing) return(invisible())
    notify("Memory per core is missing", call = .call)
  } else if (settings$allow_na && isTRUE(is.na(value))) {
    return(invisible())
  } else if (length(value) > 1) {
    notify("Memory per core must be a single value", call = .call)
  } else if (is.numeric(value)) {
    notify("Memory per core must have a unit (e.g., 4G)", call = .call)
  } else {
    memory_per_core_chr <- suppressWarnings(as.character(value))
    if (!nzchar(memory_per_core_chr)) {
      notify("Memory per core is empty", call = .call)
    }
    if (!grepl("(K|M|G|T)$", memory_per_core_chr)) {
      notify("Memory per core must end with K, M, G, or T", call = .call)
    }
  }
}

#' Internal scheduler validator
#' @noRd
.validate_scheduler <- function(
  value, .call = rlang::caller_env(),
  settings = .get_validator_defaults("scheduler")
) {
  notify <- if (settings$fail_on_invalid) cli::cli_abort else cli::cli_warn
  items <- class_pb_scheduler@properties |> names()
  invalid_fields <- setdiff(names(value), items)
  if (length(invalid_fields)) {
    # rare case when working directly with the validate functions directly
    cli::cli_abort(
      c("Scheduler has invalid fields:", "{.list {invalid_fields}}"),
      call = .call
    )
  }
  scheduler_name <- value$scheduler_name
  if (.is_missing(scheduler_name)) {
    if (settings$allow_missing) return(invisible())
    notify("Scheduler name is missing", call = .call)
  } else if (settings$allow_na && isTRUE(is.na(scheduler_name))) {
    return(invisible())
  } else if (!checkmate::test_string(scheduler_name, min.chars = 1)) {
    notify("Scheduler name must be a single, non-empty character string",
           call = .call)
  } else if (
    !checkmate::test_choice(
      standardize_scheduler_name(scheduler_name),
      get_supported_schedulers(include_alias = FALSE)
    )
  ) {
    notify("Scheduler name invalid or currently not supported", call = .call)
  }
}

#' Internal packages validator
#' @noRd
.validate_packages <- function(
  value, .call = rlang::caller_env(),
  settings = .get_validator_defaults("packages")
) {
  notify <- if (settings$fail_on_invalid) cli::cli_abort else cli::cli_warn
  items <- class_pb_packages@properties |> names()
  invalid_fields <- setdiff(names(value), items)
  if (length(invalid_fields)) {
    # rare case when working directly with the validate functions directly
    cli::cli_abort(
      c("Packages has invalid fields:", "{.list {invalid_fields}}"),
      call = .call
    )
  }
  package_names <- value$package_names
  installation_path <- value$installation_path
  if (.is_missing(package_names)) {
    if (settings$allow_missing) return(invisible())
    notify("Package names are missing", call = .call)
  } else if (settings$allow_na && isTRUE(is.na(package_names))) {
    return(invisible())
  } else if (!checkmate::test_character(package_names, min.len = 1)) {
    notify("Package names must be a character vector", call = .call)
  } else if (any(!nzchar(package_names))) {
    notify("Package names cannot be empty strings", call = .call)
  }
  paths_exist <- vapply(
    installation_path, checkmate::test_directory_exists, logical(1)
  )
  if (all(!paths_exist)) {
    notify(
      c("None of the specified package installation paths exist:",
        "{.list {installation_path}}"),
      call = .call
    )
  }
  installed <- .packages_installed(package_names, installation_path)
  if (any(!installed)) {
    missing_packages <- package_names[!installed]
    notify(
      c("Requested packages are not installed:", "{.list {missing_packages}}"),
      call = .call
    )
  }
  return(invisible())
}