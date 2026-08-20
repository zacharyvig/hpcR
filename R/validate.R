# These are validation methods and internal validators
# Internals are named `.validate_*` where `*` is the name of the property

#' Validate a job object
#' @keywords internal
validate_job <- S7::new_generic("validate_job", "job")

S7::method(validate_job, class_job) <- function(
  job, stage = c("update", "submit"), .call = rlang::caller_env()
) {
  stage <- match.arg(stage)
  .validate_job(job, .call = .call, stage = stage)
}

#' Validate a property
#' @keywords internal
validate_property <- S7::new_generic("validate_property", "name")

S7::method(
  validate_property, S7::class_character
) <- function(
  name, value, use_default_settings = TRUE, .call = rlang::caller_env(),
  fail_on_invalid = TRUE, allow_na = FALSE, allow_missing = FALSE
) {
  name <- gsub("^\\.", "", name)
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
      validator, value = value, .call = .call,
      settings = settings
    )
  }
}

#' Internal job validator
#' @noRd
.validate_job <- function(
  job, stage, .call = rlang::caller_env(), exclude = "^[.]"
) {

  checkmate::assert_string(exclude)

  # get non-hidden properties
  properties <- S7::prop_names(job)
  properties <- properties[!grepl(exclude, properties)]

  for (property in properties) {
    value <- S7::prop(job, property)
    if (inherits(value, "hpcR::class_property_block")) {
      value <- S7::props(value)
    }
    settings <- .get_validator_defaults(property, stage = stage)
    validate_property(
      name = property, value = value, .call = .call,
      use_default_settings = FALSE,
      fail_on_invalid = settings$fail_on_invalid,
      allow_na = settings$allow_na,
      allow_missing = settings$allow_missing
    )
  }

}

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
      libraries = list(
        fail_on_invalid = TRUE,
        allow_na = TRUE,
        allow_missing = TRUE
      ),
      run_settings = list(
        fail_on_invalid = TRUE,
        allow_na = TRUE,
        allow_missing = TRUE
      ),
      cli::cli_abort(
        "Property has no default update validation settings: {property}",
        internal = TRUE
      )
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
      libraries = list(
        fail_on_invalid = TRUE,
        allow_na = TRUE,
        allow_missing = TRUE
      ),
      run_settings = list(
        fail_on_invalid = TRUE,
        allow_na = FALSE,
        allow_missing = FALSE
      ),
      cli::cli_abort(
        "Property has no default submit validation settings: {property}",
        internal = TRUE
      )
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
  items <- names(class_pb_input@properties)
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
      .validate_oneliner(
        value = value, .call = .call,
        settings = settings
      )
    } else {
      cli::cli_abort("Unknown input type: {.code {value$input_type}}",
                     call = .call, internal = TRUE)
    }
  }
  return(invisible())
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
    notify("{.field script_path} is missing", call = .call)
  } else if (settings$allow_na && isTRUE(is.na(script_path))) {
    return(invisible())
  } else if (!checkmate::test_string(script_path, min.chars = 1)) {
    notify(
      "{.field script_path} must be a single, non-empty character string",
      call = .call
    )
  } else if (!checkmate::test_file_exists(script_path)) {
    msg <- "{.field script_path} file does not exist: {.code {script_path}}"
    if (!settings$fail_on_invalid) {
      msg <- c(msg, "i" = "Did you supply the full path?")
    }
    notify(msg, call = .call)
  }
  if (length(value$extension) && length(value$language)) {
    ext <- tools::file_ext(script_path)
    if (isFALSE(ext == value$extension)) {
      notify(
        paste("{.field script_path} must end in {.code {value$extension}} for",
              "{.code {value$language}} jobs"),
        call = .call
      )
    }
  }
  return(invisible())
}

#' Internal oneliner validator
#' @noRd
.validate_oneliner <- function(
  value, .call = rlang::caller_env(),
  settings = .get_validator_defaults("oneliner")
) {
  notify <- if (settings$fail_on_invalid) cli::cli_abort else cli::cli_warn
  oneliner <- value$input_value
  if (.is_missing(oneliner)) {
    if (settings$allow_missing) return(invisible())
    notify("{.field oneliner} is missing", call = .call)
  } else if (settings$allow_na && isTRUE(is.na(oneliner))) {
    return(invisible())
  } else if (!checkmate::test_string(oneliner, min.chars = 1)) {
    notify(
      "{.field oneliner} must be a single, non-empty character string",
      call = .call
    )
  }
  return(invisible())
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
    notify("{.field job_name} is missing", call = .call)
    return(invisible())
  } else if (settings$allow_na && isTRUE(is.na(value))) {
    return(invisible())
  } else if (!checkmate::test_string(value, min.chars = 1)) {
    notify("{.field job_name} must be a single, non-empty character string",
           call = .call)
  } else if (nchar(value) > 15) {
    cli::cli_warn(
      "{.field job_name} may be too long for some schedulers: {.code {value}}",
      call = .call
    )
  }
  return(invisible())
}

#' Internal job directory validator
#' @noRd
.validate_job_directory <- function(
  value, .call = rlang::caller_env(),
  settings = .get_validator_defaults("job_directory")
) {
  notify <- if (settings$fail_on_invalid) cli::cli_abort else cli::cli_warn
  path <- value$path
  if (.is_missing(path)) {
    if (settings$allow_missing) return(invisible())
    notify("{.field job_directory} is missing", call = .call)
    return(invisible())
  } else if (settings$allow_na && isTRUE(is.na(path))) {
    return(invisible())
  } else if (!checkmate::test_string(path, min.chars = 1)) {
    notify("{.field job_directory} must be a single, non-empty character string",
           call = .call)
  } else if (!checkmate::test_directory_exists(path)) {
    msg <- "{.field job_directory} does not exist: {.code {path}}"
    if (settings$fail_on_invalid) {
      msg <- c(msg, "Make sure it exists before submitting the job")
    }
    notify(msg, call = .call)
  }
  return(invisible())
}

#' Internal resources validator
#' @noRd
.validate_resources <- function(
  value, .call = rlang::caller_env(),
  settings = .get_validator_defaults("resources")
) {
  notify <- if (settings$fail_on_invalid) cli::cli_abort else cli::cli_warn
  items <- names(class_pb_resources@properties)
  invalid_fields <- setdiff(names(value), items)
  if (length(invalid_fields)) {
    # rare case when working directly with the validate functions directly
    cli::cli_abort(
      c("Resources has invalid fields:", "{.list {invalid_fields}}"),
      call = .call
    )
  } else if (length(value$total_memory) &&
               length(value$memory_per_core)) {
    notify(
      "{.field total_memory} and {.field memory_per_core} are mutually exclusive",
      call = .call
    )
  } else {
    for (item in names(value)) {
      fn <- get(paste0(".validate_", item), mode = "function", inherits = TRUE)
      fn(value[[item]], .call = .call, settings = settings)
    }
  }
  return(invisible())
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
    notify("{.field n_nodes} is missing", call = .call)
  } else if (settings$allow_na && isTRUE(is.na(value))) {
    return(invisible())
  } else if (length(value) > 1) {
    notify("{.field n_nodes} must be a single value", call = .call)
  } else if (is.character(value) && !nzchar(value)) {
    notify("{.field n_nodes} is empty", call = .call)
  } else {
    n_nodes_num <- suppressWarnings(as.numeric(value))
    if (is.na(n_nodes_num) || n_nodes_num <= 0) {
      notify("{.field n_nodes} must be a positive number", call = .call)
    }
  }
  return(invisible())
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
    notify("{.field n_cores} is missing", call = .call)
  } else if (settings$allow_na && isTRUE(is.na(value))) {
    return(invisible())
  } else if (length(value) > 1) {
    notify("{.field n_cores} must be a single value", call = .call)
  } else if (is.character(value) && !nzchar(value)) {
    notify("{.field n_cores} is empty", call = .call)
  } else {
    n_cores_num <- suppressWarnings(as.numeric(value))
    if (is.na(n_cores_num) || n_cores_num <= 0) {
      notify("{.field n_cores} must be a positive number", call = .call)
    }
  }
  return(invisible())
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
    notify("{.field wall_time} is missing", call = .call)
  } else if (settings$allow_na && isTRUE(is.na(value))) {
    return(invisible())
  } else if (length(value) > 1) {
    notify("{.field wall_time} must be a single value", call = .call)
  } else if (is.character(value) && !nzchar(value)) {
    notify("{.field wall_time} is empty", call = .call)
  } else {
    regex <- paste0("^(\\d+-\\d{2}(:\\d{2}(:\\d{2})?)?|",
                    "\\d{1,2}:\\d{2}(:\\d{2})?|\\d+)$")
    wall_time_chr <- suppressWarnings(as.character(value))
    if (!grepl(regex, wall_time_chr)) {
      notify(
        paste("{.field wall_time} must be a character string in the format", 
              "MM[:SS], HH:MM:SS or dd-HH[:MM][:SS]"),
        call = .call
      )
    }
  }
  return(invisible())
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
    notify("{.field total_memory} is missing", call = .call)
  } else if (settings$allow_na && isTRUE(is.na(value))) {
    return(invisible())
  } else if (length(value) > 1) {
    notify("{.field total_memory} must be a single value", call = .call)
  } else if (is.numeric(value)) {
    notify("{.field total_memory} must have a unit (e.g., 4G)", call = .call)
  } else {
    total_memory_chr <- suppressWarnings(as.character(value))
    if (!nzchar(total_memory_chr)) {
      notify("{.field total_memory} is empty", call = .call)
    }
    if (!grepl("(K|M|G|T)$", total_memory_chr)) {
      notify("{.field total_memory} must end with K, M, G, or T", call = .call)
    }
  }
  return(invisible())
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
    notify("{.field memory_per_core} is missing", call = .call)
  } else if (settings$allow_na && isTRUE(is.na(value))) {
    return(invisible())
  } else if (length(value) > 1) {
    notify("{.field memory_per_core} must be a single value", call = .call)
  } else if (is.numeric(value)) {
    notify("{.field memory_per_core} must have a unit (e.g., 4G)", call = .call)
  } else {
    memory_per_core_chr <- suppressWarnings(as.character(value))
    if (!nzchar(memory_per_core_chr)) {
      notify("{.field memory_per_core} is empty", call = .call)
    }
    if (!grepl("(K|M|G|T)$", memory_per_core_chr)) {
      notify("{.field memory_per_core} must end with K, M, G, or T", call = .call)
    }
  }
  return(invisible())
}

#' Internal scheduler validator
#' @noRd
.validate_scheduler <- function(
  value, .call = rlang::caller_env(),
  settings = .get_validator_defaults("scheduler")
) {
  notify <- if (settings$fail_on_invalid) cli::cli_abort else cli::cli_warn
  items <- names(class_pb_scheduler@properties)
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
    notify("{.field scheduler_name} is missing", call = .call)
  } else if (settings$allow_na && isTRUE(is.na(scheduler_name))) {
    return(invisible())
  } else if (!checkmate::test_string(scheduler_name, min.chars = 1)) {
    notify(
      "{.field scheduler_name} must be a single, non-empty character string",
      call = .call
    )
  } else if (
    !checkmate::test_choice(
      standardize_scheduler_name(scheduler_name),
      get_supported_schedulers(include_alias = FALSE)
    )
  ) {
    notify(
      "{.field scheduler_name} is invalid or currently not supported",
      call = .call
    )
  }
  return(invisible())
}

#' Internal packages validator
#' @noRd
.validate_packages <- function(
  value, .call = rlang::caller_env(),
  settings = .get_validator_defaults("packages")
) {
  notify <- if (settings$fail_on_invalid) cli::cli_abort else cli::cli_warn
  items <- names(class_pb_packages@properties)
  invalid_fields <- setdiff(names(value), items)
  if (length(invalid_fields)) {
    # rare case when working directly with the validate functions directly
    cli::cli_abort(
      c("Packages has invalid fields:", "{.list {invalid_fields}}"),
      call = .call
    )
  }
  package_names <- value$package_names
  install <- value$install
  install_library <- value$install_library
  policy_choices <- c("never", "ask", "always")
  if (.is_missing(package_names)) {
    if (settings$allow_missing) return(invisible())
    notify("{.field package_names} are missing", call = .call)
  } else if (settings$allow_na && isTRUE(is.na(package_names))) {
    return(invisible())
  } else if (!checkmate::test_character(package_names, min.len = 1)) {
    notify("{.field package_names} must be a character vector", call = .call)
  } else if (any(!nzchar(package_names))) {
    notify("{.field package_names} cannot be empty strings", call = .call)
  }
  policy_missing_ok <- .is_missing(install) && settings$allow_missing
  if (policy_missing_ok) {
    # skip validation of `install` if missing and allowed
  } else if (!length(install)) {
    notify(
      "Package installation policy ({.field install}) is missing",
      call = .call
    )
  } else if (settings$allow_na && isTRUE(is.na(install))) {
    # NA is an explicitly allowed value. Skip validation of `install`,
    # but continue so `install_library` is still validated below.
  } else if (!is.character(install) || length(install) != 1L ||
               is.na(install) || !install %in% policy_choices) {
    cli::cli_abort(
      "Package installation policy ({.field install}) must be one of {.or {policy_choices}}",
      call = .call
    )
  }
  if (settings$allow_na && isTRUE(is.na(install_library))) {
  # NA is allowed for `install_library`; skip its validation.
  } else if (length(install_library) && (!is.character(install_library) ||
                length(install_library) != 1L || is.na(install_library) ||
                !nzchar(install_library))) {
    notify(
      "{.field install_library} must be one non-empty path",
      call = .call
    )
  }
  return(invisible())
}

#' Internal R library environment validator
#' @noRd
.validate_libraries <- function(
  value, .call = rlang::caller_env(),
  settings = .get_validator_defaults("libraries")
) {
  notify <- if (settings$fail_on_invalid) cli::cli_abort else cli::cli_warn
  items <- names(class_pb_libraries@properties)
  invalid_fields <- setdiff(names(value), items)
  if (length(invalid_fields)) {
    cli::cli_abort(
      c("Libraries has invalid fields:", "{.list {invalid_fields}}"),
      call = .call
    )
  }
  for (field in items) {
    paths <- value[[field]]
    if (.is_missing(paths)) {
      next
    }
    if (!is.character(paths) || anyNA(paths) || any(!nzchar(paths))) {
      notify(
        "{.field {field}} must be a character vector of non-empty paths",
        call = .call
      )
    }
  }
  return(invisible())
}

#' Internal run settings validator
#' @noRd
.validate_run_settings <- function(
  value, .call = rlang::caller_env(),
  settings = .get_validator_defaults("run_settings")
) {
  notify <- if (settings$fail_on_invalid) cli::cli_abort else cli::cli_warn
  items <- names(class_pb_run_settings@properties)
  invalid_fields <- setdiff(names(value), items)
  if (length(invalid_fields)) {
    cli::cli_abort(
      c("Run settings has invalid fields:", "{.list {invalid_fields}}"),
      call = .call
    )
  }
  for (field in items) {
    val <- value[[field]]
    if (.is_missing(val)) {
      next
    }
    if (!is.logical(val) || length(val) != 1L || is.na(val)) {
      notify(
        "{.field {field}} must be a single logical value",
        call = .call
      )
    }
  }
  return(invisible())
}
