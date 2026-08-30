#' Utility functions for hpcR
#' @name hpcR_utils
NULL

#' @param scheduler_name Scheduler name to standardize
#' @param strict Logical. If \code{TRUE}, throws an error if the scheduler name
#' is not recognized. Otherwise, returns the input scheduler name.
#' @rdname hpcR_utils
#' @export
standardize_scheduler_name <- function(scheduler_name, strict = FALSE) {
  scheduler_name <- tolower(scheduler_name)
  scheduler_name <- switch(scheduler_name,
    "sbatch" = "slurm",
    "qsub" = "torque",
    "sh" = "local",
    scheduler_name
  )
  supported_schedulers <- get_supported_schedulers(include_alias = FALSE)
  if (strict && !scheduler_name %in% supported_schedulers) {
    cli::cli_abort("Invalid scheduler: {scheduler_name}")
  }
  scheduler_name
}

#' @param hpc_only Logical. If \code{TRUE}, only returns HPC schedulers,
#' excluding local scheduler and aliases. Default: \code{FALSE}.
#' @param include_alias Logical. If \code{TRUE}, includes aliases for HPC and 
#' local schedulers. Default: \code{FALSE}.
#' @rdname hpcR_utils
#' @export
get_supported_schedulers <- function(hpc_only = FALSE, include_alias = TRUE) {
  # central definition of hpc schedulers and aliases
  hpc <- c("slurm", "torque")
  hpc_alias <- c("sbatch", "qsub")
  local_alias <- c("sh")
  c(
    hpc,
    if (include_alias) hpc_alias,
    if (include_alias && !hpc_only) local_alias,
    if (!hpc_only) "local"
  )
}

#' @rdname hpcR_utils
#' @export
get_supported_languages <- function() {
  c("R")
}

#' Generate an id for internal use in job objects
#' @noRd
.generate_object_id <- function(type = c("job", "job_sequence"), n = 12L) {
  type <- match.arg(type)
  type_prefix <- switch(type,
    job = "hpcR_j_",
    job_sequence = "hpcR_s_"
  )
  paste0(
    type_prefix,
    paste0(sample(c(letters, 0:9), size = n, replace = TRUE), collapse = "")
  )
}


#' Handle `variable=value` and `variable` combinations
#' @noRd
.paste_args <- function(str_vec) {
  nms <- names(str_vec)
  sapply(seq_along(str_vec), function(x) {
    if (is.na(str_vec[x])) {
      # in case of NA just return name
      out <- nms[x]
    } else {
      # else return name and quoted argument separated by equal sign
      val <- ifelse(
        grepl("^[\"'].*[\"']$", str_vec[x], perl = TRUE),
        str_vec[x], paste0("\"", str_vec[x], "\"")
      )
      out <- paste0(nms[x], "=", val)
    }
    return(out)
  })
}

#' Assert local scheduler support on the current platform
#' @noRd
.assert_local_supported <- function() {
  if (.Platform$OS.type == "windows") {
    cli::cli_abort(
      paste("Local scheduler requires UNIX-like shell tools (backgrounding +",
            "ps); unsupported on Windows.")
    )
  }
}

#' Alternative version of sprintf that returns NULL if any argument is length 0
#' Currently used in job compiler
#' @noRd
.sprintf_null <- function(fmt, ...) {
  lengths <- vapply(list(...), length, integer(1))
  if (any(lengths == 0)) {
    return(NULL)
  } else {
    return(sprintf(fmt, ...))
  }
}

#' Convert empty vectors to NULL
#' Currently used in job compiler
#' @noRd
.nullify <- function(x) {
  if (length(x) == 0 || !nzchar(x)) {
    return(NULL)
  } else {
    return(x)
  }
}

#' Helper to convert a property block to a list, recursively
#' @noRd
.property_block_to_list <- function(x) {
  if (!isTRUE(is_property_block(x))) {
    return(x)
  }
  props <- S7::props(x)
  for (property in names(props)) {
    props[[property]] <- .property_block_to_list(props[[property]])
  }
  props
}

#' Internal function to wait until a condition is true or until a timeout is
#' reached
#' @param condition_fn A function that returns a logical value. The function
#' will be'nrepeatedly called until it returns TRUE or until the timeout is
#' reached.
#' @param timeout Numeric. Maximum time to wait in seconds. Default: 5 seconds.
#' @param poll_interval Numeric. Time to wait between checks in seconds.
#' Default: 0.05 seconds.
#' @return Logical. TRUE if the condition was met within the timeout, FALSE
#' otherwise.
#' @noRd
.wait_until <- function(condition_fn, timeout = 5, poll_interval = 0.05) {
  start_time <- Sys.time()
  while (Sys.time() - start_time < timeout) {
    if (condition_fn()) {
      return(invisible(TRUE))
    }
    Sys.sleep(poll_interval)
  }
  return(invisible(FALSE))
}

# .parse_alt_args() is not currently used
# A possible implementation I had tried was for libraries() to accept, e.g.,
# R_LIBS= as an alternative to job=, but I opted for the strict approach of 
# not using ... in sugar functions.
# Here was the idea:
# dotdotdot <- match.call(expand.dots = FALSE)$...
# value <- .parse_alt_args(
#   job = job,
#   user = user,
#   site = site,
#   .dotdotdot = dotdotdot,
#   .map = c(
#     R_LIBS = "job",
#     R_LIBS_USER = "user",
#     R_LIBS_SITE = "site"
#   ),
#   .env = rlang::caller_env(),
#   .call = rlang::caller_call()
# )

#' Internal function to parse alternative argument names that are not explicity
#' defined in the function signature.
#' @param ... The canonical arguments of the original function.
#' @param .map A named character vector that maps alternative argument names to
#' canonical argument names. The names of the vector are the alternative names,
#' and the values are the canonical names.
#' @param .dotdotdot A list of alternative arguments passed to the original
#' function. This is obtained from\code{match.call(expand.dots = FALSE)$...}.
#' @param .call The call environment of the original function.
#' @param .env The environment in which to evaluate the alternative arguments.
#' @noRd
.parse_alt_args <- function(
  ..., .map, .dotdotdot, .call = rlang::caller_call(),
  .env = rlang::caller_env()
) {

  # function that mimics base R behavior for unused arguments
  stop_unused_args <- function(dots, idx) {
    n <- length(idx)
    pieces <- vapply(
      idx,
      function(i) {
        nm <- names(dots)[[i]]
        val <- paste(deparse(dots[[i]], width.cutoff = 60), collapse = "")
        if (is.null(nm) || identical(nm, "")) {
          val
        } else {
          paste0(nm, " = ", val)
        }
      },
      character(1)
    )
    if (n == 1) {
      stop("unused argument (", pieces, ")", call. = FALSE)
    } else {
      stop(
        "unused arguments (", paste(pieces, collapse = ", "), ")",
        call. = FALSE
      )
    }
  }

  # function that mimics base R behavior for multiple arguments w/ same name
  stop_multiple_actual_args <- function(arg, call) {
    stop(
      simpleError(
        message = sprintf(
          'formal argument "%s" matched by multiple actual arguments', arg
        ),
        call = call
      )
    )
  }

  eval_dot_arg <- function(dots, i, env) {
    # If dotdotdot came from match.call(expand.dots = FALSE)$...,
    # its contents are unevaluated expressions in a pairlist.
    # If it came from list(...), they are already evaluated.
    if (typeof(dots) == "pairlist") {
      eval(dots[[i]], envir = env)
    } else {
      dots[[i]]
    }
  }

  # validate inputs with internal errors
  args <- list(...)
  if (!is.list(args) || is.null(names(args)) || any(names(args) == "")) {
    cli::cli_abort(
      "Canonical arguments passed to {.fn .parse_alt_args} must be named.",
      ..internal = TRUE
    )
  }

  if (!is.character(.map) || is.null(names(.map)) || any(names(.map) == "")) {
    cli::cli_abort(
      "{.arg map} must be a named character vector.",
      ..internal = TRUE
    )
  }

  bad_targets <- setdiff(unname(.map), names(args))
  if (length(bad_targets) > 0) {
    cli::cli_abort(c(
      "{.arg map} points to unknown canonical argument{?s}.",
      "x" = "Unknown canonical argument{?s}: {.arg {bad_targets}}."
    ), ..internal = TRUE)
  }

  if (is.null(.dotdotdot)) {
    .dotdotdot <- list()
  }

  if (!is.list(.dotdotdot)) {
    cli::cli_abort(
      "{.arg .dotdotdot} must be a list or pairlist.",
      ..internal = TRUE
    )
  }

  if (length(.dotdotdot) == 0) {
    return(args)
  }

  # mimic base R behavior for unused or duplicated arguments
  dot_names <- names(.dotdotdot)
  if (is.null(dot_names)) {
    stop_unused_args(.dotdotdot, seq_along(.dotdotdot))
  }

  unnamed <- which(dot_names == "")
  if (length(unnamed) > 0) {
    stop_unused_args(.dotdotdot, unnamed)
  }

  unknown <- which(!dot_names %in% names(.map))
  if (length(unknown) > 0) {
    stop_unused_args(.dotdotdot, unknown)
  }

  duplicated_aliases <- unique(dot_names[duplicated(dot_names)])
  if (length(duplicated_aliases) > 0) {
    stop_multiple_actual_args(duplicated_aliases[1], .call)
  }

  # map alternative argument names to canonical names and evaluate them
  for (alias in names(.map)) {
    alias_index <- match(alias, dot_names)

    if (is.na(alias_index)) {
      next
    }

    canonical <- unname(.map[[alias]])
    alias_value <- eval_dot_arg(.dotdotdot, alias_index, .env)

    # avoid overwriting canonical arguments with alternative arguments
    if (!is.null(args[[canonical]]) && !is.null(alias_value)) {
      cli::cli_abort(
        "Use only one of {.arg {canonical}} or {.arg {alias}}.",
        call = .call
      )
    }

    if (!is.null(alias_value)) {
      args[[canonical]] <- alias_value
    }
  }

  args
}

.job_obj_guard <- function(
  x, fn, alt_fn = NULL,
  .call = rlang::caller_env()
) {
  msgs <- c("You supplied one or more job objects to {.code {fn}}.")
  if (!is.null(alt_fn)) {
    msgs <- c(
      msgs, "Did you mean to use {.code {alt_fn}} instead of {.code {fn}}?"
    )
  }
  jobs <- is_job(x)
  if (any(jobs)) {
    cli::cli_abort(msgs, call = .call)
  }
  invisible(x)
}
