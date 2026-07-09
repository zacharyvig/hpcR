#' Internal utility functions for hpcR
#' @name hpcR_utils
NULL

#' @param scheduler_name Scheduler name to standardize
#' @param strict Logical. If \code{TRUE}, throws an error if the scheduler name
#' is not recognized. Otherwise, returns the input scheduler name.
#' @rdname hpcR_utils
#' @keywords internal
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
  return(scheduler_name)
}

#' @param hpc_only Logical. If \code{TRUE}, only returns HPC schedulers,
#' excluding local scheduler and aliases. Default: \code{FALSE}.
#' @param include_alias Logical. If \code{TRUE}, includes aliases for HPC and 
#' local schedulers. Default: \code{FALSE}.
#' @keywords internal
#' @rdname hpcR_utils
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

#' @keywords internal
#' @rdname hpcR_utils
get_supported_languages <- function() {
  c("R")
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

#' Internal function to check if packages are installed
#' @noRd
.packages_installed <- function(package_names, library_paths = NULL) {
  vapply(
    package_names, requireNamespace, quietly = TRUE,
    lib.loc = library_paths, FUN.VALUE = logical(1)
  )
}

#' Collapse R library paths for an R startup environment variable
#' @noRd
.collapse_r_library_paths <- function(paths) {
  paths <- unique(paths[!is.na(paths) & nzchar(paths)])
  paste(paths, collapse = .Platform$path.sep)
}

#' Expand and remove empty R library paths
#' @noRd
.expand_r_library_paths <- function(paths) {
  paths <- paths[!is.na(paths) & nzchar(paths)]
  unique(path.expand(paths))
}

#' Read R library paths from an environment variable
#' @noRd
.environment_r_library_paths <- function(variable) {
  value <- Sys.getenv(variable, unset = "")
  if (!nzchar(value)) {
    return(character(0))
  }
  .expand_r_library_paths(strsplit(
    value, .Platform$path.sep, fixed = TRUE
  )[[1]])
}

#' Return the R library paths used to resolve packages for a job
#' @noRd
.job_library_paths <- function(job) {
  unique(c(
    .expand_r_library_paths(job@r_libraries@r_libs),
    .expand_r_library_paths(job@r_libraries@r_libs_user),
    .expand_r_library_paths(job@packages@install_library),
    .environment_r_library_paths("R_LIBS"),
    .environment_r_library_paths("R_LIBS_USER"),
    .libPaths()
  ))
}

#' Find the default user library configured for package installation
#' @noRd
.default_install_library <- function(job) {
  configured <- .expand_r_library_paths(job@r_libraries@r_libs_user)
  if (length(configured) == 1L) {
    return(configured)
  }

  configured <- .environment_r_library_paths("R_LIBS_USER")
  if (length(configured) == 1L) {
    return(configured)
  }

  cli::cli_abort(
    paste(
      "No single user library is configured for installation.",
      "Supply {.code install_library} to {.fn packages}."
    )
  )
}

#' Create and validate a writable package installation library
#' @noRd
.writable_install_library <- function(path) {
  path <- path.expand(path)
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(path) || file.access(path, mode = 2) != 0) {
    cli::cli_abort("Package installation library is not writable: {.path {path}}")
  }
  path
}

#' Install requested packages before submission when configured to do so
#' @noRd
.prepare_job_packages <- function(job) {
  package_names <- job@packages@package_names
  library_paths <- .job_library_paths(job)
  missing <- package_names[!.packages_installed(package_names, library_paths)]
  if (!length(missing)) {
    return(invisible())
  }

  install <- job@packages@install
  if (identical(install, "never")) {
    cli::cli_abort(
      c(
        "Requested packages are not installed:",
        "{.list {missing}}",
        "i" = "Install them before submission or use {.code install = 'ask'} or {.code install = 'always'} in {.fn packages}."
      )
    )
  }
  if (identical(install, "ask")) {
    if (!interactive()) {
      cli::cli_abort(
        "Missing requested packages with {.code install = 'ask'} in a non-interactive session. Use {.code install = 'always'} or install them before submission."
      )
    }
    confirmed <- utils::askYesNo(
      sprintf("Install missing package(s): %s?", paste(missing, collapse = ", "))
    )
    if (!isTRUE(confirmed)) {
      cli::cli_abort("Package installation was declined; job was not submitted.")
    }
  }

  install_library <- job@packages@install_library
  if (!length(install_library)) {
    install_library <- .default_install_library(job)
  }
  install_library <- .writable_install_library(install_library)

  cli::cli_inform("Installing missing packages in {.path {install_library}}: {.pkg {missing}}")
  tryCatch(
    utils::install.packages(missing, lib = install_library),
    error = function(e) {
      cli::cli_abort(
        c("Package installation failed.", "x" = "{conditionMessage(e)}")
      )
    }
  )

  library_paths <- unique(c(install_library, library_paths))
  still_missing <- package_names[!
    .packages_installed(package_names, library_paths)
  ]
  if (length(still_missing)) {
    cli::cli_abort(
      c("Requested packages remain unavailable after installation:", "{.list {still_missing}}")
    )
  }
  return(invisible())
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
