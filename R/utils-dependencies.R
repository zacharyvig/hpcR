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
.collapse_library_paths <- function(paths) {
  paths <- unique(paths[!is.na(paths) & nzchar(paths)])
  paste(paths, collapse = .Platform$path.sep)
}

#' Expand and remove empty R library paths
#' @noRd
.expand_library_paths <- function(paths) {
  paths <- paths[!is.na(paths) & nzchar(paths)]
  unique(path.expand(paths))
}

#' Read R library paths from an environment variable
#' @noRd
.environment_library_paths <- function(variable) {
  value <- Sys.getenv(variable, unset = "")
  if (!nzchar(value)) {
    return(character(0))
  }
  .expand_library_paths(strsplit(
    value, .Platform$path.sep, fixed = TRUE
  )[[1]])
}

#' Return the R library paths used to resolve packages for a job
#' @noRd
.job_library_paths <- function(job) {
  unique(c(
    .expand_library_paths(job@libraries@job),
    .expand_library_paths(job@libraries@user),
    .expand_library_paths(job@packages@install_library),
    if (identical(job@input@language, "R")) {
      c(
        .environment_library_paths("R_LIBS"),
        .environment_library_paths("R_LIBS_USER")
      )
    } else {
      NULL
    },
    .libPaths()
  ))
}

#' Find the default user library configured for package installation
#' @noRd
.default_install_library <- function(job) {
  configured <- .expand_library_paths(job@libraries@user)
  if (length(configured) == 1L) {
    return(configured)
  }

  if (identical(job@input@language, "R")) {
    configured <- .environment_library_paths("R_LIBS_USER")
    if (length(configured) == 1L) {
      return(configured)
    }
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

#' Extract library environment variables for a job
#' @noRd
.get_library_env_vars <- function(job) {
  lib_paths <- if (length(job@packages@package_names)) {
    .job_library_paths(job)
  } else {
    c(job@libraries@job, job@packages@install_library)
  }
  out <- c(
    .collapse_library_paths(lib_paths),
    .collapse_library_paths(job@libraries@user),
    .collapse_library_paths(job@libraries@site)
  )
  names(out) <- switch(
    job@input@language,
    R = c("R_LIBS", "R_LIBS_USER", "R_LIBS_SITE"),
    cli::cli_abort(
      "Unsupported job language: {job@input@language}", .internal = TRUE
    )
  )
  out
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