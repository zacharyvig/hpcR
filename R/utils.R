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
.packages_installed <- function(package_names, installation_path = NULL) {
  vapply(
    package_names, requireNamespace, quietly = TRUE,
    lib.loc = installation_path, FUN.VALUE = logical(1)
  )
}

#' Internal function to attempt package installation
#' @noRd
.attempt_package_install <- function(package_names, installation_path = NULL) {
  vapply(
    package_names, function(pkg) {
      tryCatch(
        utils::install.packages(pkg, lib.loc = installation_path),
        error = function(e) {
          cli::cli_warn(
            "Failed to install package {.pkg {pkg}}."
          )
          return(FALSE)
        }
      )
      return(TRUE)
    },
    logical(1)
  )
}