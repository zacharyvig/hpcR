
#' @title Build a job to submit to an HPC
#' @description
#' \pkg{hpcR} provides a dynamic, user-friendly way to build
#' jobs to submit to high-performance clusters (HPCs). Properties of the job
#' are added using the \code{+} operator (think \pkg{ggplot2})
#'
#' Properties are represented as functions where the name of the function is the
#' property (e.g., \code{scheduler()}), or the group of properties (e.g.,
#' \code{resources(n_nodes = ...)}), and the arguments are the values. These
#' functions are light-weight and meant to be easy to remember.
#'
#' @details
#' Building a job always starts by initializing the job object using a
#' language-specific function (e.g., \code{rjob()}), depending on the language
#' of your script/code. Properties are then successively added, and validated,
#' i.e., errors are thrown about formatting and other basic mistakes.
#'
#' Property statements are either named by the property they represent (e.g.,
#' \code{script} takes a path to a script to be executed), or by the general
#' group of properties when a collection of properties are related (e.g.,
#' \code{resources()} takes arguments \code{n_nodes}, \code{n_cores}, etc.)
#'
#' @param job_name A character string. The name of the job used in dependency
#' specification and job scheduler naming.
#' @param script_path A character string. The path to the script to be executed
#' by the HPC.
#' @param job_directory  A character string. The path to the 'home' directory
#' for this job. TODO: Say what will be stored here
#' @param scheduler_name A character string. The scheduler to be used for this
#' job. Options are slurm' (or 'sbatch'), 'torque' (or 'qsub'), or 'local'
#' (or 'sh').
#' @param n_nodes A number or character string. The number of compute nodes
#' to be requested on the scheduler.
#' @param n_cores A number or character string. The number of CPUs to be
#' requested on the scheduler.
#' @param wall_time A number or character string. The compute time requested
#' on the cluster using format "MM[:SS]", "HH:MM:SS" or "D-HH[:MM][:SS]"
#' @param total_memory A number or character string. The total amount of memory
#' to requested by the job. This is mutually exclusive with
#' \code{memory_per_core}.
#' @param memory_per_core A number or character string. The amount of memory to
#' be requested per core. This is mutually exclusive with \code{total_memory}.
#' @param package_names A character vector. The names of packages to be loaded
#' for the job.
#' @param installation_path A character vector. The path(s) to the library
#' where packages are stored. Defaults to \code{.libPaths()}, which should
#' work most of the time.
#' @param print_session_info If \code{TRUE}, print the \code{sessionInfo()} and
#' \code{Sys.info()} when the job starts. Useful for debugging problems with the
#' compute environment or R installation. Default: \code{FALSE}.
#' @param print_environment If \code{TRUE}, print the \code{Sys.getenv()} when
#' the job starts. This can produce a lot of output, but can be useful if
#' certain environment variables are not being found when your job runs, leading
#' it to fail. Default: \code{FALSE}.
#'
#' @section Property Statements:
#' The following are the main property statements currently available:
#' \itemize{
#'    \item{\code{rjob()}:}{}
#' }
#'
#' @returns A job object with the properties specified by the user.
#'
#' @name build_job
NULL

#' @rdname build_job
#' @export
rjob <- function(job_name = NULL) {
  j <- class_job()
  if (!missing(job_name)) {
    j@job_name <- as.character(job_name)
  }
  input <- class_pb_input(
    input_type = "script",
    input_value = character(0),
    extension = "R",
    language = "R"
  )
  j@input <- input
  # lock object before returning
  j@.locked <- TRUE
  return(j)
}

#' @rdname build_job
#' @export
job_name <- function(job_name = NULL) {
  class_job_update(updates = list(job_name = as.character(job_name)))
}

#' @rdname build_job
#' @export
# `extension` and `language` are currently hard-coded for language-specific jobs
script <- function(script_path = NULL) {
  value <- list(
    input_type = "script",
    input_value = as.character(script_path)
  )
  class_job_update(updates = list(input = value))
}

#' @rdname build_job
#' @export
job_directory <- function(job_directory = NULL) {
  class_job_update(updates = list(job_directory = as.character(job_directory)))
}

#' @rdname build_job
#' @export
# TODO: add more arguments to this function as needed
scheduler <- function(scheduler_name = NULL) {
  value <- list(
    scheduler_name = as.character(
      standardize_scheduler_name(scheduler_name, strict = FALSE)
    )
  )
  class_job_update(updates = list(scheduler = value))
}

#' @rdname build_job
#' @export
resources <- function(
  n_nodes = NULL, n_cores = NULL, wall_time = NULL, total_memory = NULL,
  memory_per_core = NULL
) {
  value <- list(
    n_nodes = as.character(n_nodes),
    n_cores = as.character(n_cores),
    wall_time = as.character(wall_time),
    total_memory = as.character(total_memory),
    memory_per_core = as.character(memory_per_core)
  )
  class_job_update(updates = list(resources = value))
}


#' @rdname build_job
#' @export
packages <- function(package_names = NULL, installation_path = .libPaths()) {
  value <- list(
    package_names = as.character(package_names),
    installation_path = as.character(installation_path)
  )
  class_job_update(updates = list(packages = value))
}

# TODO: add more settings + add validation
#' @rdname build_job
#' @export
settings <- function(print_session_info = NULL, print_environment = NULL) {
  cli::cli_abort("Validation of settings not yet implemented")
  value <- list(
    run_settings = list(
      print_session_info = as.logical(print_session_info),
      print_environment = as.logical(print_environment)
    )
  )
  class_job_update(updates = list(.settings = value))
}