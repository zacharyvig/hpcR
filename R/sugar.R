
#' @title Build a job to submit to an HPC
#' @description
#' \pkg{hpcR} provides a dynamic, user-friendly way to build
#' jobs to submit to high-performance clusters (HPCs). Properties of the job
#' are added using the \code{+} operator (think \pkg{ggplot2})
#'
#' Properties are represented as functions where the name of the function is the
#' property (e.g., \code{scheduler()}), or the group of properties (e.g.,
#' \code{resources(n_nodes = ...)}), and the arguments are the values. These
#' functions are light-weight and meant to be easy to remember. Additional
#' properties can always be added later.
#'
#' Once a job is built, it can be submitted to the specified scheduler using the
#' \code{submit()} function. See \link{hpcR_methods} for more details on working
#' with job objects.
#'
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
#' @param job_name A character string. The name of the job used by the scheduler
#' and in the job output.
#' @param script_path A character string. The path to the script to be executed
#' by the HPC.
#' @param job_directory  A character string. The path to the 'home' directory
#' for this job.
#' @param create A logical. If \code{TRUE}, the job directory will be created if
#' it does not exist. Default: \code{FALSE}.
#' @param scheduler_name A character string. The scheduler to be used for this
#' job. Options are 'slurm' (or 'sbatch'), 'torque' (or 'qsub'), or 'local'
#' (or 'sh').
#' @param n_nodes A number or character string. The number of compute nodes
#' to be requested on the scheduler.
#' @param n_cores A number or character string. The number of CPUs to be
#' requested on the scheduler.
#' @param wall_time A number or character string. The compute time requested
#' on the cluster using format "MM[:SS]", "HH:MM:SS" or "D-HH[:MM][:SS]". The
#' \code{\link{format_wall_time}} function is helpful for converting time
#' components to the correct format.
#' @param total_memory A number or character string. The total amount of memory
#' to be requested by the job. This is mutually exclusive with
#' \code{memory_per_core}.
#' @param memory_per_core A number or character string. The amount of memory to
#' be requested per core. This is mutually exclusive with \code{total_memory}.
#' @param package_names A character vector. The names of packages to be loaded
#' for the job.
#' @param install Character. Whether to install missing packages before
#' submission: \code{"never"} (the default), \code{"ask"}, or
#' \code{"always"}. Installation occurs in the submission R session, never on
#' the compute node.
#' @param install_library A single library path for installing missing packages.
#' For R jobs, if \code{NULL}, \code{R_LIBS_USER} is used. This path must be
#' writable when an installation is requested.
#' @param job A character vector of library paths to prepend to the job's
#' library paths. For R jobs, this is the \code{R_LIBS} environment variable.
#' @param user A character vector of library paths to store in the job's user
#' library paths. For R jobs, this is the \code{R_LIBS_USER} environment
#' variable and is set by default but can be overridden by the user.
#' @param site A character vector of library paths to store in the job's site
#' library paths which is usually shared across users in a multi-user
#' environment. For R jobs, this is the \code{R_LIBS_SITE} environment variable.
#' @param print_session_info If \code{TRUE}, print the \code{sessionInfo()} and
#' \code{Sys.info()} in the output file when the job starts. Useful for
#' debugging problems with the compute environment or R installation. Default:
#' \code{FALSE}.
#' @param print_environment If \code{TRUE}, print the \code{Sys.getenv()} in the
#' output file when the job starts. This can produce a lot of output, but can be
#' useful if certain environment variables are not being found when your job
#' runs, leading it to fail. Default: \code{FALSE}.
#'
#' @returns A job object with the properties specified by the user.
#'
#' @examples
#' tmp_script <- tempfile(pattern = "my_script", fileext = ".R")
#' writeLines("print('ok')", tmp_script)
#'
#' my_job <- rjob("my_job") +
#'   script(tmp_script) +
#'   scheduler("slurm") +
#'   resources(
#'     n_nodes = 2,
#'     n_cores = 4,
#'     wall_time = "01:00:00"
#'   )
#'
#' my_job
#' @name build_job
NULL

#' @rdname build_job
#' @export
rjob <- function(job_name = NULL) {
  job <- class_job()
  if (!missing(job_name)) {
    job@job_name <- as.character(job_name)
  }
  input <- class_pb_input(
    input_type = "script",
    input_value = character(0),
    extension = "R",
    language = "R"
  )
  job@input <- input
  # lock object before returning
  job@.locked <- TRUE
  job
}

#' @rdname build_job
#' @export
job_name <- function(job_name = NULL) {
  class_job_update(updates = list(job_name = as.character(job_name)))
}

#' @rdname build_job
#' @export
script <- function(script_path = NULL) {
  # `extension` and `language` are currently hard-coded for language-specific
  # jobs
  value <- list(
    input_type = "script",
    input_value = as.character(script_path)
  )
  class_job_update(updates = list(input = value))
}

#' @rdname build_job
#' @export
job_directory <- function(job_directory = NULL, create = FALSE) {
  value <- list(
    path = as.character(job_directory),
    create = as.logical(create)
  )
  class_job_update(updates = list(job_directory = value))
}

#' @rdname build_job
#' @export
scheduler <- function(scheduler_name = NULL) {
  # TODO: add more arguments to this function as needed
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
  n_nodes = NULL,
  n_cores = NULL,
  wall_time = NULL,
  total_memory = NULL,
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
packages <- function(
  package_names = NULL,
  install = NULL,
  install_library = NULL
) {
  value <- list(
    package_names = as.character(package_names),
    install = as.character(install),
    install_library = as.character(install_library)
  )
  class_job_update(updates = list(packages = value))
}

#' @rdname build_job
#' @export
libraries <- function(
  job = NULL,
  user = NULL,
  site = NULL
) {
  value <- list(
    job = as.character(job),
    user = as.character(user),
    site = as.character(site)
  )
  class_job_update(updates = list(libraries = value))
}

# TODO: add more settings
#' @rdname build_job
#' @export
settings <- function(
  print_session_info = NULL,
  print_environment = NULL
) {
  run_settings = list(
    print_session_info = print_session_info,
    print_environment = print_environment
  )
  class_job_update(updates = list(.run_settings = run_settings))
}
