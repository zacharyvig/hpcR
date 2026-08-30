#' @title Working with hpcR objects
#'
#' @description \pkg{hpcR} provides a set of methods for working with job and
#' job sequence objects. Methods include \code{submit()} for submitting jobs,
#' \code{clone()} for cloning, and \code{summary()} for summarizing job objects.
#'
#' @section Methods:
#' \describe{
#'   \item{\link{submit}}{Submit a job or job sequence.}
#'   \item{\link{clone}}{Clone a job or job sequence.}
#'   \item{\link{summary}}{Create a summary of a job.}
#'   \item{\link{print}}{Print an hpcR object.}
#' }
#'
#' @seealso
#'   \code{\link{submit}},
#'   \code{\link{clone}},
#'   \code{\link{summary}},
#'   \code{\link{print}}
#'
#' @name hpcR_methods
NULL

#' @title Submit a job or job sequence to an HPC scheduler
#' @param x An \link{hpcR} job or job sequence object.
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
#' \dontrun{
#' submit(my_job)
#' }
#' @name submit
#' @docType methods
#' @export
submit <- S7::new_generic("submit", "x", function(x) {
  S7::S7_dispatch()
})

S7::method(submit, class_job) <- function(x) {
  defaulted_job <- .hydrate_defaults(x)
  # validate job for submission
  .validate_job(defaulted_job, stage = "submit", .call = rlang::caller_env())
  # ensure declared packages are available before compiling the job
  # TODO generalize this step to "prepare" including other needed steps
  .prepare_job_packages(defaulted_job)
  # compile job for submission
  compiled_job <- .compile_job(defaulted_job)
  # submit job
  ui_job_name <- if (length(compiled_job@job_name)) {
    compiled_job@job_name
  } else {
    "<no name>"
  }
  cli::cli_progress_step(
    "Submitting job '{ui_job_name}'"
  )
  out <- .submit_job(
    input = compiled_job@.compiled@submission_input,
    input_type = compiled_job@.compiled@submission_input_type,
    scheduler_name = compiled_job@scheduler@scheduler_name,
    env_variables = compiled_job@.compiled@env_variables,
    control = compiled_job@.compiled@submit_control,
    .call = rlang::caller_env()
  )
  if (is.null(out)) {
    # TODO: consider situations when out is NULL and return message
    invisible(NULL)
  } else {
    cli::cli_progress_step(
      "Job submitted successfully with ID: {.code {out}}"
    )
    invisible(out)
  }
}

S7::method(submit, class_job_sequence) <- function(x) {
  cli::cli_abort(
    "Job sequence submission is not yet implemented.",
    call = rlang::caller_env()
  )
}

S7::method(submit, S7::class_any) <- function(x) {
  cli::cli_abort(
    "Invalid object type for submission; must be a job or job sequence.",
    call = rlang::caller_env()
  )
}

#' @title Clone a job or job sequence
#' @param x An \link{hpcR} job or job sequence object.
#' @param new_name Optional. A new name for the cloned job or job sequence.
#' @name clone
#' @docType methods
#' @export
clone <- S7::new_generic("clone", "x", function(x, new_name = NULL) {
  S7::S7_dispatch()
})

S7::method(clone, class_job) <- function(x, new_name = NULL) {
  .clone_object(x, new_name = new_name)
}

S7::method(clone, class_job_sequence) <- function(x, new_name = NULL) {
  .clone_object(x, new_name = new_name)
}

S7::method(clone, S7::class_any) <- function(x, new_name = NULL) {
  cli::cli_abort(
    "Invalid object type for cloning; must be a job or job sequence.",
    call = rlang::caller_env()
  )
}

#' @title Summarize a job object
#' @param object A job object to be summarized.
#' @param ... Currently not used.
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
#' summary(my_job)
#'
#' @name summary
#' @aliases summary.class_job
#' @method summary class_job
#' @docType methods
#' @export
S7::method(summary, class_job) <- function(object, ...) {
  .job_to_summary(object)
}

#' @title Format and print hpcR objects
#'
#' @param x An hpcR object to be formatted or printed.
#' @param ... Currently not used.
#'
#' @name format_print
#' @aliases format
#' @aliases print
#' @aliases format.class_job
#' @aliases print.class_job
#' @aliases format.class_job_summary
#' @aliases print.class_job_summary
#' @docType methods
#' @keywords internal
NULL


S7::method(format, class_job) <- function(x, ...) {
  .format_job(x, ...)
}

S7::method(print, class_job) <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}

S7::method(format, class_job_summary) <- function(x, ...) {
  .format_job_summary(x, ...)
}

S7::method(print, class_job_summary) <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}
