#' @title Working with hpcR job objects
#'
#' @description Once a job is built (see \link{build_job}), it can be submitted
#' to the specified scheduler using the \code{submit()} function. It can also be
#' easily examined using the \code{summary()} method, which provides a concise
#' summary of the job's key properties.
#'
#' @param job,x,object An \link{hpcR} job object or job summary object.
#' @param ... Not currently used
#'
#' @section Methods:
#'
#' The following methods are available and useful for working with \link{hpcR}
#' job objects:
#' \describe{
#'  \item{\code{submit()}:}{Submits a job to the specified scheduler.}
#'  \item{\code{summary()}:}{
#'    Provides a summary of the job object, including key
#'    properties.
#'   }
#' }
#'
#' @examples
#' \dontrun{
#' my_job <- rjob("my_job") +
#'    script("path/to/script.R") +
#'    scheduler("slurm") +
#'    resources(n_nodes = 2, n_cores = 4,
#'              wall_time = "01:00:00")
#' submit(my_job)
#' summary(my_job)
#' }
#'
#' @name hpcR_methods
#' @aliases submit print summary format
#' @import S7
#' @docType methods
NULL

#' @export
submit <- S7::new_generic("submit", "job")

#' @rdname hpcR_methods
#' @name submit.class_job
#' @method submit class_job
#' @export
S7::method(submit, class_job) <- function(job, ...) {
  # validate job for submission
  .validate_job(job, stage = "submit", .call = rlang::caller_env())
  # ensure declared packages are available before compiling the job
  .prepare_job_packages(job)
  # compile job for submission
  compiled_job <- .compile_job(job)
  # submit job
  cli::cli_progress_step(
    "Submitting job '{job@job_name}'"
  )
  out <- .submit_job(
    input = compiled_job@.compiled@submit_system_file,
    input_type = compiled_job@input@input_type,
    scheduler_name = compiled_job@scheduler@scheduler_name,
    env_variables = compiled_job@.compiled@env_variables,
    control = compiled_job@.compiled@submit_control,
    .call = rlang::caller_env()
  )
  if (is.null(out)) {
    # TODO: consider situations when out is NULL and return message
    return(invisible(NULL))
  } else {
    cli::cli_progress_step(
      "Job submitted successfully with ID: {.code {out}}"
    )
    return(invisible(out))
  }
}

#' @rdname hpcR_methods
#' @name format.class_job
#' @method format class_job
#' @export
S7::method(format, class_job) <- function(x, ...) {
  .format_job(x, ...)
}

#' @rdname hpcR_methods
#' @name format.class_job_summary
#' @method format class_job_summary
#' @export
S7::method(format, class_job_summary) <- function(x, ...) {
  .format_job_summary(x, ...)
}

#' @rdname hpcR_methods
#' @name print.class_job
#' @method print class_job
#' @export
S7::method(print, class_job) <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}

#' @rdname hpcR_methods
#' @name summary.class_job
#' @method summary class_job
#' @export
S7::method(summary, class_job) <- function(object, ...) {
  .job_to_summary(object)
}

#' @rdname hpcR_methods
#' @name format.class_job_summary
#' @method format class_job_summary
#' @export
S7::method(format, class_job_summary) <- function(x, ...) {
  .format_job_summary(x, ...)
}

#' @rdname hpcR_methods
#' @name print.class_job_summary
#' @method print class_job_summary
#' @export
S7::method(print, class_job_summary) <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}
