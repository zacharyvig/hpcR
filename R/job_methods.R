#' @title Working with hpcR job objects
#'
#' @param job A job object
#' @param ... Not currently used
#'
#' @name hpcR_methods
#' @aliases submit
#' @import S7
#' @docType methods
NULL

#' @rdname hpcR_methods
#' @export
submit <- S7::new_generic("submit", "job")


S7::method(submit, class_job) <- function(job) {
  # validate job for submission
  .validate_job(job, stage = "submit", .call = rlang::caller_env())
  # compile job for submission
  compiled_job <- .compile_job(job)
  # submit job
  out <- .submit_job(
    input = compiled_job@.compiled@submit_system_file,
    input_type = compiled_job@input@input_type,
    scheduler_name = compiled_job@scheduler@scheduler_name,
    env_variables = compiled_job@.compiled@env_variables,
    control = compiled_job@.compiled@submit_control,
    echo = TRUE,
    .call = rlang::caller_env()
  )
  if (is.null(out)) {
    return(invisible(NULL))
  } else {
    return(out)
  }
}


S7::method(format, class_job) <- function(x, ...) {
  .format_job(x, ...)
}

S7::method(print, class_job) <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}

S7::method(summary, class_job) <- function(object, ...) {
  .job_to_summary(object)
}

S7::method(format, class_job_summary) <- function(x, ...) {
  .format_job_summary(x, ...)
}

S7::method(print, class_job_summary) <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}
