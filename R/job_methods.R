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
  cli::cli_alert("Submit method not yet implemented")
  return(invisible(NULL))
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
