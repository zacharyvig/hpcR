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