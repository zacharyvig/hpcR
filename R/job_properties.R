#' @title S7 property for read-only character objects
#' @param value The character value to be made read-only
#' @return A read-only S7 property
#' @noRd
read_only <- function(value, class = S7::class_character) {
  stopifnot(inherits(value, class))
  S7::new_property(
    class = class,
    getter = function(self) return(value)
  )
}

#' @title S7 property for guarded character objects
#' @description This function returns a property that can only
#' be set/edited if the `.locked` property is `FALSE`.
#' @param property The property to be guarded
#' @param class The class of the property
#' @param default The default value for the property
#' @return A guarded S7 property
#' @noRd
guarded <- function(property, class, default = NULL) {
  S7::new_property(
    class = class,
    setter = function(self, value) {
      if (!S7::prop_exists(self, ".locked")) {
        cli::cli_abort(
          "{.code .locked} property does not exist in {class(self)}",
          .internal = TRUE
        )
      }
      if (self@.locked) cli::cli_abort(
        "{.code {property}} is locked"
      )
      S7::prop(self, property) <- value
      self
    }
  )
}

#' @title S7 property for named lists
#' @format An S7 property that requires that
#' all elements in a list are named.
#' @noRd
named_list <- S7::new_property(
  class = S7::class_list,
  validator = function(value) {
    if (
      length(value) && (any(is.null(names(value))) || any(names(value) == ""))
    ) {
      "elements must be named"
    }
  }
)

#' @title S7 property for named character vectors
#' @format An S7 property that requires that
#' all elements in a character vector are named.
#' @noRd
named_character <- S7::new_property(
  class = S7::class_character,
  validator = function(value) {
    if (
      length(value) && (any(is.null(names(value))) || any(names(value) == ""))
    ) {
      "elements must be named"
    }
  }
)

#' @title S7 property for a list of job objects
#' @format An S7 property that requires that
#' all elements in a list are job objects and
#' each element is named by the job name.
#' @noRd
named_job_list <- S7::new_property(
  class = S7::class_list,
  validator = function(value) {
    if (length(value) && !all(vapply(value, is_job, logical(1)))) {
      "elements must be job objects"
    }
    if (length(value) && (any(is.null(names(value))) || any(names(value) == ""))) {
      "elements must be named"
    }
  }
)

#' @title S7 property for a list of job sequence objects
#' @format An S7 property that is a list of job sequence objects
#' @noRd
job_sequence_list <- S7::new_property(
  class = S7::class_list,
  validator = function(value) {
    if (length(value) && !all(vapply(value, is_job_sequence, logical(1)))) {
      "elements must be job sequence objects"
    }
  }
)