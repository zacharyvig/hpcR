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
      length(value) && (any(is.null(names(value))) | any(names(value) == ""))
    ) {
      "elements must be named"
    }
  }
)