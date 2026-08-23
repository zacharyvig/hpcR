# TODO: more formal documentation


#' @title Incrementally build a job using `+`
#'
#' @description
#' ...
#'
#' @details
#' ...
#'
#' @param e1 A class_job object to be updated
#' @param e2 A class_job_update object containing the updates
#' @param ... Not currently used
#'
#'
#' @name update_job
#' @docType methods
#' @include job_classes.R
#' @export
update_job <- S7::new_generic("update_job", c("e1", "e2"))


S7::method(`+`, list(class_job, class_job_update)) <- function(e1, e2) {
  .update_job(e1, e2, use_default_settings = TRUE)
}


S7::method(
  update_job, list(class_job, class_job_update)
) <- function(
  e1, e2, ...
) {
  .update_job(e1, e2, ...)
}


#' Internal function to update a Job object by adding a class_job_update object
#' and validate properties
#' @param warn_overwrite Whether to warn about overwritten properties
#' @param .call The calling environment for warning/error messages
#' @param use_default_settings Whether to use default validation settings
#'
#' @noRd
.update_job <- function(
  e1, e2,
  warn_overwrite = TRUE,
  overwrite = TRUE,
  use_default_settings = TRUE,
  skip_validation = FALSE
) {
  # RHS must be a job update object
  if (is_job_update(e2)) {
    .update_call <- e2@.update_call
    e2 <- e2@updates
  } else {
    cli::cli_abort(
      "Invalid job update object; must be of class 'class_job_update'",
    )
  }
  if (is.null(names(e2))) {
    cli::cli_abort("Job update is empty")
  }
  if (!all(names(e2) %in% S7::prop_names(e1))) {
    cli::cli_abort("Job update contains unknown properties")
  }
  # unlock if necessary
  has_lock <- ".locked" %in% S7::prop_names(e1)
  if (has_lock) {
    old_lock <- e1@.locked
    e1@.locked <- FALSE
    on.exit({
      e1@.locked <- old_lock
    }, add = TRUE)
  }
  # only update non-empty properties
  nonempty <- e2[as.logical(vapply(e2, length, integer(1)))]
  overwritten <- c()
  for (property in names(nonempty)) {
    old_value <- S7::prop(e1, property)
    new_value <- nonempty[[property]]
    is_block_update <- is.list(new_value) && is_property_block(old_value)
    if (is.list(new_value) && !is_property_block(old_value)) {
      cli::cli_abort(
        "Attempting to overwrite a non-list property with a list in job update",
        internal = TRUE
      )
    }
    # store overwritten properties
    if (is_block_update) {
      merged <- .merge_property_block(
        S7::props(old_value), new_value, overwrite = overwrite
      )
      if (length(merged$overwritten)) {
        overwritten <- c(overwritten, merged$overwritten)
      }
      new_value <- merged$merged_props
    } else if (length(old_value) && !identical(old_value, new_value)) {
      if (!overwrite) {
        nonempty[[property]] <- old_value
        next
      }
      overwritten <- c(overwritten, property)
    }
    if (!skip_validation) {
      validate_property(
        name = property, value = new_value, .call = .update_call,
        use_default_settings = use_default_settings
      )
    }
    # rehydrate property block if applicable
    if (is_block_update) {
      new_value_out <- old_value
      S7::props(new_value_out) <- .coerce_empty_atomic(
        S7::props(old_value), merged$merged_props
      )
    } else {
      new_value_out <- .coerce_empty_atomic(old_value, new_value)
    }
    # update property
    nonempty[[property]] <- new_value_out
  }
  # update properties
  S7::props(e1) <- nonempty
  if (overwrite && warn_overwrite && length(overwritten)) {
    cli::cli_alert_warning(
      "The following properties were overwritten: {.list {overwritten}}"
    )
  }
  if (has_lock) e1@.locked <- TRUE
  e1
}

#' Internal helper to merge two lists that will be used to update a property
#' block
#' @noRd
.merge_property_block <- function(
  old_values, new_values,
  overwrite = TRUE,
  .call = rlang::caller_env()
) {
  # Implementation for merging property blocks
  if (!all(names(new_values) %in% names(old_values))) {
    cli::cli_abort("Job update contains unknown properties", call = .call)
  }
  # retrieve non-empty new values
  idx <- vapply(new_values, length, integer(1))
  nonempty <- new_values[as.logical(idx)]
  # track which properties are being overwritten
  overwritten <- c()
  for (property in names(nonempty)) {
    old_value <- old_values[[property]]
    new_value <- nonempty[[property]]
    if (length(old_value) && !identical(old_value, new_value)) {
      if (!overwrite) {
        nonempty[[property]] <- old_value
        next
      }
      overwritten <- c(overwritten, property)
    }
    nonempty[[property]] <- new_value
  }
  # merge new values into old (to preserve any properties not being updated)
  merged_props <- old_values
  merged_props[names(nonempty)] <- nonempty
  list(merged_props = merged_props, overwritten = overwritten)
}

#' Coerce empty atomic values to the correct class of length zero;
#' supplying NA is how properties are "deleted"
#' @noRd
.coerce_empty_atomic <- function(old_value, new_value) {
  if (is.list(new_value) && is.list(old_value)) {
    out <- new_value
    for (property in names(out)) {
      out[[property]] <- .coerce_empty_atomic(
        old_value[[property]], out[[property]]
      )
    }
    return(out)
  }
  if (is.atomic(new_value) && isTRUE(is.na(new_value))) {
    # handle empty atomic values by coercing to the correct class
    return(do.call(class(old_value), list(0)))
  }
  new_value
}
