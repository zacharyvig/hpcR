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
#'
#' @name update_job
#' @usage e1 + e2
#' @aliases +
#' @docType methods
#' @include job_classes.R
#' @export
S7::method(`+`, list(class_job, class_job_update)) <- function(e1, e2) {
  .update_job(e1, e2, use_default_settings = TRUE)
}

#' Internal function to update a job by adding a class_job_update object
#' and validate properties
#' @param e1 The job object to be updated
#' @param e2 The job update object containing the updates
#' @param warn_overwrite Whether to warn about overwritten properties
#' @param overwrite Whether to overwrite existing, non-empty properties
#' @param use_default_settings Whether to use default validation settings
#' @param skip_validation Whether to skip validation of updated properties
#'
#' @noRd
.update_job <- function(
  e1, e2,
  warn_overwrite = TRUE,
  overwrite = TRUE,
  use_default_settings = TRUE,
  skip_validation = FALSE
) {
  if (is_job_update(e2)) {
    .update_call <- e2@.update_call
    e2 <- e2@updates
  } else {
    cli::cli_abort(
      "Invalid job update object; must be of class 'class_job_update'."
    )
  }

  if (is.null(names(e2))) {
    cli::cli_abort("Job update is empty.", call = .update_call)
  }

  if (!all(names(e2) %in% S7::prop_names(e1))) {
    cli::cli_abort(
      "Job update contains unknown properties.", call = .update_call
    )
  }

  updates <- e2[as.logical(vapply(e2, length, integer(1)))]
  overwritten <- character()

  for (property in names(updates)) {
    old_value <- S7::prop(e1, property)
    new_value <- updates[[property]]

    updated <- .update_value(
      old_value = old_value,
      new_value = new_value,
      property = property,
      overwrite = overwrite,
      .call = .update_call
    )

    if (length(updated$overwritten)) {
      overwritten <- c(overwritten, updated$overwritten)
    }

    if (!skip_validation) {
      validate_property(
        name = property,
        value = updated$value,
        .call = .update_call,
        use_default_settings = use_default_settings
      )
    }

    updates[[property]] <- .coerce_update_value(old_value, updated$value)
  }

  has_lock <- ".locked" %in% S7::prop_names(e1)
  if (has_lock) {
    old_lock <- e1@.locked
    e1@.locked <- FALSE
    on.exit({
      e1@.locked <- old_lock
    }, add = TRUE)
  }

  S7::props(e1) <- updates

  if (overwrite && warn_overwrite && length(overwritten)) {
    cli::cli_alert_warning(
      "The following properties were overwritten: {.list {overwritten}}"
    )
  }

  if (has_lock) {
    e1@.locked <- old_lock
  }

  e1
}

#' Helper to update a value in a job object via a job update object
#' @param old_value The old value of the property
#' @param new_value The new value of the property
#' @param property The name of the property being updated
#' @param overwrite Whether to overwrite existing, non-empty properties
#' @param .call The call to use for error messages
#' @noRd
.update_value <- function(old_value,
                          new_value,
                          property,
                          overwrite = TRUE,
                          .call = rlang::caller_call()) {
  # property block update
  if (isTRUE(is_property_block(old_value)) && is.list(new_value)) {
    merged <- .merge_property_block(
      old_block = old_value,
      new_values = new_value,
      overwrite = overwrite,
      .call = .call
    )
    return(merged)
  }

  if (is.list(new_value) && isFALSE(is_property_block(old_value))) {
    cli::cli_abort(
      "Attempting to overwrite a non-property block with a list in job update.",
      call = .call,
      .internal = TRUE
    )
  }

  overwritten <- character()

  if (length(old_value) && !identical(old_value, new_value)) {
    if (!overwrite) {
      return(list(value = old_value, overwritten = character()))
    }

    overwritten <- property
  }

  list(
    value = new_value,
    overwritten = overwritten
  )
}

#' Keep old values and add new values in the case of a property block update
#' @param old_block The old property block
#' @param new_values The new values to update the property block with
#' @param overwrite Whether to overwrite existing, non-empty properties
#' @param .call The call to use for error messages
#' @noRd
.merge_property_block <- function(old_block,
                                  new_values,
                                  overwrite = TRUE,
                                  .call = rlang::caller_call()) {
  # Start with a plain-list version of the old block's properties.
  # This is what prevents nested S7 property blocks from reaching validation.
  old_props <- S7::props(old_block)
  out_props <- .property_block_to_list(old_block)
  unknown_props <- setdiff(names(new_values), names(old_props))

  if (length(unknown_props)) {
    cli::cli_abort(
      "Job update contains unknown properties: {.field {unknown_props}}.",
      call = .call
    )
  }

  updates <- new_values[as.logical(vapply(new_values, length, integer(1)))]

  overwritten <- character()

  for (property in names(updates)) {
    old_value <- old_props[[property]]
    new_value <- updates[[property]]

    updated <- .update_value(
      old_value = old_value,
      new_value = new_value,
      property = property,
      overwrite = overwrite,
      .call = .call
    )

    out_props[[property]] <- updated$value
    overwritten <- c(overwritten, updated$overwritten)
  }

  list(
    value = out_props,
    overwritten = overwritten
  )
}

#' Coerce updated values to the correct class of the old value
#' This allows for the user to use NA when they want to "delete" a property
#' @param old_value The old value of the property
#' @param new_value The new value of the property
#' @noRd
.coerce_update_value <- function(old_value, new_value) {
  # for property blocks, coerce each property
  if (isTRUE(is_property_block(old_value)) && is.list(new_value)) {
    out <- old_value
    S7::props(out) <- .coerce_update_value(
      S7::props(old_value),
      new_value
    )
    return(out)
  }

  # for lists, coerce each element
  if (is.list(new_value) && is.list(old_value)) {
    out <- new_value
    for (property in names(out)) {
      out[[property]] <- .coerce_update_value(
        old_value[[property]],
        out[[property]]
      )
    }
    return(out)
  }

  # for atomics, coerce to class of old_value with length zero
  if (
    is.atomic(new_value) &&
      length(new_value) == 1L &&
      isTRUE(is.na(new_value))
  ) {
    return(old_value[0])
  }

  new_value
}

#' Internal function to clone a job object or a job sequence object
#' @param obj The job or job sequence object to clone
#' @param new_name The new name for the cloned object (job_name or
#' sequence_name). A message is issued if no new name is provided.
#'
#' @noRd
.clone_object <- function(obj, new_name = NULL) {
  if (is_job(obj)) {
    obj_type <- obj_label <- "job"
    name_prop <- "job_name"
  } else if (is_job_sequence(obj)) {
    obj_type <- "job_sequence"
    obj_label <- "job sequence"
    name_prop <- "sequence_name"
  } else {
    cli::cli_abort(
      "Invalid object type for cloning; must be a job or job sequence",
      .internal = TRUE
    )
  }

  has_lock <- ".locked" %in% S7::prop_names(obj)
  if (has_lock) {
    old_lock <- S7::prop(obj, ".locked")
    S7::prop(obj, ".locked") <- FALSE
    on.exit(S7::prop(obj, ".locked") <- old_lock, add = TRUE)
  }

  new_name <- as.character(new_name)
  old_name <- S7::prop(obj, name_prop)
  if (length(old_name) && !length(new_name)) {
    cli::cli_inform(c(
      "!" = "Cloning {obj_label} without specifying a new name.",
      "The cloned {obj_label} will have the same name as the original {obj_label}: {.code {old_name}}"
    ))
  } else if (length(new_name)) {
    S7::prop(obj, name_prop) <- new_name
  }

  metadata <- S7::prop(obj, ".metadata")
  S7::prop(metadata, "created_at") <- Sys.time()
  S7::prop(metadata, "object_id") <- .generate_object_id(obj_type)
  S7::prop(obj, ".metadata") <- metadata

  obj
}