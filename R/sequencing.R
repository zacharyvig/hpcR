#' @title Create job sequences using the \code{\%->\%} operator
#'
#' @description
#' \pkg{hpcR} provides intuitive tools for building both simple and complex
#' sequences of jobs. Sequences are useful when one or more 'downstream' jobs
#' depend on one or more 'upstream' jobs.
#'
#' Job schedulers have built-in functionality for managing job sequencing
#' (dependencies), but setting dependencies requires knowing job ID numbers
#' which are only available after submission. \pkg{hpcR} only requires the user
#' to build a sequence object and handles everything else internally
#' post-submission.
#'
#' \pkg{hpcR} introduces the "arrow operator" (\code{\%->\%}) used to specify
#' the order of the sequence, e.g., \code{job_A \%->\% job_B}. Branching and
#' parallel downstream jobs are also supported via the \code{branch()} function.
#' See the Details section.
#'
#'
#' @details
#' ...
#'
#' @param lhs A job sequence or a job object. This specifies which job or
#' jobs are "upstream" from those in \code{rhs}.
#' @param rhs A job sequence or job object. This specifies which job or jobs are
#' "downstream" from those in \code{lhs}. Note that \code{branch()} can be used
#' to specify multiple downstream jobs and returns a job sequence.
#' @param ... Branching jobs or sequences to be added to a job sequence.
#'
#' @return A job sequence object
#'
#' @examples
#' job_A <- rjob("job_A")
#' job_B <- rjob("job_B")
#' job_C <- rjob("job_C")
#' job_D <- rjob("job_D")
#'
#' # simple linear sequencing
#' sequence_1 <- job_A %->% job_B %->% job_C %->% job_D
#' # or equivalently
#' sequence_2 <- job_sequence("my_sequence") %->%
#'  job_A %->%
#'  job_B %->%
#'  job_C %->%
#'  job_D
#'
#' # creating branches using `branch()`
#' sequence_3 <- job_sequence("my_branching_sequence") %->%
#'  job_A %->%
#'  branch(
#'    job_B,
#'    job_C %->% job_D
#'  )
#' # this sequence starts with A, then branches into parallel sequences,
#' # B and C -> D
#'
#' @name job_sequences
#' @docType methods
#' @include job_classes.R
#' @export
`%->%` <- function(lhs, rhs) {
  lhs_label <- rlang::as_label(rlang::enexpr(lhs))
  rhs_label <- rlang::as_label(rlang::enexpr(rhs))
  .arrow(
    lhs = lhs,
    rhs = rhs,
    lhs_label = lhs_label,
    rhs_label = rhs_label
  )
}

# generic for the arrow operator
.arrow <- S7::new_generic(".arrow", c("lhs", "rhs"))


S7::method(.arrow, list(class_job_sequence, class_job)) <- function(
  lhs, rhs, lhs_label = NULL, rhs_label = NULL
) {
  .add_job_to_sequence(lhs, rhs, label = rhs_label)
}

S7::method(.arrow, list(class_job, class_job)) <- function(
  lhs, rhs, lhs_label = NULL, rhs_label = NULL
) {
  lhs_object_id <- lhs@.metadata@object_id
  lhs_label <- .clean_node_label(lhs_label)
  # create a new job sequence with the lhs job as the first node
  sequence <- class_job_sequence(
    node_objects = stats::setNames(list(lhs), lhs_object_id),
    node_labels = stats::setNames(lhs_label, lhs_object_id),
    start_nodes = lhs_object_id,
    end_nodes = lhs_object_id
  )
  # add the rhs job to the sequence
  out <- .add_job_to_sequence(sequence, rhs, label = rhs_label)
  out
}

S7::method(.arrow, list(class_job_sequence, class_job_sequence)) <- function(
  lhs, rhs, lhs_label = NULL, rhs_label = NULL
) {
  .combine_job_sequences(lhs, rhs)
}

S7::method(.arrow, list(class_job, class_job_sequence)) <- function(
  lhs, rhs, lhs_label = NULL, rhs_label = NULL
) {
  .combine_job_sequences(
    .as_job_sequence(lhs, label = lhs_label),
    rhs
  )
}

#' @rdname job_sequences
#' @export
branch <- function(...) {
  quos <- rlang::enquos(...)
  if (!length(quos)) {
    cli::cli_abort("No jobs or sequences provided to {.fn branch}.")
  }

  inputs <- lapply(quos, rlang::eval_tidy)
  labels <- vapply(quos, rlang::as_label, character(1))
  sequences <- Map(.as_job_sequence, inputs, labels)

  # make sure all job ids are unique across branches
  do.call(.check_duplicate_ids, c(sequences, .call = rlang::caller_env(n = 0)))

  out <- class_job_sequence()

  # add all sequences to the output sequence without connecting them
  for (seq in sequences) {
    out@node_objects <- c(out@node_objects, seq@node_objects)
    out@node_labels <- c(out@node_labels, seq@node_labels)
    out@edges <- rbind(out@edges, seq@edges)
  }

  out@start_nodes <- unlist(
    lapply(sequences, function(x) x@start_nodes),
    use.names = FALSE
  )

  out@end_nodes <- unlist(
    lapply(sequences, function(x) x@end_nodes),
    use.names = FALSE
  )

  out
}

#' internal function to convert jobs to sequences for branching
#' this creates a global logic that every branch is a sequence
#' @noRd
.as_job_sequence <- function(x, label = NULL) {
  if (is_job_sequence(x)) {
    return(x)
  }

  if (is_job(x)) {
    object_id <- x@.metadata@object_id
    label <- .clean_node_label(label)
    return(class_job_sequence(
      node_objects = stats::setNames(list(x), object_id),
      node_labels = stats::setNames(label, object_id),
      start_nodes = object_id,
      end_nodes = object_id
    ))
  }

  cli::cli_abort(
    "Invalid input to {.fn branch}; must be a job or job sequence."
  )
}

#' Internal function to get a clean node label
#' @noRd
.clean_node_label <- function(label) {
  if (!checkmate::test_string(label, min.chars = 1, null.ok = FALSE)) {
    return("")
  }
  label
}

#' Internal function to add a new job to a sequence
#' @noRd
.add_job_to_sequence <- function(lhs, rhs, label = NULL) {
  # validate lhs/rhs types
  if (!is_job(rhs)) {
    cli::cli_abort("Invalid right-hand side; must be a job object.")
  }
  if (!is_job_sequence(lhs)) {
    cli::cli_abort("Invalid left-hand side; must be a job sequence object.")
  }

  rhs_seq <- .as_job_sequence(rhs, label = label)

  .check_duplicate_ids(lhs, rhs_seq)

  new_object_id <- names(rhs_seq@node_objects)

  has_lock <- ".locked" %in% S7::prop_names(lhs)
  if (has_lock) {
    old_lock <- lhs@.locked
    lhs@.locked <- FALSE
    on.exit({
      lhs@.locked <- old_lock
    }, add = TRUE)
  }

  lhs@node_objects <- c(lhs@node_objects, rhs_seq@node_objects)
  lhs@node_labels <- c(lhs@node_labels, rhs_seq@node_labels)

  lhs <- .add_node_to_end(sequence = lhs, new_object_id = new_object_id)
  if (has_lock) lhs@.locked <- TRUE
  lhs
}

#' Internal function to update a job sequence's edges
#' and end nodes with a new job
#' @noRd
.add_node_to_end <- function(sequence, new_object_id) {
  if (length(sequence@end_nodes)) {
    sequence@edges <- rbind(
      sequence@edges,
      data.frame(
        from = sequence@end_nodes,
        to = new_object_id,
        stringsAsFactors = FALSE
      )
    )
  } else if (!length(sequence@start_nodes)) {
    sequence@start_nodes <- new_object_id
  }
  sequence@end_nodes <- new_object_id
  sequence
}

#' Internal function to combine two job sequences
#' @noRd
.combine_job_sequences <- function(lhs, rhs) {
  if (!is_job_sequence(lhs)) {
    cli::cli_abort("Invalid left-hand side; must be a job sequence object.")
  }

  if (!is_job_sequence(rhs)) {
    cli::cli_abort("Invalid right-hand side; must be a job sequence object.")
  }

  lhs_node_ids <- names(lhs@node_objects)
  rhs_node_ids <- names(rhs@node_objects)

  # Empty cases -- pretend the empty one never existed
  if (!length(lhs_node_ids)) {
    return(rhs)
  }
  if (!length(rhs_node_ids)) {
    return(lhs)
  }

  # check for duplicate job ids
  .check_duplicate_ids(lhs, rhs)

  has_lock <- ".locked" %in% S7::prop_names(lhs)
  if (has_lock) {
    old_lock <- lhs@.locked
    lhs@.locked <- FALSE
    on.exit({
      lhs@.locked <- old_lock
    }, add = TRUE)
  }

  # Merge nodes (objects and labels)
  lhs@node_objects <- c(lhs@node_objects, rhs@node_objects)
  lhs@node_labels <- c(lhs@node_labels, rhs@node_labels)

  # Merge existing edges without changing RHS topology.
  lhs@edges <- rbind(lhs@edges, rhs@edges)

  # Connect current lhs ends to rhs starts.
  # We use the full grid since specific branching should use other
  #  machinery, mainly branch()
  join_edges <- expand.grid(
    from = lhs@end_nodes,
    to = rhs@start_nodes,
    stringsAsFactors = FALSE
  )

  lhs@edges <- rbind(lhs@edges, join_edges)

  # Starts remain lhs starts; ends become rhs ends.
  lhs@end_nodes <- rhs@end_nodes
  if (has_lock) lhs@.locked <- TRUE
  lhs
}


#' internal function guard against duplicate job ids in a series of sequences
#' @param ... A list of job sequence objects
#' @param call The call to use for error messages
#' @noRd
.check_duplicate_ids <- function(..., .call = NULL) {
  sequences <- list(...)

  if (length(sequences) < 2) {
    return(invisible(TRUE))
  }

  # get all object ids
  object_ids <- unlist(
    lapply(sequences, function(x) names(x@node_objects)),
    use.names = FALSE
  )

  # get all labels for the object ids (thus they're the same length)
  labels <- unlist(
    lapply(sequences, function(x) unname(x@node_labels[names(x@node_objects)])),
    use.names = FALSE
  )

  # find duplicated ids
  duplicate_ids <- unique(object_ids[duplicated(object_ids)])
  if (!length(duplicate_ids)) {
    return(invisible(TRUE))
  }

  # which labels are at the same index as the duplicate ids?
  duplicate <- object_ids %in% duplicate_ids
  duplicate_labels <- labels[duplicate]

  # for missing labels just throw a generic error message
  missing_labels <- is.na(duplicate_labels) | !nzchar(duplicate_labels)
  if (any(missing_labels)) {
    cli::cli_abort(
      c(
        "Some jobs are duplicated in this sequence.",
        "x" = "Sequences must have unique jobs." 
      ),
      call = .call
    )
  }

  # otherwise, list the pairs of labels that are duplicated
  pair_lines <- unlist(
    lapply(duplicate_ids, function(id) {
      id_labels <- labels[object_ids == id]
      pairs <- utils::combn(id_labels, 2, simplify = FALSE)
      vapply(
        pairs,
        function(pair) {
          paste0(
            cli::format_inline("{.code {pair[[1]]}}"),
            " and ",
            cli::format_inline("{.code {pair[[2]]}}"),
            " are the same job."
          )
        },
        character(1)
      )
    }),
    use.names = FALSE
  )

  names(pair_lines) <- rep("x", length(pair_lines))

  cli::cli_abort(
    # TODO: FIX PLURALIZATION HERE
    c(
      "{cli::qty(length(pair_lines))}Duplicated job{?s} in this sequence.",
      pair_lines,
      "i" = "Sequences must have unique jobs."
    ),
    call = .call
  )
}