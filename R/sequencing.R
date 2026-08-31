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
#' parallel downstream jobs are supported via the \code{branch()} function.
#' Alternative, specifying dependencies via the \code{sequencing()}  sugar
#' function is supported. See details below.
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
#' @param sequence_name A character string. The name of the job sequence to
#' create.
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
  .call <- rlang::current_call()
  lhs_label <- rlang::as_label(rlang::enquo(lhs))
  rhs_label <- rlang::as_label(rlang::enquo(rhs))
  out <- .arrow(
    lhs = lhs,
    rhs = rhs,
    lhs_label = lhs_label,
    rhs_label = rhs_label,
    .call = .call
  )
  if (is_job_sequence(out) &&
    ".locked" %in% S7::prop_names(out)) {
    out@.locked <- TRUE
  }
  out
}

#' @rdname job_sequences
#' @export
job_sequence <- function(sequence_name = NULL, ...) {
  seq <- .sequence_job_objs(
    sequence_name = sequence_name,
    ...,
    .call = rlang::current_call()
  )
  seq@.locked <- TRUE
  seq
}

# generic for the arrow operator
.arrow <- S7::new_generic(".arrow", c("lhs", "rhs"))

S7::method(.arrow, list(class_job_sequence, class_job)) <- function(
  lhs, rhs, lhs_label = NULL, rhs_label = NULL, .call = rlang::caller_call()
) {
  .inform_ignored_upstream_names(job = rhs, label = rhs_label, .call = .call)
  .add_job_to_sequence(lhs, rhs, label = rhs_label, .call = .call)
}

S7::method(.arrow, list(class_job, class_job)) <- function(
  lhs, rhs, lhs_label = NULL, rhs_label = NULL, .call = rlang::caller_call()
) {
  .inform_ignored_upstream_names(job = lhs, label = lhs_label, .call = .call)
  .inform_ignored_upstream_names(job = rhs, label = rhs_label, .call = .call)
  lhs_object_id <- lhs@.metadata@object_id
  lhs_label <- .clean_node_label(lhs_label)
  # create a new job sequence with the lhs job as the first node
  sequence_graph <- class_pb_sequence_graph(
    node_objects = stats::setNames(list(lhs), lhs_object_id),
    node_labels = stats::setNames(lhs_label, lhs_object_id),
    start_nodes = lhs_object_id,
    end_nodes = lhs_object_id
  )
  sequence <- class_job_sequence(sequence_graph = sequence_graph)
  # add the rhs job to the sequence
  out <- .add_job_to_sequence(sequence, rhs, label = rhs_label, .call = .call)
  out
}

S7::method(.arrow, list(class_job_sequence, class_job_sequence)) <- function(
  lhs, rhs, lhs_label = NULL, rhs_label = NULL, .call = rlang::caller_call()
) {
  .combine_job_sequences(lhs, rhs, .call = .call)
}

S7::method(.arrow, list(class_job, class_job_sequence)) <- function(
  lhs, rhs, lhs_label = NULL, rhs_label = NULL, .call = rlang::caller_call()
) {
  .inform_ignored_upstream_names(job = lhs, label = lhs_label, .call = .call)
  .combine_job_sequences(
    .as_job_sequence(lhs, label = lhs_label, .call = .call),
    rhs = rhs, .call = .call
  )
}

#' @rdname job_sequences
#' @export
branch <- function(...) {
  .call <- rlang::current_call()

  quos <- rlang::enquos(...)
  if (!length(quos)) {
    cli::cli_abort(
      "No jobs or sequences provided to {.fn branch}.",
      call = .call
    )
  }

  inputs <- lapply(quos, rlang::eval_tidy)
  labels <- vapply(quos, rlang::as_label, character(1))
  sequences <- withCallingHandlers(
    purrr::map2(
      inputs,
      labels,
      function (x, label) .as_job_sequence(x, label = label, .call = .call)
    ),
    purrr_error_indexed = function(err) {
      rlang::cnd_signal(err$parent)
    }
  )

  # make sure all job object ids are unique across branches
  .check_duplicate_ids(
    sequences = sequences,
    .call = .call
  )

  sequence_graph <- class_pb_sequence_graph()
  out <- class_job_sequence(sequence_graph = sequence_graph)

  # add all sequences to the output sequence without connecting them
  for (seq in sequences) {
    out@sequence_graph@node_objects <- c(
      out@sequence_graph@node_objects,
      seq@sequence_graph@node_objects
    )
    out@sequence_graph@node_labels <- c(
      out@sequence_graph@node_labels,
      seq@sequence_graph@node_labels
    )
    out@sequence_graph@edges <- rbind(
      out@sequence_graph@edges,
      seq@sequence_graph@edges
    )
  }

  out@sequence_graph@start_nodes <- unlist(
    lapply(sequences, function(x) x@sequence_graph@start_nodes),
    use.names = FALSE
  )

  out@sequence_graph@end_nodes <- unlist(
    lapply(sequences, function(x) x@sequence_graph@end_nodes),
    use.names = FALSE
  )

  .validate_sequence_graph(
    out@sequence_graph,
    .call = .call
  )

  out@.locked <- TRUE

  out

}

#' internal function to convert jobs to sequences for branching
#' this creates a global logic that every branch is a sequence
#' @noRd
.as_job_sequence <- function(x, label = NULL, .call = rlang::current_call()) {
  if (is_job_sequence(x)) {
    return(x)
  }

  if (is_job(x)) {
    object_id <- x@.metadata@object_id
    label <- .clean_node_label(label)
    sequence_graph <- class_pb_sequence_graph(
      node_objects = stats::setNames(list(x), object_id),
      node_labels = stats::setNames(label, object_id),
      start_nodes = object_id,
      end_nodes = object_id
    )
    sequence <- class_job_sequence(sequence_graph = sequence_graph)
    return(sequence)
  }

  cli::cli_abort(
    "Invalid input to {.fn branch}; must be a job or job sequence.",
    call = .call
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
.add_job_to_sequence <- function(
  lhs, rhs, label = NULL, .call = rlang::caller_call()
) {
  # validate lhs/rhs types
  if (!is_job(rhs)) {
    cli::cli_abort(
      "Invalid right-hand side; must be a job object.",
      call = .call
    )
  }
  if (!is_job_sequence(lhs)) {
    cli::cli_abort(
      "Invalid left-hand side; must be a job sequence object.",
      call = .call
    )
  }

  rhs_seq <- .as_job_sequence(rhs, label = label, .call = .call)

  .check_duplicate_ids(sequences = list(lhs, rhs_seq), .call = .call)

  new_object_id <- names(rhs_seq@sequence_graph@node_objects)

  has_lock <- ".locked" %in% S7::prop_names(lhs)
  if (has_lock) {
    old_lock <- lhs@.locked
    lhs@.locked <- FALSE
    on.exit({
      lhs@.locked <- old_lock
    }, add = TRUE)
  }

  lhs@sequence_graph@node_objects <- c(
    lhs@sequence_graph@node_objects,
    rhs_seq@sequence_graph@node_objects
  )
  lhs@sequence_graph@node_labels <- c(
    lhs@sequence_graph@node_labels,
    rhs_seq@sequence_graph@node_labels
  )

  lhs <- .add_node_to_end(sequence = lhs, new_object_id = new_object_id)
  if (has_lock) lhs@.locked <- old_lock
  lhs
}

#' Internal function to update a job sequence's edges
#' and end nodes with a new job
#' @noRd
.add_node_to_end <- function(sequence, new_object_id) {
  if (length(sequence@sequence_graph@end_nodes)) {
    sequence@sequence_graph@edges <- rbind(
      sequence@sequence_graph@edges,
      tibble::tibble(
        from = sequence@sequence_graph@end_nodes,
        to = new_object_id
      )
    )
  } else if (!length(sequence@sequence_graph@start_nodes)) {
    sequence@sequence_graph@start_nodes <- new_object_id
  }
  sequence@sequence_graph@end_nodes <- new_object_id
  sequence
}

#' Internal function to combine two job sequences
#' @noRd
.combine_job_sequences <- function(lhs, rhs, .call = rlang::caller_call()) {
  if (!is_job_sequence(lhs)) {
    cli::cli_abort(
      "Invalid left-hand side; must be a job sequence object.",
      call = .call
    )
  }

  if (!is_job_sequence(rhs)) {
    cli::cli_abort(
      "Invalid right-hand side; must be a job sequence object.",
      call = .call
    )
  }

  lhs_has_lock <- ".locked" %in% S7::prop_names(lhs)
  rhs_has_lock <- ".locked" %in% S7::prop_names(rhs)
  if (lhs_has_lock) {
    old_lhs_lock <- lhs@.locked
    lhs@.locked <- FALSE
    on.exit({
      lhs@.locked <- old_lhs_lock
    }, add = TRUE)
  }
  if (rhs_has_lock) {
    old_rhs_lock <- rhs@.locked
    rhs@.locked <- FALSE
    on.exit({
      rhs@.locked <- old_rhs_lock
    }, add = TRUE)
  }

  # handle sequence names -- if lhs has a name, keep it; otherwise use rhs name
  lhs_name <- lhs@sequence_name
  rhs_name <- rhs@sequence_name
  if (length(lhs_name) > 0) {
    if (length(rhs_name) > 0 && !identical(lhs_name, rhs_name)) {
      cli::cli_warn(
        c(
          "Merging two job sequences with different names.",
          "i" = "Left-hand side sequence name: {.code {lhs_name}}.",
          "i" = "Right-hand side sequence name: {.code {rhs_name}}.",
          "i" = "Using left-hand side sequence name."
        ),
        call = .call
      )
    }
    rhs@sequence_name <- lhs_name
  } else if (length(lhs@sequence_name) == 0 && length(rhs@sequence_name) > 0) {
    lhs@sequence_name <- rhs@sequence_name
  }

  lhs_node_ids <- names(lhs@sequence_graph@node_objects)
  rhs_node_ids <- names(rhs@sequence_graph@node_objects)

  # Empty cases -- pretend the empty one never existed
  if (!length(lhs_node_ids)) {
    if (rhs_has_lock) rhs@.locked <- old_rhs_lock
    return(rhs)
  }
  if (!length(rhs_node_ids)) {
    if (lhs_has_lock) lhs@.locked <- old_lhs_lock
    return(lhs)
  }

  # check for duplicate job object ids
  .check_duplicate_ids(sequences = list(lhs, rhs), .call = .call)

  # Merge nodes (objects and labels)
  lhs@sequence_graph@node_objects <- c(
    lhs@sequence_graph@node_objects,
    rhs@sequence_graph@node_objects
  )
  lhs@sequence_graph@node_labels <- c(
    lhs@sequence_graph@node_labels,
    rhs@sequence_graph@node_labels
  )

  # Merge existing edges without changing RHS topology.
  lhs@sequence_graph@edges <- rbind(
    lhs@sequence_graph@edges,
    rhs@sequence_graph@edges
  )

  # Connect current lhs ends to rhs starts.
  # We use the full grid since specific branching should use other
  #  machinery, mainly branch()
  join_edges <- expand.grid(
    from = lhs@sequence_graph@end_nodes,
    to = rhs@sequence_graph@start_nodes,
    stringsAsFactors = FALSE
  )
  lhs@sequence_graph@edges <- rbind(lhs@sequence_graph@edges, join_edges)

  # Starts remain lhs starts; ends become rhs ends.
  lhs@sequence_graph@end_nodes <- rhs@sequence_graph@end_nodes

  .validate_sequence_graph(lhs@sequence_graph, .call = .call)

  if (lhs_has_lock) lhs@.locked <- old_lhs_lock
  lhs
}


#' internal function guard against duplicate job object ids in >1 sequences
#' @param sequences A list of job sequence objects
#' @param call The call to use for error messages
#' @param .call The call to use for error messages
#' @noRd
.check_duplicate_ids <- function(sequences, .call = NULL) {
  if (length(sequences) < 2L) {
    return(invisible(TRUE))
  }

  object_ids <- unlist(
    lapply(
      sequences,
      function(x) names(x@sequence_graph@node_objects)
    ),
    use.names = FALSE
  )

  labels <- unlist(
    lapply(
      sequences,
      function(x) {
        ids <- names(x@sequence_graph@node_objects)

        unname(x@sequence_graph@node_labels[ids])
      }
    ),
    use.names = FALSE
  )

  .check_duplicate_object_ids(
    object_ids = object_ids,
    labels = labels,
    .call = .call
  )
}

#' Internal function to guard against duplicate object IDs
#'
#' @param object_ids Character vector of object IDs.
#' @param labels Character vector of user-facing labels, same length as
#'   `object_ids`.
#' @param .call The call to use for error messages.
#'
#' @noRd
.check_duplicate_object_ids <- function(object_ids, labels, .call = NULL) {

  duplicate_ids <- unique(object_ids[duplicated(object_ids)])

  if (!length(duplicate_ids)) {
    return(invisible(TRUE))
  }

  duplicate <- object_ids %in% duplicate_ids
  duplicate_labels <- labels[duplicate]

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
    c(
      "{cli::qty(length(pair_lines))}Duplicated job{?s} in this sequence.",
      pair_lines,
      "i" = "Sequences must have unique jobs."
    ),
    call = .call
  )
}

#' Function that parses sequencing() information (if provided) to build
#' a job sequence object
#'
#' @param sequence_name The name of the job sequence to create.
#' @param ... Job objects to include in the sequence.
#' @param .call The call to use for error messages.
#' @noRd
.sequence_job_objs <- function(
  sequence_name = NULL, ..., .call = rlang::caller_call()
) {

  job_quos <- rlang::enquos(...)
  job_seq <- class_job_sequence()

  has_lock <- ".locked" %in% S7::prop_names(job_seq)

  if (has_lock) {
    old_lock <- job_seq@.locked
    job_seq@.locked <- FALSE
    on.exit({
      job_seq@.locked <- old_lock
    }, add = TRUE)
  }

  if (!is.null(sequence_name)) {
    sequence_name <- as.character(sequence_name)
    validate_property(
      name = "sequence_name",
      value = sequence_name,
      use_default_settings = TRUE,
      .call = .call
    )
    job_seq@sequence_name <- sequence_name
  }

  if (!length(job_quos)) {
    if (has_lock) {
      job_seq@.locked <- old_lock
    }
    return(job_seq)
  }

  jobs <- lapply(job_quos, rlang::eval_tidy)
  job_labels <- vapply(job_quos, rlang::as_label, character(1))

  is_job_vec <- vapply(jobs, is_job, logical(1))
  if (!all(is_job_vec)) {
    bad <- job_labels[!is_job_vec]
    cli::cli_abort(
      c(
        "All objects in {.arg ...} must be job objects.",
        "x" = "Invalid object{?s}: {.code {bad}}."
      ),
      call = .call
    )
  }

  lookup <- purrr::list_rbind(
    purrr::map2(jobs, job_labels, function(job, label) {
      has_name <- length(job@job_name) > 0
      tibble::tibble(
        node_id = job@.metadata@object_id,
        job_label = label,
        job_name = if (has_name) job@job_name else NA_character_,
        has_name = has_name,
        job_obj = list(job),
        upstream_names = list(
          job@scheduler@sequencing@upstream_names
        ),
        upstream_ids = list(
          job@scheduler@sequencing@upstream_ids
        )
      )
    })
  )

  # check for duplicate jobs (object IDs) in the sequence
  .check_duplicate_object_ids(
    object_ids = lookup$node_id,
    labels = lookup$job_label,
    .call = .call
  )

  .validate_named_sequence(lookup, .call = .call)

  edges <- tibble::tibble(
    from = character(0),
    to = character(0)
  )

  edge_list <- lapply(seq_len(nrow(lookup)), function(i) {

    ups <- unique(lookup$upstream_names[[i]])

    if (!length(ups)) {
      return(tibble::tibble(
        from = character(0),
        to = character(0)
      ))
    }

    from_ids <- lookup$node_id[
      match(ups, lookup$job_name)
    ]
    to_id <- lookup$node_id[[i]]

    tibble::tibble(
      from = from_ids,
      to = to_id
    )
  })

  edges <- do.call(rbind, edge_list)

  graph <- class_pb_sequence_graph(
    node_objects = stats::setNames(lookup$job_obj, lookup$node_id),
    node_labels = stats::setNames(lookup$job_label, lookup$node_id),
    edges = edges,
    start_nodes = setdiff(lookup$node_id, edges$to),
    end_nodes = setdiff(lookup$node_id, edges$from)
  )

  .validate_sequence_graph(graph, .call = .call)

  job_seq@sequence_graph <- graph

  if (has_lock) {
    job_seq@.locked <- old_lock
  }

  job_seq

}


#' Validate a job sequence graph
#' @noRd
.validate_sequence_graph <- function(graph, .call = NULL) {
  node_ids <- names(graph@node_objects)

  if (!length(node_ids)) {
    return(invisible(TRUE))
  }

  # validate graph edge structure
  required_edge_names <- c("from", "to")

  if (!all(required_edge_names %in% names(graph@edges))) {
    cli::cli_abort(
      "Sequence graph edges must contain {.field from} and {.field to} columns.",
      call = .call,
      .internal = TRUE
    )
  }

  edges <- graph@edges[, required_edge_names, drop = FALSE]

  invalid_from <- setdiff(edges$from, node_ids)
  invalid_to <- setdiff(edges$to, node_ids)

  if (length(invalid_from) || length(invalid_to)) {
    cli::cli_abort(
      "Sequence graph contains edges referring to unknown nodes.",
      call = .call,
      internal = TRUE
    )
  }

  self_edges <- edges$from == edges$to

  if (any(self_edges)) {
    cli::cli_abort(
      c(
        "A job cannot depend on itself.",
        "x" = "The sequence graph contains a self-dependency."
      ),
      call = .call
    )
  }

  # validate the actual graph is acyclic
  if (!.is_dag(node_ids, edges)) {
    cli::cli_abort(
      c(
        "Job sequence contains a cycle.",
        "x" = "Job dependencies must not form any loops."
      ),
      call = .call
    )
  }

  invisible(TRUE)
}


#' Check whether a directed sequence graph is acyclic
#'
#' @param node_ids Character vector of graph node IDs.
#' @param edges Data frame with `from` and `to` character columns.
#' @return `TRUE` if the graph is acyclic, otherwise `FALSE`.
#' @noRd
.is_dag <- function(node_ids, edges) {
  if (!is.character(node_ids)) {
    cli::cli_abort(
      "{.arg node_ids} must be a character vector.",
      .internal = TRUE
    )
  }

  if (!is.data.frame(edges) || !all(c("from", "to") %in% names(edges))) {
    cli::cli_abort(
      "{.arg edges} must be a data frame with {.field from} and {.field to} columns.",
      .internal = TRUE
    )
  }

  if (!nrow(edges)) {
    return(TRUE)
  }

  if (!is.character(edges$from) || !is.character(edges$to)) {
    cli::cli_abort(
      "The {.field from} and {.field to} edge columns must be character vectors.",
      .internal = TRUE
    )
  }

  unknown_nodes <- setdiff(
    unique(c(edges$from, edges$to)),
    node_ids
  )

  if (length(unknown_nodes)) {
    cli::cli_abort(
      "Edges refer to unknown graph nodes: {.val {unknown_nodes}}.",
      .internal = TRUE
    )
  }

  downstream <- split(edges$to, edges$from)

  visit <- function(node, path) {
    path <- c(path, node)
    next_nodes <- downstream[[node]]

    if (!length(next_nodes)) {
      return(TRUE)
    }

    for (next_node in next_nodes) {
      if (next_node %in% path) {
        return(FALSE)
      }

      if (isFALSE(visit(next_node, path))) {
        return(FALSE)
      }
    }

    TRUE
  }

  for (node in node_ids) {
    if (isFALSE(visit(node, character(0)))) {
      return(FALSE)
    }
  }

  TRUE
}

.validate_named_sequence <- function(
  lookup,
  .call = rlang::caller_call()
) {
  all_upstream_names <- unique(
    unlist(lookup$upstream_names, use.names = FALSE)
  )

  if (!length(all_upstream_names)) {
    return(invisible(TRUE))
  }

  named_names <- lookup$job_name[lookup$has_name]

  duplicated_names <- unique(
    named_names[
      duplicated(named_names)
    ]
  )

  if (length(duplicated_names)) {
    cli::cli_abort(
      c(
        "Job names must be unique for job sequences.",
        "x" = "Duplicated job name{?s}: {.code {duplicated_names}}.",
        "i" = "Use unique job names for {.code upstream_names}."
      ),
      call = .call
    )
  }

  # check for upstream name references that don't exist in the sequence
  invalid <- lapply(lookup$upstream_names, function(upstream_names) {
    setdiff(unique(upstream_names), named_names)
  })

  names(invalid) <- lookup$node_id
  invalid <- invalid[lengths(invalid) > 0L]

  if (length(invalid)) {
    n_invalid <- sum(lengths(invalid))

    invalid_lines <- vapply(
      names(invalid),
      function(node_id) {
        job_label <- lookup$job_label[
          match(node_id, lookup$node_id)
        ]

        bad_names <- invalid[[node_id]]

        bad_names <- paste(
          cli::format_inline("{.code {bad_names}}"),
          collapse = ", "
        )

        cli::format_inline(
          "{.code {job_label}} has {cli::qty(length(invalid[[node_id]]))}unknown upstream name{?s}: {bad_names}."
        )
      },
      character(1)
    )

    names(invalid_lines) <- rep("x", length(invalid_lines))

    cli::cli_abort(
      c(
        "{cli::qty(n_invalid)}Invalid upstream name{?s} in job sequence.",
        invalid_lines,
        "i" = "Upstream names must refer to named jobs included in the same sequence."
      ),
      call = .call
    )
  }

  for (i in seq_len(nrow(lookup))) {
    upstream_names <- unique(lookup$upstream_names[[i]])

    if (!length(upstream_names)) {
      next
    }

    from_ids <- lookup$node_id[
      match(upstream_names, lookup$job_name)
    ]

    if (lookup$node_id[[i]] %in% from_ids) {
      cli::cli_abort(
        c(
          "Job {.code {lookup$job_label[[i]]}} cannot depend on itself.",
          "x" = "A job was listed as its own upstream dependency."
        ),
        call = .call
      )
    }
  }

  invisible(TRUE)
}

#' Sequences created with %->% ignore upstream_names
#' @noRd
.inform_ignored_upstream_names <- function(job, label, .call = NULL) {
  upstream_names <- job@scheduler@sequencing@upstream_names

  if (!length(upstream_names)) {
    return(invisible(FALSE))
  }

  label <- .clean_node_label(label)

  cli::cli_inform(
    c(
      "i" = "Ignoring {.code upstream_names} for {.code {label}}.",
      "i" = "Sequences made with {.code %->%} ignore explicit upstream names."
    ),
    call = .call
  )

  invisible(TRUE)
}