#' @title Working with hpcR objects
#'
#' @description \pkg{hpcR} provides a set of methods for working with job and
#' job sequence objects. Methods include \code{submit()} for submitting jobs,
#' \code{clone()} for cloning, and \code{summary()} for summarizing job objects.
#'
#' @section Methods:
#' \describe{
#'   \item{\link{submit}}{Submit a job or job sequence.}
#'   \item{\link{clone}}{Clone a job or job sequence.}
#'   \item{\link{summary}}{Create a summary of a job.}
#'   \item{\link{print}}{Print an hpcR object.}
#' }
#'
#' @seealso
#'   \code{\link{submit}},
#'   \code{\link{clone}},
#'   \code{\link{summary}},
#'   \code{\link{print}}
#'
#' @name hpcR_methods
NULL

#' @title Submit a job or job sequence to an HPC scheduler
#' @param x An \link{hpcR} job or job sequence object.
#' @examples
#' tmp_script <- tempfile(pattern = "my_script", fileext = ".R")
#' writeLines("print('ok')", tmp_script)
#'
#' my_job <- rjob("my_job") +
#'   script(tmp_script) +
#'   scheduler("slurm") +
#'   resources(
#'     n_nodes = 2,
#'     n_cores = 4,
#'     wall_time = "01:00:00"
#'   )
#'
#' \dontrun{
#' submit(my_job)
#' }
#' @name submit
#' @docType methods
#' @export
submit <- S7::new_generic("submit", "x", function(x) {
  S7::S7_dispatch()
})

S7::method(submit, class_job) <- function(x) {
  defaulted_job <- .hydrate_defaults(x)
  # validate job for submission
  .validate_job(defaulted_job, stage = "submit", .call = rlang::caller_env())
  # ensure declared packages are available before compiling the job
  # TODO generalize this step to "prepare" including other needed steps
  .prepare_job_packages(defaulted_job)
  # compile job for submission
  compiled_job <- .compile_job(defaulted_job)
  # submit job
  ui_job_name <- if (length(compiled_job@job_name)) {
    compiled_job@job_name
  } else {
    "<no name>"
  }
  cli::cli_progress_step(
    "Submitting job '{ui_job_name}'"
  )
  out <- .submit_job(
    input = compiled_job@.compiled@submission_input,
    input_type = compiled_job@.compiled@submission_input_type,
    scheduler_name = compiled_job@scheduler@scheduler_name,
    upstream = compiled_job@.compiled@upstream,
    env_variables = compiled_job@.compiled@env_variables,
    control = compiled_job@.compiled@submit_control,
    .call = rlang::caller_env()
  )
  if (is.null(out)) {
    # TODO: consider situations when out is NULL and return message
    invisible(NULL)
  } else {
    cli::cli_progress_step(
      "Job submitted successfully with ID: {.code {out}}"
    )
    invisible(out)
  }
}

S7::method(submit, class_job_sequence) <- function(x) {
  .call <- rlang::current_call()

  graph <- x@sequence_graph
  node_ids <- names(graph@node_objects)

  if (!length(node_ids)) {
    cli::cli_abort(
      "Cannot submit an empty job sequence.",
      call = .call
    )
  }

  # validate sequence for submission
  .validate_sequence_graph(graph = graph, .call = .call)

  # keep track of submission ids (as opposed to object ids)
  submitted_ids <- character(0)

  # nodes that have not yet been submitted.
  pending <- node_ids

  # ---------------------------------------------------------------------------
  # Submit until every node has been submitted.
  #
  # This loop does not wait for jobs to finish. It only waits for the
  # submission process to produce the scheduler ID needed by downstream jobs.
  # The scheduler itself handles execution-time dependencies.
  # ---------------------------------------------------------------------------

  while (length(pending)) {

    ready <- vapply(
      pending,
      function(node_id) {
        upstream_nodes <- graph@edges$from[graph@edges$to == node_id]
        upstream_nodes <- unique(upstream_nodes)
        # A job is ready to submit when every graph-defined upstream job has
        # already been submitted and therefore has a scheduler ID.
        all(upstream_nodes %in% names(submitted_ids))
      },
      logical(1)
    )

    # ready nodes are ones whose upstream jobs have submission ids
    ready_nodes <- pending[ready]

    # This should not happen for a valid DAG. It is a defensive check in case
    # the graph was modified after validation or the internal state became
    # inconsistent.
    if (!length(ready_nodes)) {
      cli::cli_abort(
        c(
          "Unable to determine the next job{?s} to submit.",
          "x" = "No pending job has all required upstream scheduler IDs.",
          "i" = "The sequence may contain a cycle or invalid dependency edges."
        ),
        call = .call
      )
    }

    # submit "ready" nodes
    for (node_id in ready_nodes) {
      job <- graph@node_objects[[node_id]]

      # upstream nodes for this job as object ids
      upstream_nodes <- graph@edges$from[graph@edges$to == node_id]
      upstream_nodes <- unique(upstream_nodes)

      # object ids -> submission ids conversion
      graph_dependency_ids <- unname(
        submitted_ids[upstream_nodes]
      )

      # other user-supplied dependency ids
      external_dependency_ids <- job@scheduler@sequencing@upstream_ids
      dependency_ids <- unique(
        c(
          external_dependency_ids,
          graph_dependency_ids
        )
      )

      # add scheduler dependency IDs to the job before submission.
      if (length(dependency_ids)) {
        update <- list(
          scheduler = list(
            sequencing = list(
              upstream_ids = dependency_ids
            )
          )
        )
        job <- .update_job(
          e1 = job,
          e2 = class_job_update(updates = update),
          warn_overwrite = FALSE,
          overwrite = TRUE,
          skip_validation = TRUE,
          .call = .call
        )
      }

      # submit the job
      scheduler_id <- submit(job)

      if (is.null(scheduler_id) || !length(scheduler_id)) {
        # failed submissions/NULL ids should be handled by invoke_system,
        # but this guard is here just in case
        cli::cli_abort(
          c(
            "Submission of job {.code {node_id}} returned no scheduler ID.",
            "x" = "Downstream jobs cannot be submitted without this ID."
          ),
          call = .call
        )
      }

      scheduler_id <- as.character(scheduler_id)
      if (length(scheduler_id) != 1L || is.na(scheduler_id)) {
        cli::cli_abort(
          c(
            "Submission of job {.code {node_id}} returned an invalid scheduler ID.",
            "x" = "Expected one non-missing character value."
          ),
          call = .call
        )
      }

      # add scheduler id to running list
      submitted_ids[[node_id]] <- scheduler_id
    }

    # remove submitted jobs from pending list
    pending <- pending[!pending %in% ready_nodes]
  }

  invisible(submitted_ids)

}

S7::method(submit, S7::class_any) <- function(x) {
  cli::cli_abort(
    "Invalid object type for submission; must be a job or job sequence.",
    call = rlang::caller_env()
  )
}

#' @title Clone a job or job sequence
#' @param x An \link{hpcR} job or job sequence object.
#' @param new_name Optional. A new name for the cloned job or job sequence.
#' @name clone
#' @docType methods
#' @export
clone <- S7::new_generic("clone", "x", function(x, new_name = NULL) {
  S7::S7_dispatch()
})

S7::method(clone, class_job) <- function(x, new_name = NULL) {
  .clone_object(x, new_name = new_name)
}

S7::method(clone, class_job_sequence) <- function(x, new_name = NULL) {
  .clone_object(x, new_name = new_name)
}

S7::method(clone, S7::class_any) <- function(x, new_name = NULL) {
  cli::cli_abort(
    "Invalid object type for cloning; must be a job or job sequence.",
    call = rlang::caller_env()
  )
}

#' @title Summarize a job object
#' @param object A job object to be summarized.
#' @param ... Currently not used.
#' @examples
#' tmp_script <- tempfile(pattern = "my_script", fileext = ".R")
#' writeLines("print('ok')", tmp_script)
#'
#' my_job <- rjob("my_job") +
#'   script(tmp_script) +
#'   scheduler("slurm") +
#'   resources(
#'     n_nodes = 2,
#'     n_cores = 4,
#'     wall_time = "01:00:00"
#'   )
#'
#' summary(my_job)
#'
#' @name summary
#' @aliases summary.class_job
#' @method summary class_job
#' @docType methods
#' @export
S7::method(summary, class_job) <- function(object, ...) {
  .job_to_summary(object)
}

#' @title Format and print hpcR objects
#'
#' @param x An hpcR object to be formatted or printed.
#' @param ... Currently not used.
#'
#' @name format_print
#' @aliases format
#' @aliases print
#' @aliases format.class_job
#' @aliases print.class_job
#' @aliases format.class_job_summary
#' @aliases print.class_job_summary
#' @docType methods
#' @keywords internal
NULL


S7::method(format, class_job) <- function(x, ...) {
  .format_job(x, ...)
}

S7::method(print, class_job) <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}

S7::method(format, class_job_summary) <- function(x, ...) {
  .format_job_summary(x, ...)
}

S7::method(print, class_job_summary) <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}
