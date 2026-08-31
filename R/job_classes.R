#' @title Classes for constructing jobs
#' @description \pkg{hpcR} internally uses a suite of S7 classes
#' to construct and manage job objects. These allow for strict
#' validation and method dispatch. Technically a user could interact
#' directly with these classes, but they're optimized for the user-friendly
#' \code{+} operator approach to building jobs.
#'
#' The basis of job objects in \pkg{hpcR} is the \code{class_job}
#' class. This class stores all information needed to submit a job
#' to a high-performance cluster (HPC).
#'
#' The \code{class_job_update} class is the basis of all update
#' objects in \pkg{hpcR}. These objects are used to update job
#' objects when using the \code{+} operator. Job updates are lightweight
#' and usually store a few properties, or a single \code{class_property_block}.
#'
#' The \code{class_property_block} class is an abstract
#' class for all property blocks in \pkg{hpcR}. Property
#' blocks are collections of related job properties, such as
#' resource specifications. These classes are intended to help
#' with quicker/easier validation.
#'
#' @details Job objects are meant to be mostly for internal use, thus they
#' are intentionally difficult to edit using the usual S7 approach
#' (\code{S7_class@property <- value}). Instead, job objects have a
#' \code{.locked} property which, when \code{TRUE}, prevents editing. This
#' property is handled by the \code{+} operator approach to job building.
#' This ensures job objects can be strictly validated and protects them against
#' the vast majority of possible (user) errors.
#'
#' @section Top-Level S7 Classes:
#'
#' Job objects store information about jobs intended to be submitted
#' to high-performance clusters (HPCs). The following are top-level
#' \link[S7]{S7_class} objects used in constructing, updating, and storing
#' properties of job objects.
#' \describe{
#'    \item{\code{class_job}}{
#'      Top-level class for all jobs. Basis of the "job object" that
#'      stores all information needed to submit a job.
#'    }
#'    \item{\code{class_job_update}}{
#'      Top-level class for all updates. These are intermediate
#'      objects used to update job objects when using the \code{+} operator.
#'    }
#'    \item{\code{class_property_block}}{
#'      Top-level (abstract) class for all property blocks, which are
#'      collections of related job properties. These are useful for
#'      validation and method dispatch.
#'    }
#'    \item{\code{class_job_summary}}{
#'      Top-level class for job summaries. These are used to provide
#'      a concise overview of a job's properties.
#'    }
#' }
#' @section S7 Subclasses:
#'
#' For specific job types and property blocks, \pkg{hpcR} uses
#' a collection of S7 subclasses. These include:
#'
#' \describe{
#'    \item{\code{class_pb_input}}{
#'      Subclass of \code{class_property_block} for storing
#'      the main information (script or oneliner) that characterizes
#'      the job. For script input, it also stores information about
#'      the script, such as its extension and language.
#'    }
#'    \item{\code{class_pb_job_directory}}{
#'      Subclass of \code{class_property_block} for storing
#'      job directory information, including the path and whether
#'      to create the directory if it doesn't exist.
#'    }
#'    \item{\code{class_pb_resources}}{
#'      Subclass of \code{class_property_block} for storing
#'      resource-related job properties, such as number of nodes,
#'      number of cores, wall time, and memory.
#'    }
#'    \item{\code{class_pb_scheduler}}{
#'      Subclass of \code{class_property_block} for storing
#'      scheduler-specific job properties, providing easy
#'      validation for different schedulers.
#'    }
#'    \item{\code{class_pb_packages}}{
#'      Subclass of \code{class_property_block} for storing
#'      packages needed for the job to run.
#'    }
#'    \item{\code{class_pb_libraries}}{
#'      Subclass of \code{class_property_block} for storing R library
#'      environment settings for the job.
#'    }
#'    \item{\code{class_pb_compiled}}{
#'      Subclass of \code{class_property_block} for storing compiled information
#'      about the job to pass to submission.
#'    }
#'    \item{\code{class_pb_run_settings}}{
#'      Subclass of \code{class_property_block} for storing
#'      run-time settings for the job.
#'    }
#' }
#'
#' There are also \code{is_*} functions for checking the type of job objects.
#'
#' @section Rationale:
#'
#' The central idea of job construction is that job objects
#' are incrementally updated such that \code{class_job <- class_job +
#' class_job_update}. This keeps the internals lightweight (e.g., as opposed to
#' creating two job objects and combining them). Properties can be stored
#' directly in the job object, e.g., \code{class_job@property}, or in the more
#' formal \code{class_property_block} class. Property blocks are themselves
#' collections of properties, e.g., \code{class_property_block@property}, but
#' they make it easier to do validation on properties that interact (e.g.,
#' mutually exclusive properties). \code{class_property_block} is an abstract
#' class, thus it's purpose is for specific sub-classes to inherit it.
#'
#' @name hpcR_classes
#' @include job_properties.R
#' @import S7
#' @docType class
NULL

class_property_block <- S7::new_class("class_property_block", abstract = TRUE)

class_pb_input <- S7::new_class(
  "class_pb_input",
  parent = class_property_block,
  properties = list(
    input_type = S7::class_character,
    input_value = S7::class_character,
    extension = S7::class_character,
    language = S7::class_character,
    code_quo = S7::class_any
  )
)

class_pb_job_directory <- S7::new_class(
  "class_pb_job_directory",
  parent = class_property_block,
  properties = list(
    path = S7::class_character,
    create = S7::class_logical
  )
)

class_pb_resources <- S7::new_class(
  "class_pb_resources",
  parent = class_property_block,
  properties = list(
    n_nodes = S7::class_character,
    n_cores = S7::class_character,
    wall_time = S7::class_character,
    total_memory = S7::class_character,
    memory_per_core = S7::class_character
  )
)

class_pb_sequencing <- S7::new_class(
  "class_pb_sequencing",
  parent = class_property_block,
  properties = list(
    upstream_names = S7::class_character,
    upstream_ids = S7::class_character
  ),
  constructor = function(
    upstream_names = character(0),
    upstream_ids = character(0)
  ) {
    S7::new_object(
      S7::S7_object(),
      upstream_names = unique(upstream_names),
      upstream_ids = unique(upstream_ids)
    )
  }
)

class_pb_scheduler <- S7::new_class(
  "class_pb_scheduler",
  parent = class_property_block,
  properties = list(
    scheduler_name = S7::class_character,
    sequencing = class_pb_sequencing
  )
)


class_pb_packages <- S7::new_class(
  "class_pb_packages",
  parent = class_property_block,
  properties = list(
    package_names = S7::class_character,
    install = S7::class_character,
    install_library = S7::class_character
  )
)

class_pb_libraries <- S7::new_class(
  "class_pb_libraries",
  parent = class_property_block,
  properties = list(
    job = S7::class_character,
    user = S7::class_character,
    site = S7::class_character
  )
)

class_pb_compiled <- S7::new_class(
  "class_pb_compiled",
  parent = class_property_block,
  properties = list(
    submission_input = S7::class_character,
    submission_input_type = S7::class_character,
    upstream = S7::class_character,
    env_variables = S7::class_character,
    submit_control = S7::class_list
  )
)

class_pb_run_settings <- S7::new_class(
  "class_pb_run_settings",
  parent = class_property_block,
  properties = list(
    print_session_info = S7::class_logical,
    print_environment = S7::class_logical
  )
)

class_pb_metadata <- S7::new_class(
  "class_pb_metadata",
  parent = class_property_block,
  properties = list(
    created_at = S7::class_POSIXct,
    object_id = S7::class_character
  ),
  constructor = function(
    type = c("job", "job_sequence"),
    created_at = Sys.time(),
    object_id = .generate_object_id(type)
  ) {
    S7::new_object(
      S7::S7_object(),
      created_at = created_at,
      object_id = object_id
    )
  }
)

class_job <- S7::new_class(
  "class_job",
  properties = list(
    job_name = guarded("job_name", S7::class_character),
    scheduler = guarded("scheduler", class_pb_scheduler),
    input = guarded("input", class_pb_input),
    job_directory = guarded("job_directory", class_pb_job_directory),
    resources = guarded("resources", class_pb_resources),
    packages = guarded("packages", class_pb_packages),
    libraries = guarded("libraries", class_pb_libraries),
    .locked = S7::new_property(
      class = S7::class_logical,
      default = FALSE
    ),
    .defaulted = S7::new_property(
      class = S7::class_logical,
      default = FALSE
    ),
    .compiled = guarded(".compiled", class_pb_compiled),
    .run_settings = guarded(".run_settings", class_pb_run_settings),
    .metadata = guarded(".metadata", class_pb_metadata)
  )
)

class_job_update <- S7::new_class(
  "class_job_update",
  properties = list(
    updates = named_list,
    .update_call = S7::class_any
  ),
  # this automatically gets the right error call for sugar functions
  constructor = function(updates) {
    S7::new_object(
      S7::S7_object(), updates = updates,
      .update_call = rlang::caller_call()
    )
  }
)

class_job_summary <- S7::new_class(
  "class_job_summary",
  properties = list(
    job_name = guarded("job_name", S7::class_character),
    scheduler_name = guarded("scheduler_name", S7::class_character),
    input_value = guarded("input_value", S7::class_character),
    input_type = guarded("input_type", S7::class_character),
    language = guarded("language", S7::class_character),
    resources = guarded("resources", S7::class_list),
    job_directory = guarded("job_directory", S7::class_character),
    .locked =  S7::new_property(
      class = S7::class_logical,
      default = FALSE
    )
  )
)

class_pb_sequence_graph <- S7::new_class(
  "class_pb_sequence_graph",
  parent = class_property_block,
  properties = list(
    node_objects = S7::class_list,
    node_labels = S7::class_character,
    edges = S7::class_data.frame,
    start_nodes = S7::class_character,
    end_nodes = S7::class_character
  ),
  constructor = function(
    node_objects = list(),
    node_labels = character(0),
    edges = tibble::tibble(
      from = character(0), to = character(0)
    ),
    start_nodes = character(0),
    end_nodes = character(0)
  ) {
    S7::new_object(
      S7::S7_object(),
      node_objects = node_objects,
      node_labels = node_labels,
      edges = edges,
      start_nodes = start_nodes,
      end_nodes = end_nodes
    )
  }
)

class_job_sequence <- S7::new_class(
  "class_job_sequence",
  properties = list(
    sequence_name = guarded("sequence_name", S7::class_character),
    sequence_graph = guarded("sequence_graph", class_pb_sequence_graph),
    .locked = S7::new_property(
      class = S7::class_logical,
      default = FALSE
    ),
    .metadata = guarded(".metadata", class_pb_metadata)
  )
)

#' @noRd
is_job <- function(x, language = NULL) {
  # this function is vectorized
  x <- c(x)
  vapply(
    seq_along(x),
    function(i) {
      S7::S7_inherits(x[[i]], class = class_job) &&
        (is.null(language) || x[[i]]$language == language)
    },
    logical(1)
  )
}

#' @noRd
is_job_update <- function(x) {
  x <- c(x)
  vapply(
    seq_along(x),
    function(i) S7::S7_inherits(x[[i]], class = class_job_update),
    logical(1)
  )
}

#' @noRd
is_property_block <- function(x, type = NULL) {
  x <- c(x)
  vapply(
    seq_along(x),
    function(i) {
      S7::S7_inherits(x[[i]], class = class_property_block) &&
        (is.null(type) || inherits(x[[i]], type))
    },
    logical(1)
  )
}

#' @noRd
is_job_summary <- function(x) {
  x <- c(x)
  vapply(
    seq_along(x),
    function(i) S7::S7_inherits(x[[i]], class = class_job_summary),
    logical(1)
  )
}

#' @noRd
is_job_sequence <- function(x) {
  x <- c(x)
  vapply(
    seq_along(x),
    function(i) S7::S7_inherits(x[[i]], class = class_job_sequence),
    logical(1)
  )
}