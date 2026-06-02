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
#'    \item{\code{class_pb_compiled}}{
#'     Subclass of \code{class_property_block} for storing compiled information
#'    about the job to pass to submission.
#'    }
#' }
#'
#' There are also \code{is_*} functions for checking the type of job objects.
#'
#' @section Job Properties:
#'
#' (Full description of properties/slots)
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
    language = S7::class_character
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

class_pb_scheduler <- S7::new_class(
  "class_pb_scheduler",
  parent = class_property_block,
  properties = list(
    scheduler_name = S7::class_character
  )
)

class_pb_packages <- S7::new_class(
  "class_pb_packages",
  parent = class_property_block,
  properties = list(
    package_names = S7::class_character,
    installation_path = S7::class_character
  )
)

class_pb_compiled <- S7::new_class(
  "class_pb_compiled",
  parent = class_property_block,
  properties = list(
    env_variables = S7::class_character,
    scheduler_arguments = S7::class_character
  )
)

class_pb_run_settings <- S7::new_class(
  "class_pb_run_settings",
  parent = class_property_block,
  properties = list(
    print_session_info = S7::new_property(
      class = S7::class_logical,
      default = FALSE
    ),
    print_environment = S7::new_property(
      class = S7::class_logical,
      default = FALSE
    )
  )
)

class_pb_settings <- S7::new_class(
  "class_pb_settings",
  parent = class_property_block,
  properties = list(
    run_settings = class_pb_run_settings
  )
)

class_job <- S7::new_class(
  "class_job",
  properties = list(
    job_name = guarded("job_name", S7::class_character),
    scheduler = guarded("scheduler", class_pb_scheduler),
    input = guarded("input", class_pb_input),
    job_directory = guarded("job_directory", S7::class_character),
    resources = guarded("resources", class_pb_resources),
    packages = guarded("packages", class_pb_packages),
    .locked = S7::new_property(
      class = S7::class_logical,
      default = FALSE
    ),
    .compiled = guarded(".compiled", class_pb_compiled),
    .settings = guarded(".settings", class_pb_settings)
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
      .update_call = sys.call(-1)
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

#' @noRd
is_job <- function(x, language = NULL) {
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
