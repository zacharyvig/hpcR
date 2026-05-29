#' @title Classes for constructing jobs
#' @description \pkg{hpcR} internally uses a suite of S7 classes
#' to construct and manage job objects. These allow for strict
#' validation and method dispatch. Technically a user could interact
#' directly with these classes, but they're optimized for the user-friendly
#' \code{+} operator approach to building jobs.
#'
#' @section Top-Level S7 Classes:
#'
#' Job objects store information about jobs intended to be submitted
#' to high-performance clusters (HPCs). The following are top-level
#' \link[S7]{S7_class} objects used in constructing, updating, and storing
#' properties of job objects.
#' \itemize{
#'    \item{\code{\link{class_job}}:}{
#'      Top-level class for all jobs. Basis of the "job object" that
#'      stores all information needed to submit a job.
#'    }
#'    \item{\code{\link{class_job_update}}:}{
#'      Top-level class for all updates. These are intermediate
#'      objects used to update job objects when using the \code{+} operator.
#'    }
#'    \item{\code{\link{class_property_block}}:}{
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
#' \itemize{
#'    \item{\code{\link{class_pb_resources}}}{
#'      Subclass of \code{class_property_block} for storing
#'      resource-related job properties, such as number of nodes,
#'      number of cores, wall time, and memory.
#'    }
#'    \item{\code{\link{class_pb_script}}}{
#'      Subclass of \code{class_property_block} for storing
#'      script-related job properties, such as script path,
#'      extension, and language.
#'    }
#' }
#'
#' @section Rationale:
#' The central idea of job construction is that job objects
#' are incrementally updated such that \code{class_job <- class_job +
#' class_job_update}. This keeps the internals lightweight (e.g., as opposed to
#' creating two job objects and combining them). Properties can be stored
#' directly in the job object, e.g., \code{class_job@property}, or in the more
#' formal \code{class_property_block} class. Property blocks are themselves
#' collections of properties, e.g., \code{class_property_block@property}, but
#' they make it easier to do validation on properties that interact (e.g.,
#' mutually exclusive properties). \code{PropertyBlock} is an abstract class,
#' thus it's purpose is for specific sub-classes to inherit it.
#'
#' @name hpcR_classes
#' @keywords internal
NULL

#' @title  Class for storing related groups of job properties
#' @description The \code{PropertyBlock} class is an abstract
#' class for all property blocks in \pkg{hpcR}. Property
#' blocks are collections of related job properties, such as
#' resource specifications. These classes are intended to help
#' with quicker/easier validation.
#' @name class_property_block
#' @keywords internal
class_property_block <- S7::new_class("class_property_block", abstract = TRUE)

#' @rdname class_property_block
#' @keywords internal
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

#' @rdname class_property_block
#' @keywords internal
class_pb_script <- S7::new_class(
  "class_pb_script",
  parent = class_property_block,
  properties = list(
    script_path = S7::class_character,
    extension = S7::class_character,
    language = S7::class_character
  )
)

#' @rdname class_property_block
#' @keywords internal
class_pb_scheduler <- S7::new_class(
  "class_pb_scheduler",
  parent = class_property_block,
  properties = list(
    scheduler_name = S7::class_character
  )
)

#' @title Top-level job object class
#' @description The basis of job objects in \pkg{hpcR} is the \code{class_job}
#' class. This class stores all information needed to submit a job
#' to a high-performance cluster (HPC). Specific job types, such as
#' R jobs, are implemented as sub-classes of \code{class_job} and help with
#' specific property storage, job validation, and method dispatch.
#' @details Job objects are meant to be mostly for internal use, thus they
#' are intentionally difficult to edit using the usual S7 approach
#' (\code{S7_class@property <- value}). Instead, job objects have a
#' \code{.locked} property which, when \code{TRUE}, prevents editing. This
#' property is handled by the \code{+} operator approach to job building as
#' well as the \link{edit} approach to job editing. This ensures job objects
#' can be strictly validating and protects them against the vast majority
#' of possible (user) errors. See \link{build_job} for details about the
#' properties of a job object.
#' @name class_job
#' @keywords internal
class_job <- S7::new_class(
  "class_job",
  properties = list(
    job_name = guarded("job_name", S7::class_character),
    scheduler = guarded("scheduler", class_pb_scheduler),
    script = guarded("script", class_pb_script),
    job_directory = guarded("job_directory", S7::class_character),
    resources = guarded("resources", class_pb_resources),
    .locked = S7::new_property(
      class = S7::class_logical,
      default = FALSE
    )
  )
)

#' @title Class for storing job updates
#' @description The \code{class_job_update} class is the basis of all update
#' objects in \pkg{hpcR}. These objects are used to update job
#' objects when using the \code{+} operator. Job updates are lightweight
#' and usually store a few properties, or a single \link{PropertyBlock}.
#' @name class_job_update

#' @keywords internal
class_job_update <- S7::new_class(
  "class_job_update",
  properties = list(
    updates = named_list
  )
)

#' @rdname class_job
#' @keywords internal
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

#' @rdname class_job_update
#' @keywords internal
is_job_update <- function(x) {
  x <- c(x)
  vapply(
    seq_along(x),
    function(i) S7::S7_inherits(x[[i]], class = class_job_update),
    logical(1)
  )
}

#' @rdname class_property_block
#' @keywords internal
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
