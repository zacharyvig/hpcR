#' Convert a job object to a lighter weight summary object
#' @noRd
.job_to_summary <- function(job) {
  has_lock <- ".locked" %in% S7::prop_names(job)
  if (has_lock) job@.locked <- FALSE
  summary_object <- class_job_summary(
    job_name = job@job_name,
    scheduler_name = job@scheduler@scheduler_name,
    input_value = job@input@input_value,
    input_type = job@input@input_type,
    language = job@input@language,
    job_directory = job@job_directory@path,
    resources = S7::props(job@resources)
  )
  if (has_lock) job@.locked <- TRUE
  return(summary_object)
}

#' Format a job object for printing
#' @noRd
.format_job <- function(job, ...) {
  job_name <- if (length(job@job_name)) job@job_name else "(no name)"
  cli::cli_format_method({
    cli::cli_text("<<{.pkg hpcR} Job Object: '{job_name}'>>")
  })
}

#' Format a job summary object for printing
#' @noRd
.format_job_summary <- function(job_summary, ...) {
  theme <- .get_summary_theme()
  job_type <- switch(
    job_summary@input_type,
    oneliner = "One-liner",
    script = sprintf("%s Job", tools::toTitleCase(job_summary@language))
  )
  if (length(job_summary@input_value)) {
    input_value <- switch(
      job_summary@input_type,
      oneliner = sprintf("{.code %s}", job_summary@input_value),
      script = sprintf("{.file .../%s}", basename(job_summary@input_value))
    )
  } else {
    input_value <- .get_empty_label()
  }
  cli::cli_format_method({
    div <- cli::cli_div(theme = theme)
    cli::cli_text("{.pkg hpcR} {cli::symbol$line} {.strong {job_type}}")
    cli::cli_text("[", input_value, "]")
    ul_properties <- cli::cli_ul(c(
      .format_summary_row("scheduler", job_summary@scheduler_name),
      .format_summary_row(
        "job_directory", job_summary@job_directory, format = "path"
      )
    ))
    cli::cli_li("Resources:")
    ul_resources <- cli::cli_ul(c(
      .format_summary_row("n_nodes", job_summary@resources$n_nodes),
      .format_summary_row("n_cores", job_summary@resources$n_cores),
      .format_summary_row("wall_time", job_summary@resources$wall_time),
      .format_summary_row("total_memory", job_summary@resources$total_memory),
      .format_summary_row(
        "memory_per_core",
        job_summary@resources$memory_per_core
      )
    ))
    cli::cli_end(ul_resources)
    cli::cli_end(ul_properties)
    cli::cli_end(div)
  })
}

#' Internal function to format a summary row with label and value
#' @noRd
.format_summary_row <- function(name, value, format = NULL) {
  empty_label <- .get_empty_label()
  label <- .get_property_label(name)
  value <- if (is.null(value) || length(value) == 0) empty_label else value
  if (!is.null(format) && value != empty_label) {
    value <- sprintf("{.%s %s}", format, value)
  }
  paste0(label, ": ", value)
}

#' Internal function to get human-readable label for a property name
#' @noRd
.get_property_label <- function(property_names) {
  sapply(property_names, function(property_name) {
    switch(
      property_name,
      job_name = "Job name",
      scheduler = "Scheduler",
      job_directory = "Job Directory",
      resources = "Resources",
      n_nodes = "Number of Nodes",
      n_cores = "Number of Cores",
      wall_time = "Wall Time",
      total_memory = "Total Memory",
      memory_per_core = "Memory Per Core",
      packages = "Packages",
      property_name
    )
  })
}

#' Custom theming for job summary
#' @noRd
.get_summary_theme <- function () {
  list(
    "span.empty_value" = list(color = "grey90")
  )
}

#' Custom value for empty properties in summary
#' @noRd
.get_empty_label <- function() {
  structure("<none>", class = "empty_value")
}
