#!/usr/bin/env Rscript --vanilla

on.exit({
  if (as.logical(args$is_r_script_tmp)) {
    try(unlink(args$r_script))
  }
  if (as.logical(args$is_post_subs_r_script_tmp)) {
    try(unlink(args$post_subs_r_script))
  }
})

# load packages
options(conflicts.policy = list(warn = FALSE))
if (!require("R.utils", quietly = TRUE)) {
  install.packages("R.utils", quiet = TRUE)
  suppressPackageStartupMessages(
    library(
      R.utils, quiety = TRUE, verbose = FALSE,
      warn.conflicts = FALSE
      )
    )
}
if(!require("cli", quietly = TRUE)) {
  install.packages("cli", quiet = TRUE)
  suppressPackageStartupMessages(
    library(
      cli, quietly = TRUE, verbose = FALSE,
      warn.conflicts = FALSE
      )
    )
}

# read in command line arguments as list (all come in as character)
args <- R.utils::commandArgs(asValues = TRUE)

cli_alert_info(
  "Job start time: {Sys.time()}\n"
)

#  if (!is.null(self$sqlite_db)) syntax <- c(syntax, glue::glue("fmri.pipeline::update_tracked_job_status('{self$sqlite_db}', Sys.getenv('JOBID'), 'STARTED')"))

if (as.logical(args$print_session_info)) {
  cli_alert_info(
    "Printing session info:"
  )
  print(sessionInfo())
  print(Sys.info())
}

if (as.logical(args$print_environment)) {
  cli_alert_info(
    "Printing environment:"
  )
  print(Sys.getenv())
}

if (!(args$r_packages == "")) {
  options(conflicts.policy = list(warn = TRUE))
  if (!require(pacman, quietly = TRUE)) {
    install.packages('pacman', quiet = TRUE)
    library(pacman, quietly = TRUE)
  }
  r_packages <- unlist(strsplit(args$r_packages, ","))
  cli_alert_info(
    "Loading R packages:"
  )
  pacman::p_load(char = trimws(r_packages))
}

if (!(args$input_rdata_file == "")) {
  if (file.exists(args$input_rdata_file)) {
    cli_alert_info(
      "Loading input RData file:"
    )
    load(args$input_rdata_file)
  } else {
    cli_abort("Cannot load input environment object: {args$input_rdata_file}")
  }
}

tryCatch({
  cli_alert_info(
    "Running R code:"
  )
  source(args$r_script, echo = TRUE, print.eval = TRUE)
  },
  error = function(e) {
    print(as.character(e))
    # if (!is.null(self$sqlite_db)) glue::glue("    fmri.pipeline::update_tracked_job_status('{self$sqlite_db}', Sys.getenv('JOBID'), 'FAILED', cascade = TRUE)")
    cli_abort("R Code failed! Ending batch run.")
    }
  )

if (!(args$output_rdata_file == "")) {
  tryCatch({
    cli_alert_info(
      "Saving output RData file:"
    )
    save.image(file = args$output_rdata_file)
    },
    error = function(e) {
      print(as.character(e))
      # if (!is.null(self$sqlite_db)) glue::glue("    fmri.pipeline::update_tracked_job_status('{self$sqlite_db}', Sys.getenv('JOBID'), 'FAILED', cascade = TRUE)")
      cli_abort("Saving output RData file failed! Ending batch run.")
    }
  )
}



if (as.logical(args$wait_for_subs)) {
  cl <- c('numeric', 'integer', 'character')
  if (exists("SUB_JOB_IDS") && inherits(SUB_JOB_IDS, cl)) {
    # sapply(
    #   SUB_JOB_IDS,
    #   function(id) fmri.pipeline::add_tracked_job_parent(sqlite_db = '{self$sqlite_db}', job_id = id, parent_job_id = Sys.getenv('JOBID'), child_level = 2)
    # )
    success <- hpcR::wait_for_job(
      job_ids = SUB_JOB_IDS, quiet = FALSE,
      repolling_interval = as.numeric(args$repolling_interval),
      max_wait = as.numeric(args$max_wait),
      scheduler= args$scheduler
      )
    if (as.logical(args$all_subs_success)) {
      # if (isFALSE(success)) {{ fmri.pipeline::update_tracked_job_status(self$sqlite_db, Sys.getenv('JOBID'), 'FAILED', cascade = TRUE, exclude = SUB_JOB_IDS); stop() }}
    }
  } else {
    cli_warn(c("Attempt to wait for child jobs failed!",
    "Non-existent or improper {.code SUB_JOB_IDS} variable."))
  }

  # add any post-children R code to be executed (e.g., combining outputs from the child jobs)
  if (!(args$post_subs_r_script == "")) {
    tryCatch({
      cli_alert_info(
        "Running post-subs R code:"
      )
      source(args$post_subs_r_script, echo = TRUE, print.eval = TRUE)
      },
      error = function(e) {
        print(as.character(e))
        # if (!is.null(self$sqlite_db)) glue::glue("    fmri.pipeline::update_tracked_job_status('{self$sqlite_db}', Sys.getenv('JOBID'), 'FAILED', cascade = TRUE)")
        cli_abort(e)
      }
    )
  }
} else if (exists("SUB_JOB_IDS")) {
  cli_warn(
    "Ignoring {.code SUB_JOB_IDS} object since {.code wait_for_subs} is FALSE."
  )
}

cli_alert_info("Job end time: {Sys.time()}\n")

#  if (!is.null(self$sqlite_db)) syntax <- c(syntax, glue::glue("fmri.pipeline::update_tracked_job_status('{self$sqlite_db}', Sys.getenv('JOBID'), 'COMPLETED')"))
