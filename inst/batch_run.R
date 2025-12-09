#!/usr/bin/env Rscript --vanilla

# load packages
if (!require("R.utils")) {
  install.packages("R.utils")
  library(R.utils)
}
if(!require("cli")) {
  install.packages("cli")
  library(cli)
}

# read in command line arguments as list
args <- R.utils::commandArgs(asValues = TRUE)

cli_alert_info(
  "Job start time: {Sys.time()}\n"
)

#  if (!is.null(self$sqlite_db)) syntax <- c(syntax, glue::glue("fmri.pipeline::update_tracked_job_status('{self$sqlite_db}', Sys.getenv('JOBID'), 'STARTED')"))

if (isTRUE(args$print_session_info)) {
  print(sessionInfo())
  print(Sys.info())
}

if (isTRUE(args$print_environment)) {
    print(Sys.getenv())
}

if (!is.null(args$r_packages)) {
  if (!require(pacman)) {
    install.packages('pacman')
    library(pacman)
  }
  r_packages <- unlist(strsplit(args$r_packages, ","))
  pacman::p_load(char = trimws(r_packages))
}

if (!is.null(args$input_rdata_file)) {
  if (file.exists(args$input_rdata_file)) {
    load(args$input_rdata_file)
  } else {
    cli_abort("Cannot load input environment object: {args$input_rdata_file}")
  }
}

tryCatch(
  eval(parse(text = args$r_code)),
  error = function(e) {
    print(as.character(e))
    # if (!is.null(self$sqlite_db)) glue::glue("    fmri.pipeline::update_tracked_job_status('{self$sqlite_db}', Sys.getenv('JOBID'), 'FAILED', cascade = TRUE)")
    cli_abort(e)
    }
  )

if (!is.null(args$output_rdata_file)) {
  tryCatch(
    save.image(file = args$output_rdata_file),
    error = function(e) {
      print(as.character(e))
      # if (!is.null(self$sqlite_db)) glue::glue("    fmri.pipeline::update_tracked_job_status('{self$sqlite_db}', Sys.getenv('JOBID'), 'FAILED', cascade = TRUE)")
      cli_abort(e)
    }
  )
}

if (isTRUE(args$wait_for_subs)) {
  cl <- c('numeric', 'integer', 'character')
  if (exists(sub_job_ids) && inherits(sub_job_ids, cl)) {
    # sapply(
    #   sub_job_ids,
    #   function(id) fmri.pipeline::add_tracked_job_parent(sqlite_db = '{self$sqlite_db}', job_id = id, parent_job_id = Sys.getenv('JOBID'), child_level = 2)
    # )
    success <- hpcR::wait_for_job(
      job_ids = sub_job_ids, quiet = FALSE,
      repolling_interval = args$repolling_interval,
      max_wait = args$max_wait,
      scheduler= args$scheduler
      )
    if (isTRUE(args$all_subs_success)) {
      # if (isFALSE(success)) {{ fmri.pipeline::update_tracked_job_status(self$sqlite_db, Sys.getenv('JOBID'), 'FAILED', cascade = TRUE, exclude = sub_job_ids); stop() }}
    }
  } else {
    cli_warn('Attempt to wait for child jobs failed due to non-existent or improper sub_job_ids variable.')
  }

  # add any post-children R code to be executed (e.g., combining outputs from the child jobs)
  if (!is.null(args$post_subs_r_code)) {
    tryCatch(
      eval(parse(text = args$post_subs_r_code)),
      error = function(e) {
        print(as.character(e))
        # if (!is.null(self$sqlite_db)) glue::glue("    fmri.pipeline::update_tracked_job_status('{self$sqlite_db}', Sys.getenv('JOBID'), 'FAILED', cascade = TRUE)")
        cli_abort(e)
      }
    )
  }
}

cli_alert_info("Job end time: {Sys.time()}\n")

#  if (!is.null(self$sqlite_db)) syntax <- c(syntax, glue::glue("fmri.pipeline::update_tracked_job_status('{self$sqlite_db}', Sys.getenv('JOBID'), 'COMPLETED')"))
