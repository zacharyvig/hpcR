#!/usr/bin/env Rscript --vanilla

# on.exit({
#   if (isTRUE(as.logical(args$is_r_script_tmp))) {
#     try(unlink(args$r_script))
#   }
#   if (isTRUE(as.logical(args$is_post_subs_r_script_tmp))) {
#     try(unlink(args$post_subs_r_script))
#   }
# })

# load packages
load_quiet <- function(pkg) {
  if (!require(pkg, quietly = TRUE, warn.conflicts = FALSE)) {
    install.packages(pkg, quiet = TRUE)
  }
  suppressPackageStartupMessages(
    library(pkg, quietly = TRUE, warn.conflicts = FALSE)
  )
}

load_quiet("R.utils")
load_quiet("cli")
load_quiet("checkmate")

# tracking function
update_job_status <- function() {}

# read in command line arguments as list (all come in as character)
args <- R.utils::commandArgs(asValues = TRUE)

cli::cli_alert_info(
  "Job start time: {Sys.time()}\n"
)

#  TODO: status is STARTED

if (isTRUE(as.logical(args$print_session_info))) {
  cli::cli_alert_info("Printing session info:")
  print(sessionInfo())
  print(Sys.info())
}

if (isTRUE(as.logical(args$print_environment))) {
  cli::cli_alert_info("Printing environment:")
  print(Sys.getenv())
}

if (!is.null(args$packages) && nzchar(args$packages)) {
  load_quiet("pacman")
  packages <- unlist(strsplit(args$packages, ","))
  cli::cli_alert_info("Loading R packages:")
  pacman::p_load(char = trimws(packages))
}

# if (!is.null(args$input_rdata_file) && nzchar(args$input_rdata_file)) {
#   if (checkmate::test_file_exists(args$input_rdata_file)) {
#     cli::cli_alert_info("Attempting to load input RData file:")
#     tryCatch({
#       load(args$input_rdata_file)
#       cli::cli_alert_success("Input RData file loaded successfully")
#     },
#     error = function(e) {
#       cli::cli_abort(
#         c("Loading input RData file failed! Ending batch run.",
#           "x" = "{as.character(e)}"
#         )
#       )
#     })
#   } else {
#     cli::cli_abort(
#       "Input RData file not found: {.path args$input_rdata_file}"
#     )
#   }
# }

tryCatch({
  cli::cli_alert_info("Running R code:")
  source(args$input, echo = TRUE, print.eval = TRUE)
  cli::cli_alert_success("R code executed successfully")
},
error = function(e) {
  # TODO: status is FAILED, cascade is TRUE
  cli::cli_abort(
    c("R Code failed to execute! Ending batch run.",
      "x" = "{as.character(e)}"
    )
  )
})

# if (!is.null(args$output_rdata_file) && nzchar(args$output_rdata_file)) {
#   tryCatch({
#     cli::cli_alert_info("Saving output RData file:")
#     save.image(file = args$output_rdata_file)
#     cli::cli_alert_success(
#       "Output RData file saved successfully: {.path {args$output_rdata_file}}"
#     )
#   },
#   error = function(e) {
#     # TODO: status is FAILED, cascade is TRUE
#     cli::cli_abort(
#       c("Saving output RData file failed! Ending batch run.",
#         "x" = "{as.character(e)}"
#       )
#     )
#   })
# }

# if (isTRUE(as.logical(args$wait_for_subs))) {
#   sub_ids_class <- c("numeric", "integer", "character")
#   if (exists("SUB_JOB_IDS") && inherits(SUB_JOB_IDS, sub_ids_class)) {
#     # TODO: add parents
#     success <- hpcR::wait_for_job(
#       job_ids = SUB_JOB_IDS, quiet = FALSE,
#       repolling_interval = as.numeric(args$repolling_interval),
#       max_wait = as.numeric(args$max_wait),
#       scheduler = args$scheduler
#     )
#     if (isTRUE(as.logical(args$all_subs_success))) {
#       # TODO: check fails, status is FAILED, cascade is TRUE (exclude subs)
#     }
#   } else {
#     cli::cli_warn(
#       c("Attempt to wait for child jobs failed!",
#         "Non-existent or improper {.code SUB_JOB_IDS} variable.")
#     )
#   }

# # add any post-children R code to be executed (e.g., combining outputs from 
# # the child jobs)
# if (!is.null(args$post_subs_r_script) && nzchar(args$post_subs_r_script)) {
#   tryCatch({
#     cli::cli_alert_info("Running post-subs R code:")
#     source(args$post_subs_r_script, echo = TRUE, print.eval = TRUE)
#   },
#   error = function(e) {
#     # TODO: status is FAILED, cascade is TRUE
#     cli::cli_abort(
#       c("Post-subs R code failed to execute! Ending batch run.",
#         "x" = "{as.character(e)}"
#       )
#     )
#   })
# } else if (exists("SUB_JOB_IDS")) {
#   cli::cli_warn(
#     "Ignoring {.code SUB_JOB_IDS} object since {.code wait_for_subs} is FALSE."
#   )
# }

cli::cli_alert_info("Job end time: {Sys.time()}\n")

# TODO: status is COMPLETED
