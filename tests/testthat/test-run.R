test_that(".submit_* succeeds on available scheduler", {
  scheduler <- NULL

  if (nzchar(Sys.which("sbatch")) && nzchar(Sys.which("sacct"))) {
    scheduler <- "slurm"
  } else if (nzchar(Sys.which("qsub")) && nzchar(Sys.which("qselect"))) {
    scheduler <- "torque"
  } else if (.Platform$OS.type != "windows") {
    scheduler <- "local"
  }

  if (is.null(scheduler)) {
    testthat::skip("No supported scheduler available for run test.")
  }

  if (scheduler == "local" && .Platform$OS.type == "windows") {
    testthat::skip("Local scheduler requires UNIX-like shell tools.")
  }

  tmp_dir <<- withr::local_tempdir()

  script_path <- file.path(tmp_dir, "job.R")
  output_path <- file.path(tmp_dir, "job_output.txt")
  output_path <- normalizePath(output_path, winslash = "/", mustWork = FALSE)
  safe_output <- gsub("'", "\\'", output_path, fixed = TRUE)

  writeLines(
    c(
      "Sys.sleep(1)",
      sprintf("writeLines('ok', con = '%s')", safe_output)
    ),
    con = script_path
  )

  if (scheduler == "local") {
    job_id <- .submit_to_local(
      input = script_path,
      input_type = "script",
      fail_on_error = TRUE,
      depends_on = NULL,
      env_variables = NULL,
      echo = FALSE
    )
  } else {
    command <- sprintf("Rscript --vanilla %s", shQuote(script_path))
    job_id <- .submit_to_hpc(
      input = command,
      input_type = "oneliner",
      scheduler_name = scheduler,
      fail_on_error = TRUE,
      depends_on = NULL,
      env_variables = NULL,
      echo = FALSE,
      scheduler_arguments = sprintf("--output=%s/test_job_.out", tmp_dir)
    )
  }

  expect_no_error({
    .wait_for_job(
      job_ids = as.character(job_id),
      repolling_interval = 1,
      max_wait = 120,
      scheduler_name = scheduler,
      quiet = TRUE,
      stop_on_timeout = TRUE
    )
  })

  expect_true(nzchar(job_id))

})

test_that(".submit_to_local oneliner submission succeeds", {
  if (.Platform$OS.type == "windows") {
    testthat::skip("Local scheduler requires UNIX-like shell tools.")
  }

  command <- "echo hello"

  job_id <- .submit_to_local(
    input = command,
    input_type = "oneliner",
    fail_on_error = TRUE,
    depends_on = NULL,
    env_variables = NULL,
    echo = FALSE
  )

  expect_true(nzchar(job_id))

})

test_that("local submit system file respects job_directory", {
  if (.Platform$OS.type == "windows") {
    testthat::skip("Local scheduler requires UNIX-like shell tools.")
  }

  job_dir <- normalizePath(withr::local_tempdir(), winslash = "/")
  tmp_dir <- withr::local_tempdir()
  script_path <- normalizePath(
    file.path(tmp_dir, "job.R"), winslash = "/", mustWork = FALSE
  )
  output_path <- normalizePath(
    file.path(tmp_dir, "cwd.txt"), winslash = "/", mustWork = FALSE
  )
  quoted_output <- encodeString(output_path, quote = "\"")

  writeLines(
    sprintf(
      paste(
        "writeLines(normalizePath(getwd(), winslash = '/', mustWork = TRUE),",
        "con = %s)"
      ),
      quoted_output
    ),
    con = script_path
  )

  env_variables <- c(
    job_dir = job_dir,
    R_HOME = R.home(),
    run_system_file = hpcR:::.get_system_file(
      file_type = "run",
      scheduler_name = "local",
      job_language = "R"
    ),
    input = script_path,
    scheduler_name = "local",
    print_session_info = "FALSE",
    print_environment = "FALSE",
    packages = ""
  )

  job_id <- .submit_to_local(
    input = hpcR:::.get_system_file(
      file_type = "submit",
      scheduler_name = "local",
      job_language = "R"
    ),
    input_type = "script",
    fail_on_error = TRUE,
    depends_on = NULL,
    env_variables = env_variables,
    echo = FALSE
  )

  .wait_for_job(
    job_ids = as.character(job_id),
    repolling_interval = 0.1,
    max_wait = 10,
    scheduler_name = "local",
    quiet = TRUE,
    stop_on_timeout = TRUE
  )
  .wait_until(function() file.exists(output_path), timeout = 5)

  expect_equal(readLines(output_path), job_dir)
})
