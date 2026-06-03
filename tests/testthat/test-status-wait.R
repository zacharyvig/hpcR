test_that("check_job_status dispatches to scheduler helper", {
  captured <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    .check_job_status_slurm = function(
      job_ids,
      user,
      standardize_output,
      columns = NULL,
      .call = NULL,
      ...
    ) {
      captured$job_ids <- job_ids
      captured$user <- user
      captured$standardize_output <- standardize_output
      captured$columns <- columns
      data.frame(
        JobID = job_ids,
        State = "RUNNING",
        stringsAsFactors = FALSE
      )
    },
    .package = "hpcR"
  )

  out <- check_job_status(
    job_ids = "42",
    user = "alice",
    scheduler_name = "slurm",
    standardize_output = FALSE,
    control = list(columns = c("jobid", "state"))
  )

  expect_equal(captured$job_ids, "42")
  expect_equal(captured$user, "alice")
  expect_false(captured$standardize_output)
  expect_equal(captured$columns, c("jobid", "state"))
  expect_equal(out$JobID, "42")
  expect_equal(out$State, "RUNNING")
})

test_that(".standardize_statuses maps slurm states", {
  df <- data.frame(
    JobID = c("1", "2", "3", "4"),
    State = c("RUNNING", "COMPLETED", "FAILED", "MISSING"),
    stringsAsFactors = FALSE
  )

  out <- hpcR:::.standardize_statuses(df, "slurm")
  out <- unname(out)

  expect_equal(out, c("running", "complete", "failed", "missing"))
})

test_that(".standardize_statuses maps local states", {
  df <- data.frame(
    PID = c(1, 2, 3, 4),
    STAT = c("R", "S", "C", "Z"),
    stringsAsFactors = FALSE
  )

  out <- hpcR:::.standardize_statuses(df, "local")
  out <- unname(out)

  expect_equal(out, c("running", "running", "complete", "failed"))
})

test_that("wait_for_job returns TRUE when jobs complete", {
  testthat::local_mocked_bindings(
    .check_job_status = function(job_ids, scheduler_name, ...) {
      "complete"
    },
    .package = "hpcR"
  )

  out <- .wait_for_job(
    job_ids = "100",
    repolling_interval = 0.1,
    max_wait = 10,
    scheduler_name = "slurm",
    quiet = TRUE
  )

  expect_true(out)
})

test_that(".wait_for_job returns FALSE on timeout when stop_on_timeout is FALSE", {
  testthat::local_mocked_bindings(
    .check_job_status = function(job_ids, scheduler_name, ...) {
      "running"
    },
    .package = "hpcR"
  )

  out <- .wait_for_job(
    job_ids = "101",
    repolling_interval = 0.1,
    max_wait = 0,
    scheduler_name = "slurm",
    quiet = TRUE,
    stop_on_timeout = FALSE
  )

  expect_false(out)
})

test_that(".wait_for_job errors on timeout when stop_on_timeout is TRUE", {
  testthat::local_mocked_bindings(
    .check_job_status = function(job_ids, scheduler_name, ...) {
      data.frame(JobID = job_ids, State = "RUNNING", stringsAsFactors = FALSE)
    },
    .package = "hpcR"
  )
  expect_error(
    hpcR:::.wait_for_job(
      job_ids = "102",
      repolling_interval = 0.1,
      max_wait = 0,
      scheduler_name = "slurm",
      quiet = TRUE,
      stop_on_timeout = TRUE
    ),
    "Maximum wait time",
    fixed = TRUE
  )
})
