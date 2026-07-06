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
    JobID = c("1", "2", "3", "4", "5"),
    State = c("RUNNING", "COMPLETED", "FAILED", "MISSING", "CANCELLED"),
    stringsAsFactors = FALSE
  )

  out <- hpcR:::.standardize_statuses(df, "slurm")
  out <- unname(out)

  expect_equal(out, c("running", "complete", "failed", "missing", "cancelled"))
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

test_that(".check_job_status_local treats absent pids as complete", {
  if (.Platform$OS.type == "windows") {
    testthat::skip("Local scheduler requires UNIX-like shell tools.")
  }

  out <- hpcR:::.check_job_status_local(
    job_ids = "999999999",
    standardize_output = TRUE
  )

  expect_equal(unname(out), "complete")
})

test_that(".check_job_status_torque maps qstat states", {
  qstat_output <- list(
    queued = c("Job Id: queued", "    job_state = Q"),
    running = c("Job Id: running", "    job_state = R"),
    suspended = c("Job Id: suspended", "    job_state = S"),
    complete = c(
      "Job Id: complete",
      "    job_state = C",
      "    exit_status = 0"
    ),
    failed = c(
      "Job Id: failed",
      "    job_state = C",
      "    exit_status = 1"
    ),
    cancelled = c(
      "Job Id: cancelled",
      "    job_state = C",
      "    comment = Job deleted by user"
    )
  )

  testthat::local_mocked_bindings(
    .torque_qstat_full = function(job_id) qstat_output[[job_id]],
    .torque_tracejob = function(job_id) {
      stop("tracejob should not be called for visible qstat jobs")
    },
    .package = "hpcR"
  )

  out <- hpcR:::.check_job_status_torque(
    job_ids = names(qstat_output),
    standardize_output = TRUE
  )

  expect_equal(unname(out), names(qstat_output))
})

test_that(".check_job_status_torque uses tracejob for terminal fallback", {
  testthat::local_mocked_bindings(
    .torque_qstat_full = function(job_id) {
      structure("Unknown Job Id", status = 153L)
    },
    .torque_tracejob = function(job_id) {
      switch(
        job_id,
        cancelled = "Job deleted by user request",
        complete = "Exit_status=0",
        failed = "Exit_status=271",
        missing = structure(character(0), status = 1L)
      )
    },
    .package = "hpcR"
  )

  out <- hpcR:::.check_job_status_torque(
    job_ids = c("cancelled", "complete", "failed", "missing"),
    standardize_output = TRUE
  )

  expect_equal(
    unname(out),
    c("cancelled", "complete", "failed", "missing")
  )
})

test_that(".check_job_status_torque falls back when qstat is unparseable", {
  testthat::local_mocked_bindings(
    .torque_qstat_full = function(job_id) "unparseable qstat output",
    .torque_tracejob = function(job_id) "Exit_status=0",
    .package = "hpcR"
  )

  out <- hpcR:::.check_job_status_torque(
    job_ids = "complete",
    standardize_output = TRUE
  )

  expect_equal(unname(out), "complete")
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

test_that(".wait_for_job treats cancelled jobs as terminal failures", {
  calls <- 0L
  testthat::local_mocked_bindings(
    .check_job_status = function(job_ids, scheduler_name, ...) {
      calls <<- calls + 1L
      c("100" = "cancelled")
    },
    .package = "hpcR"
  )

  out <- suppressMessages(
    .wait_for_job(
      job_ids = "100",
      repolling_interval = 0.1,
      max_wait = 10,
      scheduler_name = "slurm",
      quiet = TRUE,
      stop_on_timeout = TRUE
    )
  )

  expect_false(out)
  expect_equal(calls, 1L)
})

test_that(".give_status_update reports cancelled jobs", {
  expect_message(
    .give_status_update(c("100" = "cancelled"), "cancelled"),
    "cancelled",
    fixed = TRUE
  )
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
