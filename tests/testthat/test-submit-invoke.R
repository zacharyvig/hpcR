sequence_test_scheduler <- function() {
  if (nzchar(Sys.which("sbatch")) && nzchar(Sys.which("sacct"))) {
    return("slurm")
  }

  if (nzchar(Sys.which("qsub")) && nzchar(Sys.which("qselect"))) {
    return("torque")
  }

  if (.Platform$OS.type != "windows") {
    return("local")
  }

  NULL
}

sequence_test_wait <- function(job_ids, scheduler, max_wait = 120) {
  .wait_for_job(
    job_ids = as.character(job_ids),
    repolling_interval = if (scheduler == "local") 0.1 else 1,
    max_wait = max_wait,
    scheduler_name = scheduler,
    quiet = TRUE,
    stop_on_timeout = TRUE
  )
}

test_that("submit_job dispatches to HPC submit", {
  captured <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    .invoke_system_hpc = function(
      input,
      input_type,
      push_command,
      scheduler_arguments,
      fail_on_error,
      echo
    ) {
      captured$input <- input
      captured$input_type <- input_type
      captured$push_command <- push_command
      captured$scheduler_arguments <- scheduler_arguments
      captured$fail_on_error <- fail_on_error
      "999"
    },
    .package = "hpcR"
  )

  job_id <- submit_job(
    input = "script.R",
    input_type = "script",
    scheduler_name = "slurm",
    fail_on_error = TRUE,
    control = list(scheduler_arguments = "--mem=2g")
  )

  expect_equal(job_id, "999")
  expect_equal(captured$input, "script.R")
  expect_equal(captured$input_type, "script")
  expect_equal(captured$push_command, "sbatch")
  expect_equal(captured$scheduler_arguments, "--mem=2g")
  expect_true(captured$fail_on_error)
})

test_that("submit_job dispatches to local submit", {
  testthat::local_mocked_bindings(
    .assert_local_supported = function() TRUE,
    .invoke_system_local = function(...) "321",
    .package = "hpcR"
  )

  job_id <- submit_job(
    input = "script.R",
    input_type = "script",
    scheduler_name = "local"
  )

  expect_equal(job_id, "321")
})

test_that("submit_job fails if os is windows", {
  if (.Platform$OS.type == "windows") {
    expect_error(
      {
        submit_job(
          input = "script.R",
          input_type = "script",
          scheduler_name = "local"
        )
      },
      "Local scheduler requires UNIX-like"
    )
  } else {
    testthat::skip("This test is only relevant on Windows.")
  }
})

test_that(".invoke_system_hpc returns job id from stdout", {
  job_id <- .invoke_system_hpc(
    input = "123",
    input_type = "script",
    push_command = "echo",
    scheduler_arguments = NULL,
    fail_on_error = TRUE,
    echo = FALSE
  )

  expect_equal(job_id, "123")
})

test_that("linear job sequence submission succeeds", {
  scheduler <- sequence_test_scheduler()

  if (is.null(scheduler)) {
    skip("No supported scheduler available for sequence submission test.")
  }

  tmp_dir <- tempfile("hpcr-sequence-")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  tmp_dir <- normalizePath(
    tmp_dir,
    winslash = "/",
    mustWork = TRUE
  )

  upstream_output <- file.path(tmp_dir, "upstream.txt")
  downstream_output <- file.path(tmp_dir, "downstream.txt")

  upstream_script <- file.path(tmp_dir, "upstream.R")
  downstream_script <- file.path(tmp_dir, "downstream.R")

  writeLines(
    c(
      "Sys.sleep(0.5)",
      sprintf(
        "writeLines('upstream', %s)",
        encodeString(upstream_output, quote = "\"")
      )
    ),
    upstream_script
  )

  writeLines(
    c(
      sprintf(
        "upstream_exists <- file.exists(%s)",
        encodeString(upstream_output, quote = "\"")
      ),
      sprintf(
        "writeLines(if (upstream_exists) 'ok' else 'missing', %s)",
        encodeString(downstream_output, quote = "\"")
      )
    ),
    downstream_script
  )

  upstream_job <- rjob("upstream") +
    script(upstream_script) +
    job_directory(tmp_dir) +
    scheduler(scheduler)

  downstream_job <- rjob("downstream") +
    script(downstream_script) +
    job_directory(tmp_dir) +
    scheduler(scheduler)

  sequence <- upstream_job %->% downstream_job

  submission <- submit(sequence)

  expect_true(file.exists(upstream_script))
  expect_true(file.exists(downstream_script))

  # Adapt this section if submit(sequence) returns an updated sequence rather
  # than a named scheduler-ID vector.
  expect_true(is.character(submission))
  expect_length(submission, 2L)
  expect_false(anyNA(submission))
  expect_true(all(nzchar(submission)))

  sequence_test_wait(
    job_ids = submission,
    scheduler = scheduler
  )

  .wait_until(
    function() file.exists(downstream_output),
    timeout = 10
  )

  expect_true(file.exists(upstream_output))
  expect_true(file.exists(downstream_output))
  expect_equal(readLines(downstream_output), "ok")
})

test_that("named job sequence submission succeeds", {
  scheduler <- sequence_test_scheduler()

  if (is.null(scheduler)) {
    skip("No supported scheduler available for sequence submission test.")
  }

  tmp_dir <- tempfile("hpcr-named-sequence-")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  tmp_dir <- normalizePath(
    tmp_dir,
    winslash = "/",
    mustWork = TRUE
  )

  upstream_output <- file.path(tmp_dir, "upstream.txt")
  downstream_output <- file.path(tmp_dir, "downstream.txt")

  upstream_script <- file.path(tmp_dir, "upstream.R")
  downstream_script <- file.path(tmp_dir, "downstream.R")

  writeLines(
    sprintf(
      "writeLines('upstream', %s)",
      encodeString(upstream_output, quote = "\"")
    ),
    upstream_script
  )

  writeLines(
    sprintf(
      "writeLines(if (file.exists(%s)) 'ok' else 'missing', %s)",
      encodeString(upstream_output, quote = "\""),
      encodeString(downstream_output, quote = "\"")
    ),
    downstream_script
  )

  upstream_job <- rjob("upstream") +
    script(upstream_script) +
    job_directory(tmp_dir) +
    scheduler(scheduler)

  downstream_job <- rjob("downstream") +
    script(downstream_script) +
    job_directory(tmp_dir) +
    scheduler(scheduler) +
    sequencing(upstream_names = "upstream")

  sequence <- job_sequence(
    "named_sequence",
    upstream_job,
    downstream_job
  )

  submission <- submit(sequence)

  expect_true(is.character(submission))
  expect_length(submission, 2L)
  expect_false(anyNA(submission))
  expect_true(all(nzchar(submission)))

  sequence_test_wait(
    job_ids = submission,
    scheduler = scheduler
  )

  .wait_until(
    function() file.exists(downstream_output),
    timeout = 10
  )

  expect_equal(readLines(downstream_output), "ok")
})

test_that("branching job sequence submission succeeds", {
  scheduler <- sequence_test_scheduler()

  if (is.null(scheduler)) {
    skip("No supported scheduler available for sequence submission test.")
  }

  tmp_dir <- tempfile("hpcr-branch-sequence-")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  tmp_dir <- normalizePath(
    tmp_dir,
    winslash = "/",
    mustWork = TRUE
  )

  upstream_output <- file.path(tmp_dir, "upstream.txt")
  branch_a_output <- file.path(tmp_dir, "branch-a.txt")
  branch_b_output <- file.path(tmp_dir, "branch-b.txt")

  upstream_script <- file.path(tmp_dir, "upstream.R")
  branch_a_script <- file.path(tmp_dir, "branch-a.R")
  branch_b_script <- file.path(tmp_dir, "branch-b.R")

  writeLines(
    c(
      "Sys.sleep(0.5)",
      sprintf(
        "writeLines('upstream', %s)",
        encodeString(upstream_output, quote = "\"")
      )
    ),
    upstream_script
  )

  writeLines(
    sprintf(
      "writeLines(if (file.exists(%s)) 'branch-a-ok' else 'branch-a-missing', %s)",
      encodeString(upstream_output, quote = "\""),
      encodeString(branch_a_output, quote = "\"")
    ),
    branch_a_script
  )

  writeLines(
    sprintf(
      "writeLines(if (file.exists(%s)) 'branch-b-ok' else 'branch-b-missing', %s)",
      encodeString(upstream_output, quote = "\""),
      encodeString(branch_b_output, quote = "\"")
    ),
    branch_b_script
  )

  upstream_job <- rjob("upstream") +
    script(upstream_script) +
    job_directory(tmp_dir) +
    scheduler(scheduler)

  branch_a <- rjob("branch_a") +
    script(branch_a_script) +
    job_directory(tmp_dir) +
    scheduler(scheduler)

  branch_b <- rjob("branch_b") +
    script(branch_b_script) +
    job_directory(tmp_dir) +
    scheduler(scheduler)

  sequence <- upstream_job %->% branch(branch_a, branch_b)

  submission <- submit(sequence)

  expect_true(is.character(submission))
  expect_length(submission, 3L)
  expect_false(anyNA(submission))
  expect_true(all(nzchar(submission)))

  sequence_test_wait(
    job_ids = submission,
    scheduler = scheduler
  )

  .wait_until(
    function() {
      file.exists(branch_a_output) &&
        file.exists(branch_b_output)
    },
    timeout = 10
  )

  expect_equal(readLines(branch_a_output), "branch-a-ok")
  expect_equal(readLines(branch_b_output), "branch-b-ok")
})

test_that("joined job sequence submission succeeds", {
  scheduler <- sequence_test_scheduler()

  if (is.null(scheduler)) {
    skip("No supported scheduler available for sequence submission test.")
  }

  tmp_dir <- tempfile("hpcr-join-sequence-")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  tmp_dir <- normalizePath(
    tmp_dir,
    winslash = "/",
    mustWork = TRUE
  )

  branch_a_output <- file.path(tmp_dir, "branch-a.txt")
  branch_b_output <- file.path(tmp_dir, "branch-b.txt")
  joined_output <- file.path(tmp_dir, "joined.txt")

  branch_a_script <- file.path(tmp_dir, "branch-a.R")
  branch_b_script <- file.path(tmp_dir, "branch-b.R")
  joined_script <- file.path(tmp_dir, "joined.R")

  writeLines(
    c(
      "Sys.sleep(0.5)",
      sprintf(
        "writeLines('a', %s)",
        encodeString(branch_a_output, quote = "\"")
      )
    ),
    branch_a_script
  )

  writeLines(
    c(
      "Sys.sleep(0.5)",
      sprintf(
        "writeLines('b', %s)",
        encodeString(branch_b_output, quote = "\"")
      )
    ),
    branch_b_script
  )

  writeLines(
    sprintf(
      "writeLines(if (file.exists(%s) && file.exists(%s)) 'ok' else 'missing', %s)",
      encodeString(branch_a_output, quote = "\""),
      encodeString(branch_b_output, quote = "\""),
      encodeString(joined_output, quote = "\"")
    ),
    joined_script
  )

  branch_a <- rjob("branch_a") +
    script(branch_a_script) +
    job_directory(tmp_dir) +
    scheduler(scheduler)

  branch_b <- rjob("branch_b") +
    script(branch_b_script) +
    job_directory(tmp_dir) +
    scheduler(scheduler)

  joined <- rjob("joined") +
    script(joined_script) +
    job_directory(tmp_dir) +
    scheduler(scheduler)

  sequence <- branch(branch_a, branch_b) %->% joined

  submission <- submit(sequence)

  expect_true(is.character(submission))
  expect_length(submission, 3L)
  expect_false(anyNA(submission))
  expect_true(all(nzchar(submission)))

  sequence_test_wait(
    job_ids = submission,
    scheduler = scheduler
  )

  .wait_until(
    function() file.exists(joined_output),
    timeout = 10
  )

  expect_equal(readLines(joined_output), "ok")
})
