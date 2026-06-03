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
