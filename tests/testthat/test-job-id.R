make_test_job <- function(name = "id_job") {
  tmp_script <- tempfile(pattern = "id_script", fileext = ".R")
  writeLines("print('ok')", tmp_script)
  rjob(name) + script(tmp_script) + scheduler("slurm")
}

make_test_sequence <- function() {
  make_test_job("a") %->% make_test_job("b")
}

test_that("job_id() is empty for unsubmitted jobs and sequences", {
  expect_identical(job_id(make_test_job()), character(0))
  expect_identical(job_id(make_test_sequence()), character(0))
})

test_that("job_id() returns stored scheduler ids", {
  job <- make_test_job()
  job@.submission <- list(job_id = "123")
  expect_identical(job_id(job), "123")

  seq <- make_test_sequence()
  ids <- c(job_a = "1", job_b = "2")
  seq@.submission <- list(job_ids = ids)
  expect_identical(job_id(seq), ids)
})

test_that("job_id() rejects non-job objects", {
  expect_error(job_id("123"), "must be a job")
})

test_that("clone() clears submission data", {
  job <- make_test_job()
  job@.submission <- list(job_id = "123")
  expect_identical(job_id(clone(job, "new")), character(0))
})

test_that("status helpers point to job_id() when given a job", {
  job <- make_test_job()
  expect_error(
    check_job_status(job, scheduler_name = "slurm"),
    "job_id"
  )
  expect_error(wait_for_job(job, scheduler_name = "slurm"), "job_id")
})

test_that("submitted jobs print their scheduler id", {
  job <- make_test_job()
  job@.submission <- list(job_id = "123")
  expect_match(paste(format(job), collapse = " "), "123")
})
test_that("submit() returns the job with its scheduler id (mocked scheduler)", {
  counter <- 0
  testthat::local_mocked_bindings(
    .submit_job = function(...) {
      counter <<- counter + 1
      as.character(100 + counter)
    },
    .package = "hpcR"
  )
  tmp <- withr::local_tempdir()
  job <- make_test_job("a") + job_directory(tmp)
  out <- suppressMessages(submit(job))
  expect_true(is_job(out))
  expect_identical(job_id(out), "101")
  expect_identical(job_id(job), character(0))

  seq <- (make_test_job("a") + job_directory(tmp)) %->%
    (make_test_job("b") + job_directory(tmp))
  seq_out <- suppressMessages(submit(seq))
  expect_true(S7::S7_inherits(seq_out, class_job_sequence))
  expect_length(job_id(seq_out), 2L)
  expect_false(is.null(names(job_id(seq_out))))
})
