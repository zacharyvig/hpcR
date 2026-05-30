test_that("job builds and updates with simple properties", {
  tmp_script <- tempfile(fileext = ".R")
  writeLines("print('ok')", tmp_script)
  tmp_dir <- tempdir()

  job <- rjob("test") +
    script(tmp_script) +
    job_directory(tmp_dir) +
    resources(n_nodes = 1, n_cores = 2)

  expect_true(hpcR:::is_job(job))
  expect_equal(job@job_name, "test")
  expect_equal(S7::props(job@resources)$n_nodes, "1")
  expect_equal(S7::props(job@resources)$n_cores, "2")
})

test_that("job updates overwrite properties", {
  tmp_script <- tempfile(fileext = ".R")
  writeLines("print('ok')", tmp_script)

  job <- rjob("test") +
    script(tmp_script) +
    resources(n_nodes = 1)

  expect_message(job2 <- job + resources(n_nodes = 4),
                 "overwritten: n_nodes", fixed = TRUE)

  expect_equal(S7::props(job2@resources)$n_nodes, "4")
})

test_that("validation warns on invalid resource values", {
  tmp_script <- tempfile(fileext = ".R")
  writeLines("print('ok')", tmp_script)

  job <- rjob("test") + script(tmp_script)

  expect_error(
    job + resources(wall_time = "not-a-time"),
    "Wall time",
    fixed = TRUE
  )

  expect_error(
    job + resources(total_memory = 10, memory_per_core = 2),
    "mutually exclusive",
    fixed = TRUE
  )
})

test_that("validation warns on invalid scheduler name", {
  expect_error(
    rjob("test") + scheduler("unknown_scheduler"),
    "invalid or currently not supported",
    fixed = TRUE
  )
})
