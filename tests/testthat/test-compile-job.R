test_that(".get_scheduler_directive builds scheduler flags", {
  expect_equal(
    hpcR:::.get_scheduler_directive("slurm", "job_name", "test"),
    "--job-name=test"
  )

  resources_block <- class_pb_resources(
    n_nodes = "2",
    n_cores = "4",
    wall_time = "01:00:00",
    total_memory = character(0),
    memory_per_core = character(0)
  )

  expect_equal(
    hpcR:::.get_scheduler_directive("slurm", "resources", resources_block),
    c("-N 2", "-n 4", "--time=01:00:00")
  )

  expect_equal(
    hpcR:::.get_scheduler_directive("torque", "resources", resources_block),
    c("-l nodes=2:ppn=4", "-l walltime=01:00:00")
  )

  total_memory_block <- class_pb_resources(
    n_nodes = character(0),
    n_cores = character(0),
    wall_time = character(0),
    total_memory = "16G",
    memory_per_core = character(0)
  )
  memory_per_core_block <- class_pb_resources(
    n_nodes = character(0),
    n_cores = character(0),
    wall_time = character(0),
    total_memory = character(0),
    memory_per_core = "4G"
  )

  expect_equal(
    hpcR:::.get_scheduler_directive("slurm", "resources", total_memory_block),
    "--mem=16G"
  )
  expect_equal(
    hpcR:::.get_scheduler_directive(
      "slurm", "resources", memory_per_core_block
    ),
    "--mem-per-cpu=4G"
  )
  expect_equal(
    hpcR:::.get_scheduler_directive("torque", "resources", total_memory_block),
    "-l mem=16G"
  )
  expect_equal(
    hpcR:::.get_scheduler_directive(
      "torque", "resources", memory_per_core_block
    ),
    "-l pmem=4G"
  )
})

test_that(".get_scheduler_arguments gathers job directives", {
  tmp_script <- tempfile(fileext = ".R")
  writeLines("print('ok')", tmp_script)

  job <- rjob("test") +
    script(tmp_script) +
    job_directory(tempdir()) +
    resources(n_nodes = 2, n_cores = 4, wall_time = "01:00:00") +
    scheduler("slurm") +
    packages(character(0), install = "never")

  out <- hpcR:::.get_scheduler_arguments(job)
  out <- unname(out) # remove names for testing

  expected <- c("--job-name=test", "-N 2", "-n 4", "--time=01:00:00")
  expect_equal(sort(out), sort(expected))
  expect_true(all(nzchar(out)))
})

test_that(".get_system_file resolves bundled system files", {
  submit_file <- hpcR:::.get_system_file(
    file_type = "submit",
    scheduler_name = "slurm",
    job_language = "R"
  )
  torque_submit_file <- hpcR:::.get_system_file(
    file_type = "submit",
    scheduler_name = "torque",
    job_language = "R"
  )
  run_file <- hpcR:::.get_system_file(
    file_type = "run",
    scheduler_name = "slurm",
    job_language = "R"
  )

  expect_true(file.exists(submit_file))
  expect_true(file.exists(torque_submit_file))
  expect_true(file.exists(run_file))
})

test_that(".get_env_variables collects submission metadata", {
  testthat::local_mocked_bindings(
    .get_system_file = function(file_type, ...) {
      if (file_type == "submit") "submit_path" else "run_path"
    },
    .package = "hpcR"
  )

  tmp_script <- tempfile(fileext = ".R")
  writeLines("print('ok')", tmp_script)

  job <- rjob("test") +
    script(tmp_script) +
    job_directory(tempdir()) +
    resources(n_nodes = 2, n_cores = 4, wall_time = "01:00:00") +
    scheduler("slurm") +
    packages(c("stats", "utils"), install = "never")
  job <- hpcR:::.hydrate_defaults(job)
  env_vars <- hpcR:::.get_env_variables(job)

  expect_equal(env_vars[["run_system_file"]], "run_path")
  expect_equal(env_vars[["scheduler_name"]], "slurm")
  expect_equal(env_vars[["print_session_info"]], "FALSE")
  expect_equal(env_vars[["print_environment"]], "FALSE")
  expect_equal(env_vars[["packages"]], "stats,utils")
  expect_equal(
    env_vars[["R_LIBS"]],
    hpcR:::.collapse_library_paths(hpcR:::.job_library_paths(job))
  )
})

test_that(".get_env_variables combines explicit and package library paths", {
  tmp_script <- tempfile(fileext = ".R")
  writeLines("print('ok')", tmp_script)
  explicit_libs <- c("/project/R/library", "/shared/R/library")

  job <- rjob("test") +
    script(tmp_script) +
    job_directory(tempdir()) +
    scheduler("local") +
    packages("stats", install = "never") +
    libraries(job = explicit_libs, user = "~/R/library")

  env_vars <- hpcR:::.get_env_variables(job)
  expected_r_libs <- hpcR:::.collapse_library_paths(
    hpcR:::.job_library_paths(job)
  )

  expect_equal(env_vars[["R_LIBS"]], expected_r_libs)
  expect_equal(env_vars[["R_LIBS_USER"]], "~/R/library")
})

test_that(".get_env_variables includes an explicit installation library", {
  tmp_script <- tempfile(fileext = ".R")
  install_library <- tempfile("hpcr-r-library-")
  dir.create(install_library)
  writeLines("print('ok')", tmp_script)

  job <- rjob("test") +
    script(tmp_script) +
    job_directory(tempdir()) +
    scheduler("local") +
    packages("stats", install = "never", install_library = install_library)

  env_vars <- hpcR:::.get_env_variables(job)
  expected_r_libs <- hpcR:::.collapse_library_paths(
    hpcR:::.job_library_paths(job)
  )

  expect_equal(env_vars[["R_LIBS"]], expected_r_libs)
})

test_that(".get_env_variables preserves explicit library-path precedence", {
  old_r_libs <- Sys.getenv("R_LIBS", unset = NA_character_)
  old_r_libs_user <- Sys.getenv("R_LIBS_USER", unset = NA_character_)
  on.exit({
    if (is.na(old_r_libs)) Sys.unsetenv("R_LIBS") else Sys.setenv(R_LIBS = old_r_libs)
    if (is.na(old_r_libs_user)) Sys.unsetenv("R_LIBS_USER") else Sys.setenv(R_LIBS_USER = old_r_libs_user)
  }, add = TRUE)
  Sys.setenv(
    R_LIBS = paste(c("/env-library", "/shared-library"),
                   collapse = .Platform$path.sep),
    R_LIBS_USER = "/environment-user-library"
  )

  tmp_script <- tempfile(fileext = ".R")
  writeLines("print('ok')", tmp_script)
  job <- rjob("test") +
    script(tmp_script) +
    job_directory(tempdir()) +
    scheduler("local") +
    packages("stats", install = "never", install_library = "/install-library") +
    libraries(
      job = c("/explicit-library", "/shared-library"),
      user = "/job-user-library"
    )

  env_vars <- hpcR:::.get_env_variables(job)
  expected_paths <- c(
    "/explicit-library",
    "/shared-library",
    "/job-user-library",
    "/install-library",
    "/env-library",
    "/environment-user-library",
    .libPaths()
  )

  expect_equal(
    env_vars[["R_LIBS"]],
    paste(expected_paths, collapse = .Platform$path.sep)
  )
  expect_equal(env_vars[["R_LIBS_USER"]], "/job-user-library")
})

test_that(".compile_job stores compiled artifacts", {
  testthat::local_mocked_bindings(
    .get_scheduler_arguments = function(job) c("--job-name=test"),
    .get_env_variables = function(job) c(foo = "bar"),
    .get_system_file = function(file_type, ...) {
      if (file_type == "run") "run_path" else "submit_path"
    },
    .package = "hpcR"
  )

  tmp_script <- tempfile(fileext = ".R")
  writeLines("print('ok')", tmp_script)

  job <- rjob("test") +
    script(tmp_script) +
    job_directory(tempdir()) +
    resources(n_nodes = 2, n_cores = 4, wall_time = "01:00:00") +
    scheduler("slurm") +
    packages(character(0), install = "never")

  out <- hpcR:::.compile_job(job)

  expect_true(S7::S7_inherits(out@.compiled, class_pb_compiled))
  compiled <- S7::props(out@.compiled)
  expect_equal(compiled$env_variables, c(foo = "bar"))
  expect_equal(compiled$submit_control$scheduler_arguments, "--job-name=test")
  expect_equal(compiled$submit_system_file, "submit_path")
})

test_that(".compile_job supports torque submit system file", {
  tmp_script <- tempfile(fileext = ".R")
  writeLines("print('ok')", tmp_script)

  job <- rjob("test") +
    script(tmp_script) +
    job_directory(tempdir()) +
    resources(n_nodes = 2, n_cores = 4, wall_time = "01:00:00") +
    scheduler("torque") +
    packages(character(0), install = "never")

  out <- hpcR:::.compile_job(job)
  compiled <- S7::props(out@.compiled)

  expect_equal(basename(compiled$submit_system_file), "submit_to_torque.pbs")
  expect_equal(
    unname(compiled$submit_control$scheduler_arguments),
    c("-N test", "-l nodes=2:ppn=4", "-l walltime=01:00:00")
  )
})

test_that(".compile_job dispatches by scheduler", {
  called <- new.env(parent = emptyenv())
  called$schedulers <- c()

  testthat::local_mocked_bindings(
    .compile_job = function(job) {
      called$schedulers <- c(called$schedulers, job@scheduler@scheduler_name)
      job
    },
    .package = "hpcR"
  )

  tmp_script <- tempfile(fileext = ".R")
  writeLines("print('ok')", tmp_script)

  hpc_job <- rjob("test") +
    script(tmp_script) +
    job_directory(tempdir()) +
    resources(n_nodes = 2, n_cores = 4, wall_time = "01:00:00") +
    scheduler("slurm") +
    packages(character(0), install = "never")

  local_job <- rjob("test") +
    script(tmp_script) +
    job_directory(tempdir()) +
    resources(n_nodes = 2, n_cores = 4, wall_time = "01:00:00") +
    scheduler("local") +
    packages(character(0), install = "never")

  hpcR:::.compile_job(hpc_job)
  hpcR:::.compile_job(local_job)

  expect_equal(called$schedulers, c("slurm", "local"))
})

test_that(".compile_job rejects unsupported schedulers", {
  tmp_script <- tempfile(fileext = ".R")
  writeLines("print('ok')", tmp_script)

  job <- rjob("test") +
    script(tmp_script) +
    job_directory(tempdir()) +
    resources(n_nodes = 2, n_cores = 4, wall_time = "01:00:00") +
    scheduler("slurm") +
    packages(character(0), install = "never")

  job@.locked <- FALSE
  props <- S7::props(job)
  props$scheduler <- class_pb_scheduler(scheduler_name = "unsupported")
  S7::props(job) <- props
  job@.locked <- TRUE

  expect_error(
    hpcR:::.compile_job(job),
    "'arg' should be one of",
    fixed = TRUE
  )
})
