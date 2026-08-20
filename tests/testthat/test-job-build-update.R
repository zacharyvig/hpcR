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
    "wall_time",
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

test_that("packages defer availability checks until submission", {
  job <- rjob("test") +
    packages(c("dplyr", "nonexistentpkg123"), install = "never")

  expect_equal(job@packages@package_names, c("dplyr", "nonexistentpkg123"))
  expect_error(
    hpcR:::.prepare_job_packages(job),
    "Requested packages are not installed",
    fixed = TRUE
  )

  expect_error(
    rjob("test") +
      packages("dplyr", install = "sometimes"),
    "must be one of",
    fixed = TRUE
  )

  expect_error(
    rjob("test") +
      packages("dplyr", install = "never", install_library = c("one", "two")),
    "install_library",
    fixed = TRUE
  )
})

test_that("package preflight installs only when explicitly requested", {
  install_library <- tempfile("hpcr-r-library-")
  job <- rjob("test") +
    packages("examplepkg", install = "always", install_library = install_library)
  installed <- FALSE
  installed_packages <- NULL
  installed_library <- NULL

  testthat::local_mocked_bindings(
    .packages_installed = function(package_names, ...) {
      rep(installed, length(package_names))
    },
    .package = "hpcR"
  )
  testthat::local_mocked_bindings(
    install.packages = function(pkgs, lib, ...) {
      installed_packages <<- pkgs
      installed_library <<- lib
      installed <<- TRUE
      invisible(NULL)
    },
    .package = "utils"
  )

  expect_message(hpcR:::.prepare_job_packages(job),
                 "Installing missing", fixed = TRUE)
  expect_equal(installed_packages, "examplepkg")
  expect_equal(installed_library, install_library)
})

test_that("package preflight does not install when packages are available", {
  job <- rjob("test") + packages("examplepkg", install = "always")
  install_called <- FALSE

  testthat::local_mocked_bindings(
    .packages_installed = function(package_names, ...) {
      rep(TRUE, length(package_names))
    },
    .package = "hpcR"
  )
  testthat::local_mocked_bindings(
    install.packages = function(...) {
      install_called <<- TRUE
      stop("install.packages should not have been called")
    },
    .package = "utils"
  )

  expect_no_error(hpcR:::.prepare_job_packages(job))
  expect_false(install_called)
})

test_that("ask policy does not install from a non-interactive submission", {
  if (interactive()) {
    testthat::skip("This assertion requires a non-interactive R session.")
  }

  job <- rjob("test") + packages("examplepkg", install = "ask")
  install_called <- FALSE

  testthat::local_mocked_bindings(
    .packages_installed = function(package_names, ...) {
      rep(FALSE, length(package_names))
    },
    .package = "hpcR"
  )
  testthat::local_mocked_bindings(
    install.packages = function(...) {
      install_called <<- TRUE
    },
    .package = "utils"
  )

  expect_error(
    hpcR:::.prepare_job_packages(job),
    "non-interactive session",
    fixed = TRUE
  )
  expect_false(install_called)
})

test_that("package preflight reports installation and verification failures", {
  install_library <- tempfile("hpcr-r-library-")
  on.exit(unlink(install_library, recursive = TRUE, force = TRUE), add = TRUE)
  job <- rjob("test") +
    packages("examplepkg", install = "always", install_library = install_library)

  testthat::local_mocked_bindings(
    .packages_installed = function(package_names, ...) {
      rep(FALSE, length(package_names))
    },
    .package = "hpcR"
  )
  testthat::local_mocked_bindings(
    install.packages = function(...) stop("repository unavailable"),
    .package = "utils"
  )
  expect_message(
    expect_error(
      hpcR:::.prepare_job_packages(job),
      "Package installation failed",
      fixed = TRUE
    ),
    "Installing missing",
    fixed = TRUE
  )

  testthat::local_mocked_bindings(
    install.packages = function(...) invisible(NULL),
    .package = "utils"
  )
  expect_message(
    expect_error(
      hpcR:::.prepare_job_packages(job),
      "remain unavailable after installation",
      fixed = TRUE
    ),
    "Installing missing",
    fixed = TRUE
  )
})

test_that("default installation library prefers one configured user library", {
  configured_user_library <- "~/hpcr-test-library"
  job <- rjob("test") +
    libraries(user = configured_user_library)

  expect_equal(
    hpcR:::.default_install_library(job),
    path.expand(configured_user_library)
  )

  old_r_libs_user <- Sys.getenv("R_LIBS_USER", unset = NA_character_)
  on.exit({
    if (is.na(old_r_libs_user)) {
      Sys.unsetenv("R_LIBS_USER")
    } else {
      Sys.setenv(R_LIBS_USER = old_r_libs_user)
    }
  }, add = TRUE)
  Sys.setenv(R_LIBS_USER = paste(c("/first", "/second"),
                                 collapse = .Platform$path.sep))

  expect_error(
    hpcR:::.default_install_library(rjob("test")),
    "No single user library is configured",
    fixed = TRUE
  )
})
