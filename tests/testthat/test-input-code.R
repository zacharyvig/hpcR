# tests for when the user supplies raw code via code({...})

test_that(".sanitize_file_component() creates safe file-name pieces", {
  expect_equal(.sanitize_file_component("my_job"), "my_job")
  expect_equal(.sanitize_file_component("my job"), "my_job")
  expect_equal(.sanitize_file_component("my/job:name"), "my_job_name")
  expect_equal(.sanitize_file_component("___"), "job")
  expect_equal(.sanitize_file_component(""), "job")
})

test_that(".deparse_code_quo() converts simple quosures to code text", {
  q <- rlang::quo(print("ok"))

  out <- .deparse_code_quo(q)

  expect_type(out, "character")
  expect_length(out, 1)
  expect_match(out, 'print\\("ok"\\)')
})

test_that(".deparse_code_quo() handles braced multi-line code", {
  q <- rlang::quo({
    x <- 1
    print(x)
  })

  out <- .deparse_code_quo(q)

  expect_type(out, "character")
  expect_length(out, 1)
  expect_match(out, "x <- 1", fixed = TRUE)
  expect_match(out, "print\\(x\\)")
})

test_that("code() stores code as a quosure on the job input", {
  job <- rjob("code_job") +
    code({
      x <- 1
      print(x)
    })

  expect_true(rlang::is_quosure(job@input@code_quo))
})

test_that(".prepare_input_code() writes code and stores generated script path in input_value", {
  dir <- withr::local_tempdir()

  job <- rjob("code_job") +
    code({
      x <- 1
      print(x)
    }) +
    job_directory(dir)

  script_path <- .prepare_input_code(job)

  expect_type(script_path, "character")
  expect_length(script_path, 1)
  expect_true(file.exists(script_path))

  expect_equal(
    normalizePath(dirname(script_path), mustWork = TRUE),
    normalizePath(dir, mustWork = TRUE)
  )

  contents <- paste(readLines(script_path, warn = FALSE), collapse = "\n")

  expect_match(contents, "x <- 1", fixed = TRUE)
  expect_match(contents, "print\\(x\\)")
})

test_that(".prepare_input_code() does not mutate job input_value", {
  dir <- withr::local_tempdir()

  job <- rjob("code_job") +
    code({
      print("ok")
    }) +
    job_directory(dir)

  old_input_value <- job@input@input_value

  script_path <- .prepare_input_code(job)

  expect_true(file.exists(script_path))
  expect_identical(job@input@input_value, old_input_value)
})

test_that(".prepare_input_code() writes captured code without evaluating it", {
  dir <- withr::local_tempdir()

  marker <- FALSE

  job <- rjob("code_job") +
    code({
      marker <- TRUE
      print("hello")
    }) +
    job_directory(dir)

  script_path <- .prepare_input_code(job)

  expect_false(marker)

  contents <- paste(readLines(script_path, warn = FALSE), collapse = "\n")

  expect_match(contents, "marker <- TRUE", fixed = TRUE)
  expect_match(contents, 'print\\("hello"\\)')
})

test_that(".prepare_input_code() creates fallback staging directory when job_directory is absent", {
  wd <- withr::local_tempdir()
  withr::local_dir(wd)

  job <- rjob("fallback_job") +
    code({
      print("ok")
    })

  script_path <- .prepare_input_code(job)

  expect_true(file.exists(script_path))
  expect_true(dir.exists(dirname(script_path)))

  expect_true(
    startsWith(
      normalizePath(dirname(script_path), mustWork = TRUE),
      normalizePath(wd, mustWork = TRUE)
    )
  )

  expect_match(basename(dirname(script_path)), "^\\.hpcR_generated_")
  expect_match(basename(script_path), "^\\.hpcR_generated_fallback_job_")
  expect_match(basename(script_path), "\\.R$")

  contents <- paste(readLines(script_path, warn = FALSE), collapse = "\n")
  expect_match(contents, 'print\\("ok"\\)')
})

test_that(".prepare_input_code() errors when code quosure is missing", {
  dir <- withr::local_tempdir()

  job <- rjob("missing_code_job") +
    job_directory(dir)

  expect_error(
    .prepare_input_code(job),
    "No valid code quosure|No code provided"
  )
})

test_that(".generate_staged_script_path() uses supplied job directory", {
  dir <- withr::local_tempdir()

  job <- rjob("my job") +
    code({
      print("ok")
    }) +
    job_directory(dir)

  script_path <- .generate_staged_script_path(job)

  expect_type(script_path, "character")
  expect_length(script_path, 1)

  expect_equal(
    normalizePath(dirname(script_path), mustWork = TRUE),
    normalizePath(dir, mustWork = TRUE)
  )

  expect_match(basename(script_path), "^\\.hpcR_generated_my_job_")
  expect_match(basename(script_path), "\\.R$")
})

test_that(".generate_staging_dir() creates writable fallback directory under working directory", {
  wd <- withr::local_tempdir()
  withr::local_dir(wd)

  dir <- .generate_staging_dir()

  expect_type(dir, "character")
  expect_length(dir, 1)
  expect_true(dir.exists(dir))

  expect_true(
    startsWith(
      normalizePath(dir, mustWork = TRUE),
      normalizePath(wd, mustWork = TRUE)
    )
  )

  expect_equal(unname(file.access(dir, mode = 2)), 0)
  expect_match(basename(dir), "^\\.hpcR_generated_")
})

test_that(".compile_job() materializes code input into job input_value", {
  dir <- withr::local_tempdir()

  job <- rjob("compile_code_job") +
    code({
      x <- 1
      print(x)
    }) +
    job_directory(dir) +
    scheduler("slurm")

  compiled_job <- .compile_job(job)

  script_path <- compiled_job@input@input_value

  expect_type(script_path, "character")
  expect_length(script_path, 1)
  expect_true(file.exists(script_path))

  expect_equal(compiled_job@input@input_type, "code")

  expect_equal(
    normalizePath(dirname(script_path), mustWork = TRUE),
    normalizePath(dir, mustWork = TRUE)
  )

  contents <- paste(readLines(script_path, warn = FALSE), collapse = "\n")

  expect_match(contents, "x <- 1", fixed = TRUE)
  expect_match(contents, "print\\(x\\)")

  expect_equal(compiled_job@.compiled@submission_input_type, "script")
  expect_type(compiled_job@.compiled@submission_input, "character")
  expect_length(compiled_job@.compiled@submission_input, 1)
})

test_that(".compile_job() materializes code without evaluating it", {
  dir <- withr::local_tempdir()

  marker <- FALSE

  job <- rjob("compile_code_job") +
    code({
      marker <- TRUE
      print("hello")
    }) +
    job_directory(dir) +
    scheduler("slurm")

  compiled_job <- .compile_job(job)

  expect_false(marker)

  contents <- paste(
    readLines(compiled_job@input@input_value, warn = FALSE),
    collapse = "\n"
  )

  expect_match(contents, "marker <- TRUE", fixed = TRUE)
  expect_match(contents, 'print\\("hello"\\)')
})