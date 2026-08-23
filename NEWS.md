# hpcR (development version)

## Package libraries

* `packages()` now checks declared packages against the active library search path and supplies that path to the compute R process through `R_LIBS`.
* `packages()` now supports opt-in, pre-submission installation through `install = "ask"` or `install = "always"`, with a singular `install_library` target. Installation never occurs on compute nodes.
* Added `libraries(job = , user = , site = )` for explicit R-library configuration. Explicit `job` (`R_LIBS`) paths take precedence over the paths inferred from `packages()`.
* R job runners only verify and load packages; they no longer install packages  or depend on `pacman`, `R.utils`, or `cli` at compute runtime.

## Documentation and testing

* Added introductory and submission-debugging vignettes, including guidance on  package provisioning, `packages()`, and `libraries()`.
* Added tests for compiled library environment variables and for propagation of  `R_LIBS` to a local R job.

## New features

- Added support for building job sequences with the `%->%` operator.
  Jobs can now be chained so downstream jobs depend on upstream jobs.

- Added `job_sequence()` for initializing empty job sequences.

- Added `branch()` for defining parallel branches within a job sequence.
  Branches return job sequence objects, so they can be combined with other
  jobs or branches using `%->%`.

- Added internal sequence graph support using job object IDs as node
  identifiers. This allows job names to remain optional while keeping sequence
  dependencies stable.

- Added user-facing duplicate job detection in sequences. When possible, errors
  refer to the expressions used by the user, for example indicating that
  `job` and `job2` are the same job. 

- Assumed logic that reassigning an old job to a new object does not create a 
  new job, i.e., the new object will be treated as an identical job. Job cloning
  will be supported through the `clone()` function, of which only a skeleton
  exists now.

- Added support for inline code jobs. Code supplied with `code()` is captured,
  materialized as a generated R script during compilation, and submitted through
  the standard script submission pathway.

## Internal changes

- Refactored code-input preparation so `.prepare_input_code()` returns the
  generated script path, while `.compile_job()` assigns that path to the job
  input before submission.

- Updated job compilation to preserve and restore lock state when mutating job
  objects and job sequence objects internally.

- Added centralized job default hydration for selected unset job properties.

- Added internal helpers for parsing alternate argument names, including support
  for friendly library arguments that map to R library environment variables.
