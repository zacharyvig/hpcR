# hpcR (development version)

## Package libraries

* `packages()` now checks declared packages against the active library search path and supplies that path to the compute R process through `R_LIBS`.
* `packages()` now supports opt-in, pre-submission installation through `install = "ask"` or `install = "always"`, with a singular `install_library` target. Installation never occurs on compute nodes.
* Added `libraries(job = , user = , site = )` for explicit R-library configuration. Explicit `job` (`R_LIBS`) paths take precedence over the paths inferred from `packages()`.
* R job runners only verify and load packages; they no longer install packages  or depend on `pacman`, `R.utils`, or `cli` at compute runtime.

## Documentation and testing

* Added introductory and submission-debugging vignettes, including guidance on  package provisioning, `packages()`, and `libraries()`.
* Added tests for compiled library environment variables and for propagation of  `R_LIBS` to a local R job.
