#' @title R6 Class for R Jobs
#'
#' @description
#' `R_job` is a user-friendly R6 class for creating jobs to submit to a high
#' performance computing (HPC) cluster using a scheduler.
#'
#' @details
#' ...
#'
#' @importFrom R6 R6Class
#' @importFrom cli cli_abort cli_inform cli_alert_info
#' @importFrom checkmate test_file_exists assert_multi_class assert_subset
#' assert_string assert_logical assert_character assert_number test_list
#' assert_directory_exists test_numeric
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{}
#'
#' @author Michael Hallquist, Zach Vig
#' @export
R_job <- R6::R6Class(
  classname = "R_job",

  private = list(

    # the process or scheduler job id that uniquely identifies this job
    job_id = NULL,

    # sequence id for this job
    #seq_id = NULL,

    # r_code for this job; accessible via active binding
    user_r_code = NULL,

    # r_script for this job; accessible via active binding
    user_r_script = NULL,

    # post_subs_r_code for this job; accessible via active binding
    user_post_subs_r_code = NULL,

    # post_subs_r_script for this job; accessible via active binding
    user_post_subs_r_script = NULL,

    # temporary file for storing main R code
    tmp_r_script = NULL,

    # temporary file for storing post-subs R code
    tmp_post_subs_r_script = NULL,

    gen_tmp_path = function() {
      smp <- sample(c(LETTERS, letters, 0:9), size = 12)
      str <- paste0(smp, collapse = "")
      path <- file.path(self$job_dir, sprintf(".tmp_%s.R", str))
      return(path)
    },

    write_tmp = function() {
      if (!is.null(private$user_r_code)) {
        tmp <- private$gen_tmp_path()
        private$tmp_r_script <- tmp
        writeLines(private$user_r_code, con = tmp)
      }
      if (!is.null(private$user_post_subs_r_code)) {
        tmp <- private$gen_tmp_path()
        private$tmp_post_subs_r_script <- tmp
        writeLines(private$user_post_subs_r_code, con = tmp)
      }
    },

    check = function() {
      if (
        !is.null(private$user_r_script) &
        !test_file_exists(private$user_r_script)
        ) {
        cli_abort(
          c("{.code r_script = {.path {r_script}}} does not exist.",
            "Cannot submit job.")
        )
      }
      if (is.null(private$user_r_script) & is.null(private$user_r_code)) {
        cli_abort(
          c("Both {.code r_script} and {.code r_code} are NULL.",
            "Cannot submit job.")
        )
      }
      if (
        !is.null(private$user_post_subs_r_script) &
        !test_file_exists(private$user_post_subs_r_script)
        ) {
        cli_abort(
          c("{.code post_subs_r_script = {.path {post_subs_r_script}}} does not exist.",
            "Cannot submit job.")
        )
      }

    },

    # helper to get batch file using system.file
    get_batch_file = function(scheduler) {
      if (scheduler %in% c("sbatch", "slurm")) {
        batch_file <- system.file(glue("submit_batch.sbatch"), package = "hpcR")
      } else {
        batch_file <- NULL
      }
      return(batch_file)
    },

    # helper to get compute file using system.file
    get_compute_file = function() {
      compute_file <- system.file("batch_run.R", package = "hpcR")
      return(compute_file)
    },

    # helper to get scheduler arguments for submit_job
    get_sched_args = function() {
      if (self$scheduler %in% c("torque", "qsub")) {
        sched_args <- glue(
          "-l nodes={self$n_nodes}:ppn={self$n_cores}",
          "-l walltime={self$wall_time}",
          if (is.null(self$mem_per_cpu)) { NULL } else { glue("-l pmem={self$mem_per_cpu}") },
          if (is.null(self$mem_total)) { NULL } else { glue("-l mem={self$mem_total}") },
          if (is.null(self$array)) { NULL } else { glue("-t {array}") },
          if (is.null(self$job_name)) { NULL } else { glue("-N {substr(self$job_name, 1, 15)}") },
          "{paste(self$scheduler_options, collapse=' ')}",
          .trim = TRUE, .sep = " ", .null = NULL
        )
        return(sched_args)
      } else if (self$scheduler %in% c("slurm", "sbatch")) {
        sched_args <- glue(
          "-N {self$n_nodes}",
          "-n {self$n_cores}",
          "--time={self$wall_time}",
          if (is.null(self$mem_per_cpu)) { NULL } else { glue("--mem-per-cpu={self$mem_per_cpu}") },
          if (is.null(self$mem_total)) { NULL } else { glue("--mem={self$mem_total}") },
          if (is.null(self$array)) { NULL } else { glue("--array {array}") },
          if (is.null(self$job_name)) { NULL } else { glue("--job-name={self$job_name}") },
          "{paste(self$scheduler_options, collapse=' ')}",
          .trim = TRUE, .sep = " ", .null = NULL
        )
        return(sched_args)
      } else {
        return(NULL)
      }
    },

    # helper to get environmental variables for submit_job
    get_env_variables = function() {
      r_script <- if (!is.null(private$user_r_script)) {
        private$user_r_script
      } else {
        private$tmp_r_script
      }
      post_subs_r_script <- if (!is.null(private$user_post_subs_r_script)) {
        private$user_post_subs_r_script
      } else {
        private$tmp_post_subs_r_script
      }
      env_variables <- c(
        job_dir = self$job_dir,
        shell_code = paste(self$shell_code, collapse = "&&"),
        R_HOME = R.home(),
        compute_file = private$get_compute_file(),
        print_session_info = self$print_session_info,
        print_environment = self$print_environment,
        r_packages = paste(self$r_packages, collapse = ", "),
        input_rdata_file = self$input_rdata_file,
        r_script = r_script,
        is_r_script_tmp = !is.null(private$tmp_r_script),
        wait_for_subs = self$wait_for_subs,
        repolling_interval = self$control.wait_for_subs$repolling_interval,
        max_wait = self$control.wait_for_subs$max_wait,
        scheduler = self$scheduler,
        all_subs_success = self$all_subs_success,
        post_subs_r_script = post_subs_r_script,
        is_post_subs_r_script_tmp = !is.null(private$tmp_post_subs_r_script),
        output_rdata_file = self$output_rdata_file
      )
      return(env_variables)
    }

  ),

  active = list(

    #' @field r_script The R code to be executed by this job. This can be a character
    #' vector that includes multiple R statements or an expression object containing
    #' the R code to be evaluated
    r_script = function(value) {
      if (missing(value)) {
        return(private$user_r_script)
      }
      if (is.null(private$user_r_code)) {
        if (!is.null(value) & !test_file_exists(value)) {
          cli_warn(
            c("{.code r_script = '{.path {value}}'} does not exist.",
            "Did you mean to pass to {.code r_code}?")
          )
        }
        if (!is.null(private$user_r_script)) {
          cli_inform(
            "Overwriting old {.code r_script}"
          )
        }
        private$user_r_script <- value
      } else {
        cli_warn(
          c("{.code r_code} is already set (and is mutually exclusive).",
            "Set it to NULL if you want to update {.code r_script}")
        )
      }
    },

    #' @field r_code The R code to be executed by this job. This can be a character
    #' vector that includes multiple R statements or an expression object containing
    #' the R code to be evaluated. (Mutually exclusive with \code{r_script})
    r_code = function(value) {
      if (missing(value)) {
        return(private$user_r_code)
      }
      if (is.null(private$user_r_script)) {
        r_code <- if (is.expression(value)) {
          as.character(value)
        } else if (is.character(value)) {
          unlist(strsplit(value, "\n"))
        } else if (is.null(value)) {
          NULL
        } else {
          cli_abort(
            "Invalid class: {.code r_code}"
          )
        }
        if (!is.null(private$user_r_code)) {
          cli_inform(
            "Overwriting old {.code r_code}"
          )
        }
        private$user_r_code <- r_code
      } else {
        cli_warn(
          c("{.code r_script} is already set (and is mutually exclusive).",
            "Set it to NULL if you want to update {.code r_code}")
        )
      }
    },

    #' @field post_subs_r_script The path to an R script to be executed by the batch after
    #' all sub jobs complete Only relevant if \code{wait_for_subs = TRUE}. (Mutually
    #' exclusive with \code{post_subs_r_code}).
    post_subs_r_script = function(value){
      if (missing(value)) {
        return(private$user_post_subs_r_script)
      }
      if (is.null(private$user_post_subs_r_code)) {
        if (!is.null(value) & !test_file_exists(value)) {
          cli_warn(
            c("{.code post_subs_r_script = {.path {value}}} does not exist.",
              "Did you mean to pass to {.code post_subs_r_code}?")
          )
        }
        if (!is.null(private$userpost_subs_r_script)) {
          cli_inform(
            "Overwriting old {.code post_subs_r_script}"
          )
        }
        private$user_post_subs_r_script <- value
      } else {
        cli_warn(
          c("{.code post_subs_r_code} is already set (and is mutually exclusive).",
            "Set it to NULL if you want to update {.code post_subs_r_script}")
        )
      }
    },

    #' @field post_subs_r_code The R code to be executed by the batch after all sub jobs
    #' complete. This can be a character vector that includes multiple R statements
    #' or an expression object containing the R code to be evaluated. Only relevant
    #' if \code{wait_for_subs = TRUE}. (Mutually exclusive with \code{post_subs_r_script})
    post_subs_r_code = function(value) {
      if (missing(value)) {
        return(private$user_post_subs_r_code)
      }
      if (is.null(private$user_post_subs_r_script)) {
        post_subs_r_code <- if (is.expression(value)) {
          as.character(value)
        } else if (is.character(value)) {
          unlist(strsplit(value, "\n"))
        } else if (is.null(value)) {
          NULL
        } else {
          cli_abort(
            "Invalid class: {.code post_subs_r_code}"
          )
        }
        if (!is.null(private$user_post_subs_r_code)) {
          cli_inform(
            "Overwriting old {.code post_subs_r_code}"
          )
        }
        private$user_post_subs_r_code <- post_subs_r_code
      } else {
        cli_warn(
          c("{.code post_subs_r_script} is already set (and is mutually exclusive).",
            "Set it to NULL if you want to update {.code post_subs_r_code}")
        )
      }
    }

  ),

  public = list(

    #' @field job_name A user-defined name for the job used for specifying job
    #' dependencies and informative job status queries on a job scheduler
    job_name = NULL,

    #' @field job_dir Location of "home" directory for this job. Needs to be somewhere
    #' that both compute nodes and login nodes can access, so /tmp is not suggested.
    job_dir = NULL,

    #' @field scheduler The job scheduler to be used for this batch. Options are:
    #' "slurm"/"sbatch", "torque"/"qsub", or "local"/"sh".
    scheduler = "slurm",

    #' @field n_nodes The number of nodes to be requested on the job scheduler
    n_nodes = "1",

    #' @field n_cores The number of cores (aka 'cpus', ignoring hyperthreading)
    #' to be requested on the job scheduler
    n_cores = "4",

    #' @field wall_time The amount of time requested on the job scheduler,
    #' following d-hh:mm:ss format. Defaults to
    #'    "4:00:00", which is 4 hours.
    wall_time = "4:00:00",

    #' @field mem_total The total amount of memory (RAM) requested by the job
    mem_total = "4G",

    #' @field mem_per_cpu The amount of memory (RAM) requested per cpu
    #' (total = mem_per_cpu * n_cores)
    mem_per_cpu = NULL,

    #' @field array A set of indexes specifying how many iterations of the job to
    #' run and how to identify them, e.g., "1-10".
    array = NULL,

    #' @field parent_jobs A vector of job names that are upstream of this job
    #' and influence its execution
    parent_jobs = NULL,

    #' @field wait_for_subs If TRUE, code will be inserted to wait for all jobs
    #' in a vector called \code{SUB_JOB_IDS} to finish before the batch exits.
    #' It's up to your code to use this variable name
    wait_for_subs = FALSE,

    #' @field all_subs_success If TRUE, all jobs in vector \code{SUB_JOB_IDS}
    #' must be successful for this job to finish (see \code{wait_for_subs} field)
    all_subs_success = FALSE,

    #' @field sqlite_db File path to job tracking SQLite database
    sqlite_db = NULL,

    #' @field scheduler_options An optional character vector of scheduler arguments
    #' to be included in the batch script header that control additional features
    #' such as job emails or group permissions. These directives are added with #SBATCH
    #' or #PBS headings, depending on the scheduler, and are ignored if the scheduler
    #' is "local".
    scheduler_options = NULL,

    #' @field r_packages The R packages to be loaded into the environment before
    #' job execution. These are loaded by pacman::p_load, which will install any
    #' missing packages before attempting to load
    r_packages = NULL,

    #' @field input_objects An environment containing all objects to be written
    #' to an RData object and passed to the batch job at execution
    input_objects = NULL,

    #' @field input_rdata_file The name of the environment to be loaded at the
    #' beginning of the R batch prior to executing other code. Used to setup any
    #' local objects needed to begin computation.
    input_rdata_file = NULL,

    #' @field shell_code Shell code to be included in the batch script prior to
    #' the R code to be run. This can include module load statements, environment
    #' variable exports, etc.
    shell_code = NULL,

    #' @field output_rdata_file The name of the environment to be saved at the
    #' end of the R batch execution, which can then be loaded by subsequent jobs.
    output_rdata_file = NULL,

    #' @field control.submit_job A named list with control variables used by
    #' \code{wait_for_job} when waiting for parent jobs to finish. Arguments include
    #' \code{repolling_interval}, which is the number of seconds to wait between
    #' successive checks on whether parent jobs have completed, and \code{max_wait}
    #' which is the maximum number of seconds to wait for parent jobs to finish
    #' before ending execution.
    control.submit_job = NULL,

    #' @field control.wait_for_subs A named list with control variables used by
    #' \code{wait_for_job} when waiting for sub jobs to finish before this job
    #' continues. See \code{control.submit_job} for details.
    control.wait_for_subs = NULL,

    #' @field print_session_info If TRUE, print the `sessionInfo()` and `Sys.info()`
    #' when the job starts. Useful for debugging problems with the compute
    #' environment or R installation.
    print_session_info = TRUE,

    #' @field print_environment If `TRUE`, print the `Sys.getenv()` when the job
    #' starts. This can produce a lot of output, but can be useful if certain
    #' environment variables are not being found when your job runs, leading it to fail.
    print_environment = FALSE,

    #' @description Create a new R_job object
    #' @details The scheduler-generated job ID is made available to your R
    #' script as \code{JOB_ID}.
    #' @param r_script The path to an R script to be executed by the batch.
    #' (Mutually exclusive with \code{r_code})
    #' @param r_code A character vector or expression containing R code to be executed.
    #' (Mutually exclusive with \code{r_script})
    #' @param job_name A character string. The name of the job used in dependency
    #' specification and job scheduler naming.
    #' @param job_dir  A character string. The path to the 'home' directory for this job.
    #' TODO: Say what will be stored here
    #' @param n_nodes A number or character string. The number of compute nodes
    #' to be requested on the scheduler.
    #' @param n_cores A number or character string. The number of CPUs to be
    #' requested on the scheduler.
    #' @param wall_time A number or character string. The compute time requested
    #' on the cluster using format "dd-HH:MM:SS".
    #' @param mem_total A number or character string. The total amount of memory
    #' to requested by the job.
    #' @param mem_per_cpu A number or character string. The amount of memory to
    #' be requested per CPU
    #' @param array An integer vector or character string. If specified, will run
    #' as many iterations as is the length of the vector, with each iteration being
    #' identified by that number, e.g., \code{c(1, 2, 3)} or "1-3" will iterate
    #' over the job three times. Schedulers also accept sequence notation, e.g.,
    #' "1-13:3" iterates from 1 to 13 by intervals of 3 (i.e., 1, 4, 7, 10, 13).
    #' The job array number will be made available to your R script as \code{ARRAY_ID}
    #' @param scheduler A character string. The scheduler to be used for this compute.
    #' Options are slurm' (or 'sbatch'), 'torque' (or 'qsub'), or 'local' (or 'sh').
    #' @param parent_jobs Numberical or character vector of one or more job ids
    #' that are parents of this job.
    #' @param wait_for_subs Logical. If \code{TRUE}, do not end this job until all
    #' sub-jobs have completed. Assign sub-job ids to \code{SUB_JOB_IDS} in your
    #' code. Default: \code{FALSE}
    #' @param all_subs_success If \code{TRUE}, don't count this job as successful
    #' unless all sub-jobs are successful. Default: \code{FALSE}
    #' @param post_subs_r_script The path to an R script to be executed by the batch
    #' after sub jobs have completed. Only relevant if \code{wait_for_subs = TRUE}
    #' (Mutually exclusive with \code{post_subs_r_code})
    #' @param post_subs_r_code A character vector or expression containing R code to be
    #' executed after sub jobs have completed. Only relevant if \code{wait_for_subs = TRUE}
    #' (Mutually exclusive with \code{post_subs_r_script})
    #' @param sqlite_db A character string. The location of the SQLite database
    #' to be used for job tracking. If `NULL`, job tracking will be disabled.
    #' TODO: If sqlite db provided doesn't exist, write to job_dir
    #' @param scheduler_options A character vector of scheduler options to be
    #' added to the header of the batch script
    #' @param r_packages A character vector of R packages to be loaded when
    #' compute script runs
    #' @param input_rdata_file A character string specifying the path to the
    #' environment to be loaded at the beginning of the R batch prior to executing code
    #' @param input_objects A list or environment in the current execution environment
    #' to be cached and used as input to the R batch. This is mutually exclusive
    #' with input_rdata_file at present.
    #' @param shell_code A character vector of shell code to be included in the batch
    #' script that submits the job
    #' @param output_rdata_file The name of the environment to be saved at the
    #' end of the R batch execution
    #' @param control.submit_job A named list with control variables used by
    #' \code{submit_job} when submitting this job, e.g., \code{repolling_interval},
    #' \code{max_wait}, and \code{wait_signal}. This argument defaults when
    #' \code{scheduler = "local"}. For expert use only.
    #' @param control.wait_for_subs A named list with control variables used by
    #' \code{wait_for_job} when waiting for sub jobs to finish. Arguments include
    #' \code{repolling_interval} and \code{max_wait}. This argument defaults when
    #' \code{wait_for_subs = TRUE}. For expert use only.
    #' @param print_session_info Logical. If \code{TRUE}, print information about
    #' the R environment `sessionInfo()` and compute environment `Sys.info()` when
    #' the job starts. Default: \code{TRUE}
    #' @param print_environment Logical. If \code{TRUE}, print the session environment
    #' via `Sys.getenv()` when the job starts. Default: \code{FALSE}.
    initialize = function(r_script = NULL, r_code = NULL, job_name = NULL,
                          job_dir = NULL, scheduler = NULL, n_nodes = NULL,
                          n_cores = NULL, wall_time = NULL, mem_total = NULL,
                          mem_per_cpu = NULL, array = NULL, parent_jobs = NULL,
                          wait_for_subs = FALSE, all_subs_success = FALSE,
                          post_subs_r_script = NULL, post_subs_r_code = NULL,
                          sqlite_db = NULL, scheduler_options = NULL,
                          r_packages = NULL, input_rdata_file = NULL,
                          input_objects = NULL, shell_code = NULL,
                          output_rdata_file = NULL, control.submit_job = NULL,
                          control.wait_for_subs = NULL, print_session_info = TRUE,
                          print_environment = FALSE
                          ) {
      # TODO: only need job dir if using sqlite db and out_rdata. Make job_dir active binding (with those other two)?
      if (!is.null(job_dir)) {
        assert_string(job_dir)
        if (!test_directory_exists(job_dir)) {
          cli_warn(
            c(
              "Job directory currently does not exist: {.path {job_dir}}",
              "i" = "Make sure it exists by the time of job submission to avoid errors!"
            )
          )
        }
        self$job_dir <- job_dir
      } else {
        self$job_dir <- getwd()
        cli_inform(
          "Defaulting job directory to your working directory: {self$job_dir}"
        )
      }

      if (is.null(r_script) & is.null(r_code)) {
        cli_abort(
          "Unable to initialize {.code R_job} object without {.code r_script} or {.code r_code}."
        )
      } else if (!is.null(r_script) & !is.null(r_code)) {
        cli_abort(
          "{.code r_script} and {.code r_code} are mutually exclusive!"
        )
      } else {
        if (!is.null(r_script)) {
          self$r_script <- r_script
        } else {
          self$r_code <- r_code
        }
      }

      if (!is.null(job_name)) {
        assert_string(job_name)
        self$job_name <- as.character(job_name)
      }
      # TODO: make EVERYTHING active bindings for verification checks (??)
      if (is.null(scheduler)) {
        cli_abort(
          "{.code scheduler} missing with no default"
        )
      } else {
        assert_subset(scheduler, c("torque", "qsub", "slurm", "sbatch", "sh", "local"))
        self$scheduler <- scheduler
      }

      if (is.null(n_nodes)) {
        cli_inform(
          "Using default number of nodes: {self$n_nodes}"
        )
      } else {
        n_nodes <- as.character(n_nodes)
        assert_character(n_nodes)
        self$n_nodes <- as.character(n_nodes)
      }

      if (is.null(n_cores)) {
        cli_inform(
          "Using default number of cores: {self$n_cores}"
        )
      } else {
        n_cores <- as.character(n_cores)
        assert_string(n_cores)
        self$n_cores <- as.character(n_cores)
      }

      if (is.null(wall_time)) {
        cli_inform(
          "Using default wall time: {self$wall_time}"
          )
      } else {
        assert_string(wall_time)
        self$wall_time <- as.character(wall_time)
      }
      # TODO: Default is mem_total but this is mutually exclusive with mem_per_cpu
      # If user provides mem_per_cpu but not mem_total, nullify mem_total. If user
      # provides both, throw error. Active bindings?
      if (is.null(mem_total)) {
        cli_inform(
          "Using default total memory: {self$mem_total}"
        )
      } else {
        assert_string(mem_total)
        self$mem_total <- mem_total
      }

      if (!is.null(mem_per_cpu)) {
        assert_string(mem_per_cpu)
        self$mem_per_cpu <- mem_per_cpu
      }

      if (!is.null(array)) {
        assert_multi_class(array, c("character", "numeric"))
        if (test_numeric(array)) {
          array <- paste(array, collapse = ",")
        }
        self$array <- array
      }

      if (!is.null(parent_jobs)) {
        parent_jobs <- as.character(parent_jobs)
        assert_character(parent_jobs)
        self$parent_jobs <- parent_jobs
      }

      if (!is.null(wait_for_subs)) {
        assert_logical(wait_for_subs, len = 1L)
        self$wait_for_subs <- wait_for_subs
      }

      if (!is.null(all_subs_success)) {
        assert_logical(all_subs_success, len = 1L)
        self$all_subs_success <- all_subs_success
      }

      if (!is.null(sqlite_db)) {
        assert_string(sqlite_db)
        if (!test_file_exists(sqlite_db)) {
          # ...
        }
        self$sqlite_db <- sqlite_db
      }

      if (!is.null(scheduler_options)) {
        assert_character(scheduler_options)
        self$scheduler_options <- scheduler_options
      }

      if (!is.null(r_packages)) {
        assert_character(r_packages)
        self$r_packages <- r_packages
      }

      if (!is.null(input_objects)) {
        if (!is.null(input_rdata_file)) {
          cli_abort(
            "Cannot specify both `input_rdata_file` and `input_objects` as inputs."
            )
        }
        assert_multi_class(input_objects, c("list", "environment"))
        if (test_list(input_objects)) {
          if (is.null(names(input_objects))) {
            cli_abort(
              "For list `input_objects` input, elements of the list must be named.")

          }
          # always convert to environment for type consistency
          input_objects <- as.environment(input_objects)
        }
        self$input_objects <- input_objects # store objects internally for output
        self$input_rdata_file <- "R_job_environment.RData" # TODO: put in job directory?
      }

      if (!is.null(input_rdata_file)) {
        assert_string(input_rdata_file)
        if (!test_file_exists(input_rdata_file)) {
          cli_warn(
            c(
              "Input RData file currently does not exist: {.path {input_rdata_file}}",
              "i" = "Make sure it exists by the time of job submission to avoid errors!"
            )
          )
        }
        self$input_rdata_file <- input_rdata_file
      }

      if (!is.null(shell_code)) {
        assert_character(shell_code)
        shell_code <- gsub("\n", "&&", shell_code, perl = TRUE)
        self$shell_code <- shell_code
      }

      if (!is.null(post_subs_r_script) & !is.null(post_subs_r_code)) {
        cli_abort(
          "{.code post_subs_r_script} and {.code post_subs_r_code} are mutually exclusive!"
        )
      } else if (!is.null(post_subs_r_script) | !is.null(post_subs_r_code)) {
        if (!is.null(post_subs_r_script)) {
          self$post_subs_r_script <- post_subs_r_script
        } else {
          self$post_subs_r_code <- post_subs_r_code
        }
      }

      if (!is.null(output_rdata_file)) {
        assert_character(output_rdata_file)
        self$output_rdata_file <- output_rdata_file
      }

      if (is.null(control.submit_job) & scheduler == "local") {
        self$control.submit_job <- list(
          repolling_interval = 60L,
          max_wait = 60 * 60 * 24
        )
      }
      if(!is.null(control.submit_job$sched_args)) {
        cli_warn(
          c("Scheduler arguments are already handled by {.code R_job}.",
            "Ignoring {.code control.submit_job$sched_args}")
        )
      }

      if (is.null(control.wait_for_subs) & isTRUE(wait_for_subs)) {
        self$control.wait_for_subs <- list(
          repolling_interval = 300L,
          max_wait = 60 * 60 * 24
        )
      }

      if (!is.null(print_session_info)) {
        assert_logical(print_session_info, len = 1L)
        self$print_session_info <- print_session_info
      }

      if (!is.null(print_environment)) {
        assert_logical(print_environment, len = 1L)
        self$print_environment <- print_environment
      }

    },

    #' @description Submit job to scheduler or local compute
    submit = function() {
      cd <- getwd(); setwd(self$job_dir)
      # make sure scripts or code exist, if applicable
      private$check()
      # write temporary files (if necessary)
      private$write_tmp()
      # print submission message
      job_name <- if (is.null(self$job_name)) { "(no name)" } else { self$job_name }
      cli_alert_info(
        "Submitting job: {job_name}"
      )
      # TODO: if a job_id already exists and submit is called again, do we check
      # job status, insist a 'forced' submission?
      private$job_id <- tryCatch({
        job_id <- submit_job(
          scheduler = self$scheduler,
          input = private$get_batch_file(self$scheduler),
          is_script = TRUE,
          echo = FALSE,
          fail_on_error = TRUE,
          wait_jobs = self$parent_jobs,
          env_variables = private$get_env_variables(),
          control = self$control.submit_job
        )
        return(job_id)
      },
      error = function(e) {
        # delete tmp files in case of error
        if (!is.null(private$tmp_r_script)) {
          try(unlink(private$tmp_r_script))
        }
        if (!is.null(private$tmp_post_subs_r_script)) {
          try(unlink(private$tmp_post_subs_r_script))
        }
        cli_abort(
          c("Job submission failed!",
          e$message)
        )
      })
      cli_alert_info(
        "Job received with job ID: {private$job_id}"
      )
      # reset working directory (don't attempt if that directory is absent)
      if (!is.null(cd) && dir.exists(cd)) setwd(cd)
      # return job id in case there are subsidiary scripts that depend on this
      return(private$job_id)
    },

    #' @description Create a deep copy of a batch job with minor changes
    #' @details This method exposes a few named parameters that can be used to override
    #' the copied fields with new values to avoid needing to change these one-by-one
    #' using obj$<field> <- x syntax
    #' @param r_script The path to an R script to be executed by the batch.
    #' (Mutually exclusive with \code{r_code})
    #' @param r_code A character vector or expression containing R code to be executed.
    #' (Mutually exclusive with \code{r_script})
    #' @param job_name The name of the job used in dependency specification and job
    #' scheduler naming
    #' @param n_nodes A number or character string. The number of compute nodes
    #' to be requested on the scheduler.
    #' @param n_cores A number or character string. The number of CPUs to be
    #' requested on the scheduler.
    #' @param wall_time A number or character string. The compute time requested
    #' on the cluster using format "dd-HH:MM:SS".
    #' @param mem_total A number or character string. The total amount of memory
    #' to requested by the job.
    #' @param mem_per_cpu A number or character string. The amount of memory to
    #' be requested per CPU
    copy = function(r_script = NULL, r_code = NULL, job_name = NULL, n_nodes = NULL,
                    n_cores = NULL, wall_time = NULL, mem_total = NULL, mem_per_cpu = NULL
                ) {
      cloned <- self$clone(deep = TRUE)
      if (!is.null(r_script) & !is.null(r_code)) {
        cli_abort(
          "{.code r_script} and {.code r_code} are mutually exclusive!"
        )
      } else {
        if (!is.null(r_script)) {
          cloned$r_script <- r_script
        } else {
          cloned$r_code <- r_code
        }
      }

      if (!is.null(job_name)) {
        assert_string(job_name)
        cloned$job_name <- as.character(job_name)
      }
      if (!is.null(n_nodes)) {
        n_nodes <- as.character(n_nodes)
        assert_character(n_nodes)
        cloned$n_nodes <- as.character(n_nodes)
      }

      if (!is.null(n_cores)) {
        n_cores <- as.character(n_cores)
        assert_string(n_cores)
        cloned$n_cores <- as.character(n_cores)
      }

      if (!is.null(wall_time)) {
        assert_string(wall_time)
        cloned$wall_time <- as.character(wall_time)
      }

      if (!is.null(mem_total)) {
        assert_string(mem_total)
        cloned$mem_total <- mem_total
      }
      if (!is.null(mem_per_cpu)) {
        assert_string(mem_per_cpu)
        cloned$mem_per_cpu <- mem_per_cpu
      }
      return(cloned)
    },

    #' @description Return the job id of this job (populated by job submission)
    get_job_id = function() {
      private$job_id
    }
  )
)
