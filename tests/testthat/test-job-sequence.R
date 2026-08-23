# tests/testthat/test-job-sequences.R

make_seq_job <- function(name = NULL) {
  if (is.null(name)) {
    rjob()
  } else {
    rjob(name)
  }
}

edge_key <- function(x) {
  paste(x$from, x$to, sep = "->")
}

expect_edges <- function(sequence, edges) {
  expect_setequal(edge_key(sequence@edges), edges)
}

node_ids <- function(sequence) {
  names(sequence@node_objects)
}

node_label_values <- function(sequence) {
  unname(sequence@node_labels)
}

test_that("%->% creates a sequence from two jobs", {
  job_a <- make_seq_job("a")
  job_b <- make_seq_job("b")

  seq <- job_a %->% job_b

  id_a <- job_a@.metadata@object_id
  id_b <- job_b@.metadata@object_id

  expect_true(is_job_sequence(seq))

  expect_setequal(node_ids(seq), c(id_a, id_b))
  expect_equal(seq@start_nodes, id_a)
  expect_equal(seq@end_nodes, id_b)

  expect_edges(seq, paste(id_a, id_b, sep = "->"))

  expect_identical(seq@node_objects[[id_a]], job_a)
  expect_identical(seq@node_objects[[id_b]], job_b)
  expect_equal(unname(seq@node_labels[id_a]), "job_a")
  expect_equal(unname(seq@node_labels[id_b]), "job_b")
})

test_that("%->% adds a job to an empty sequence", {
  job_a <- make_seq_job("a")

  seq <- job_sequence() %->% job_a

  id_a <- job_a@.metadata@object_id

  expect_true(is_job_sequence(seq))
  expect_setequal(node_ids(seq), id_a)
  expect_equal(seq@start_nodes, id_a)
  expect_equal(seq@end_nodes, id_a)
  expect_equal(nrow(seq@edges), 0L)
  expect_equal(unname(seq@node_labels[id_a]), "job_a")
})

test_that("%->% chains jobs linearly", {
  job_a <- make_seq_job("a")
  job_b <- make_seq_job("b")
  job_c <- make_seq_job("c")

  seq <- job_sequence() %->% job_a %->% job_b %->% job_c

  id_a <- job_a@.metadata@object_id
  id_b <- job_b@.metadata@object_id
  id_c <- job_c@.metadata@object_id

  expect_setequal(node_ids(seq), c(id_a, id_b, id_c))
  expect_equal(seq@start_nodes, id_a)
  expect_equal(seq@end_nodes, id_c)

  expect_edges(seq, c(
    paste(id_a, id_b, sep = "->"),
    paste(id_b, id_c, sep = "->")
  ))

  expect_equal(unname(seq@node_labels[id_a]), "job_a")
  expect_equal(unname(seq@node_labels[id_b]), "job_b")
  expect_equal(unname(seq@node_labels[id_c]), "job_c")
})

test_that("%->% combines two linear sequences", {
  job_a <- make_seq_job("a")
  job_b <- make_seq_job("b")
  job_c <- make_seq_job("c")
  job_d <- make_seq_job("d")

  seq1 <- job_a %->% job_b
  seq2 <- job_c %->% job_d

  seq <- seq1 %->% seq2

  id_a <- job_a@.metadata@object_id
  id_b <- job_b@.metadata@object_id
  id_c <- job_c@.metadata@object_id
  id_d <- job_d@.metadata@object_id

  expect_setequal(node_ids(seq), c(id_a, id_b, id_c, id_d))
  expect_equal(seq@start_nodes, id_a)
  expect_equal(seq@end_nodes, id_d)

  expect_edges(seq, c(
    paste(id_a, id_b, sep = "->"),
    paste(id_b, id_c, sep = "->"),
    paste(id_c, id_d, sep = "->")
  ))
})

test_that("%->% handles empty sequence on left", {
  job_a <- make_seq_job("a")
  job_b <- make_seq_job("b")

  seq <- job_sequence() %->% (job_a %->% job_b)

  id_a <- job_a@.metadata@object_id
  id_b <- job_b@.metadata@object_id

  expect_setequal(node_ids(seq), c(id_a, id_b))
  expect_equal(seq@start_nodes, id_a)
  expect_equal(seq@end_nodes, id_b)

  expect_edges(seq, paste(id_a, id_b, sep = "->"))
})

test_that("%->% handles empty sequence on right", {
  job_a <- make_seq_job("a")
  job_b <- make_seq_job("b")

  seq <- (job_a %->% job_b) %->% job_sequence()

  id_a <- job_a@.metadata@object_id
  id_b <- job_b@.metadata@object_id

  expect_setequal(node_ids(seq), c(id_a, id_b))
  expect_equal(seq@start_nodes, id_a)
  expect_equal(seq@end_nodes, id_b)

  expect_edges(seq, paste(id_a, id_b, sep = "->"))
})

test_that("job %->% sequence works", {
  job_a <- make_seq_job("a")
  job_b <- make_seq_job("b")
  job_c <- make_seq_job("c")

  seq <- job_a %->% (job_b %->% job_c)

  id_a <- job_a@.metadata@object_id
  id_b <- job_b@.metadata@object_id
  id_c <- job_c@.metadata@object_id

  expect_setequal(node_ids(seq), c(id_a, id_b, id_c))
  expect_equal(seq@start_nodes, id_a)
  expect_equal(seq@end_nodes, id_c)

  expect_edges(seq, c(
    paste(id_a, id_b, sep = "->"),
    paste(id_b, id_c, sep = "->")
  ))

  expect_equal(unname(seq@node_labels[id_a]), "job_a")
})

test_that("branch() returns a job sequence", {
  job_a <- make_seq_job("a")
  job_b <- make_seq_job("b")

  seq <- branch(job_a, job_b)

  id_a <- job_a@.metadata@object_id
  id_b <- job_b@.metadata@object_id

  expect_true(is_job_sequence(seq))
  expect_setequal(node_ids(seq), c(id_a, id_b))
  expect_setequal(seq@start_nodes, c(id_a, id_b))
  expect_setequal(seq@end_nodes, c(id_a, id_b))
  expect_equal(nrow(seq@edges), 0L)

  expect_equal(unname(seq@node_labels[id_a]), "job_a")
  expect_equal(unname(seq@node_labels[id_b]), "job_b")
})

test_that("branch() accepts jobs and job sequences", {
  job_b <- make_seq_job("b")
  job_c <- make_seq_job("c")
  job_d <- make_seq_job("d")
  job_e <- make_seq_job("e")

  seq <- branch(
    job_b %->% job_d %->% job_e,
    job_c
  )

  id_b <- job_b@.metadata@object_id
  id_c <- job_c@.metadata@object_id
  id_d <- job_d@.metadata@object_id
  id_e <- job_e@.metadata@object_id

  expect_true(is_job_sequence(seq))
  expect_setequal(node_ids(seq), c(id_b, id_c, id_d, id_e))
  expect_setequal(seq@start_nodes, c(id_b, id_c))
  expect_setequal(seq@end_nodes, c(id_e, id_c))

  expect_edges(seq, c(
    paste(id_b, id_d, sep = "->"),
    paste(id_d, id_e, sep = "->")
  ))
})

test_that("job %->% branch() attaches job to all branch starts", {
  job_a <- make_seq_job("a")
  job_b <- make_seq_job("b")
  job_c <- make_seq_job("c")

  seq <- job_a %->% branch(job_b, job_c)

  id_a <- job_a@.metadata@object_id
  id_b <- job_b@.metadata@object_id
  id_c <- job_c@.metadata@object_id

  expect_true(is_job_sequence(seq))
  expect_setequal(node_ids(seq), c(id_a, id_b, id_c))
  expect_equal(seq@start_nodes, id_a)
  expect_setequal(seq@end_nodes, c(id_b, id_c))

  expect_edges(seq, c(
    paste(id_a, id_b, sep = "->"),
    paste(id_a, id_c, sep = "->")
  ))
})

test_that("sequence %->% branch() does not serialize branches", {
  job_a <- make_seq_job("a")
  job_b <- make_seq_job("b")
  job_c <- make_seq_job("c")
  job_d <- make_seq_job("d")
  job_e <- make_seq_job("e")

  seq <- job_sequence() %->%
    job_a %->%
    branch(
      job_b %->% job_d %->% job_e,
      job_c
    )

  id_a <- job_a@.metadata@object_id
  id_b <- job_b@.metadata@object_id
  id_c <- job_c@.metadata@object_id
  id_d <- job_d@.metadata@object_id
  id_e <- job_e@.metadata@object_id

  expect_setequal(node_ids(seq), c(id_a, id_b, id_c, id_d, id_e))
  expect_equal(seq@start_nodes, id_a)
  expect_setequal(seq@end_nodes, c(id_e, id_c))

  expect_edges(seq, c(
    paste(id_a, id_b, sep = "->"),
    paste(id_b, id_d, sep = "->"),
    paste(id_d, id_e, sep = "->"),
    paste(id_a, id_c, sep = "->")
  ))

  # Non-serialization checks
  expect_false(paste(id_e, id_c, sep = "->") %in% edge_key(seq@edges))
  expect_false(paste(id_c, id_b, sep = "->") %in% edge_key(seq@edges))
})

test_that("a job after a branch joins all branch end nodes", {
  job_a <- make_seq_job("a")
  job_b <- make_seq_job("b")
  job_c <- make_seq_job("c")
  job_d <- make_seq_job("d")
  job_e <- make_seq_job("e")
  job_f <- make_seq_job("f")

  seq <- job_sequence() %->%
    job_a %->%
    branch(
      job_b %->% job_d %->% job_e,
      job_c
    ) %->%
    job_f

  id_a <- job_a@.metadata@object_id
  id_b <- job_b@.metadata@object_id
  id_c <- job_c@.metadata@object_id
  id_d <- job_d@.metadata@object_id
  id_e <- job_e@.metadata@object_id
  id_f <- job_f@.metadata@object_id

  expect_setequal(node_ids(seq), c(id_a, id_b, id_c, id_d, id_e, id_f))
  expect_equal(seq@start_nodes, id_a)
  expect_equal(seq@end_nodes, id_f)

  expect_edges(seq, c(
    paste(id_a, id_b, sep = "->"),
    paste(id_b, id_d, sep = "->"),
    paste(id_d, id_e, sep = "->"),
    paste(id_a, id_c, sep = "->"),
    paste(id_e, id_f, sep = "->"),
    paste(id_c, id_f, sep = "->")
  ))
})

test_that("branch can be added to an empty sequence", {
  job_a <- make_seq_job("a")
  job_b <- make_seq_job("b")

  seq <- job_sequence() %->% branch(job_a, job_b)

  id_a <- job_a@.metadata@object_id
  id_b <- job_b@.metadata@object_id

  expect_setequal(node_ids(seq), c(id_a, id_b))
  expect_setequal(seq@start_nodes, c(id_a, id_b))
  expect_setequal(seq@end_nodes, c(id_a, id_b))
  expect_equal(nrow(seq@edges), 0L)
})

test_that("branch() %->% branch() creates all-to-all dependencies between branch layers", {
  job_a <- make_seq_job("a")
  job_b <- make_seq_job("b")
  job_c <- make_seq_job("c")
  job_d <- make_seq_job("d")

  seq <- branch(job_a, job_b) %->% branch(job_c, job_d)

  id_a <- job_a@.metadata@object_id
  id_b <- job_b@.metadata@object_id
  id_c <- job_c@.metadata@object_id
  id_d <- job_d@.metadata@object_id

  expect_setequal(node_ids(seq), c(id_a, id_b, id_c, id_d))
  expect_setequal(seq@start_nodes, c(id_a, id_b))
  expect_setequal(seq@end_nodes, c(id_c, id_d))

  expect_edges(seq, c(
    paste(id_a, id_c, sep = "->"),
    paste(id_a, id_d, sep = "->"),
    paste(id_b, id_c, sep = "->"),
    paste(id_b, id_d, sep = "->")
  ))
})

test_that("%->% errors when the same job object appears twice", {
  job <- make_seq_job("a")
  job2 <- job

  expect_error(
    job %->% job2,
    "same job"
  )
})

test_that("branch() errors when the same job object appears in multiple branch arms", {
  job <- make_seq_job("a")
  job2 <- job

  expect_error(
    branch(job, job2),
    "same job|Some jobs are duplicated"
  )
})

test_that("%->% errors when combining sequences with duplicate job objects", {
  job_a <- make_seq_job("a")
  job_b <- make_seq_job("b")

  seq1 <- job_a %->% job_b
  seq2 <- job_b %->% make_seq_job("c")

  expect_error(
    seq1 %->% seq2,
    "same job|Some jobs are duplicated"
  )
})

test_that("%->% errors when branch includes an upstream job object", {
  job_a <- make_seq_job("a")
  job_b <- make_seq_job("b")
  job_a2 <- job_a

  expect_error(
    job_a %->% branch(job_b, job_a2),
    "same job|Some jobs are duplicated"
  )
})

test_that("branch() errors on invalid input", {
  expect_error(
    branch("not a job"),
    "Invalid input"
  )
})

test_that("branch() errors with no inputs", {
  expect_error(
    branch(),
    "No jobs or sequences provided"
  )
})

test_that("duplicate object errors use expression labels when available", {
  job <- make_seq_job("a")
  job2 <- job

  expect_error(
    job %->% job2,
    "job.*job2.*same job"
  )
})

test_that("duplicate object errors in branch use expression labels when available", {
  job <- make_seq_job("a")
  job2 <- job

  expect_error(
    branch(job, job2),
    "job.*job2.*same job"
  )
})

test_that("duplicate object errors use all expression labels when multiple duplicates exist", {
  job <- make_seq_job("a")
  job2 <- job
  job3 <- job

  expect_error(
    branch(job, job2, job3),
    "job.*job2.*job3.*same job"
  )
})

test_that(".check_duplicate_ids() uses generic message when labels are missing", {
  # this is an edge case
  job <- make_seq_job("a")
  id <- job@.metadata@object_id

  seq1 <- class_job_sequence(
    node_objects = stats::setNames(list(job), id),
    node_labels = stats::setNames("", id),
    start_nodes = id,
    end_nodes = id
  )

  seq2 <- class_job_sequence(
    node_objects = stats::setNames(list(job), id),
    node_labels = stats::setNames("job2", id),
    start_nodes = id,
    end_nodes = id
  )

  expect_error(
    .check_duplicate_ids(seq1, seq2),
    "Some jobs are duplicated"
  )
})

test_that("job sequences are locked after construction", {
  job_a <- rjob("a")
  job_b <- rjob("b")

  seq <- job_a %->% job_b

  expect_true(seq@.locked)
})

test_that("job sequence lock state is restored after extension", {
  job_a <- rjob("a")
  job_b <- rjob("b")
  job_c <- rjob("c")

  seq <- job_a %->% job_b
  seq@.locked <- TRUE

  out <- seq %->% job_c

  expect_true(out@.locked)
})