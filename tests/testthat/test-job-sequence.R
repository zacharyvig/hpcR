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
  expect_setequal(edge_key(sequence@sequence_graph@edges), edges)
}

node_ids <- function(sequence) {
  names(sequence@sequence_graph@node_objects)
}

node_label_values <- function(sequence) {
  unname(sequence@sequence_graph@node_labels)
}

job_object_id <- function(job) {
  job@.metadata@object_id
}

test_that("%->% creates a sequence from two jobs", {
  job_a <- make_seq_job("a")
  job_b <- make_seq_job("b")

  seq <- job_a %->% job_b

  id_a <- job_a@.metadata@object_id
  id_b <- job_b@.metadata@object_id

  expect_true(is_job_sequence(seq))

  expect_setequal(node_ids(seq), c(id_a, id_b))
  expect_equal(seq@sequence_graph@start_nodes, id_a)
  expect_equal(seq@sequence_graph@end_nodes, id_b)

  expect_edges(seq, paste(id_a, id_b, sep = "->"))

  expect_identical(seq@sequence_graph@node_objects[[id_a]], job_a)
  expect_identical(seq@sequence_graph@node_objects[[id_b]], job_b)
  expect_equal(unname(seq@sequence_graph@node_labels[id_a]), "job_a")
  expect_equal(unname(seq@sequence_graph@node_labels[id_b]), "job_b")
})

test_that("%->% adds a job to an empty sequence", {
  job_a <- make_seq_job("a")

  seq <- job_sequence() %->% job_a

  id_a <- job_a@.metadata@object_id

  expect_true(is_job_sequence(seq))
  expect_setequal(node_ids(seq), id_a)
  expect_equal(seq@sequence_graph@start_nodes, id_a)
  expect_equal(seq@sequence_graph@end_nodes, id_a)
  expect_equal(nrow(seq@sequence_graph@edges), 0L)
  expect_equal(unname(seq@sequence_graph@node_labels[id_a]), "job_a")
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
  expect_equal(seq@sequence_graph@start_nodes, id_a)
  expect_equal(seq@sequence_graph@end_nodes, id_c)

  expect_edges(seq, c(
    paste(id_a, id_b, sep = "->"),
    paste(id_b, id_c, sep = "->")
  ))

  expect_equal(unname(seq@sequence_graph@node_labels[id_a]), "job_a")
  expect_equal(unname(seq@sequence_graph@node_labels[id_b]), "job_b")
  expect_equal(unname(seq@sequence_graph@node_labels[id_c]), "job_c")
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
  expect_equal(seq@sequence_graph@start_nodes, id_a)
  expect_equal(seq@sequence_graph@end_nodes, id_d)

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
  expect_equal(seq@sequence_graph@start_nodes, id_a)
  expect_equal(seq@sequence_graph@end_nodes, id_b)

  expect_edges(seq, paste(id_a, id_b, sep = "->"))
})

test_that("%->% handles empty sequence on right", {
  job_a <- make_seq_job("a")
  job_b <- make_seq_job("b")

  seq <- (job_a %->% job_b) %->% job_sequence()

  id_a <- job_a@.metadata@object_id
  id_b <- job_b@.metadata@object_id

  expect_setequal(node_ids(seq), c(id_a, id_b))
  expect_equal(seq@sequence_graph@start_nodes, id_a)
  expect_equal(seq@sequence_graph@end_nodes, id_b)

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
  expect_equal(seq@sequence_graph@start_nodes, id_a)
  expect_equal(seq@sequence_graph@end_nodes, id_c)

  expect_edges(seq, c(
    paste(id_a, id_b, sep = "->"),
    paste(id_b, id_c, sep = "->")
  ))

  expect_equal(unname(seq@sequence_graph@node_labels[id_a]), "job_a")
})

test_that("branch() returns a job sequence", {
  job_a <- make_seq_job("a")
  job_b <- make_seq_job("b")

  seq <- branch(job_a, job_b)

  id_a <- job_a@.metadata@object_id
  id_b <- job_b@.metadata@object_id

  expect_true(is_job_sequence(seq))
  expect_setequal(node_ids(seq), c(id_a, id_b))
  expect_setequal(seq@sequence_graph@start_nodes, c(id_a, id_b))
  expect_setequal(seq@sequence_graph@end_nodes, c(id_a, id_b))
  expect_equal(nrow(seq@sequence_graph@edges), 0L)

  expect_equal(unname(seq@sequence_graph@node_labels[id_a]), "job_a")
  expect_equal(unname(seq@sequence_graph@node_labels[id_b]), "job_b")
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
  expect_setequal(seq@sequence_graph@start_nodes, c(id_b, id_c))
  expect_setequal(seq@sequence_graph@end_nodes, c(id_e, id_c))

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
  expect_equal(seq@sequence_graph@start_nodes, id_a)
  expect_setequal(seq@sequence_graph@end_nodes, c(id_b, id_c))

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
  expect_equal(seq@sequence_graph@start_nodes, id_a)
  expect_setequal(seq@sequence_graph@end_nodes, c(id_e, id_c))

  expect_edges(seq, c(
    paste(id_a, id_b, sep = "->"),
    paste(id_b, id_d, sep = "->"),
    paste(id_d, id_e, sep = "->"),
    paste(id_a, id_c, sep = "->")
  ))

  # Non-serialization checks
  expect_false(paste(id_e, id_c, sep = "->") %in% edge_key(seq@sequence_graph@edges))
  expect_false(paste(id_c, id_b, sep = "->") %in% edge_key(seq@sequence_graph@edges))
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
  expect_equal(seq@sequence_graph@start_nodes, id_a)
  expect_equal(seq@sequence_graph@end_nodes, id_f)

  expect_edges(seq, c(
    paste(id_a, id_b, sep = "->"),
    paste(id_b, id_d, sep = "->"),
    paste(id_d, id_e, sep = "->"),
    paste(id_a, id_c, sep = "->"),
    paste(id_e, id_f, sep = "->"),
    paste(id_c, id_f, sep = "->")
  ))
})

test_that("branch() preserves topology for multiple multi-node branches", {
  job_a <- make_seq_job("a")
  job_b <- make_seq_job("b")
  job_c <- make_seq_job("c")
  job_d <- make_seq_job("d")

  out <- branch(
    job_a %->% job_b,
    job_c %->% job_d
  )

  id_a <- job_object_id(job_a)
  id_b <- job_object_id(job_b)
  id_c <- job_object_id(job_c)
  id_d <- job_object_id(job_d)

  expect_setequal(out@sequence_graph@start_nodes, c(id_a, id_c))
  expect_setequal(out@sequence_graph@end_nodes, c(id_b, id_d))

  expect_edges(out, c(
    paste(id_a, id_b, sep = "->"),
    paste(id_c, id_d, sep = "->")
  ))
})

test_that("branch can be added to an empty sequence", {
  job_a <- make_seq_job("a")
  job_b <- make_seq_job("b")

  seq <- job_sequence() %->% branch(job_a, job_b)

  id_a <- job_a@.metadata@object_id
  id_b <- job_b@.metadata@object_id

  expect_setequal(node_ids(seq), c(id_a, id_b))
  expect_setequal(seq@sequence_graph@start_nodes, c(id_a, id_b))
  expect_setequal(seq@sequence_graph@end_nodes, c(id_a, id_b))
  expect_equal(nrow(seq@sequence_graph@edges), 0L)
})

test_that("a sequence can start with a branch", {
  job_a <- make_seq_job("a")
  job_b <- make_seq_job("b")

  seq <- branch(job_a, job_b) %->% make_seq_job("c")

  id_a <- job_a@.metadata@object_id
  id_b <- job_b@.metadata@object_id
  id_c <- seq@sequence_graph@end_nodes

  expect_setequal(node_ids(seq), c(id_a, id_b, id_c))
  expect_setequal(seq@sequence_graph@start_nodes, c(id_a, id_b))
  expect_equal(seq@sequence_graph@end_nodes, id_c)

  expect_edges(seq, c(
    paste(id_a, id_c, sep = "->"),
    paste(id_b, id_c, sep = "->")
  ))
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
  expect_setequal(seq@sequence_graph@start_nodes, c(id_a, id_b))
  expect_setequal(seq@sequence_graph@end_nodes, c(id_c, id_d))

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

  graph1 <- class_pb_sequence_graph(
    node_objects = stats::setNames(list(job), id),
    node_labels = stats::setNames("", id),
    start_nodes = id,
    end_nodes = id
  )
  seq1 <- class_job_sequence(sequence_graph = graph1)

  graph2 <- class_pb_sequence_graph(
    node_objects = stats::setNames(list(job), id),
    node_labels = stats::setNames("job2", id),
    start_nodes = id,
    end_nodes = id
  )
  seq2 <- class_job_sequence(sequence_graph = graph2)

  expect_error(
    .check_duplicate_ids(sequences = list(seq1, seq2)),
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

test_that("job_sequence() returns a locked sequence", {
  seq <- job_sequence()

  expect_true(seq@.locked)
})

test_that("job_sequence() returns a locked populated sequence", {
  job_a <- make_seq_job("a")
  job_b <- make_seq_job("b")

  seq <- job_sequence(NULL, job_a, job_b)

  expect_true(seq@.locked)
})

test_that("job_sequence() creates an empty job sequence", {
  seq <- job_sequence()

  expect_true(is_job_sequence(seq))

  graph <- seq@sequence_graph

  expect_equal(length(graph@node_objects), 0L)
  expect_equal(length(graph@node_labels), 0L)
  expect_equal(nrow(graph@edges), 0L)
  expect_equal(graph@start_nodes, character(0))
  expect_equal(graph@end_nodes, character(0))
})

test_that("job_sequence() stores sequence_name when supplied", {
  seq <- job_sequence("my_sequence")

  expect_equal(seq@sequence_name, "my_sequence")
})

test_that("job_sequence() with jobs and no upstream_names treats all jobs as starts", {
  job_a <- make_seq_job("a")
  job_b <- make_seq_job("b")
  job_c <- make_seq_job("c")

  seq <- job_sequence(NULL, job_a, job_b, job_c)

  id_a <- job_object_id(job_a)
  id_b <- job_object_id(job_b)
  id_c <- job_object_id(job_c)

  expect_setequal(node_ids(seq), c(id_a, id_b, id_c))
  expect_setequal(seq@sequence_graph@start_nodes, c(id_a, id_b, id_c))
  expect_setequal(seq@sequence_graph@end_nodes, c(id_a, id_b, id_c))
  expect_equal(nrow(seq@sequence_graph@edges), 0L)

  expect_equal(unname(seq@sequence_graph@node_labels[id_a]), "job_a")
  expect_equal(unname(seq@sequence_graph@node_labels[id_b]), "job_b")
  expect_equal(unname(seq@sequence_graph@node_labels[id_c]), "job_c")
})

test_that("job_sequence() creates one edge from upstream_names", {
  job_a <- make_seq_job("a")

  job_b <- make_seq_job("b") +
    sequencing(upstream_names = "a")

  seq <- job_sequence(NULL, job_a, job_b)

  id_a <- job_object_id(job_a)
  id_b <- job_object_id(job_b)

  expect_setequal(node_ids(seq), c(id_a, id_b))
  expect_equal(seq@sequence_graph@start_nodes, id_a)
  expect_equal(seq@sequence_graph@end_nodes, id_b)

  expect_edges(
    seq,
    paste(id_a, id_b, sep = "->")
  )
})

test_that("job_sequence() builds a linear chain from upstream_names", {
  job_a <- make_seq_job("a")

  job_b <- make_seq_job("b") +
    sequencing(upstream_names = "a")

  job_c <- make_seq_job("c") +
    sequencing(upstream_names = "b")

  seq <- job_sequence(NULL, job_a, job_b, job_c)

  id_a <- job_object_id(job_a)
  id_b <- job_object_id(job_b)
  id_c <- job_object_id(job_c)

  expect_setequal(node_ids(seq), c(id_a, id_b, id_c))
  expect_equal(seq@sequence_graph@start_nodes, id_a)
  expect_equal(seq@sequence_graph@end_nodes, id_c)

  expect_edges(
    seq,
    c(
      paste(id_a, id_b, sep = "->"),
      paste(id_b, id_c, sep = "->")
    )
  )
})

test_that("job_sequence() supports one job depending on multiple upstream_names", {
  job_a <- make_seq_job("a")
  job_b <- make_seq_job("b")

  job_c <- make_seq_job("c") +
    sequencing(upstream_names = c("a", "b"))

  seq <- job_sequence(NULL, job_a, job_b, job_c)

  id_a <- job_object_id(job_a)
  id_b <- job_object_id(job_b)
  id_c <- job_object_id(job_c)

  expect_setequal(seq@sequence_graph@start_nodes, c(id_a, id_b))
  expect_equal(seq@sequence_graph@end_nodes, id_c)

  expect_edges(
    seq,
    c(
      paste(id_a, id_c, sep = "->"),
      paste(id_b, id_c, sep = "->")
    )
  )
})

test_that("job_sequence() supports branching via upstream_names", {
  job_a <- make_seq_job("a")

  job_b <- make_seq_job("b") +
    sequencing(upstream_names = "a")

  job_c <- make_seq_job("c") +
    sequencing(upstream_names = "a")

  seq <- job_sequence(NULL, job_a, job_b, job_c)

  id_a <- job_object_id(job_a)
  id_b <- job_object_id(job_b)
  id_c <- job_object_id(job_c)

  expect_equal(seq@sequence_graph@start_nodes, id_a)
  expect_setequal(seq@sequence_graph@end_nodes, c(id_b, id_c))

  expect_edges(
    seq,
    c(
      paste(id_a, id_b, sep = "->"),
      paste(id_a, id_c, sep = "->")
    )
  )
})

test_that("job_sequence() supports joins via upstream_names", {
  job_a <- make_seq_job("a")
  job_b <- make_seq_job("b")

  job_c <- make_seq_job("c") +
    sequencing(upstream_names = c("a", "b"))

  job_d <- make_seq_job("d") +
    sequencing(upstream_names = "c")

  seq <- job_sequence(NULL, job_a, job_b, job_c, job_d)

  id_a <- job_object_id(job_a)
  id_b <- job_object_id(job_b)
  id_c <- job_object_id(job_c)
  id_d <- job_object_id(job_d)

  expect_setequal(seq@sequence_graph@start_nodes, c(id_a, id_b))
  expect_equal(seq@sequence_graph@end_nodes, id_d)

  expect_edges(
    seq,
    c(
      paste(id_a, id_c, sep = "->"),
      paste(id_b, id_c, sep = "->"),
      paste(id_c, id_d, sep = "->")
    )
  )
})

test_that("job_sequence() resolves upstream_names regardless of argument order", {
  job_a <- make_seq_job("a")

  job_b <- make_seq_job("b") +
    sequencing(upstream_names = "a")

  seq <- job_sequence(NULL, job_b, job_a)

  id_a <- job_object_id(job_a)
  id_b <- job_object_id(job_b)

  expect_equal(seq@sequence_graph@start_nodes, id_a)
  expect_equal(seq@sequence_graph@end_nodes, id_b)

  expect_edges(
    seq,
    paste(id_a, id_b, sep = "->")
  )
})

test_that("job_sequence() stores expression labels in the graph", {
  job_a <- make_seq_job("a")
  job_b <- make_seq_job("b")

  seq <- job_sequence(NULL, job_a, job_b)

  id_a <- job_object_id(job_a)
  id_b <- job_object_id(job_b)

  expect_equal(unname(seq@sequence_graph@node_labels[id_a]), "job_a")
  expect_equal(unname(seq@sequence_graph@node_labels[id_b]), "job_b")
  expect_setequal(node_label_values(seq), c("job_a", "job_b"))
})

test_that("job_sequence() treats upstream_ids as external dependencies", {
  job_a <- make_seq_job("a") +
    sequencing(upstream_ids = "12345")

  expect_message(
    seq <- job_sequence(NULL, job_a),
    "Upstream scheduler IDs"
  )

  id_a <- job_object_id(job_a)

  expect_setequal(node_ids(seq), id_a)
  expect_equal(seq@sequence_graph@start_nodes, id_a)
  expect_equal(seq@sequence_graph@end_nodes, id_a)
  expect_equal(nrow(seq@sequence_graph@edges), 0L)
})

test_that("job_sequence() uses upstream_names for graph edges and ignores upstream_ids", {
  job_a <- make_seq_job("a")

  job_b <- make_seq_job("b") +
    sequencing(
      upstream_names = "a",
      upstream_ids = "12345"
    )

  expect_message(
    seq <- job_sequence(NULL, job_a, job_b),
    "Upstream scheduler IDs"
  )

  expect_setequal(
    node_ids(seq),
    c(job_object_id(job_a), job_object_id(job_b))
  )

  expect_edges(
    seq,
    paste(
      job_object_id(job_a),
      job_object_id(job_b),
      sep = "->"
    )
  )
})


test_that("sequencing() supports upstream_names without upstream_ids", {
  update <- sequencing(upstream_names = "a")

  expect_equal(
    update@updates$scheduler$sequencing$upstream_names,
    "a"
  )

  expect_equal(
    update@updates$scheduler$sequencing$upstream_ids,
    character(0)
  )
})

test_that("job_sequence() errors if ... contains non-job objects", {
  job_a <- make_seq_job("a")
  not_a_job <- "hello"

  expect_error(
    job_sequence(NULL, job_a, not_a_job),
    "must be job objects"
  )
})

test_that("job_sequence() errors if the same job object is supplied twice", {
  job <- make_seq_job("a")
  job2 <- job

  expect_error(
    job_sequence(NULL, job, job2),
    "same job|Some jobs are duplicated"
  )
})

test_that("job_sequence() duplicate job errors use argument labels", {
  job <- make_seq_job("a")
  job2 <- job

  expect_error(
    job_sequence(NULL, job, job2),
    "job.*job2.*same job"
  )
})

test_that("job_sequence() allows duplicate job names if upstream_names are not used", {
  job_a1 <- make_seq_job("a")
  job_a2 <- make_seq_job("a")

  seq <- job_sequence(NULL, job_a1, job_a2)

  expect_setequal(
    node_ids(seq),
    c(job_object_id(job_a1), job_object_id(job_a2))
  )

  expect_equal(nrow(seq@sequence_graph@edges), 0L)
})

test_that("job_sequence() errors on duplicate job names when upstream_names are used", {
  job_a1 <- make_seq_job("a")
  job_a2 <- make_seq_job("a")

  job_b <- make_seq_job("b") +
    sequencing(upstream_names = "a")

  expect_error(
    job_sequence(NULL, job_a1, job_a2, job_b),
    "Job names must be unique"
  )
})

test_that("job_sequence() errors when upstream_names reference missing jobs", {
  job_a <- make_seq_job("a")

  job_b <- make_seq_job("b") +
    sequencing(upstream_names = "missing_job")

  expect_error(
    job_sequence(NULL, job_a, job_b),
    "Invalid upstream name|Unknown upstream name"
  )
})

test_that("job_sequence() reports multiple invalid upstream_names for one job", {
  job_a <- make_seq_job("a")

  job_b <- make_seq_job("b") +
    sequencing(upstream_names = c("missing_1", "missing_2"))

  expect_error(
    job_sequence(NULL, job_a, job_b),
    "missing_1.*missing_2|missing_2.*missing_1"
  )
})

test_that("job_sequence() reports invalid upstream_names across multiple jobs", {
  job_a <- make_seq_job("a")

  job_b <- make_seq_job("b") +
    sequencing(upstream_names = "missing_b")

  job_c <- make_seq_job("c") +
    sequencing(upstream_names = "missing_c")

  expect_error(
    job_sequence(NULL, job_a, job_b, job_c),
    "missing_b.*missing_c|missing_c.*missing_b"
  )
})

test_that("job_sequence() errors when a job depends on itself by name", {
  job_a <- make_seq_job("a") +
    sequencing(upstream_names = "a")

  expect_error(
    job_sequence(NULL, job_a),
    "cannot depend on itself"
  )
})

test_that("job_sequence() allows unnamed jobs when they are not referenced", {
  job_a <- make_seq_job()
  job_b <- make_seq_job("b")

  seq <- job_sequence(NULL, job_a, job_b)

  expect_setequal(
    node_ids(seq),
    c(job_object_id(job_a), job_object_id(job_b))
  )

  expect_setequal(
    seq@sequence_graph@start_nodes,
    c(job_object_id(job_a), job_object_id(job_b))
  )

  expect_setequal(
    seq@sequence_graph@end_nodes,
    c(job_object_id(job_a), job_object_id(job_b))
  )

  expect_equal(nrow(seq@sequence_graph@edges), 0L)
})

test_that("job_sequence() stores a class_pb_sequence_graph", {
  job_a <- make_seq_job("a")
  job_b <- make_seq_job("b") +
    sequencing(upstream_names = "a")

  seq <- job_sequence(NULL, job_a, job_b)

  expect_true(S7::S7_inherits(seq@sequence_graph, class_pb_sequence_graph))
})

test_that("sequencing() creates a scheduler update for sequencing fields", {
  update <- sequencing(
    upstream_names = c("a", "b"),
    upstream_ids = c("123", "456")
  )

  expect_true(is_job_update(update))

  expect_equal(
    update@updates$scheduler$sequencing$upstream_names,
    c("a", "b")
  )

  expect_equal(
    update@updates$scheduler$sequencing$upstream_ids,
    c("123", "456")
  )
})

test_that("sequencing() defaults upstream fields to character(0)", {
  update <- sequencing()

  expect_true(is_job_update(update))

  expect_equal(
    update@updates$scheduler$sequencing$upstream_names,
    character(0)
  )

  expect_equal(
    update@updates$scheduler$sequencing$upstream_ids,
    character(0)
  )
})

test_that("sequencing() and %->% build the same sequence", {
  job_a <- make_seq_job("a")
  job_b <- make_seq_job("b") +
    sequencing(upstream_names = "a")

  expect_message(
    seq1 <- job_a %->% job_b,
    "Ignoring `upstream_names`"
  )
  seq2 <- job_sequence(NULL, job_a, job_b)

  expect_setequal(
    node_ids(seq1),
    node_ids(seq2)
  )

  expect_setequal(
    edge_key(seq1@sequence_graph@edges),
    edge_key(seq2@sequence_graph@edges)
  )

  expect_setequal(
    seq1@sequence_graph@start_nodes,
    seq2@sequence_graph@start_nodes
  )

  expect_setequal(
    seq1@sequence_graph@end_nodes,
    seq2@sequence_graph@end_nodes
  )
})

test_that("a named job_sequence can be extended with %->%", {
  job_a <- make_seq_job("a")

  job_b <- make_seq_job("b") +
    sequencing(upstream_names = "a")

  job_c <- make_seq_job("c")

  seq <- job_sequence("named_sequence", job_a, job_b) %->% job_c

  id_a <- job_object_id(job_a)
  id_b <- job_object_id(job_b)
  id_c <- job_object_id(job_c)

  expect_true(is_job_sequence(seq))
  expect_equal(seq@sequence_name, "named_sequence")

  expect_setequal(
    node_ids(seq),
    c(id_a, id_b, id_c)
  )

  expect_equal(seq@sequence_graph@start_nodes, id_a)
  expect_equal(seq@sequence_graph@end_nodes, id_c)

  expect_edges(
    seq,
    c(
      paste(id_a, id_b, sep = "->"),
      paste(id_b, id_c, sep = "->")
    )
  )
})

test_that("a job can precede a named job_sequence with %->%", {
  job_a <- make_seq_job("a")
  job_b <- make_seq_job("b")

  job_c <- make_seq_job("c") +
    sequencing(upstream_names = "b")

  named_seq <- job_sequence("named_sequence", job_b, job_c)

  seq <- job_a %->% named_seq

  id_a <- job_object_id(job_a)
  id_b <- job_object_id(job_b)
  id_c <- job_object_id(job_c)

  expect_true(is_job_sequence(seq))

  expect_setequal(
    node_ids(seq),
    c(id_a, id_b, id_c)
  )

  expect_equal(seq@sequence_graph@start_nodes, id_a)
  expect_equal(seq@sequence_graph@end_nodes, id_c)

  expect_edges(
    seq,
    c(
      paste(id_a, id_b, sep = "->"),
      paste(id_b, id_c, sep = "->")
    )
  )
})

test_that("two named job_sequences can be combined with %->%", {
  job_a <- make_seq_job("a")

  job_b <- make_seq_job("b") +
    sequencing(upstream_names = "a")

  job_c <- make_seq_job("c")

  job_d <- make_seq_job("d") +
    sequencing(upstream_names = "c")

  seq_left <- job_sequence("left", job_a, job_b)
  seq_right <- job_sequence("right", job_c, job_d)

  expect_warning(
    seq <- seq_left %->% seq_right,
    "Merging two job sequences with different names"
  )

  id_a <- job_object_id(job_a)
  id_b <- job_object_id(job_b)
  id_c <- job_object_id(job_c)
  id_d <- job_object_id(job_d)

  expect_true(is_job_sequence(seq))

  expect_setequal(
    node_ids(seq),
    c(id_a, id_b, id_c, id_d)
  )

  expect_equal(seq@sequence_graph@start_nodes, id_a)
  expect_equal(seq@sequence_graph@end_nodes, id_d)

  expect_edges(
    seq,
    c(
      paste(id_a, id_b, sep = "->"),
      paste(id_b, id_c, sep = "->"),
      paste(id_c, id_d, sep = "->")
    )
  )
})

test_that("an unnamed left sequence adopts the right sequence name", {
  job_a <- make_seq_job("a")
  job_b <- make_seq_job("b")

  lhs <- job_sequence(NULL, job_a)
  rhs <- job_sequence("right", job_b)

  out <- lhs %->% rhs

  expect_equal(out@sequence_name, "right")
})

test_that("a named left sequence retains its name when the right is unnamed", {
  job_a <- make_seq_job("a")
  job_b <- make_seq_job("b")

  lhs <- job_sequence("left", job_a)
  rhs <- job_sequence(NULL, job_b)

  out <- lhs %->% rhs

  expect_equal(out@sequence_name, "left")
})

test_that("a job appended to a named sequence follows all sequence end nodes", {
  job_a <- make_seq_job("a")

  job_b <- make_seq_job("b") +
    sequencing(upstream_names = "a")

  job_c <- make_seq_job("c") +
    sequencing(upstream_names = "a")

  job_d <- make_seq_job("d")

  seq <- job_sequence("branched", job_a, job_b, job_c) %->% job_d

  id_a <- job_object_id(job_a)
  id_b <- job_object_id(job_b)
  id_c <- job_object_id(job_c)
  id_d <- job_object_id(job_d)

  expect_setequal(
    seq@sequence_graph@start_nodes,
    id_a
  )

  expect_equal(
    seq@sequence_graph@end_nodes,
    id_d
  )

  expect_edges(
    seq,
    c(
      paste(id_a, id_b, sep = "->"),
      paste(id_a, id_c, sep = "->"),
      paste(id_b, id_d, sep = "->"),
      paste(id_c, id_d, sep = "->")
    )
  )
})

test_that("a job preceding a named sequence connects to all sequence starts", {
  job_a <- make_seq_job("a")

  job_b <- make_seq_job("b")
  job_c <- make_seq_job("c")

  named_seq <- job_sequence("parallel_starts", job_b, job_c)

  seq <- job_a %->% named_seq

  id_a <- job_object_id(job_a)
  id_b <- job_object_id(job_b)
  id_c <- job_object_id(job_c)

  expect_equal(
    seq@sequence_graph@start_nodes,
    id_a
  )

  expect_setequal(
    seq@sequence_graph@end_nodes,
    c(id_b, id_c)
  )

  expect_edges(
    seq,
    c(
      paste(id_a, id_b, sep = "->"),
      paste(id_a, id_c, sep = "->")
    )
  )
})

test_that("combining a named sequence with the same job object errors", {
  job_a <- make_seq_job("a")
  job_b <- make_seq_job("b")

  job_b_copy <- job_b

  seq <- job_sequence("named", job_a, job_b)

  expect_error(
    seq %->% job_b_copy,
    "same job|Some jobs are duplicated"
  )
})

test_that("any missing duplicate label uses the generic duplicate message", {
  job_a <- make_seq_job("a")
  job_b <- make_seq_job("b")

  id_a <- job_object_id(job_a)
  id_b <- job_object_id(job_b)

  graph1 <- class_pb_sequence_graph(
    node_objects = stats::setNames(list(job_a, job_b), c(id_a, id_b)),
    node_labels = stats::setNames(c("", "job_b"), c(id_a, id_b)),
    start_nodes = c(id_a, id_b),
    end_nodes = c(id_a, id_b)
  )

  graph2 <- class_pb_sequence_graph(
    node_objects = stats::setNames(list(job_a, job_b), c(id_a, id_b)),
    node_labels = stats::setNames(c("job_a2", "job_b2"), c(id_a, id_b)),
    start_nodes = c(id_a, id_b),
    end_nodes = c(id_a, id_b)
  )

  seq1 <- class_job_sequence(sequence_graph = graph1)
  seq2 <- class_job_sequence(sequence_graph = graph2)

  expect_error(
    .check_duplicate_ids(sequences = list(seq1, seq2)),
    "Some jobs are duplicated"
  )
})

test_that("duplicate upstream_names do not create duplicate edges", {
  job_a <- make_seq_job("a")

  job_b <- make_seq_job("b") +
    sequencing(upstream_names = c("a", "a"))

  seq <- job_sequence(NULL, job_a, job_b)

  id_a <- job_object_id(job_a)
  id_b <- job_object_id(job_b)

  expect_edges(seq, paste(id_a, id_b, sep = "->"))
  expect_equal(nrow(seq@sequence_graph@edges), 1L)
})

test_that("job_sequence() rejects cyclic name-based dependencies", {
  job_a <- make_seq_job("a") +
    sequencing(upstream_names = "b")

  job_b <- make_seq_job("b") +
    sequencing(upstream_names = "a")

  expect_error(
    job_sequence(NULL, job_a, job_b),
    "cycle|loop"
  )
})

test_that("job_sequence() rejects longer dependency cycles", {
  job_a <- make_seq_job("a") +
    sequencing(upstream_names = "c")

  job_b <- make_seq_job("b") +
    sequencing(upstream_names = "a")

  job_c <- make_seq_job("c") +
    sequencing(upstream_names = "b")

  expect_error(
    job_sequence(NULL, job_a, job_b, job_c),
    "cycle|loop"
  )
})

test_that(".is_dag() identifies acyclic and cyclic graphs", {
  acyclic <- data.frame(
    from = c("a", "b"),
    to = c("b", "c")
  )

  cyclic <- data.frame(
    from = c("a", "b", "c"),
    to = c("b", "c", "a")
  )

  expect_true(.is_dag(c("a", "b", "c"), acyclic))
  expect_false(.is_dag(c("a", "b", "c"), cyclic))
})

test_that("%->% informs when the RHS job has upstream_names", {
  job_a <- make_seq_job("a")

  job_b <- make_seq_job("b") +
    sequencing(upstream_names = "not_used")

  expect_message(
    job_a %->% job_b,
    "Ignoring.*upstream_names"
  )
})

test_that("%->% informs when the LHS job has upstream_names", {
  job_a <- make_seq_job("a") +
    sequencing(upstream_names = "not_used")

  job_b <- make_seq_job("b")

  expect_message(
    job_a %->% job_b,
    "Ignoring.*upstream_names"
  )
})

test_that("%->% informs when a job precedes a sequence", {
  job_a <- make_seq_job("a") +
    sequencing(upstream_names = "not_used")

  job_b <- make_seq_job("b")
  sequence <- job_sequence(NULL, job_b)

  expect_message(
    job_a %->% sequence,
    "Ignoring.*upstream_names"
  )
})