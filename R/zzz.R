# From ggplot2:
rlang::on_load(
  if (getRversion() >= "4.3.0") {
    registerS3method("+", "hpcR::class_job", update_job)
  }
)

rlang::on_load(S7::methods_register())
.onLoad <- function(...) {
  rlang::run_on_load()
}
