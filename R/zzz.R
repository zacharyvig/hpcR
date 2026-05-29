rlang::on_load(S7::methods_register())
.onLoad <- function(...) {
  rlang::run_on_load()
}
