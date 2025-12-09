#' Handle `variable=value` and `variable` combinations
#' @noRd
paste_args <- function(str_vec) {
  nms <- names(str_vec)
  sapply(seq_along(str_vec), function(x) {
    if (is.na(str_vec[x])) {
      # in case of NA just return name
      return(nms[x])
    } else {
      # else return name and quoted argument separated by equal sign
      val <- ifelse(grepl("^[\"'].*[\"']$", str_vec[x], perl = TRUE), str_vec[x], paste0("\"", str_vec[x], "\""))
      return(paste0(nms[x], "=", val))
    }
  })
}
