#' @export
`[.labeled.data.frame` <- function(x, ..., drop = FALSE) {
  x <- as_base_data_frame(x)
  atts <- get_all_lab_atts(x)
  x1 <- x[...]
  if (!is.data.frame(x1)) {
    x1 <- x[..., drop = drop]
  }
  if (is.data.frame(x1)) {
    x1 <- as_base_data_frame(x1)
    x1 <- add_lab_atts(x1, atts)
  }
  x1
}
