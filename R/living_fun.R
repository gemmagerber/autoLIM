#' living_fun() part of prepack::multinets function
#' Grabs living nodes
#'
#' @param x an object?
#'
living_fun <- function(x) {
  row.i <-
    grep(
      rownames(x),
      pattern = "Input|Export|CO2",
      value = TRUE,
      invert = TRUE
    )
  living.vector <-
    !grepl(row.i, pattern = "NLNode", ignore.case = TRUE)
}
